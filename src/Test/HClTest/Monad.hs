{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Test.HClTest.Monad
  ( HClTest(..)
  , Config(..)
  , runHClTest
  , runHClTestTrace
  , failTest
  , testStep
  , traceMsg
  , testIO
  , randomParallel
  , withWorkingDirectory
  , timeoutFactor
  , wdLock
  ) where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Concurrent.RLock as RLock
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import qualified Data.DList as DL
import           Data.Foldable (for_)
import           Data.List
import           Data.List.Split
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           System.Random.Shuffle
import           Test.HClTest.Trace

data Config = Config 
  { _wdLock        :: RLock.RLock
  , _timeoutFactor :: Double
  }
makeLenses ''Config

newtype HClTest w a = HClTest { unHClTest :: ReaderT Config (MaybeT (WriterT (DL.DList w) IO)) a } deriving (Functor, Applicative, Monad, MonadIO, MonadPlus, Alternative, MonadReader Config)

instance MonadBase IO (HClTest w) where
  liftBase = liftIO

instance MonadBaseControl IO (HClTest w) where
  data StM (HClTest w) a = HClTestSt { unHClTestSt :: StM (ReaderT Config (MaybeT (WriterT (DL.DList w) IO))) a }
  liftBaseWith f = HClTest $ liftBaseWith (\k -> f (fmap HClTestSt . k . unHClTest ))
  restoreM = HClTest . restoreM . unHClTestSt
  
runHClTestTrace :: Double -> HClTest Trace () -> IO (Bool, DL.DList Trace)
runHClTestTrace tf (HClTest a) = runWriterT $ do

  pwd <- liftIO getCurrentDirectory
  tmp <- liftIO $ getTemporaryDirectory >>= flip createTempDirectory "hcltest"
  liftIO $ setCurrentDirectory tmp
  tell $ pure $ Trace "Change to temporary directory" ["Working directory is now: " ++ tmp ++ "\n"]

  wdLockVar <- liftIO RLock.new
  s <- has _Just <$> runMaybeT (runReaderT a $ Config wdLockVar tf)
  
  when s $ liftIO $ removeDirectoryRecursive tmp
  tell $ pure $ Trace (if s then "Removed temporary directory" else "Temporary directory not removed") []

  liftIO $ setCurrentDirectory pwd
  return s

runHClTest :: Double -> HClTest Trace () -> IO (Bool,String)
runHClTest tf a = runHClTestTrace tf a & mapped._2 %~ unlines . map showTrace . DL.toList
  
failTest :: a -> HClTest a b
failTest x = traceMsg x *> HClTest mzero

traceMsg :: a -> HClTest a ()
traceMsg = HClTest . tell . pure

testIO :: String -> IO Bool -> HClTest Trace ()
testIO desc action = testStep ("Test :: " ++ desc) $ do
  success <- liftIO action
  unless success $ failTest "Failed" 

testStep :: String -> HClTest String a -> HClTest Trace a
testStep desc (HClTest action) = HClTest $ hoist (hoist k) action
  where k :: (Functor m, Monad m) => WriterT (DL.DList String) m a -> WriterT (DL.DList Trace) m a
        k a = do
          (b,w) <- lift $ runWriterT a
          b <$ tell (pure $ Trace desc $ DL.toList w)

randomParallel :: Int -> [HClTest Trace ()] -> HClTest Trace ()
randomParallel n tests = do

  testsShuffled <- liftIO $ shuffleM tests
  let workLoads = transpose $ chunksOf n testsShuffled

  settings <- ask

  resultVar <- liftIO $ newTVarIO (mempty,mempty)
  nfinishedVar <- liftIO $ newTVarIO n
  liftIO $ for_ workLoads $ \workLoad -> forkIO $ do
    void $ runMaybeT $ for_ workLoad $ \test -> do
      (c,_) <- liftIO $ readTVarIO resultVar
      guard $ getAll c           
      (s,t) <- liftIO $ runWriterT $ fmap (has _Just) $ runMaybeT $ flip runReaderT settings $ unHClTest test
      liftIO $ atomically $ modifyTVar' resultVar (<> (All s,t))
    liftIO $ atomically $ modifyTVar' nfinishedVar pred
  
  liftIO $ atomically $ readTVar nfinishedVar >>= check . (== 0)
  (success, trac) <- liftIO $ readTVarIO resultVar
 
  HClTest $ tell trac
  guard $ getAll success  

withWorkingDirectory :: FilePath -> HClTest Trace a -> HClTest Trace a
withWorkingDirectory path a = do
  pwd <- liftIO getCurrentDirectory
  lock <- view wdLock
  liftIO $ RLock.acquire lock
  liftIO $ setCurrentDirectory path
  traceMsg $ Trace "Change working directory" ["Working directory is now: " ++ show (pwd </> path) ++ "\n"]
  a `finally` liftIO (setCurrentDirectory pwd >> RLock.release lock)
