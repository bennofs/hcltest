{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- | This module defines the basic test type, HClTest, which is a monad. It also provides functions
-- for creating and running tests.
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
  , timeoutFactor
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
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
import           System.IO.Temp
import           System.Random.Shuffle
import           Test.HClTest.Trace

-- | The config is passed in a Reader to the test cases.
data Config = Config
  { _timeoutFactor :: Double
  }
makeLenses ''Config

-- | The HClTest monad. A HClTest action describes a single test case. The first argument is the type
-- of the trace entries. For tests, this should be 'Trace'. For a single test step, this should be 'String'.
newtype HClTest w a = HClTest { unHClTest :: ReaderT Config (MaybeT (WriterT (DL.DList w) IO)) a } deriving (Functor, Applicative, Monad, MonadIO, MonadPlus, Alternative, MonadReader Config)

instance MonadBase IO (HClTest w) where
  liftBase = liftIO

instance MonadBaseControl IO (HClTest w) where
  data StM (HClTest w) a = HClTestSt { unHClTestSt :: StM (ReaderT Config (MaybeT (WriterT (DL.DList w) IO))) a }
  liftBaseWith f = HClTest $ liftBaseWith (\k -> f (fmap HClTestSt . k . unHClTest ))
  restoreM = HClTest . restoreM . unHClTestSt

-- | Run a HClTest. The first argument is the timeout for waiting for output
-- of the process, in milliseconds. The second argument is the test case.
--
-- This will run the test in a temporary working directory. Use the functions
-- in Test.HClTest.Setup to setup the environment.
--
-- Returns True when the test succeeded, False otherwise.
runHClTestTrace :: Double -> HClTest Trace () -> IO (Bool, DL.DList Trace)
runHClTestTrace tf (HClTest a) = runWriterT $ do

  pwd <- liftIO getCurrentDirectory
  tmp <- liftIO $ getTemporaryDirectory >>= flip createTempDirectory "hcltest"
  liftIO $ setCurrentDirectory tmp
  tell $ pure $ Trace "Change to temporary directory" ["Working directory is now: " ++ tmp ++ "\n"]

  s <- has _Just <$> runMaybeT (runReaderT a $ Config tf)

  when s $ liftIO $ removeDirectoryRecursive tmp
  tell $ pure $ Trace (if s then "Removed temporary directory" else "Temporary directory not removed") []

  liftIO $ setCurrentDirectory pwd
  return s

-- | Like runHClTestTrace, but already shows the trace so that you get a string.
runHClTest :: Double -> HClTest Trace () -> IO (Bool,String)
runHClTest tf a = runHClTestTrace tf a & mapped._2 %~ unlines . map showTrace . DL.toList

-- | This is a HClTest action that always fails. The first argument is the trace to leave.
-- If you want to fail without leaving a trace, you can just use 'mzero'.
failTest :: a -> HClTest a b
failTest x = traceMsg x *> HClTest mzero

-- | Add a message to the log.
traceMsg :: a -> HClTest a ()
traceMsg = HClTest . tell . pure

-- | Run an IO action, and fail if that action returns false. The first argument
-- is a description of the IO action which will be used for the trace messages.
testIO :: String -> IO Bool -> HClTest Trace ()
testIO desc action = testStep ("Test :: " ++ desc) $ do
  success <- liftIO action
  unless success $ failTest "Failed"

-- | A single test step. The first argument is a description of the step. The test step
-- can produce trace messages of type 'String'. Those will be collected an exactly one
-- 'Trace' will be emitted.
testStep :: String -> HClTest String a -> HClTest Trace a
testStep desc (HClTest action) = HClTest $ hoist (hoist k) action
  where k :: (Functor m, Monad m) => WriterT (DL.DList String) m a -> WriterT (DL.DList Trace) m a
        k a = do
          (b,w) <- lift $ runWriterT a
          b <$ tell (pure $ Trace desc $ DL.toList w)

-- | Run a number of tests in parallel, in random order. The first argument is the number of threads
-- to use.
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

  liftIO $ void $ atomically $ readTVar nfinishedVar >>= check . (== 0)
  (success, trac) <- liftIO $ readTVarIO resultVar

  HClTest $ tell trac
  guard $ getAll success
