{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.HClTest.Test where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Writer
import qualified Data.DList as DL
import           Data.Foldable (for_)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO.Temp
import           System.Process
import           Test.HClTest.Driver
import           Test.Tasty.Providers

data TestCase = TestCase
                { driver        :: Driver ExitCode
                , arguments     :: [String]
                , program       :: FilePath
                }

data HClTest = HClTest
            { maxResponseTime :: Int
            , tests :: [TestCase]
            , inputDirectory :: Maybe FilePath
            }

data TestResult = ExitCodeE ExitCode ExitCode
                | MatchE    MatchFailure
                | HangE
                | Success

evalTestCase :: Int -> TestCase -> IO (DL.DList Trace, TestResult)
evalTestCase time t = do
  (Just stdinH,Just stdoutH,Just stderrH,p) <- createProcess $ (proc (program t) (arguments t))
    { std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    }
  (r,trac) <- runWriterT $ runEitherT $ runDriver time (stdinH, stdoutH, stderrH) $ driver t
  fmap ((,) trac) $ case r of
    Right e -> do
      e' <- getProcessExitCode p
      case e' of
        Nothing -> do
          terminateProcess p
          return HangE
        Just e'' -> return $ if e'' == e then Success else ExitCodeE e e''
    Left failure -> return $ MatchE failure

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM f = flip foldM ([],[]) $ \(a,b) x -> do
  v <- f x
  return $ if v then (x:a,b) else (a,x:b)

copyFiles :: FilePath -> FilePath -> IO ()
copyFiles source target = do
  (files,dirs) <- partitionM doesFileExist =<< getDirectoryContents source
  let dirs' = filter (not . (`elem` [".",".."])) dirs
  for_ files $ \file -> copyFile file $ target </> file
  for_ dirs' $ \dir  -> do
    createDirectory $ target </> dir
    copyFiles dir $ target </> dir

evalHClTest :: (Progress -> IO ()) -> HClTest -> IO ([(String, DL.DList Trace)], TestResult)
evalHClTest status (HClTest time ts inp) =
  withSystemTempDirectory "hcltest" $ \tmp -> do
    case inp of
      Nothing -> return ()
      Just inp' -> do
        status $ Progress ("Copying files from " ++ inp' ++ " to " ++ tmp) 0.2
        copyFiles inp' tmp
    go ts 0
  where go :: [TestCase] -> Int -> IO ([(String, DL.DList Trace)], TestResult)
        go [] _ = return ([], Success)
        go (a:as) !i = do
          let command = program a ++ " " ++ unwords (arguments a)
          status $ Progress ("Running test command " ++ show i ++ ": " ++ command) $ inc * fromIntegral i + 0.2
          r <- evalTestCase time a
          case r of
            (t,Success) -> fmap (first ((command, t):)) $ go as $ succ i
            (t,r')       -> return ([(command, t)], r')

        inc = 0.8 / fromIntegral (length ts)
