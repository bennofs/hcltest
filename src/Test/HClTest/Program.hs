{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
-- | In this module there are functions for creating test cases that run
-- programs. It also provides functions for running programs that require input.
module Test.HClTest.Program
  ( Stream(..)
  , Driver(), runDriver
  , expect
  , expectEOF
  , send
  , testInteractive
  , testStdout
  , testExitCode
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Trans.Either
import           Control.Monad.Writer
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           System.Exit
import           System.IO
import           System.Process
import           System.Timeout
import           Test.HClTest.Monad
import           Test.HClTest.Trace

-- | A output stream. 
data Stream = Stdout | Stderr deriving Show

-- | This is the functor from which the free monad Driver is generated. 
-- It is an enumeration of possible primitive operations possible in the Driver monad.
data DriverF a = MatchStream Stream T.Text a
               | SendInput T.Text a
               | ExpectEOF Stream a
 deriving Functor

-- | The driver monad. The driver monad is used to run programs that require input. It allows
-- you to specify a "script" of actions, like "send input" or "expect output".
type Driver = Free DriverF

-- | Send some text to the process. The text will be encoded as UTF-8.
send :: T.Text -> Driver ()
send = liftF . flip SendInput ()

-- | Check that the process outputs the given text on the given output stream. This only
-- matches a prefix, so it also succeeds if the process produces more output. If you want
-- to check that this is the only output, use expectEOF.
expect :: Stream -> T.Text -> Driver ()
expect s = liftF . flip (MatchStream s) ()

-- | Check that the process' output ended.
expectEOF :: Stream -> Driver ()
expectEOF = liftF . flip ExpectEOF ()
 
-- | Run a driver. The first argument is the timeout for waiting for output of the process.
-- The second argument are handles to stdin, stdout and stderr of the process. The third
-- argument is the driver to run. This produces a test step.
runDriver :: Int -> (Handle, Handle, Handle) -> Driver a -> HClTest String a
runDriver time (stdinH,stdoutH,stderrH) = iterM interpret
  where interpret :: DriverF (HClTest String a) -> HClTest String a
        interpret (SendInput str next) = do
          liftIO $ do
            BS.hPut stdinH $ T.encodeUtf8 str
            hFlush stdinH
          traceMsg $ T.unpack $ ">>> " <> str
          next

        interpret (MatchStream s str next) = do
          let enc = T.encodeUtf8 str
          i <- liftIO $ tryGetTimeout (streamH s) time $ BS.length enc
          case i of
            Left (eof,part)
              | eof       -> matchFailure str part "<EOF>" "Output was too short."
              | otherwise -> matchFailure str part "<Timeout>" "Response timount exceeded"
            Right full    -> do
              unless (T.decodeUtf8 full == str) $ matchFailure str full "" "Output didn't match"
              traceMsg $ T.unpack $ T.decodeUtf8 full
              return ()
          next

        interpret (ExpectEOF s next) = do
          eof <- liftIO $ hIsEOF (streamH s)
          unless eof $ failTest $ unlines
            [ "- Output too long -"
            , "Stream: " ++ show s
            ]
          next

        streamH :: Stream -> Handle
        streamH Stderr = stderrH
        streamH Stdout = stdoutH

        matchFailure :: T.Text -> ByteString -> T.Text -> T.Text -> HClTest String a
        matchFailure ex got e desc = failTest $ T.unpack $ T.unlines 
          [ "- Match failure -"
          , "Expected: " <> ex
          , "Got: " <> T.decodeUtf8 got <> e
          , desc
          ]

-- | Try to read a number of bytes from the given handle, but fail if a timeout is reached.
-- The second argument is the timeout, the third is the number of bytes to read.
tryGetTimeout :: Handle -> Int -> Int -> IO (Either (Bool,BS.ByteString) BS.ByteString)
tryGetTimeout h time m = runEitherT $ do
  eof <- lift (hIsEOF h)
  when eof $ left (True,BS.empty)
  
  hasInput <- lift $ hWaitForInput h time
  unless hasInput $ left (False,BS.empty)
  
  inp <- lift $ BS.hGetNonBlocking h m
  if BS.length inp < m
    then do
      n <- lift $ tryGetTimeout h time $ m - BS.length inp
      either left right $ n & _Left._2 %~ mappend inp
    else return inp

-- | Read all available data from a handle. The first argument is a timeout for waiting for output. 
-- If the process outputs nothing for more than timeout milliseconds, that is considered end of output.
hReadAvailable :: Int -> Handle -> IO BS.ByteString
hReadAvailable time h = do
  eof <- hIsEOF h
  if eof
    then return BS.empty
    else do
      hasInput <- hWaitForInput h time
      if hasInput
        then do
          c <- BS.hGetNonBlocking h 1024
          mappend c <$> hReadAvailable time h
        else return BS.empty

-- | Make a test step for an interactive program. The first argument is either the working directory
-- or Nothing, which doesn't change the working directory. The second argument is the timeout in seconds 
-- for waiting for output of the process. The third argument is the executable file. The forth argument
-- are the arguments for the executable and the fifth is the driver to use. The driver should return
-- the expected exit code.
testInteractive :: Maybe FilePath -> Int -> FilePath -> [String] -> Driver ExitCode -> HClTest Trace ()
testInteractive wd time prog args driver = do

  let cmdline = prog ++ " " ++ unwords args
  (Just stdinH, Just stdoutH, Just stderrH, p) <- liftIO $ createProcess (proc prog args)
                                              { std_in = CreatePipe
                                              , std_out = CreatePipe
                                              , std_err = CreatePipe
                                              , cwd = wd
                                              }

  testStep ("Run command :: " ++ cmdline ++ maybe "" (\x -> " [WD: " ++ x ++ "]") wd) $ do 

    exitCode <- runDriver time (stdinH,stdoutH,stderrH) driver

    liftIO $ hClose stdinH
    out <- liftIO $ hReadAvailable time stdoutH
    err <- liftIO $ hReadAvailable time stderrH
    traceMsg $ T.unpack $ T.decodeUtf8 out
    traceMsg $ T.unpack $ T.decodeUtf8 err
  
    exitCode' <- liftIO $ timeout (time * 1000) $ waitForProcess p
    liftIO $ when (isNothing exitCode') $ terminateProcess p    
    
    case exitCode' of
      Nothing -> failTest "- Process didn't exit -\n"
      Just exitCode'' -> 
        unless (exitCode'' == exitCode) $ failTest $ unlines
          [ "- Exit code didn't match - "
          , "Expected: " ++ show exitCode
          , "Got: " ++ show exitCode''
          ]

    return ()

-- | A restricted form of testInteractive that Only tests that the process produces the given output on stderr, and no more. See 
-- 'testInteractive' for a description of the arguments.
testStdout :: Maybe FilePath -> Int -> FilePath -> [String] -> ExitCode -> T.Text -> HClTest Trace ()
testStdout wd time prog args exit out = testInteractive wd time prog args $ exit <$ expect Stdout out <* expectEOF Stdout

-- | A restricted form of testInteractive that only tests that the process exits with the given exit code.
-- See 'testInteractive' for a description of the arguments.
testExitCode :: Maybe FilePath -> Int -> FilePath -> [String] -> ExitCode -> HClTest Trace ()
testExitCode wd time prog args = testInteractive wd time prog args . return
