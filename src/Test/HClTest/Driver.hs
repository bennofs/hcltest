{-# LANGUAGE DeriveFunctor #-}
module Test.HClTest.Driver 
  ( Stream(..)
  , Trace(..)
  , FailureReason(..)
  , MatchFailure(..)
  , Driver(), runDriver
  , putBytes, putText
  , expectBytes, expectText
  ) where

import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Writer
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.DList as DL
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           System.IO

data DriverF a = MatchStream Stream ByteString a
               | SendInput ByteString a deriving Functor

type Driver = Free DriverF

data Stream = Stderr | Stdout deriving Show

data Trace = Sent ByteString
           | Accepted Stream ByteString
  deriving Show

data FailureReason = Timeout | EOF | Mismatch deriving Show

data MatchFailure = MatchFailure
  { expected :: ByteString
  , got      :: ByteString
  , reason   :: FailureReason
  } deriving Show

putBytes :: ByteString -> Driver ()
putBytes = liftF . flip SendInput ()

putText :: T.Text -> Driver ()
putText = putBytes . T.encodeUtf8

expectBytes :: Stream -> ByteString -> Driver ()
expectBytes s = liftF . flip (MatchStream s) ()

expectText :: Stream -> T.Text -> Driver ()
expectText s = expectBytes s . T.encodeUtf8

runDriver :: Int -> (Handle, Handle, Handle) -> Driver a -> EitherT MatchFailure (WriterT (DL.DList Trace) IO) a
runDriver time (stdinH,stdoutH,stderrH) = iterM interpret
  where interpret (SendInput str next) = do
          lift . lift $ do
            BS.hPut stdinH str
            hFlush stdinH
          lift $ tell $ return $ Sent str
          next
        interpret (MatchStream s str next) = do
          i <- lift . lift $ tryGetTimeout (streamH s) time $ BS.length str
          case i of
            Left (eof,part) -> left $ MatchFailure str part $ if eof then EOF else Timeout
            Right full -> do
              unless (full == str) $ left $ MatchFailure str full Mismatch
              lift $ tell $ return $ Accepted s full
          next

        streamH :: Stream -> Handle
        streamH Stderr = stderrH
        streamH Stdout = stdoutH

tryGetTimeout :: Handle -> Int -> Int -> IO (Either (Bool,BS.ByteString) BS.ByteString)
tryGetTimeout h time m = runEitherT $ do
  lift (hIsEOF h) >>= flip when (left (True,BS.empty))
  hasInput <- lift $ hWaitForInput h time
  unless hasInput $ left (False,BS.empty)
  inp <- lift $ BS.hGetNonBlocking h m
  if BS.length inp < m
    then EitherT $ fmap (liftM2 either (fmap Left . fmap) (fmap Right) $ mappend inp) $ tryGetTimeout h time $ m - BS.length inp
    else return inp
