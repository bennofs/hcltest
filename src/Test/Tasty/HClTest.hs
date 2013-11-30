{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Tasty.HClTest 
  (
  ) where

import           Data.ByteString (ByteString)
import qualified Data.DList as DL
import           Data.List
import           Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Typeable
import           Test.HClTest
import           Test.Tasty.Options
import           Test.Tasty.Providers

newtype HClTasty = HClTasty HClTest deriving Typeable

-- | Factor to apply to the test timeout. 
newtype HClTestTimeoutFactor = HClTestTimeoutFactor Float deriving (Typeable, Ord, Num, Eq, Real)

instance IsOption HClTestTimeoutFactor where
  defaultValue = 1
  parseValue = fmap HClTestTimeoutFactor . safeRead
  optionName = return "hcltest-timeout-factor"
  optionHelp = return "If you set this value, all timeouts specified by the tests will get multiplied by it.\
                      \This is useful to run tests made for a faster computer on a slower computer."

instance IsTest HClTasty where
  testOptions = return
    [ Option (Proxy :: Proxy HClTestTimeoutFactor)
    ]

  run opts (HClTasty t) status = fmap toTastyResult $ evalHClTest status $ t {maxResponseTime = truncate $ fromIntegral (maxResponseTime t) * factor}
    where HClTestTimeoutFactor factor = lookupOption opts

toTastyResult :: ([(String, DL.DList Trace)], TestResult) -> Result
toTastyResult (_, Success) = Result True ""
toTastyResult (t, HangE)   = Result False $ "The program passed all assertions, but didn't exit.\n" ++ showTrace t
toTastyResult (t, ExitCodeE e e') = Result False $ "The exitcode didn't match. Expected " ++ show e ++ " but got " ++ show e' ++ "\n" ++ showTrace t
toTastyResult (t, MatchE m) = Result False $ case reason m of
  EOF -> "Output of the program was too short. " ++ expectedMsg ++ "<EOF>\n" ++ showTrace t
  Timeout -> "The program didn't respond within the timeout. " ++ expectedMsg ++ "<Timeout>\n" ++ showTrace t
  Mismatch -> "The output of the program didn't match. " ++ expectedMsg ++ "\n" ++ showTrace t
  where expectedMsg = "Expected " ++ bs2str (expected m) ++ " but got " ++ bs2str (got m)

bs2str :: ByteString -> String
bs2str = T.unpack . T.decodeUtf8

showTrace :: [(String, DL.DList Trace)] -> String
showTrace t = "Trace (>>> marks input, # is stderr):\n" ++ intercalate "\n" (map showProgramTrace t)
  where showProgramTrace (prog,trac) = "Command: " ++ prog ++ "\n" ++ unlines (DL.toList $ DL.map showSingleTrace trac)
        showSingleTrace (Sent bs)   = ">>> " ++ bs2str bs
        showSingleTrace (Accepted Stdout bs) = bs2str bs
        showSingleTrace (Accepted Stderr bs) = "# " ++ bs2str bs
