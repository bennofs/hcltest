{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides support for running hcltest cases using tasty.
module Test.Tasty.HClTest 
  ( hcltest
  , module X
  ) where

import Data.Proxy
import Data.Typeable
import Options.Applicative
import Test.HClTest as X
import Test.Tasty.Options
import Test.Tasty.Providers

newtype HClTasty = HClTasty (HClTest Trace ()) deriving Typeable

-- | Factor to apply to the test timeout. 
newtype HClTestTimeoutFactor = HClTestTimeoutFactor Double deriving (Typeable, Ord, Num, Eq, Real)
instance IsOption HClTestTimeoutFactor where
  defaultValue = 1
  parseValue = fmap HClTestTimeoutFactor . safeRead
  optionName = return "hcltest-timeout-factor"
  optionHelp = return "If you set this value, all timeouts specified by the tests will get multiplied by it.\
                      \This is useful to run tests made for a faster computer on a slower computer."

newtype HClTestSuccessLog = HClTestSuccessLog Bool deriving (Typeable)
instance IsOption HClTestSuccessLog where
  defaultValue = HClTestSuccessLog False
  parseValue = const $ return $ HClTestSuccessLog True
  optionName = return "hcltest-success-log"
  optionHelp = return "Also print the log when the test succeeded"
  optionCLParser = HClTestSuccessLog <$> switch (long "hcltest-success-log" <> help "Also print the log when the test succeeded")
  
instance IsTest HClTasty where
  testOptions = return
    [ Option (Proxy :: Proxy HClTestTimeoutFactor)
    , Option (Proxy :: Proxy HClTestSuccessLog)
    ]

  run opts (HClTasty t) _ = toResult <$> runHClTest factor t
    where HClTestTimeoutFactor factor = lookupOption opts
          HClTestSuccessLog    sl     = lookupOption opts
          toResult (True,l) = testPassed $ if sl then l else ""
          toResult (False,l) = testFailed l

-- | Make a new test case with the given name using a HClTest for testing.
hcltest :: TestName -> HClTest Trace () -> TestTree
hcltest n = singleTest n . HClTasty
