-- | A Trace is a log entry for a single test step.
module Test.HClTest.Trace
  ( Trace(..)
  , showTrace
  ) where

-- | A trace has a step description and some messages produced by that step in it.
data Trace = Trace
  { stepDescription :: String
  , messages        :: [String]
  }
 
-- | Pretty print a trace. 
showTrace :: Trace -> String
showTrace (Trace desc msgs) = concat $ header : msgs
  where header = "::: " ++ desc ++ "\n"
