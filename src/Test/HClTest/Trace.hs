module Test.HClTest.Trace
  ( Trace(..)
  , showTrace
  ) where

data Trace = Trace
  { stepDescription :: String
  , messages        :: [String]
  }
  
showTrace :: Trace -> String
showTrace (Trace desc msgs) = concat $ header : msgs
  where header = "::: " ++ desc ++ "\n"
