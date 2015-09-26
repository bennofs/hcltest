hcltest  [![Build Status](https://secure.travis-ci.org/bennofs/hcltest.png?branch=master)](http://travis-ci.org/bennofs/hcltest)
====================

hcltest is a library for testing command-line applications. 

### Usage

The library provides integration with tasty. Here's an example of a simple test using the library together with tasty:

```haskell
{-# LANGUAGE OverloadedStrings #-} -- we enable OverloadedStrings because hcltest uses text
module Main where

import Test.Tasty
import Test.Tasty.HClTest -- this is the library
import System.Exit (ExitCode(ExitSuccess))

main :: IO ()
main = defaultMain $ testGroup "main"
  [ hcltest "true is successful" (testExitCode Nothing Nothing 1000 "true" [] ExitSuccess) 
  , hcltest "cat works" $ testInteractive Nothing Nothing 1000 "cat" [] $ do
      send "Hey!"
      expect Stdout "Hey!"
      return ExitSuccess
  ]
```

Here, we created a tasty test case with the `hcltest` function. The first argument of that
function specifies the name of the test case. In the second argument, you can then perform
hcltest actions, like `testExitCode` or `testInterative`. See the haddocks for the details
of the arguments to these functions.
