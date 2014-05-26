hcltest
=======

[![Build Status](https://secure.travis-ci.org/bennofs/hcltest.png?branch=master)](http://travis-ci.org/bennofs/hcltest)

HClTest is a library for testing command line applications. It allows to test interactive command line applications
as well as non-interactive applications and integrates with the tasty testing framework.

Usage
-----

To use HClTest, first import it. You also need tasty and the `System.Exit` module, which provides the `ExitCode` datatype:

```haskell
import Test.Tasty
import Test.Tasty.HClTest
import System.Exit
```

You can then use the `hcltest` function to create a tasty test case that uses HClTest:

```haskell
main :: IO ()
main = defaultMain $ testGroup "foo"
  [ hcltest $ do
      testExitCode Nothing Nothing 1000 "true" [] ExitSuccess
	  testExitCode Nothing Nothing 1000 "false" [] $ ExitFailure 1
  ]
```

Here, `testExitCode` takes 6 arguments:

- The first argument is the working directory relative to the current directory, or Nothing if
  you want to keep the current working directory as-is. Note that the current directory wíll be a
  temporary directory created afresh for each test run. So you have to copy the data that you need
  for your test yourself. You can use the functions from `Test.HClTest.Setup` for that.
- The second argument specifies the environment variables, or Nothing to inherit the environment.
- The third argument is a timeout so that the test case doesn't keep running forever when a program hungs up.
- The forth argument specifies the program to run.
- The fifth argument specifies the options that the program is given
- and the sixth argument is the expected exit code.

If you want to learn more, take a look at the [hackage documentation](http://hackage.haskell.org/hcltest).

Contributing
------------

If you have any problems, bug reports or feature requests, feel free to open an issue on github. Contributions
are always welcome!
