module Main

import SpecExample
import SpecIOExample

main : JS_IO ()
main = do
  SpecExample.specSuite
  SpecIOExample.specSuite
