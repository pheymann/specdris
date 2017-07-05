||| Central module use to import the Specdris test framework
module Specdris.Spec

import Specdris.Console

import public Specdris.Data.SpecAction
import public Specdris.Data.SpecState
import public Specdris.Data.SpecResult

import public Specdris.Core
import public Specdris.Expectations

%access export
%default total

||| Adds a context/description to the spec test. It can have
||| nested descriptions or spec cases.
describe: (description : String) -> SpecAction -> SpecAction
describe descr actions = do (Describe descr)
                            actions

||| Adds a spec case to the spec test. Spec cases consist only
||| of expectations. Nested spec cases or descriptions are not
||| allowed.
it : (description : String) -> SpecResult -> SpecAction
it descr spec = It descr (pure spec)

||| Executes a spec test and prints the result to the command line.
specWithState : SpecAction -> IO SpecState
specWithState actions
  = do state <- evaluate (\spec => spec) actions
       
       putStrLn (stateToStr state)       
       pure state
  where
    stateToStr : SpecState -> String
    stateToStr state
      = colorise (if failed state == 0 then Green else Red) $
          "\n" 
            ++ indent 1
            ++ (if failed state == 0 then "Passed" else "Failed") ++ ": "
            ++ show state

||| Executes a spec test and prints the result to the command line.
spec : SpecAction -> IO ()
spec actions
  = do specWithState actions
       pure ()
