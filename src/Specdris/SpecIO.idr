||| Central module use to import the Specdris test framework for
||| io code.
module Specdris.SpecIO

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
it : (description : String) -> IO SpecResult -> SpecAction
it descr spec = It descr spec

||| Empty `IO` effect
defaultIO : IO ()
defaultIO = pure ()

||| No `around` effect, just returns the given spec case
noAround : IO SpecResult -> IO SpecResult
noAround spec = spec

||| Executes a spec test and prints the result to the command line.
|||
||| @ beforeAll `IO` effect which will be executed before the spec test (optional)
||| @ afterAll  `IO` effect which will be executed after the spec test (optional)
||| @ around a function to perform effects before/after every spec case (optional)
specWithState : {default defaultIO beforeAll : IO ()} ->
                {default defaultIO afterAll : IO ()} ->
                {default noAround around : IO SpecResult -> IO SpecResult} ->

                SpecAction -> 
                IO SpecState
specWithState {beforeAll} {around} actions {afterAll}
  = do beforeAll
       state <- evaluate around actions
       afterAll
       
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
|||
||| @ beforeAll `IO` effect which will be executed before the spec test (optional)
||| @ afterAll  `IO` effect which will be executed after the spec test (optional)
||| @ around a function to perform effects before/after every spec case (optional)
spec : {default defaultIO beforeAll : IO ()} ->
       {default defaultIO afterAll : IO ()} ->
       {default noAround around : IO SpecResult -> IO SpecResult} ->
       
       SpecAction ->
       IO ()
spec {beforeAll} {around} actions {afterAll}
  = do specWithState {beforeAll = beforeAll} {afterAll = afterAll} {around = around} actions
       pure ()
