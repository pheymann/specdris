||| Central module use to import the Specdris test framework for
||| io code.
module Specdris.SpecIO

import System

import public Specdris.Data.SpecState
import public Specdris.Data.SpecResult
import public Specdris.Data.SpecInfo

import public Specdris.Core
import public Specdris.Expectations

import Specdris.Data.ConsoleColor

%access export
%default total

||| Adds a context/description to the spec test. It can have
||| nested descriptions or spec cases.
describe: (description : String) -> SpecTree -> SpecTree
describe descr tree = Node (Leaf $ Left $ Describe descr)
                           (tree)

||| Adds a spec case to the spec test. Spec cases consist only
||| of expectations. Nested spec cases or descriptions are not
||| allowed.
it : (description : String) -> IO SpecResult -> SpecTree
it descr spec = Node (Leaf $ Left $ It  descr)
                     (Leaf $ Right spec)

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
specIOWithState : {default defaultIO beforeAll : IO ()} ->
                  {default defaultIO afterAll : IO ()} ->
                  {default noAround around : IO SpecResult -> IO SpecResult} ->
                  {default False storeOutput : Bool} ->
                
                  SpecTree -> 
                  IO SpecState
specIOWithState {beforeAll} {around} tree {afterAll} {storeOutput}
  = do beforeAll
       state <- evaluate around storeOutput tree
       afterAll
       
       putStrLn $ "\n" ++ stateToStr state
       pure state
  where
    stateToStr : SpecState -> String
    stateToStr state
      = colorise (if failed state == 0 then Green else Red) $
          indent 1
            ++ (if failed state == 0 then "Passed" else "Failed") ++ ": "
            ++ show state

||| Executes a spec test and prints the result to the command line.
|||
||| @ beforeAll `IO` effect which will be executed before the spec test (optional)
||| @ afterAll  `IO` effect which will be executed after the spec test (optional)
||| @ around a function to perform effects before/after every spec case (optional)
specIO : {default defaultIO beforeAll : IO ()} ->
         {default defaultIO afterAll : IO ()} ->
         {default noAround around : IO SpecResult -> IO SpecResult} ->
         {default False storeOutput : Bool} ->
       
         SpecTree ->
         IO ()
specIO {beforeAll} {around} tree {afterAll} {storeOutput}
  = do state <- specIOWithState {beforeAll = beforeAll} {afterAll = afterAll} {around = around} {storeOutput = storeOutput} tree
       if (failed state) > 0 then
         exitFailure
       else
         pure ()
