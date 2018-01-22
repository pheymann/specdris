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
describe: (description : String) -> SpecTree' ffi -> SpecTree' ffi
describe descr tree = Node (Leaf $ Left $ Describe descr)
                           (tree)

||| Adds a spec case to the spec test. Spec cases consist only
||| of expectations. Nested spec cases or descriptions are not
||| allowed.
it : (description : String) -> IO' ffi SpecResult -> SpecTree' ffi
it descr spec = Node (Leaf $ Left $ It  descr)
                     (Leaf $ Right spec)

||| Empty `IO` effect
defaultIO : IO' ffi ()
defaultIO = pure ()

||| No `around` effect, just returns the given spec case
noAround : IO' ffi SpecResult -> IO' ffi SpecResult
noAround spec = spec

||| Executes a spec test.
|||
||| @ beforeAll `IO` effect which will be executed before the spec test (optional)
||| @ afterAll  `IO` effect which will be executed after the spec test (optional)
||| @ around a function to perform effects before/after every spec case (optional)
specIOWithState : {default defaultIO beforeAll : IO' ffi ()} ->
                  {default defaultIO afterAll : IO' ffi ()} ->
                  {default noAround around : IO' ffi SpecResult -> IO' ffi SpecResult} ->
                  {default False storeOutput : Bool} ->
                
                  SpecTree' ffi ->
                  IO' ffi SpecState
specIOWithState {beforeAll} {around} tree {afterAll} {storeOutput}
  = do beforeAll
       state <- evaluate around storeOutput tree
       afterAll

       pure state

||| Executes a spec test and prints the result.
|||
||| @ beforeAll `IO' ffi` effect which will be executed before the spec test (optional)
||| @ afterAll  `IO' ffi` effect which will be executed after the spec test (optional)
||| @ around a function to perform effects before/after every spec case (optional)
specIO' : {default defaultIO beforeAll : IO' ffi ()} ->
          {default defaultIO afterAll : IO' ffi ()} ->
          {default noAround around : IO' ffi SpecResult -> IO' ffi SpecResult} ->
          {default False storeOutput : Bool} ->
       
          SpecTree' ffi ->
          IO' ffi ()
specIO' {beforeAll} {around} tree {afterAll} {storeOutput}
  = do state <- specIOWithState {beforeAll = beforeAll} {afterAll = afterAll} {around = around} {storeOutput = storeOutput} tree
       putStrLn' $ "\n" ++ stateToStr state

||| Executes a spec test, prints the result to the command line, and exits
||| with an error code if any tests have failed.
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
    
       putStrLn $ "\n" ++ stateToStr state
       
       if (failed state) > 0 then
         exitFailure
       else
         pure ()
