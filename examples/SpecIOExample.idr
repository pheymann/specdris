module SpecIOExample

import Specdris.SpecIO

%access private
%default total

startDb : IO ()
startDb = putStrLn "start database ..."

stopDb : IO ()
stopDb = putStrLn "... stoped database"

testData : IO SpecResult -> IO SpecResult
testData resultIO = do putStrLn "insert test data"
                       result <- resultIO
                       putStrLn "delete test data"
                       
                       pure result

record User where
  constructor MkUser
  
  id : Nat
  name : String

getUser : Nat -> IO User
getUser id = pure (MkUser id "foo")

getUserSpec : SpecTree
getUserSpec = describe "User table" $ do
                it "get a user by id" $ do
                  user <- getUser 0
                  
                  pure $ do (name user) === "foo"

export
specSuite : IO ()
specSuite = specIO {beforeAll = startDb} {around = testData} {afterAll = stopDb} $ do 
              getUserSpec
