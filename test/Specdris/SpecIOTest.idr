module Specdris.SpecIOTest

import Specdris.SpecIO

testAndPrint : SpecState -> (totalNum : Nat) -> (failed : Nat) -> (pending : Nat) -> IO ()
testAndPrint state totNum failedNum pendNum 
  = if (totalNum state) /= totNum || (failed state) /= failedNum || (pending state) /= pendNum then
      putStrLn "\n    [failed]"
    else
      putStrLn "\n    [success]"

testCase : IO ()
testCase
  = do state <- specWithState $ do
             describe "context 1" $ do
               describe "context 1.1" $ do
                 it "context 1.1.1" $ do
                   a <- pure 1
                   
                   pure $ do a === 2
                             a === 1                  
        
               it "context 1.2" $ do
                 pure $ "hello" `shouldSatisfy` (\str => (length str) > 5)          
               it "context 1.3" $ do
                 pure $ 1 `shouldBe` 2
               it "context 1.4" $ do
                 pure $ pendingWith "for some reason"
       
       testAndPrint state 4 3 1

withBeforeAndAfterAll : IO ()
withBeforeAndAfterAll
  = do state <- specWithState {beforeAll = putStrLn "beforeAll"} {afterAll = putStrLn "afterAll"} $ do
                  describe "context 1" $ do
                    it "context 1.1" $ do
                      pure $ 1 === 1

       testAndPrint state 1 0 0

around : IO SpecResult -> IO SpecResult
around spec = do putStrLn "before"
                 result <- spec
                 putStrLn "after"
                 
                 pure result

withAround : IO ()
withAround
  = do state <- specWithState {around = around} $ do
                  describe "context 1" $ do
                    it "context 1.1" $ do
                      putStrLn "      => expectation"
                      pure $ 1 === 1
       
       testAndPrint state 1 0 0

export
specSuite : IO ()
specSuite = do putStrLn "\n  spec-io:"
               testCase
               withBeforeAndAfterAll
               withAround
