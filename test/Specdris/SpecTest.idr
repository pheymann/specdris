module Specdris.SpecTest

import Specdris.Spec

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
                   1 === 2
                   1 === 1                  
        
               it "context 1.2" $ do
                 "hello" `shouldSatisfy` (\str => (length str) > 5)          
               it "context 1.3" $ do
                 1 `shouldBe` 2
               it "context 1.4" $ do
                 pendingWith "for some reason"
       
       testAndPrint state 4 3 1

export
specSuite : IO ()
specSuite = do putStrLn "\n  spec:"
               testCase
