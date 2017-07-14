module Specdris.SpecIOTest

import Specdris.SpecIO
import Specdris.TestUtil

testCase : IO ()
testCase
  = do state <- specIOWithState $ do
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
       
       testAndPrint "spec io test" state (MkState 4 3 1 Nothing) (==)

withBeforeAndAfterAll : IO ()
withBeforeAndAfterAll
  = do state <- specIOWithState {beforeAll = putStrLn "beforeAll"} {afterAll = putStrLn "afterAll"} $ do
                  describe "context 1" $ do
                    it "context 1.1" $ do
                      pure $ 1 === 1

       testAndPrint "spec io test with before and after all" state (MkState 1 0 0 Nothing) (==)
       

around : IO SpecResult -> IO SpecResult
around spec = do putStrLn "before"
                 result <- spec
                 putStrLn "after"
                 
                 pure result

withAround : IO ()
withAround
  = do state <- specIOWithState {around = around} $ do
                  describe "context 1" $ do
                    it "context 1.1" $ do
                      putStrLn "      => expectation"
                      pure $ 1 === 1
       
       testAndPrint "spec io test with around" state (MkState 1 0 0 Nothing) (==)

export
specSuite : IO ()
specSuite = do putStrLn "\n  spec-io:"
               testCase
               withBeforeAndAfterAll
               withAround
