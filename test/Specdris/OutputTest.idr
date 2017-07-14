module Specdris.OutputTest

import Specdris.Spec
import Specdris.TestUtil

expected : List String
expected = ["\ESC[37m  context 1\ESC[0m", 
            "\ESC[37m    context 1.1\ESC[0m", 
            "\ESC[37m      + context 1.1.1\ESC[0m", 
            "\ESC[31m         [x] not equal\n             actual:   1\n             expected: 2\ESC[0m", 
            "\ESC[37m    + context 1.2\ESC[0m", 
            "\ESC[31m       [x] not equal\n           actual:   1\n           expected: 2\ESC[0m", 
            "\ESC[37m    + context 1.3\ESC[0m", 
            "\ESC[33m       [] pending: for some reason\ESC[0m"]

testCase : IO ()
testCase
  = do state <- specWithState {storeOutput = True} $ do
             describe "context 1" $ do
               describe "context 1.1" $ do
                 it "context 1.1.1" $ do
                   1 === 2        
               it "context 1.2" $ do
                 1 `shouldBe` 2
               it "context 1.3" $ do
                 pendingWith "for some reason"
       
       testAndPrint "spec output" (output state) (Just expected) (==)

export
specSuite : IO ()
specSuite = do putStrLn "\n  output:"
               testCase
