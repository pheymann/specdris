module Specdris.SpecTest

import Specdris.Spec
import Specdris.TestUtil

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
       
       testAndPrint "spec test" state (MkState 4 3 1 Nothing) (==)

export
specSuite : IO ()
specSuite = do putStrLn "\n  spec:"
               testCase
