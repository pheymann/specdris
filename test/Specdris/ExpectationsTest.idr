module Specdris.ExpectationsTest

import Specdris.Data.SpecInfo
import Specdris.Data.SpecResult
import Specdris.Data.SpecState

import Specdris.Core
import Specdris.Expectations

import Specdris.TestUtil

%access private
%default total

test : (description : String) -> SpecResult -> SpecTree
test descr spec = Node (Leaf $ Left $ Describe descr)
                       (Leaf $ Right $ pure spec)

showTestCase : String -> String
showTestCase test = "expectation_" ++ test

noAround : IO SpecResult -> IO SpecResult
noAround spec = spec

testPending : IO ()
testPending
  = do state1 <- evaluate noAround False $ test (showTestCase "pending") $ pending
       state2 <- evaluate noAround False $ test (showTestCase "pendingWith") $ pendingWith "test"

       testAndPrint "pending" state1 (MkState 1 0 1 Nothing) (==)
       testAndPrint "pendingWith" state2 (MkState 1 0 1 Nothing) (==)

testEqual : IO ()
testEqual
  = do state1 <- evaluate noAround False $ test (showTestCase "shouldBe") $ 1 `shouldBe` 1
       state2 <- evaluate noAround False $ test (showTestCase "===") $ 1 === 2
       
       testAndPrint "equal" state1 (MkState 1 0 0 Nothing) (==)
       testAndPrint "===" state2 (MkState 1 1 0 Nothing) (==)

testUnequal : IO ()
testUnequal
  = do state1 <- evaluate noAround False $ test (showTestCase "shouldNotBe") $ 1 `shouldNotBe` 2
       state2 <- evaluate noAround False $ test (showTestCase "/==") $ 1 /== 1
  
       testAndPrint "unequal" state1 (MkState 1 0 0 Nothing) (==)
       testAndPrint "/==" state2 (MkState 1 1 0 Nothing) (==)

testSatisfy : IO ()
testSatisfy
  = do state1 <- evaluate noAround False $ test (showTestCase "sat") $ 1 `shouldSatisfy` (> 0)
       state2 <- evaluate noAround False $ test (showTestCase "!sat") $ 1 `shouldSatisfy` (> 1)
        
       testAndPrint "satisfy" state1 (MkState 1 0 0 Nothing) (==)
       testAndPrint "satisfy" state2 (MkState 1 1 0 Nothing) (==)

export
specSuite : IO ()
specSuite = do putStrLn "\n  expectations:"
               testPending
               testEqual
               testUnequal
               testSatisfy
