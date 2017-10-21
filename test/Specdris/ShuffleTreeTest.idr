module Specdris.ShuffleTreeTest

import Specdris.Spec

import Specdris.TestUtil

%access private

Eq SpecTree where
  (==) (Node l0 r0) (Node l1 r1) = (l0 == l1) && (r0 == r1)
  (==) (Leaf (Left (Describe lMsg))) (Leaf (Left (Describe rMsg))) = lMsg == rMsg
  (==) (Leaf (Left (It lMsg))) (Leaf (Left (It rMsg))) = lMsg == rMsg
  (==) (Leaf (Right _)) (Leaf (Right _)) = True
  (==) _ _ = False

Show SpecTree where
  show (Node l r) = "node(" ++ show l ++ " , " ++ show r ++ ")"
  show (Leaf (Left (Describe msg))) = "describe = " ++ msg
  show (Leaf (Left (It msg))) = "it = " ++ msg
  show (Leaf (Right _)) = "test case"

original : SpecTree
original =
  describe "a" $ do
    describe "b" $ do
      it "c1" (1 === 1)
      it "c2" (1 === 1)
    it "d" (2 === 2)

shuffled : SpecTree
shuffled =
  describe "a" $ do
    it "d" (2 === 2)
    describe "b" $ do
      it "c2" (1 === 1)
      it "c1" (1 === 1)
    

testShuffle : IO ()
testShuffle = testAndPrint "shuffle" (shuffle {rand = \_ => 0.0} original 1) shuffled (==)

export
specSuite : IO ()
specSuite = do putStrLn "\n  shuffle:"
               testShuffle
