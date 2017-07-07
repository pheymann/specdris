module Specdris.Core

import Specdris.Data.SpecInfo
import Specdris.Data.SpecResult
import Specdris.Data.SpecState

%access export
%default total

||| BTree with elements in the leafs.
public export
data Tree : Type -> Type where
     Leaf : (elem : a) -> Tree a
     Node : (left : Tree a) -> (right : Tree a) -> Tree a

public export
SpecTree : Type
SpecTree = Tree (Either SpecInfo (IO SpecResult))

namespace SpecTreeDo
  (>>=) : SpecTree -> (() -> SpecTree) -> SpecTree
  (>>=) leftTree f = let rightTree = f () in
                         Node leftTree rightTree

{- Evaluates every leaf in the `SpecTree` and folds the different `IO`s to collect
   a final `SpecState`.-}
evaluateTree : SpecTree -> SpecState -> (around : IO SpecResult -> IO SpecResult) -> (level : Nat) -> IO SpecState
evaluateTree (Leaf (Left info)) state _ level         = do evalInfo info level
                                                           pure state
evaluateTree (Leaf (Right specIO)) state around level = evalResult !(around specIO) state level

evaluateTree (Node left right) state around level 
  = case left of
        (Leaf _) => do newState <- evaluateTree left state around (level + 1)
                       evaluateTree right newState around (level + 1)
        _        => do newState <- evaluateTree left state around level
                       evaluateTree right newState around level

evaluate : (around : IO SpecResult -> IO SpecResult) -> SpecTree -> IO SpecState
evaluate around tree = evaluateTree tree neutral around 0
