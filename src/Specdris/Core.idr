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
   a final `SpecState`.
 -}
evaluateTree : SpecTree -> 
               SpecState -> 
               (around : IO SpecResult -> IO SpecResult) -> 
               (storeOutput : Bool) -> 
               (level : Nat) -> 
               IO SpecState
evaluateTree (Leaf (Left info)) state _ store level         = let out = evalInfo info level in
                                                                  if store then
                                                                    pure $ addLine out state
                                                                  else do 
                                                                    putStrLn out
                                                                    pure state
evaluateTree (Leaf (Right specIO)) state around store level = evalResult !(around specIO) state store level

evaluateTree (Node left right) state around store level 
  = case left of
        (Leaf _) => do newState <- evaluateTree left state around store (level + 1)
                       evaluateTree right newState around store (level + 1)
        _        => do newState <- evaluateTree left state around store level
                       evaluateTree right newState around store level

evaluate : (around : IO SpecResult -> IO SpecResult) -> (storeOutput : Bool) -> SpecTree -> IO SpecState
evaluate around store tree = evaluateTree tree neutral around store 0
