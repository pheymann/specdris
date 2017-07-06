module Specdris.Core

import Specdris.Data.SpecResult
import Specdris.Data.SpecState

import Specdris.Console

%access export
%default total

||| Tree data structure with elements only in the leafs.
public export
data Tree : Type -> Type where
     Leaf : (elem : a) -> Tree a
     Node : (left : Tree a) -> (right : Tree a) -> Tree a

public export
SpecTree : Type
SpecTree = Tree (IO SpecResult)

namespace SpecTreeDo
  (>>=) : SpecTree -> (() -> SpecTree) -> SpecTree
  (>>=) leftTree f = let rightTree = f () in
                         Node leftTree rightTree

{- Prints the description for `describe`, `it`, `pending` or the failure message
   to the console. Depending on the result of the spec case the `SpecState` 
   will be changed.-}
private
updateStateAndPrint : SpecResult -> SpecState -> (level : Nat) -> IO SpecState
updateStateAndPrint (Print line sign color) state level = do putStrLn (format (sign ++ " " ++ line) color level)
                                                             pure state

updateStateAndPrint (Pending message) state level = let output = case message of
                                                                   (Just msg) => format (" [] pending: " ++ msg) Yellow (level + 1)
                                                                   Nothing    => format " [] pending" Yellow (level + 1) in
                                                        do putStrLn output
                                                           pure (addPending state)

updateStateAndPrint Success state level = pure (addSpec state)

updateStateAndPrint (UnaryFailure val reason) state level  = do putStrLn (format (" [x] " ++ show val ++ " " ++ reason) Red (level + 1))
                                                                pure (addFailure state)
                                                                
updateStateAndPrint (BinaryFailure a b reason) state level = do putStrLn (format (" [x] " ++ show a ++ " " ++ reason ++ " " ++ show b) Red (level + 1))
                                                                pure (addFailure state)

{- Evaluates every leaf in the `SpecTree` and folds the different `IO`s to collect
   a final `SpecState`.-}
evaluateTree : SpecTree -> SpecState -> (around : IO SpecResult -> IO SpecResult) -> (level : Nat) -> IO SpecState
evaluateTree (Leaf specIO) state around level 
  = do result <- (around specIO)
       updateStateAndPrint result state level
                                          
evaluateTree (Node left right) state around level 
  = case left of
        (Leaf _) => do newState <- evaluateTree left state around (level + 1)
                       evaluateTree right newState around (level + 1)
        _        => do newState <- evaluateTree left state around level
                       evaluateTree right newState around level

evaluate : (around : IO SpecResult -> IO SpecResult) -> SpecTree -> IO SpecState
evaluate around tree = evaluateTree tree neutral around 0
