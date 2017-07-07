module Specdris.Core

import Specdris.Data.SpecAction
import Specdris.Data.SpecResult
import Specdris.Data.SpecState

import Specdris.Console

%access export
%default total

||| BTree with elements in the leafs.
public export
data Tree : Type -> Type where
     Leaf : (elem : a) -> Tree a
     Node : (left : Tree a) -> (right : Tree a) -> Tree a

public export
SpecTree : Type
SpecTree = Tree (IO SpecResult)

{- Transforms the lazy spec test description into a Tree representation.
   By doing this we create an instance for every spec case rather than
   having a lazy `Bind`.
   
   The spec cases are not evaluated yet as they are described with `IO`.-} 
buildSpecTree : SpecAction -> (around : IO SpecResult -> IO SpecResult) -> SpecTree
buildSpecTree (Describe message) _      = Leaf (pure (Print message "" White))
buildSpecTree (It message spec)  around = Node (Leaf (pure (Print message " +" White))) (Leaf (around spec))
buildSpecTree (Bind actions f)   around = let bindActions = f () in
                                              case buildSpecTree actions around of
                                                lResult => case buildSpecTree bindActions around of
                                                             rResult => Node lResult rResult

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
evaluateTree : SpecTree -> SpecState -> (level : Nat) -> IO SpecState
evaluateTree (Leaf specIO) state level = updateStateAndPrint !specIO state level
                                          
evaluateTree (Node left right) state level = case left of
                                               (Leaf _) => do newState <- evaluateTree left state (level + 1)
                                                              evaluateTree right newState (level + 1)
                                               _        => do newState <- evaluateTree left state level
                                                              evaluateTree right newState level

evaluate : (around : IO SpecResult -> IO SpecResult) -> SpecAction -> IO SpecState
evaluate around actions = evaluateTree (buildSpecTree actions around) neutral 0
