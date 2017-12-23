module SpecExample

import Specdris.Spec

import System

%access export
%default total

add : Nat -> Nat -> Nat
add a b = a + b

addSpec : SpecTree
addSpec = describe "Add two Nats" $ do
            it "neutral element" $ do
              (1 `add` 0) === 1
            it "be associative" $ do
              ((1 `add` 2) `add` 3) === (1 `add` (2 `add` 3))
            it "be commutative" $ do
              (1 `add` 2) === (2 `add` 1)

multi : Nat -> Nat -> Nat
multi a b = a * b

multiSpec : SpecTree
multiSpec = describe "Multiply two Nats" $ do
              it "neutral element" $ do
                (2 `multi` 1) === 2
              it "be associative" $ do
                ((1 `multi` 2) `multi` 3) === (1 `multi` (2 `multi` 3))
              it "be commutative" $ do
                (1 `multi` 2) === (2 `multi` 1)

justASpec : SpecTree
justASpec = describe "Two-part test" $ do
              it "3 should map to 'b'" $ do
                x <- shouldBeJust (lookup 3 [(4, 'a'), (3, 'b')])
                x === 'b'

partial
specSuite : IO ()
specSuite = do seed <- System.time
               spec $ do addSpec
                         multiSpec
                         (shuffle multiSpec 999999)
                         (shuffle multiSpec seed)
                         justASpec
