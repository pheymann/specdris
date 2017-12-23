module Specdris.Expectations

import Specdris.Data.SpecResult
import Specdris.Core

%access export
%default total

||| Placeholder for the future test implementation. Is listed in the
||| final spec result.
pending : {auto a : x} -> SpecResult x
pending {a} = Pending a Nothing

||| Same as `pending`, just adds a more detailed description
|||
||| @ message description of what the spec case should test
pendingWith : {auto a : x} -> (message : String) -> SpecResult x
pendingWith {a} message = Pending a (Just message)

||| Checks if two elements are equal.
shouldBe : (Eq a, Show a) => (actual : a) -> (expected : a) -> SpecResult ()
shouldBe actual expected = if actual == expected then
                             Success ()
                           else
                             BinaryFailure actual expected "not equal"

infixr 7 ===

||| Checks if two elements are equal
(===) : (Eq a, Show a) => (actual : a) -> (expected : a) -> SpecResult ()
(===) = shouldBe

||| Checks if two element are unequal
shouldNotBe : (Eq a, Show a) => (actual : a) -> (expected : a) -> SpecResult ()
shouldNotBe actual expected = if actual /= expected then
                                Success ()
                              else
                                BinaryFailure actual expected "equal"

infixr 7 /==

||| Checks if two element are unequal
(/==) : (Eq a, Show a) => (actual : a) -> (expected : a) -> SpecResult ()
(/==) = shouldNotBe

||| Checks if the given element auf type `Bool` is `True`
shouldBeTrue : (actual : Bool) -> SpecResult ()
shouldBeTrue actual = actual === True

||| Checks if the given element auf type `Bool` is `False`
shouldBeFalse : (actual : Bool) -> SpecResult ()
shouldBeFalse actual = actual === False

||| Checks if the given element satisfies the predicate
shouldSatisfy : Show a => (actual : a) -> (pred : a -> Bool) -> SpecResult ()
shouldSatisfy actual pred = if pred actual then
                              Success ()
                            else
                              UnaryFailure actual "doesn't satisfy predicate"

||| Checks if the given element is (Just x), returning x in the SpecResult
shouldBeJust : Show x => (actual : Maybe x) -> SpecResult x
shouldBeJust actual = case actual of
                         Nothing => UnaryFailure actual "is Nothing"
                         Just x => Success x
