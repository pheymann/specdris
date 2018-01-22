module Specdris.Expectations

import Specdris.Data.SpecResult
import Specdris.Core

%access export
%default total

||| Placeholder for the future test implementation. Is listed in the
||| final spec result.
pending : SpecResult
pending = Pending Nothing

||| Same as `pending`, just adds a more detailed description
||| 
||| @ message description of what the spec case should test
pendingWith : (message : String) -> SpecResult
pendingWith message = Pending (Just message)

||| Checks if two elements are equal.
shouldBe : (Eq a, Show a) => (actual : a) -> (expected : a) -> SpecResult
shouldBe actual expected = if actual == expected then
                             Success
                           else
                             BinaryFailure actual expected "not equal"

infixr 7 ===

||| Checks if two elements are equal
(===) : (Eq a, Show a) => (actual : a) -> (expected : a) -> SpecResult
(===) = shouldBe

||| Checks if two element are unequal
shouldNotBe : (Eq a, Show a) => (actual : a) -> (expected : a) -> SpecResult
shouldNotBe actual expected = if actual /= expected then
                                Success
                              else
                                BinaryFailure actual expected "equal"

infixr 7 /==

||| Checks if two element are unequal
(/==) : (Eq a, Show a) => (actual : a) -> (expected : a) -> SpecResult
(/==) = shouldNotBe

||| Checks if the given element auf type `Bool` is `True`
shouldBeTrue : (actual : Bool) -> SpecResult
shouldBeTrue actual = actual === True

||| Checks if the given element auf type `Bool` is `False`
shouldBeFalse : (actual : Bool) -> SpecResult
shouldBeFalse actual = actual === False

||| Checks if the given element satisfies the predicate
shouldSatisfy : Show a => (actual : a) -> (pred : a -> Bool) -> SpecResult
shouldSatisfy actual pred = if pred actual then
                              Success
                            else
                              UnaryFailure actual "doesn't satisfy predicate"

shouldBeJust : Show a => (actual : Maybe a) -> (expectation : a -> SpecResult) -> SpecResult
shouldBeJust (Just a) expectation = expectation a
shouldBeJust {a} Nothing _        = UnaryFailure (Nothing {a}) "is not `Just`"

shouldBeNothing : Show a => (actual: Maybe a) -> SpecResult
shouldBeNothing Nothing = Success
shouldBeNothing actual  = UnaryFailure actual "is not `Nothing`"
