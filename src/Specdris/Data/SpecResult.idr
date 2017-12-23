module Specdris.Data.SpecResult

import Specdris.Data.ConsoleColor
import Specdris.Data.SpecState

%access export
%default total

||| Result of a single `Expectation`s or `Pending` if none
||| exists.
public export
data SpecResult : Type -> Type where
     Pending : x -> (message : Maybe String) -> SpecResult x

     Success : x -> SpecResult x

     UnaryFailure : Show a => (val : a) -> (reason : String) -> SpecResult x
     BinaryFailure : (Show a, Show b) => (left : a) -> (right : b) -> (reason : String) -> SpecResult x

Eq (SpecResult x) where
  (==) (Pending _ aMsg) (Pending _ bMsg)                       = aMsg == bMsg
  (==) (Success _) (Success _)                                 = True
  (==) (UnaryFailure _ aReason) (UnaryFailure _ bReason)       = aReason == bReason
  (==) (BinaryFailure _ _ aReason) (BinaryFailure _ _ bReason) = aReason == bReason
  (==) _ _ = False


private
show : SpecResult x -> (level : Nat) -> String
show (Pending _ message) _ = case message of
                               (Just msg) => " [] pending: " ++ msg
                               Nothing    => " [] pending"
show (Success _) _ = ""
show (UnaryFailure actual reason) level           = " [x] " ++ reason ++
                                                    "\n     " ++ indent(level) ++ "actual: " ++ show actual
show (BinaryFailure actual expected reason) level = " [x] " ++ reason ++
                                                    "\n     " ++ indent(level) ++ "actual:   " ++ show actual ++
                                                    "\n     " ++ indent(level) ++ "expected: " ++ show expected

namespace SpecResultDo
  (>>=) : SpecResult x -> (x -> SpecResult y) -> SpecResult y
  (>>=) (UnaryFailure actual reason) _           = (UnaryFailure actual reason)
  (>>=) (BinaryFailure actual expected reason) _ = (BinaryFailure actual expected reason)
  (>>=) (Success x) f                            = f x
  (>>=) (Pending x _) f                          = f x

private
handleOutput : String -> SpecState -> (storeOutput : Bool) -> IO SpecState
handleOutput output state store = if store then
                                    pure $ addLine output state
                                  else do
                                    putStrLn output
                                    pure state

{- Evaluates a given expectation result in updates the `SpecState` accordingly:
     - `Success` : increase spec count
     - `Failure` : increase spec and failure count
     - `Pending` : encrease spec and pending count
   Furthermore it prints the result to console if `storeOutput == False`.
 -}
evalResult : SpecResult () -> SpecState -> (storeOutput : Bool) -> (level : Nat) -> IO SpecState
evalResult r@(Pending x msg) state store level = let output = format (show {x = ()} r $ level + 1) Yellow (level + 1) in
                                                   pure $ addPending !(handleOutput output state store)

evalResult (Success _) state _ _ = pure $ addSpec state
evalResult r@(UnaryFailure a reason) state store level    = let output = format (show {x = ()} r $ level + 1) Red (level + 1) in
                                                                pure $ addFailure !(handleOutput output state store)

evalResult r@(BinaryFailure a b reason) state store level = let output = format (show {x = ()} r $ level + 1) Red (level + 1) in
                                                                pure $ addFailure !(handleOutput output state store)
