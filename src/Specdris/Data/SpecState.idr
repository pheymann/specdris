module Specdris.Data.SpecState

%access export

%default total

public export
record SpecState where
  constructor MkState
  
  totalNum : Nat
  failed   : Nat
  pending  : Nat
  
Eq SpecState where
  (==) (MkState lTotal lFailed lPend) (MkState rTotal rFailed rPend) 
    = (lTotal == rTotal) && (lFailed == rFailed) && (lPend == rPend)

Show SpecState where
  show (MkState totalNum failed pending)
    = "Total = " ++ show totalNum 
        ++ ", Failed = " ++ show failed 
        ++ ", Pending = " ++ show pending

Semigroup SpecState where
  (<+>) (MkState lt lf lp) (MkState rt rf rp) = MkState (lt + rt) (lf + rf) (lp + rp)

Monoid SpecState where
  neutral = MkState 0 0 0

total
addSpec : SpecState -> SpecState
addSpec state = record {totalNum $= (+ 1)} state

total
addFailure : SpecState -> SpecState
addFailure state = record {totalNum $= (+ 1), failed $= (+ 1)} state

total
addPending : SpecState -> SpecState
addPending state = record {totalNum $= (+ 1), pending $= (+ 1)} state
