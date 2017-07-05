module Specdris.Data.SpecState

%access export

public export
record SpecState where
  constructor MkState
  
  totalNum : Nat
  failed   : Nat -- TODO make it a Fin
  pending  : Nat -- TODO make it a Fin
  
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
addSpec (MkState totalNum failed pending) = MkState (totalNum + 1) failed pending

total
addFailure : SpecState -> SpecState
addFailure (MkState totalNum failed pending) = MkState (totalNum + 1) (failed + 1) pending

total
addPending : SpecState -> SpecState
addPending (MkState totalNum failed pending) = MkState (totalNum + 1) failed (pending + 1)
