module Specdris.Data.SpecState

%access export
%default total

||| Global state of all spec cases which are executed
||| together (monadicly combined).
public export
record SpecState where
  constructor MkState
  
  totalNum : Nat
  failed   : Nat
  pending  : Nat
  
  output   : Maybe String
  
Eq SpecState where
  (==) (MkState lTotal lFailed lPend lOut) (MkState rTotal rFailed rPend rOut)
    = (lTotal == rTotal) && (lFailed == rFailed) && (lPend == rPend) && (lOut == rOut)

Show SpecState where
  show (MkState totalNum failed pending _)
    = "Total = " ++ show totalNum 
        ++ ", Failed = " ++ show failed 
        ++ ", Pending = " ++ show pending
        
Semigroup SpecState where
  (<+>) (MkState lt lf lp lo) (MkState rt rf rp ro) = MkState (lt + rt) (lf + rf) (lp + rp) (lo <+> ro)

Monoid SpecState where
  neutral = MkState 0 0 0 Nothing

addSpec : SpecState -> SpecState
addSpec state = record {totalNum $= (+ 1)} state

addFailure : SpecState -> SpecState
addFailure state = record {totalNum $= (+ 1), failed $= (+ 1)} state

addPending : SpecState -> SpecState
addPending state = record {totalNum $= (+ 1), pending $= (+ 1)} state

addLine : (line : String) -> SpecState -> SpecState
addLine line state = record {output $= (<+> (Just line))} state
