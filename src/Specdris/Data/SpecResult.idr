module Specdris.Data.SpecResult

import Specdris.Console

%access export

public export
data SpecResult : Type where     
     Print : (line : String) -> (sign : String) -> Color -> SpecResult
     
     Pending : (message : Maybe String) -> SpecResult
     
     Success : SpecResult
     
     UnaryFailure : Show a => (val : a) -> (reason : String) -> SpecResult
     BinaryFailure : (Show a, Show b) => (left : a) -> (right : b) -> (reason : String) -> SpecResult

Eq SpecResult where
  (==) (Print aLine aSign aColor) (Print bLine bSign bColor)   = aLine == bLine && aSign == bSign && aColor == bColor
  (==) (Pending aMsg) (Pending bMsg)                           = aMsg == bMsg
  (==) Success Success                                         = True
  (==) (UnaryFailure _ aReason) (UnaryFailure _ bReason)       = aReason == bReason
  (==) (BinaryFailure _ _ aReason) (BinaryFailure _ _ bReason) = aReason == bReason
  (==) _ _ = False

namespace SpecResultDo
  (>>=) : SpecResult -> (SpecResult -> SpecResult) -> SpecResult
  (>>=) (UnaryFailure actual reason) _           = (UnaryFailure actual reason)
  (>>=) (BinaryFailure actual expected reason) _ = (BinaryFailure actual expected reason)
  (>>=) result f                                 = f result
