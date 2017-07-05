module Specdris.Data.SpecAction

import Specdris.Data.SpecResult

%access export
%default total

public export
data SpecAction : Type where
     Describe : (message : String) -> SpecAction
     It : (message : String) -> (spec : IO SpecResult) -> SpecAction

     Bind : SpecAction -> (() -> SpecAction) -> SpecAction

namespace SpecActionDo
  (>>=) : SpecAction -> (() -> SpecAction) -> SpecAction
  (>>=) = Bind
