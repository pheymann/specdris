module Specdris.Data.SpecInfo

import Specdris.Data.ConsoleColor

%access export

%default total

public export
data SpecInfo : Type where
     Describe : (msg : String) -> SpecInfo
     It : (msg : String) -> SpecInfo

evalInfo : SpecInfo -> (level : Nat) -> IO ()
evalInfo (Describe msg) level = putStrLn (format msg White level)
evalInfo (It msg)       level = putStrLn (format ("+ " ++ msg) White level)
