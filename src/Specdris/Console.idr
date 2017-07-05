module Specdris.Console

%access export
%default total

||| Console colors
public export
data Color = Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | White

Eq Color where
  (==) Red Red         = True
  (==) Green Green     = True
  (==) Yellow Yellow   = True
  (==) Blue Blue       = True
  (==) Magenta Magenta = True
  (==) Cyan Cyan       = True
  (==) White White     = True
  (==) _ _ = False

Show Color where
  show Red     = "red"
  show Green   = "green"
  show Yellow  = "yellow"
  show Blue    = "blue"
  show Magenta = "magenta"
  show Cyan    = "cyan"
  show White   = "white"

private
colorToCode : Color -> Nat
colorToCode Red     = 1
colorToCode Green   = 2
colorToCode Yellow  = 3
colorToCode Blue    = 4
colorToCode Magenta = 5
colorToCode Cyan    = 6
colorToCode White   = 7

private
colorToAnsi : Color -> String
colorToAnsi code = "\ESC[3" ++ show (colorToCode code) ++ "m"

colorise : Color -> String -> String
colorise color str = colorToAnsi color ++ str ++ "\ESC[0m"

indent : (level : Nat) -> String
indent level = foldr (\el, acc => el ++ acc) "" (replicate level "  ")

format : String -> Color -> (level : Nat) -> String
format str color level = colorise color $ indent level ++ str
