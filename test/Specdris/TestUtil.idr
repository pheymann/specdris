module Specdris.TestUtil

%access export

total
testAndPrint : (Show a, Eq a) => (info : String) -> a -> a -> (a -> a -> Bool) -> IO ()
testAndPrint info a b predicate 
  = if predicate a b then
      putStrLn $ " + SUCCESS " ++ info
    else do 
      putStrLn $ " - FAILED  " ++ info ++ ":\nactual   = " ++ show a ++ "\nexpected = " ++ show b
