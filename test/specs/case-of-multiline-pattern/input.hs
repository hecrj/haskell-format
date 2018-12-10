module Test where

test :: Int -> IO ()
test n = case n of
  someLongName | someLongName == 0 ||
    someLongName == 1 -> putStrLn "0 and 1 case"
  x | even x -> putStrLn "number is even"
    | x == 3 -> putStrLn "3 case"
  _ -> putStrLn "default case"

test2 :: Maybe (a, b) -> IO ()
test2 maybe =
  case maybe of
    Just (_, _) ->
      putStrLn "Just a"

    Nothing ->
      putStrLn "Nothing"
