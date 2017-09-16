module Test where

test :: Int -> IO ()
test n = case n of
  0 -> putStrLn "0 case"
  1 -> putStrLn "1 case"
  _ -> putStrLn "default case"
