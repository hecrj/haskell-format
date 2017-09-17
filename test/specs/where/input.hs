module Test where

test :: Int -> Int -> IO ()
test a b =
  putStrLn $ mconcat ["Addition: ", show addition, ", Subtraction: ",
    show subtraction]
  where
    addition = a + b
    subtraction = a - b

main :: IO ()
main =
  output
  where
    output = test 1 2
