module Test where

test :: Int ->
  Int ->
  IO ()
test a b = print $ a + b
