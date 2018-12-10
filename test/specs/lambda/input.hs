module Main where

test :: Int -> Int -> Int -> Int
test = \a b c -> 1

test2 :: Int -> Int -> Int -> Int
test2 = \a b c ->
  a + b + c
