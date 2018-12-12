module Main where

type X a b = Y a b

type Function c d =
  c -> Int -> d
  -> Int

newtype A = B Int

data C = D String | E deriving (Show)

data F = F
  { c :: String,
    d :: String
  } deriving (Eq, Show)

build :: F
build =
  F { c = "c"
    , d = "d" }
