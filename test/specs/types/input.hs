{-# LANGUAGE TypeFamilies #-}
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

test :: Num n => n -> ()
test _ = ()

class (Num r, Ord r) => Repository r where
  create :: r -> String -> Int -> IO ()
  read :: String -> IO (Maybe Int)

instance Repository (Dict String Int) where
  type X = Y
  create dict string int = Dict.set dict string int
  read = Dict.get
