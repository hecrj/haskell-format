{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Main where

type X a b = Y a b

type Function c d =
  c -> Int -> d
  -> Int

newtype A = B Int

data C = D String | E deriving (Show)

data Z a b =
  Z
    (X a b)
    (X a b
    , X a b)

data F = F
  { c :: String,
    d :: String
  } deriving (Eq, Show)

data Sql where
    Sql :: Sql.Connection conn => conn -> Sql

build :: F
build =
  F { c = "c"
    , d = "d" }

test :: Num n => n -> F
test _ =
  build { c = "C", d = "D" } :: F

class (Num r, Ord r) => Repository r where
  create :: r -> String -> Int -> IO ()
  read :: String -> IO (Maybe Int)

instance Num a => Repository (Dict String Int) where
  type X = Y
  create dict string int = Dict.set dict string int
  read = Dict.get
