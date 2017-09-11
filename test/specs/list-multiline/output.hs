module Main where

someList :: [String]
someList =
  [ "This"
  , "list"
  , "takes"
  , "many"
  , "lines"
  ]


main :: IO ()
main =
  print someList
