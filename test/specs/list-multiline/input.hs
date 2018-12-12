module Main where

someList :: [String]
someList = [
  "This",
  "list",
  "takes", "many",
  "lines"
  ]

someList2 :: [(String, String)]
someList2 =
  [ ( "a"
    , "b"
    )
  , ( "c", "d")
  ]

someList3 :: [[String]]
someList3 =
  [ [ "a"
    , "b"
    ]
  , [ "c"
    , "d"
    ]
  ]

listInsideTuple :: ([String], [String])
listInsideTuple =
  ( [ "a"
    , "b"
    ]
  , [ "c"
    , "d"
    ]
  )

main :: IO ()
main = print someList
