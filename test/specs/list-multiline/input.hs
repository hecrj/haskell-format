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

operations :: Int -> IO ()
operations n =
  mconcat
    [ if n > 10 then
        print "n > 10"
      else
        print "n <= 10"
    , if n > 10
         && even n then
         print "n > 10 and even"
      else
        print "n <= 10 or odd"
    ]

main :: IO ()
main = print someList
