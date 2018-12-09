module Test where

concatLists :: [Int]
concatLists =
    [ 1
    , 2
    , 3
    , 4
    ]
        ++ [ 5
           , 6
           , 7
           , 8
           ]


bindExample :: Maybe Int
bindExample =
    Just 1
        >>= f []
        >>= f
            [ 1
            , 2
            , 3
            , 4
            ]
        >>= f []


f :: [Int] -> Int -> Maybe Int
f _ _ =
    Just 2
