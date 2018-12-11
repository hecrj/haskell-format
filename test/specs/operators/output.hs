module Main
    ( (|>)
    , (>>>)
    ) where

import Operators ((|>), (>>>))


test :: [Int] -> Int
test =
    foldr 1.0 (*)


test2 :: a -> a -> ( a, a )
test2 =
    (,)


(|>) :: a -> (a -> b) -> b
(|>) v f =
    f v
