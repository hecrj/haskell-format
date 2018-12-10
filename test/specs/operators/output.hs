module Main
    ( (|>)
    , (>>>)
    ) where

import Operators ((|>), (>>>))


test :: [Int] -> Int
test =
    foldr 1 (*)


(|>) :: a -> (a -> b) -> b
(|>) v f =
    f v
