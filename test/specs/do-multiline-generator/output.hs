module Test where

import Data.Char


main :: IO ()
main =
    do
        line <-
            map toUpper
                <$> getLine
        putStrLn line


test :: IO ()
test =
    do
        (first:_) <-
            map toUpper
                <$> getLine
        putStrLn [ first ]
