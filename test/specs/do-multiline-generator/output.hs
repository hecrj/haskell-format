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

        x <- Ok 1
        y <- Ok 2
        putStrLn [ first ]
