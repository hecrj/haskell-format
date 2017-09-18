module Test where

import Data.Char

main :: IO ()
main = do
  line <- map toUpper <$>
    getLine
  putStrLn line
