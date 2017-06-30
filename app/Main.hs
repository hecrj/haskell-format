module Main where

import Language.Haskell.Format as Format
import System.Environment

main :: IO ()
main = getArgs >>= format
  where
    format (file:_) = Format.file file >>= putStrLn
    format []       = error "no file provided"
