module Main where

import qualified Language.Haskell.Format as Format
import System.Environment

main :: IO ()
main = getArgs >>= format
  where
    format (file:_) = Format.file file >>= putStr
    format []       = error "no file provided"
