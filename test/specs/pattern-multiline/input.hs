module Main where

import qualified Language.Haskell.Format as Format
import System.Environment

main :: IO ()
main = getArgs >>= format 1
  where
    format :: Int -> [String] -> IO ()
    format _ (first:
      _second:_third:_fourth:
      _) = Format.file first >>= putStrLn
    format _ []       = error "no file provided"
