module Main where

import System.Environment
import qualified Language.Haskell.Format as Format


main :: IO ()
main =
    getArgs >>= format
    where
        format (file:_) =
            Format.file file >>= putStr
        format [] =
            error "no file provided"
