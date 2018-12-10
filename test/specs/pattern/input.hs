{-# LANGUAGE NamedFieldPuns #-}
module Main where

import qualified Language.Haskell.Format as Format
import System.Environment

main :: IO ()
main = getArgs >>= format
  where
    format (file:_) = Format.file file >>= putStrLn
    format []       = error "no file provided"

test :: Maybe A -> ()
test m@(Just {x,y}) = ()
test Nothing = ()
