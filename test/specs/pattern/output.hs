{-# LANGUAGE NamedFieldPuns #-}
module Main where

import System.Environment
import qualified Language.Haskell.Format as Format


main :: IO ()
main =
    getArgs >>= format
    where
        format (file : _) =
            Format.file file >>= putStrLn
        format [] =
            error "no file provided"


test :: Maybe A -> ()
test m@(Just{ x, y }) =
    ()
test Nothing =
    ()


test2 :: () -> ()
test2 () =
    ()
