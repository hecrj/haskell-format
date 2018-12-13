{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Internal
    ( Format
    , newLine
    , Language.Haskell.Format.Internal.fromString
    , toString
    , Language.Haskell.Format.Internal.length
    , intercalate
    , wrap
    , indent
    , lines
    , (<>)
    ) where

import Data.Monoid ((<>))
import Data.Text.Lazy.Builder as Builder
import Prelude hiding (lines)
import qualified Data.List as List
import qualified Data.Text.Lazy as Text


type Format = Builder


newLine :: Format
newLine =
    "\n"


indentation :: Format
indentation =
    "    "


fromString :: String -> Format
fromString =
    Builder.fromString


toString :: Format -> String
toString =
    Text.unpack . Builder.toLazyText


length :: Format -> Int
length =
    fromIntegral . Text.length . Builder.toLazyText


intercalate :: Format -> [Format] -> Format
intercalate f =
    mconcat . List.intersperse f


wrap :: Format -> Format -> Format -> [Format] -> Format
wrap start end separator elems =
    start <> intercalate separator elems <> end


indent :: Format -> Format
indent =
    intercalate newLine . map indentUnlessEmpty . lines
    where
        indentUnlessEmpty "" =
            ""
        indentUnlessEmpty line =
            indentation <> line


lines :: Format -> [Format]
lines =
    map Builder.fromLazyText . Text.splitOn "\n" . Builder.toLazyText
