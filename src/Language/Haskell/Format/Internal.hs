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
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Builder as Builder
import Prelude hiding (lines)

type Format = Builder

newLine :: Format
newLine = "\n"

indentation :: Format
indentation = "    "

fromString :: String -> Format
fromString = Builder.fromString

toString :: Format -> String
toString = Text.unpack . Builder.toLazyText

length :: Format -> Int
length = fromIntegral . Text.length . Builder.toLazyText

intercalate :: Format -> [Format] -> Format
intercalate f (x1:x2:xs)
  | x1 == mempty = intercalate f (x2:xs)
  | x2 == mempty = intercalate f (x1:xs)
  | otherwise = x1 <> f <> intercalate f (x2:xs)
intercalate _ [x]        = x
intercalate _ []         = ""

wrap :: Format -> Format -> Format -> [Format] -> Format
wrap start end separator elems =
  start <> intercalate separator elems <> end

indent :: Format -> Format
indent =
  intercalate "\n" . map (indentation <>) . lines

lines :: Format -> [Format]
lines =
  map Builder.fromLazyText . Text.splitOn "\n" . Builder.toLazyText
