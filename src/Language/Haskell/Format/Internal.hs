{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Internal
  ( Format
  , newLine
  , Language.Haskell.Format.Internal.fromString
  , toString
  , intercalate
  , wrap
  , indent
  , (<>)
  ) where

import Data.Monoid ((<>))
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Builder as Builder

type Format = Builder

newLine :: Format
newLine = "\n"

indentation :: Format
indentation = "  "

fromString :: String -> Format
fromString = Builder.fromString

toString :: Format -> String
toString = Text.unpack . Builder.toLazyText

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
  intercalate "\n" . map ((<>) indentation . Builder.fromLazyText) . Text.splitOn "\n" . Builder.toLazyText
