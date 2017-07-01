{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Internal
  ( Format
  , newLine
  , Language.Haskell.Format.Internal.fromString
  , toString
  , intercalate
  ) where

import Data.Monoid ((<>))
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Builder as Builder

type Format = Builder

newLine :: Format
newLine = "\n"

fromString :: String -> Format
fromString = Builder.fromString

toString :: Format -> String
toString = Text.unpack . Builder.toLazyText

intercalate :: Format -> [Format] -> Format
intercalate f (x1:x2:xs)
  | x1 == mempty = intercalate f (x2:xs)
  | x2 == mempty = intercalate f (x1:xs)
  | otherwise = x1 <> f <> intercalate f (x2:xs)
intercalate f [x]        = x
intercalate f []         = ""
