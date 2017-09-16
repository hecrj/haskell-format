{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Nested
  ( qop
  , if_
  , case_
  ) where

import Language.Haskell.Exts
import qualified Language.Haskell.Format.Atom as Atom
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Types

qop :: QOp CommentedSrc -> Format -> Format
qop qop' = nest (Atom.qop qop')

if_ :: Format -> Format
if_ = nest "if"

case_ :: Format -> Format
case_ = nest "case"

nest :: Format -> Format -> Format
nest anchor target =
  Format.intercalate newLine (firstLine : paddedLines)
  where
    (x1:xs) = Format.lines target
    firstLine = anchor <> " " <> x1
    paddedLines = map (padding <>) xs
    padding = Format.fromString (replicate depth ' ')
    depth = Format.length anchor + 1
