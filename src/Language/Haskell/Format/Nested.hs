{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Nested
  ( qop
  , if_
  ) where

import Language.Haskell.Exts
import qualified Language.Haskell.Format.Atom as Atom
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Types

qop :: QOp CommentedSrc -> Format -> Format
qop qop' = nest (Atom.qop qop')

if_ :: Format -> Format
if_ = nest "if"

nest :: Format -> Format -> Format
nest anchor target =
  Format.intercalate "\n" (firstLine : paddedLines)
  where
    (x1:xs) = Format.lines target
    firstLine = anchor <> " " <> x1
    paddedLines = map (padding <>) xs
    padding = Format.fromString (replicate depth ' ')
    depth = Format.length anchor + 1
