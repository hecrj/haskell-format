{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Nested
  ( qop
  ) where

import Language.Haskell.Exts
import qualified Language.Haskell.Format.Atom as Atom
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Types

qop :: QOp CommentedSrc -> Format -> Format
qop qop' = nest (Atom.qop qop')

nest :: Format -> Format -> Format
nest anchor target =
  Format.intercalate "\n" (firstLine : paddedLines)
  where
    (x1:xs) = Format.lines target
    firstLine = anchor <> " " <> x1
    paddedLines = map (padding <>) xs
    padding = Format.fromString (replicate depth ' ')
    depth = Format.length anchor + 1
