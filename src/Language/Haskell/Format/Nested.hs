{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Nested
    ( qop
    , if_
    , case_
    , pattern_
    , do_
    , generator
    ) where

import Language.Haskell.Exts
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Types
import qualified Language.Haskell.Format.Atom as Atom
import qualified Language.Haskell.Format.Pattern as Pattern


qop :: QOp CommentedSrc -> Format -> Format
qop qop' =
    nest (Atom.qop qop')


if_ :: Format -> Format
if_ =
    nest "if"


case_ :: Format -> Format
case_ =
    nest "case"


pattern_ :: Pat CommentedSrc -> Format -> Format
pattern_ pat =
    nest (Pattern.format pat <> " |")


do_ :: Format -> Format
do_ =
    nest "do"


generator :: Pat CommentedSrc -> Format -> Format
generator pat =
    nest (Pattern.format pat <> " <-")


nest :: Format -> Format -> Format
nest anchor target =
    Format.intercalate newLine (firstLine : paddedLines)
    where
        (x1 : xs) =
            Format.lines target

        firstLine =
            anchor <> " " <> x1

        paddedLines =
            map (padding <>) xs

        padding =
            Format.fromString (replicate depth ' ')

        depth =
            Format.length anchor + 1
