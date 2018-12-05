{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Pattern
  ( format
  ) where

import Language.Haskell.Exts
import qualified Language.Haskell.Format.Atom as Atom
import Language.Haskell.Format.Internal as Format
import qualified Language.Haskell.Format.Literal as Literal
import Language.Haskell.Format.Types

format :: Pat CommentedSrc -> Format
format (PVar _ name_)            = Atom.name name_
format (PWildCard _)             = "_"
format (PLit _ (Signless _) lit) = Literal.format lit
format (PLit _ (Negative _) lit) = "-" <> Literal.format lit
format (PParen _ pattern_)       = mconcat [ "(", format pattern_, ")" ]
format (PInfixApp _ left qname right) =
  mconcat [ format left, Atom.qname qname, format right ]
format (PList _ patterns) =
  Format.wrap "[" "]" "," (map format patterns)
format (PApp _ qname patterns) =
  Format.intercalate " " (Atom.qname qname : map format patterns)
format p                         = error (show p)
