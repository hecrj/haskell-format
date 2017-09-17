{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Literal
  ( format
  ) where

import Language.Haskell.Exts
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Types

format :: Literal CommentedSrc -> Format
format (String _ s _) = "\"" <> Format.fromString s <> "\""
format (Int _ _ s)    = Format.fromString s
format l              = Format.fromString (show l)
