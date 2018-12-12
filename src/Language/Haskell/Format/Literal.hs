{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Literal
    ( format
    ) where

import Language.Haskell.Exts
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Types


format :: Literal CommentedSrc -> Format
format (Char _ _ s) =
    "'" <> Format.fromString s <> "'"
format (String _ _ s) =
    "\"" <> Format.fromString s <> "\""
format (Int _ _ s) =
    Format.fromString s
format (Frac _ _ s) =
    Format.fromString s
format l =
    error (show l)
