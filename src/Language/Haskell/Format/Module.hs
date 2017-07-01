{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Module
  ( full
  ) where

import Data.Monoid ((<>))
import Language.Haskell.Exts hiding (name)
import Language.Haskell.Format.Internal as Format
import Prelude hiding (head)

full :: (Module SrcSpanInfo, [Comment]) -> Format
full (Module info head' pragmas imports declarations, comments)
  | format == mempty = mempty
  | otherwise = format <> newLine
  where
    format = Format.intercalate newLine
      [ head head'
      , mconcat $ map import' imports
      ]

head :: Maybe (ModuleHead SrcSpanInfo) -> Format
head Nothing = ""
head (Just (ModuleHead _ name' _ exports)) =
  "module " <> name name' <> " where"

name :: ModuleName SrcSpanInfo -> Format
name (ModuleName _ name') = Format.fromString name'

import' :: ImportDecl SrcSpanInfo -> Format
import' ImportDecl { importModule } =
  "import " <> name importModule
