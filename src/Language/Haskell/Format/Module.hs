{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Module
  ( full
  ) where

import Data.Monoid ((<>))
import Language.Haskell.Exts hiding (name)
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Types
import Prelude hiding (head)

full :: Module CommentedSrc -> Format
full (Module _ head' pragmas imports declarations)
  | format == mempty = mempty
  | otherwise = format <> newLine
  where
    format = Format.intercalate newLine
      [ Format.intercalate newLine (map pragma pragmas)
      , head head'
      , Format.intercalate newLine (map import' imports)
      , Format.intercalate newLine (map declaration declarations)
      ]
full _ = error "xml not supported"

pragma :: ModulePragma CommentedSrc -> Format
pragma = undefined

head :: Maybe (ModuleHead CommentedSrc) -> Format
head Nothing = ""
head (Just (ModuleHead _ name' _ _)) =
  "module " <> name name' <> " where"

name :: ModuleName CommentedSrc -> Format
name (ModuleName _ name') = Format.fromString name'

import' :: ImportDecl CommentedSrc -> Format
import' ImportDecl { importModule } =
  "import " <> name importModule

declaration :: Decl CommentedSrc -> Format
declaration = undefined
