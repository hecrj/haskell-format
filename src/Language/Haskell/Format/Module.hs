{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
module Language.Haskell.Format.Module
  ( Language.Haskell.Format.Module.decl
  ) where

import Language.Haskell.Exts hiding (name)
import Language.Haskell.Format.Atom as Atom
import Language.Haskell.Format.Import as Import
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Types
import Prelude hiding (head)

decl :: Module CommentedSrc -> Format
decl (Module _ head' pragmas imports declarations)
  | format == mempty = mempty
  | otherwise = format <> newLine
  where
    format = Format.intercalate newLine
      [ Format.intercalate newLine (map pragma pragmas)
      , head head'
      , Format.intercalate newLine (map Import.decl imports)
      , Format.intercalate newLine (map declaration declarations)
      ]
decl _ = error "xml not supported"

pragma :: ModulePragma CommentedSrc -> Format
pragma = undefined

head :: Maybe (ModuleHead CommentedSrc) -> Format
head Nothing = ""
head (Just (ModuleHead _ name' _ _)) =
  "module " <> Atom.moduleName name' <> " where"

declaration :: Decl CommentedSrc -> Format
declaration = undefined
