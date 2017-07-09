{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
module Language.Haskell.Format.Module
  ( Language.Haskell.Format.Module.format
  ) where

import Language.Haskell.Exts hiding (name)
import qualified Language.Haskell.Format.Atom as Atom
import qualified Language.Haskell.Format.Declaration as Declaration
import Language.Haskell.Format.Import as Import
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Types
import Prelude hiding (head)

format :: Module CommentedSrc -> Format
format (Module _ head' pragmas imports declarations)
  | result == mempty = mempty
  | otherwise = result <> newLine
  where
    result = Format.intercalate newLine
      [ Format.intercalate newLine (map pragma pragmas)
      , Format.intercalate (newLine <> newLine)
        [ head head'
        , Format.intercalate newLine (map Import.format imports)
        , Format.intercalate
            (newLine <> newLine <> newLine)
            (map (Format.intercalate newLine . map Declaration.format) (Declaration.group declarations))
        ]
      ]

format _ = error "xml not supported"

pragma :: ModulePragma CommentedSrc -> Format
pragma = undefined

head :: Maybe (ModuleHead CommentedSrc) -> Format
head Nothing = ""
head (Just (ModuleHead _ name' _ specList)) =
  "module "
    <> Atom.moduleName name'
    <> maybe "" ((<>) newLine . exportSpecList) specList
    <> " where"

exportSpecList :: ExportSpecList CommentedSrc -> Format
exportSpecList (ExportSpecList _ specs) =
    Format.indent $
      Format.wrap "( " (newLine <> ")") (newLine <> ", ") (map exportSpec specs)

exportSpec :: ExportSpec CommentedSrc -> Format
exportSpec (EVar _ qname) = Atom.qname qname
exportSpec (EAbs _ _ qname) = Atom.qname qname
exportSpec (EThingWith _ _ qname cnames) =
  Atom.qname qname <> " " <> Format.wrap "(" ")" ", " (map Atom.cname cnames)
exportSpec (EModuleContents _ moduleName) =
  "module " <> Atom.moduleName moduleName
