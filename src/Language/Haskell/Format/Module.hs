{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Module
    ( Language.Haskell.Format.Module.format
    ) where

import Language.Haskell.Exts hiding (name)
import Language.Haskell.Format.Import as Import
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Types
import Prelude hiding (head)
import qualified Data.List as List
import qualified Language.Haskell.Format.Atom as Atom
import qualified Language.Haskell.Format.Declaration as Declaration


format :: Module CommentedSrc -> Format
format (Module _ head' pragmas imports declarations)
    | result == mempty =
        mempty
    | otherwise =
        result <> newLine
    where
        result =
            Format.intercalate newLine $
                filter (mempty /=)
                    [ Format.intercalate newLine (List.sort $ map pragma (concatMap pragmaNames pragmas))
                    , Format.intercalate (newLine <> newLine <> newLine) $
                        filter (mempty /=)
                            [ Format.intercalate (newLine <> newLine) $
                                filter (mempty /=)
                                    [ head head'
                                    , Format.intercalate newLine (List.sort $ map Import.format imports)
                                    ]
                            , Format.intercalate
                                (newLine <> newLine <> newLine)
                                (map (Format.intercalate newLine . map Declaration.format) (Declaration.group declarations))
                            ]
                    ]
format _ =
    error "xml not supported"


pragmaNames :: ModulePragma CommentedSrc -> [Name CommentedSrc]
pragmaNames (LanguagePragma _ names) =
    names
pragmaNames _ =
    undefined


pragma :: Name CommentedSrc -> Format
pragma name =
    "{-# LANGUAGE " <> Atom.name name <> " #-}"


head :: Maybe (ModuleHead CommentedSrc) -> Format
head head_ =
    case head_ of
        Nothing ->
            ""

        Just (ModuleHead _ name' _ specList) ->
            "module "
                <> Atom.moduleName name'
                <> maybe "" ((<>) newLine . exportSpecList) specList
                <> " where"


exportSpecList :: ExportSpecList CommentedSrc -> Format
exportSpecList (ExportSpecList _ specs) =
    Format.indent $
        Format.wrap "( " (newLine <> ")") (newLine <> ", ") (map exportSpec specs)


exportSpec :: ExportSpec CommentedSrc -> Format
exportSpec (EVar _ qname)
    | Atom.isSymbol qname =
        "(" <> Atom.qname qname <> ")"
    | otherwise =
        Atom.qname qname
exportSpec (EAbs _ _ qname) =
    Atom.qname qname
exportSpec (EThingWith _ wildcard qname cnames) =
    Atom.qname qname
        <> case wildcard of
            NoWildcard _ ->
                Format.wrap "(" ")" ", " (map Atom.cname cnames)

            EWildcard _ _ ->
                "(..)"
exportSpec (EModuleContents _ moduleName) =
    "module " <> Atom.moduleName moduleName
