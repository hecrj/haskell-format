{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
module Language.Haskell.Format.Module
  ( Language.Haskell.Format.Module.decl
  ) where

import Language.Haskell.Exts hiding (name)
import qualified Language.Haskell.Format.Atom as Atom
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
      , Format.intercalate (newLine <> newLine)
        [ head head'
        , Format.intercalate newLine (map Import.decl imports)
        , Format.intercalate newLine (map declaration declarations)
        ]
      ]
decl _ = error "xml not supported"

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

declaration :: Decl CommentedSrc -> Format
declaration (TypeSig _ names type') =
  Format.intercalate ", " (map Atom.name names)
    <> " :: "
    <> Atom.type' type'

declaration (PatBind _ pattern' rhs' _) =
  Format.intercalate separator
    [ pat pattern'
    , rhs rhs'
    ]
  where
    separator = case rhs' of
      UnGuardedRhs _ _ -> " "
      GuardedRhss _ _  -> newLine

declaration d = Format.fromString (show d)

pat :: Pat CommentedSrc -> Format
pat (PVar _ name) = Atom.name name
pat p             = Format.fromString (show p)

rhs :: Rhs CommentedSrc -> Format
rhs (UnGuardedRhs _ expression') =
  Format.intercalate newLine
    [ "="
    , Format.indent (expression expression')
    ]

rhs s = Format.fromString (show s)


expression :: Exp CommentedSrc -> Format
expression (App _ e1 e2) =
  expression e1 <> " " <> expression e2

expression (Var _ qname) = Atom.qname qname
expression (Lit _ literal') = literal literal'

expression e = Format.fromString (show e)

literal :: Literal CommentedSrc -> Format
literal (String _ s _) = "\"" <> Format.fromString s <> "\""
literal l              = Format.fromString (show l)
