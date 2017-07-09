{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Declaration
  ( group
  , format
  ) where

import qualified Data.List as List
import Language.Haskell.Exts hiding (name)
import qualified Language.Haskell.Format.Atom as Atom
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Types

group :: [Decl CommentedSrc] -> [[Decl CommentedSrc]]
group =
  List.groupBy signatureBinding
  where
    signatureBinding TypeSig{} PatBind{} = True
    signatureBinding PatBind{} PatBind{} = True
    signatureBinding _ _                 = False

format :: Decl CommentedSrc -> Format
format (TypeSig _ names type') =
  Format.intercalate ", " (map Atom.name names)
    <> " :: "
    <> Atom.type' type'

format (PatBind _ pattern' rhs' _) =
  Format.intercalate separator
    [ pat pattern'
    , rhs rhs'
    ]
  where
    separator = case rhs' of
      UnGuardedRhs _ _ -> " "
      GuardedRhss _ _  -> newLine

format d = Format.fromString (show d)

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
expression (App src e1 e2)
  | takesOneLine src = expression e1 <> " " <> expression e2
  | otherwise = expression e1 <> newLine <> Format.indent (expression e2)

expression (Var _ qname) = Atom.qname qname
expression (Lit _ literal') = literal literal'
expression (List src elements)
  | takesOneLine src = Format.wrap "[ " " ]" ", " (map expression elements)
  | otherwise = Format.wrap "[ " (newLine <> "]") (newLine <> ", ") (map expression elements)

expression e = Format.fromString (show e)

literal :: Literal CommentedSrc -> Format
literal (String _ s _) = "\"" <> Format.fromString s <> "\""
literal l              = Format.fromString (show l)
