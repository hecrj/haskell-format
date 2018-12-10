{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Atom
  ( moduleName
  , name
  , cname
  , qname
  , qop
  , type'
  ) where

import Language.Haskell.Exts hiding (name)
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Types

moduleName :: ModuleName CommentedSrc -> Format
moduleName (ModuleName _ name') = Format.fromString name'

name :: Name CommentedSrc -> Format
name (Ident _ name')  = Format.fromString name'
name (Symbol _ name') = Format.fromString name'

cname :: CName CommentedSrc -> Format
cname (VarName _ name') = name name'
cname (ConName _ name') = name name'

qname :: QName CommentedSrc -> Format
qname (Qual _ moduleName' name') = moduleName moduleName' <> "." <> name name'
qname (UnQual _ name')           = name name'
qname (Special _ specialCon')    = specialCon specialCon'

isSymbol :: QName CommentedSrc -> Bool
isSymbol (Qual _ _ (Ident _ _)) = False
isSymbol (UnQual _ (Ident _ _)) = False
isSymbol _ = True

qop :: QOp CommentedSrc -> Format
qop (QVarOp _ qname')
  | isSymbol qname' = qname qname'
  | otherwise = "`" <> qname qname' <> "`"
qop (QConOp _ qname')
  | isSymbol qname' = qname qname'
  | otherwise = "`" <> qname qname' <> "`"

specialCon :: SpecialCon CommentedSrc -> Format
specialCon (UnitCon _)          = "()"
specialCon (ListCon _)          = "[]"
specialCon (FunCon _)           = "->"
specialCon TupleCon {}          = ","
specialCon (Cons _)             = ":"
specialCon (UnboxedSingleCon _) = undefined
specialCon (ExprHole _)         = undefined

type' :: Type CommentedSrc -> Format
type' (TyApp _ t1 t2)  = type' t1 <> " " <> type' t2
type' (TyCon _ qname') = qname qname'
type' (TyList _ t)     = "[" <> type' t <> "]"
type' (TyTuple src _ types)
  | takesOneLine src = Format.wrap "( " " )" ", " (map type' types)
  | otherwise = Format.wrap "( " (newLine <> ")") (newLine <> ", ") (map type' types)
type' (TyVar _ name') = name name'
type' (TyFun src t1 t2)
  | takesOneLine src =
    type' t1 <> " -> " <> type' t2
  | otherwise =
    Format.intercalate newLine
      [ type' t1
      , "-> " <> type' t2
      ]
type' (TyParen src type_)
  | takesOneLine src = "(" <> type' type_ <> ")"
  | otherwise = "(" <> type' type_ <> newLine <> ")"
type' t                = error (show t)
