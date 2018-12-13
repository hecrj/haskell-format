{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Atom
    ( moduleName
    , name
    , cname
    , qname
    , isSymbol
    , qop
    ) where

import Language.Haskell.Exts hiding (name)
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Types


moduleName :: ModuleName CommentedSrc -> Format
moduleName (ModuleName _ name') =
    Format.fromString name'


name :: Name CommentedSrc -> Format
name (Ident _ name') =
    Format.fromString name'
name (Symbol _ name') =
    Format.fromString name'


cname :: CName CommentedSrc -> Format
cname (VarName _ name') =
    name name'
cname (ConName _ name') =
    name name'


qname :: QName CommentedSrc -> Format
qname (Qual _ moduleName' name') =
    moduleName moduleName' <> "." <> name name'
qname (UnQual _ name') =
    name name'
qname (Special _ specialCon') =
    specialCon specialCon'


isSymbol :: QName CommentedSrc -> Bool
isSymbol (Qual _ _ (Ident _ _)) =
    False
isSymbol (UnQual _ (Ident _ _)) =
    False
isSymbol _ =
    True


qop :: QOp CommentedSrc -> Format
qop (QVarOp _ qname')
    | isSymbol qname' =
        qname qname'
    | otherwise =
        "`" <> qname qname' <> "`"
qop (QConOp _ qname')
    | isSymbol qname' =
        qname qname'
    | otherwise =
        "`" <> qname qname' <> "`"


specialCon :: SpecialCon CommentedSrc -> Format
specialCon (UnitCon _) =
    "()"
specialCon (ListCon _) =
    "[]"
specialCon (FunCon _) =
    "->"
specialCon (TupleCon _ _ n) =
    "(" <> mconcat (replicate (n - 1) ",") <> ")"
specialCon (Cons _) =
    ":"
specialCon (UnboxedSingleCon _) =
    undefined
specialCon (ExprHole _) =
    undefined
