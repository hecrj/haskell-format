{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Atom
  ( moduleName
  , name
  , cname
  , qname
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

specialCon :: SpecialCon CommentedSrc -> Format
specialCon (UnitCon _)          = "()"
specialCon (ListCon _)          = "[]"
specialCon (FunCon _)           = "->"
specialCon (TupleCon _ _ _)     = "(,)"
specialCon (Cons _)             = "(:)"
specialCon (UnboxedSingleCon _) = "(# #)"
