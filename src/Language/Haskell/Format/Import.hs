{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Import
  ( decl
  ) where

import Language.Haskell.Exts hiding (name)
import qualified Language.Haskell.Format.Atom as Atom
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Types

decl :: ImportDecl CommentedSrc -> Format
decl ImportDecl { importQualified, importModule, importAs = as, importSpecs = specs } =
  Format.intercalate " "
    [ "import"
    , if importQualified then "qualified" else ""
    , Atom.moduleName importModule
    , maybe "" ((<>) "as " . Atom.moduleName) as
    , maybe "" specList specs
    ]

specList :: ImportSpecList CommentedSrc -> Format
specList (ImportSpecList _ _ specs) =
  "(" <> Format.intercalate ", " (map spec specs) <> ")"

spec :: ImportSpec CommentedSrc -> Format
spec (IVar _ name) = Atom.name name
spec (IAbs _ _ name) = Atom.name name
spec (IThingAll _ name) = Atom.name name <> "(..)"
spec (IThingWith _ name cnames) = Atom.name name <> "(" <>
  Format.intercalate ", " (map Atom.cname cnames) <> ")"
