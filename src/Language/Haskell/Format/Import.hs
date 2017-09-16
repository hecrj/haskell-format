{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Import
  ( format
  ) where

import Language.Haskell.Exts hiding (name)
import qualified Language.Haskell.Format.Atom as Atom
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Types

format :: ImportDecl CommentedSrc -> Format
format ImportDecl { importQualified, importModule, importAs = as, importSpecs = specs } =
  importLine <> specsSeparator <> maybe "" specList specs
  where
    importLine =
      Format.intercalate " " $ filter (mempty /=)
        [ "import"
        , if importQualified then "qualified" else ""
        , Atom.moduleName importModule
        , maybe "" ((<>) "as " . Atom.moduleName) as
        ]
    specsSeparator =
      case specs of
        Just (ImportSpecList src _ _) ->
          if takesOneLine src then " " else newLine
        _ ->
          ""

specList :: ImportSpecList CommentedSrc -> Format
specList (ImportSpecList src _ specs)
  | takesOneLine src =
      "(" <> Format.intercalate ", " (map spec specs) <> ")"

  | otherwise =
      Format.indent $
        Format.wrap "( " (newLine <> ")") (newLine <> ", ") (map spec specs)

spec :: ImportSpec CommentedSrc -> Format
spec (IVar _ name) = Atom.name name
spec (IAbs _ _ name) = Atom.name name
spec (IThingAll _ name) = Atom.name name <> "(..)"
spec (IThingWith _ name cnames) =
  Atom.name name <> Format.wrap "(" ")" ", " (map Atom.cname cnames)
