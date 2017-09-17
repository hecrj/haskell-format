{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Declaration
  ( group
  , format
  ) where

import qualified Data.List as List
import Language.Haskell.Exts hiding (alt, binds, name)
import qualified Language.Haskell.Format.Atom as Atom
import qualified Language.Haskell.Format.Expression as Expression
import Language.Haskell.Format.Internal as Format
import qualified Language.Haskell.Format.Pattern as Pattern
import Language.Haskell.Format.Types

group :: [Decl CommentedSrc] -> [[Decl CommentedSrc]]
group =
  List.groupBy signatureBinding
  where
    signatureBinding TypeSig{} PatBind{} = True
    signatureBinding TypeSig{} FunBind{} = True
    signatureBinding _ _                 = False

format :: Decl CommentedSrc -> Format
format (TypeSig _ names type')
  | takesOneLine (ann type') =
      typeNames <> " :: " <> Atom.type' type'
  | otherwise =
      Format.intercalate newLine
        [ typeNames <> " ::"
        , Format.indent (Atom.type' type')
        ]
  where
    typeNames = Format.intercalate ", " (map Atom.name names)

format (PatBind _ pattern' rhs_ maybeWhere) =
  mconcat
    [ Pattern.format pattern'
    , rhsInlined rhs_
    , maybe mempty ((newLine <>) . Format.indent . where_) maybeWhere
    ]

format (FunBind _ matches) =
  Format.intercalate newLine
    (map match matches)

format d = Format.fromString (show d)

match :: Match CommentedSrc -> Format
match (Match _ name patterns rhs_ maybeWhere) =
  Format.intercalate " "
      [ Atom.name name
      , Format.intercalate " " (map Pattern.format patterns)
      ]
      <> rhsInlined rhs_
      <> maybe mempty ((newLine <>) . Format.indent . where_) maybeWhere

match m = Format.fromString (show m)

where_ :: Binds CommentedSrc -> Format
where_ binds_ =
  Format.intercalate newLine
    [ "where"
    , Format.indent (binds binds_)
    ]

rhsInlined :: Rhs CommentedSrc -> Format
rhsInlined (UnGuardedRhs _ expression) =
  " " <> Format.intercalate newLine
    [ "="
    , Format.indent (Expression.format expression)
    ]
rhsInlined (GuardedRhss _ guardedRhss) =
  newLine <>
    Format.indent (Format.intercalate newLine (map guardedRhs guardedRhss))

guardedRhs :: GuardedRhs CommentedSrc -> Format
guardedRhs (GuardedRhs _ [stmt] expression) =
  Format.intercalate newLine
    [ Format.intercalate " "
      [ "|"
      , Expression.statement stmt
      , "="
      ]
    , Format.indent (Expression.format expression)
    ]
guardedRhs g = Format.fromString (show g)

binds :: Binds CommentedSrc -> Format
binds (BDecls _ declarations) =
  Format.intercalate (newLine <> newLine) $
    map (Format.intercalate newLine . map format) (group declarations)

binds b = Format.fromString $ show b
