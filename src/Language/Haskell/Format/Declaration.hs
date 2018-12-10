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
import Prelude hiding (head)

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

format (TypeDecl src head_ type_)
  | takesOneLine src = "type " <> head head_ <> " = " <> Atom.type' type_
  | otherwise =
    "type " <> head head_ <> " =" <> newLine <> Format.indent (Atom.type' type_)

format (DataDecl _ dataOrNew _ head_ qualCons derivings) =
  Format.intercalate newLine $ filter (mempty /=)
    [ instruction <> " " <> head head_
    , Format.indent $
        "= " <> Format.intercalate (newLine <> "| ")
          (map qualifiedConstructor qualCons)
    , case derivings of
        first : _ ->
          Format.indent $ "deriving " <> deriving_ first

        _ ->
          ""
    ]
  where
    instruction =
      case dataOrNew of
        DataType _ ->
          "data"

        NewType _ ->
          "newtype"

format d = error (show d)

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

head :: DeclHead CommentedSrc -> Format
head (DHead _ name) = Atom.name name
head (DHApp _ head_ var) =
  head head_ <> " " <> typeVarBind var
head h = error $ show h

typeVarBind :: TyVarBind CommentedSrc -> Format
typeVarBind (UnkindedVar _ name) = Atom.name name
typeVarBind t = error $ show t

qualifiedConstructor :: QualConDecl CommentedSrc -> Format
qualifiedConstructor (QualConDecl _ _ _ con_) = constructor con_

constructor :: ConDecl CommentedSrc -> Format
constructor (ConDecl _ name types) =
  Format.intercalate " " (Atom.name name : map Atom.type' types)
constructor (RecDecl src name fields)
  | takesOneLine src = Atom.name name <> " " <> Format.wrap "{ " " }" ", " (map field fields)
  | otherwise =
    Atom.name name <> newLine <>
      Format.indent
        (Format.wrap "{ " (newLine <> "}") (newLine <> ", ") (map field fields))
constructor con = error $ show con

field :: FieldDecl CommentedSrc -> Format
field (FieldDecl _ (name:_) type_) =
  Atom.name name <> " :: " <> Atom.type' type_
field f = error $ show f

deriving_ :: Deriving CommentedSrc -> Format
deriving_ (Deriving _ _ instanceRules) =
  case instanceRules of
    [instanceRule_] ->
      instanceRule instanceRule_

    _ ->
      Format.wrap "(" ")" ", " (map instanceRule instanceRules)

instanceRule :: InstRule CommentedSrc -> Format
instanceRule (IRule _ _ _ instanceHead_) = instanceHead instanceHead_
instanceRule (IParen _ rule) = instanceRule rule

instanceHead :: InstHead CommentedSrc -> Format
instanceHead (IHCon _ qname) = Atom.qname qname
instanceHead (IHApp _ instanceHead_ type_) =
  instanceHead instanceHead_ <> " " <> Atom.type' type_
instanceHead instanceHead_ = error $ show instanceHead_
