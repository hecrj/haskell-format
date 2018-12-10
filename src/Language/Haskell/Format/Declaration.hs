{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Declaration
  ( group
  , format
  ) where

import qualified Data.List as List
import Language.Haskell.Exts hiding (alt, binds, name)
import qualified Language.Haskell.Format.Atom as Atom
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Nested as Nested
import qualified Language.Haskell.Format.Pattern as Pattern
import qualified Language.Haskell.Format.Literal as Literal
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
rhsInlined (UnGuardedRhs _ expr) =
  " " <> Format.intercalate newLine
    [ "="
    , Format.indent (expression expr)
    ]
rhsInlined (GuardedRhss _ guardedRhss) =
  newLine <>
    Format.indent (Format.intercalate newLine (map guardedRhs guardedRhss))

guardedRhs :: GuardedRhs CommentedSrc -> Format
guardedRhs (GuardedRhs _ [stmt] expr) =
  Format.intercalate newLine
    [ Format.intercalate " "
      [ "|"
      , statement stmt
      , "="
      ]
    , Format.indent (expression expr)
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

-- Expression

expression :: Exp CommentedSrc -> Format
expression (Var _ qname) = Atom.qname qname
expression (Con _ qname) = Atom.qname qname
expression (Lit _ literal') = Literal.format literal'
expression (App src e1 e2)
  | takesOneLine src = expression e1 <> " " <> expression e2
  | otherwise = expression e1 <> newLine <> Format.indent (expression e2)
expression (List _ []) = "[]"
expression (List src elements)
  | takesOneLine src = Format.wrap "[ " " ]" ", " (map expression elements)
  | otherwise = Format.wrap "[ " (newLine <> "]") (newLine <> ", ") (map expression elements)
expression (Tuple src _ elements)
  | takesOneLine src = Format.wrap "( " " )" ", " (map expression elements)
  | otherwise = Format.wrap "( " (newLine <> ")") (newLine <> ", ") (map expression elements)
expression (InfixApp src left qop right)
  | takesOneLine src =
    Format.intercalate " "
      [ expression left
      , Atom.qop qop
      , expression right
      ]
  | otherwise =
    expression left <> newLine <>
      (case right of
        List _ _ ->
          Format.indent (Nested.qop qop (expression right))

        _ ->
          Format.indent (Atom.qop qop <> " " <> expression right)
      )
expression (If src cond then_ else_)
  | takesOneLine src =
    Format.intercalate " "
      [ "if"
      , expression cond
      , "then"
      , expression then_
      , "else"
      , expression else_
      ]
  | otherwise =
    Format.intercalate newLine $
      ifThen
        ++ [ Format.indent (expression then_)
           , "else"
           , Format.indent (expression else_)
           ]
  where
    ifThen
      | takesOneLine (ann cond) =
        [ Format.intercalate " " [ "if", expression cond, "then" ] ]
      | otherwise =
        [ "if " <> expression cond
        , "then"
        ]
expression (Case _ target alts) =
  Format.intercalate newLine
    [ caseOf
    , Format.indent cases
    ]
  where
    caseOf
      | takesOneLine (ann target) =
        Format.intercalate " "
          [ "case"
          , expression target
          , "of"
          ]
      | otherwise =
        Format.intercalate newLine
          [ "case"
          , Format.indent (expression target)
          , "of"
          ]

    cases =
      Format.intercalate (newLine <> newLine) (map alt alts)

expression (Do _ statements) =
  Format.intercalate newLine
    [ "do"
    , Format.indent $ Format.intercalate newLine (map statement statements)
    ]

expression (Let _ binds_ expr) =
  "let" <> newLine <>
    Format.intercalate newLine
    [ Format.indent (binds binds_)
    , "in"
    , Format.indent (expression expr)
    ]
expression (Lambda src patterns expr)
  | takesOneLine src =
    "\\" <> Format.intercalate " " (map Pattern.format patterns) <> " -> " <> expression expr
  | otherwise =
    "\\" <> Format.intercalate " " (map Pattern.format patterns) <> " ->" <> newLine <>
      Format.indent (expression expr)
expression (RightSection _ qop expr) = "(" <> Atom.qop qop <> " " <> expression expr <> ")"
expression (LeftSection _ expr qop) = "(" <> expression expr <> " " <> Atom.qop qop <> ")"
expression (Paren _ expr)
  | takesOneLine (ann expr) = "(" <> expression expr <> ")"
  | otherwise = "(" <> expression expr <> newLine <> ")"

expression e = error (show e)

statement :: Stmt CommentedSrc -> Format
statement (Generator src pattern_ expr)
  | takesOneLine src = Pattern.format pattern_ <> " <- " <> expression expr
  | otherwise =
    Format.intercalate newLine
      [ Pattern.format pattern_ <> " <-"
      , Format.indent (expression expr)
      ]
statement (Qualifier _ expr) =
  expression expr
statement (LetStmt _ binds_) =
  "let" <> newLine <> Format.indent (binds binds_) <> newLine
statement s = error $ show s


alt :: Alt CommentedSrc -> Format
alt (Alt _ pat_ (UnGuardedRhs _ expr) _) =
  Format.intercalate " "
    [ Pattern.format pat_
    , Format.intercalate newLine
      [ "->"
      , Format.indent (expression expr)
      ]
    ]
alt (Alt _ pat (GuardedRhss _ rhss) _) =
  Format.intercalate (newLine <> newLine) (map guardedRhs rhss)
  where
    guardedRhs (GuardedRhs _ [stmt] expr) =
      Format.intercalate newLine
        [ Format.intercalate " "
          [ Pattern.format pat <> " | " <> statement stmt
          , "->"
          ]
        , Format.indent (expression expr)
        ]
    guardedRhs g = Format.fromString (show g)
