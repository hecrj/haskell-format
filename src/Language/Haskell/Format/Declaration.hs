{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Declaration
  ( group
  , format
  ) where

import qualified Data.List as List
import Language.Haskell.Exts hiding (alt, name)
import qualified Language.Haskell.Format.Atom as Atom
import Language.Haskell.Format.Internal as Format
import qualified Language.Haskell.Format.Nested as Nested
import Language.Haskell.Format.Types

group :: [Decl CommentedSrc] -> [[Decl CommentedSrc]]
group =
  List.groupBy signatureBinding
  where
    signatureBinding TypeSig{} PatBind{} = True
    signatureBinding PatBind{} PatBind{} = True
    signatureBinding TypeSig{} FunBind{} = True
    signatureBinding FunBind{} FunBind{} = True
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

format (PatBind _ pattern' rhs' _) =
  Format.intercalate separator
    [ pat pattern'
    , rhs rhs'
    ]
  where
    separator = case rhs' of
      UnGuardedRhs _ _ -> " "
      GuardedRhss _ _  -> newLine

format (FunBind _ matches) =
  Format.intercalate newLine
    (map match matches)

format d = Format.fromString (show d)

pat :: Pat CommentedSrc -> Format
pat (PVar _ name)             = Atom.name name
pat (PWildCard _)             = "_"
pat (PLit _ (Signless _) lit) = literal lit
pat (PLit _ (Negative _) lit) = "-" <> literal lit
pat p                         = Format.fromString (show p)

match :: Match CommentedSrc -> Format
match (Match _ name patterns rhs' _) =
  Format.intercalate " "
    [ Atom.name name
    , Format.intercalate " " (map pat patterns)
    , rhs rhs'
    ]
match m = Format.fromString (show m)

rhs :: Rhs CommentedSrc -> Format
rhs (UnGuardedRhs _ expression') =
  Format.intercalate newLine
    [ "="
    , Format.indent (expression expression')
    ]

rhs s = Format.fromString (show s)


expression :: Exp CommentedSrc -> Format
expression (Var _ qname) = Atom.qname qname
expression (Con _ qname) = Atom.qname qname
expression (Lit _ literal') = literal literal'
expression (App src e1 e2)
  | takesOneLine src = expression e1 <> " " <> expression e2
  | otherwise = expression e1 <> newLine <> Format.indent (expression e2)
expression (List _ []) = "[]"
expression (List src elements)
  | takesOneLine src = Format.wrap "[ " " ]" ", " (map expression elements)
  | otherwise = Format.wrap "[ " (newLine <> "]") (newLine <> ", ") (map expression elements)
expression (InfixApp src left qop right)
  | takesOneLine src = Format.intercalate " "
    [ expression left
    , Atom.qop qop
    , expression right
    ]
  | otherwise =
    expression left <> newLine <>
      Format.indent (Nested.qop qop (expression right))
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
        [ Nested.if_ (expression cond)
        , "then"
        ]
expression (Case _ target alts) =
  Format.intercalate newLine
    [ caseOf
    , Format.indent cases
    ]
  where
    caseOf =
      Format.intercalate " "
        [ "case"
        , expression target
        , "of"
        ]

    cases =
      Format.intercalate (newLine <> newLine) (map alt alts)

expression e = Format.fromString (show e)

literal :: Literal CommentedSrc -> Format
literal (String _ s _) = "\"" <> Format.fromString s <> "\""
literal (Int _ _ s)    = Format.fromString s
literal l              = Format.fromString (show l)

alt :: Alt CommentedSrc -> Format
alt (Alt _ pat_ (UnGuardedRhs _ expr) _) =
  Format.intercalate newLine
    [ pat pat_ <> " ->"
    , Format.indent (expression expr)
    ]

alt a =
  Format.fromString (show a)
