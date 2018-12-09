{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Expression
  ( format
  , statement
  ) where

import Language.Haskell.Exts hiding (alt)
import qualified Language.Haskell.Format.Atom as Atom
import Language.Haskell.Format.Internal as Format
import qualified Language.Haskell.Format.Nested as Nested
import qualified Language.Haskell.Format.Literal as Literal
import qualified Language.Haskell.Format.Pattern as Pattern
import Language.Haskell.Format.Types

format :: Exp CommentedSrc -> Format
format (Var _ qname) = Atom.qname qname
format (Con _ qname) = Atom.qname qname
format (Lit _ literal') = Literal.format literal'
format (App src e1 e2)
  | takesOneLine src = format e1 <> " " <> format e2
  | otherwise = format e1 <> newLine <> Format.indent (format e2)
format (List _ []) = "[]"
format (List src elements)
  | takesOneLine src = Format.wrap "[ " " ]" ", " (map format elements)
  | otherwise = Format.wrap "[ " (newLine <> "]") (newLine <> ", ") (map format elements)
format (InfixApp src left qop right)
  | takesOneLine src =
    Format.intercalate " "
      [ format left
      , Atom.qop qop
      , format right
      ]
  | otherwise =
    format left <> newLine <>
      (case right of
        List _ _ ->
          Format.indent (Nested.qop qop (format right))

        _ ->
          Format.indent (Atom.qop qop <> " " <> format right)
      )
format (If src cond then_ else_)
  | takesOneLine src =
    Format.intercalate " "
      [ "if"
      , format cond
      , "then"
      , format then_
      , "else"
      , format else_
      ]
  | otherwise =
    Format.intercalate newLine $
      ifThen
        ++ [ Format.indent (format then_)
           , "else"
           , Format.indent (format else_)
           ]
  where
    ifThen
      | takesOneLine (ann cond) =
        [ Format.intercalate " " [ "if", format cond, "then" ] ]
      | otherwise =
        [ "if " <> format cond
        , "then"
        ]
format (Case _ target alts) =
  Format.intercalate newLine
    [ caseOf
    , Format.indent cases
    ]
  where
    caseOf
      | takesOneLine (ann target) =
        Format.intercalate " "
          [ "case"
          , format target
          , "of"
          ]
      | otherwise =
        Format.intercalate newLine
          [ "case"
          , Format.indent (format target)
          , "of"
          ]

    cases =
      Format.intercalate (newLine <> newLine) (map alt alts)

format (Do _ statements) =
  Format.intercalate newLine
    [ "do"
    , Format.indent $ Format.intercalate newLine (map statement statements)
    ]

format (Paren _ expr)
  | takesOneLine (ann expr) = "(" <> format expr <> ")"
  | otherwise = "(" <> format expr <> newLine <> ")"

format e = error (show e)

statement :: Stmt CommentedSrc -> Format
statement (Generator src pattern_ expression)
  | takesOneLine src = Pattern.format pattern_ <> " <- " <> format expression
  | otherwise =
    Format.intercalate newLine
      [ Pattern.format pattern_ <> " <-"
      , Format.indent (format expression)
      ]
statement (Qualifier _ expression) =
  format expression
statement s = Format.fromString (show s)

alt :: Alt CommentedSrc -> Format
alt (Alt _ pat_ (UnGuardedRhs _ expression) _) =
  Format.intercalate " "
    [ Pattern.format pat_
    , Format.intercalate newLine
      [ "->"
      , Format.indent (format expression)
      ]
    ]
alt (Alt _ pat (GuardedRhss _ rhss) _) =
  Format.intercalate (newLine <> newLine) (map guardedRhs rhss)
  where
    guardedRhs (GuardedRhs _ [stmt] expression) =
      Format.intercalate newLine
        [ Format.intercalate " "
          [ Pattern.format pat <> " | " <> statement stmt
          , "->"
          ]
        , Format.indent (format expression)
        ]
    guardedRhs g = Format.fromString (show g)
