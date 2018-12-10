{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Pattern
    ( format
    ) where

import Language.Haskell.Exts
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Types
import qualified Language.Haskell.Format.Atom as Atom
import qualified Language.Haskell.Format.Literal as Literal


format :: Pat CommentedSrc -> Format
format (PVar _ name@(Ident _ _)) =
    Atom.name name
format (PVar _ name@(Symbol _ _)) =
    "(" <> Atom.name name <> ")"
format (PWildCard _) =
    "_"
format (PLit _ (Signless _) lit) =
    Literal.format lit
format (PLit _ (Negative _) lit) =
    "-" <> Literal.format lit
format (PParen _ pattern_) =
    mconcat [ "(", format pattern_, ")" ]
format (PInfixApp _ left qname right) =
    mconcat [ format left, Atom.qname qname, format right ]
format (PList _ patterns) =
    case patterns of
        [] ->
            "[]"

        _ ->
            Format.wrap "[ " " ]" ", " (map format patterns)
format (PTuple _ _ patterns) =
    Format.wrap "( " " )" ", " (map format patterns)
format (PRec src qname fieldPatterns)
    | takesOneLine src =
        Atom.qname qname <> Format.wrap "{ " " }" ", " (map fieldPattern fieldPatterns)
    | otherwise =
        Atom.qname qname <> newLine
            <> Format.indent
                (Format.wrap "{ " (newLine <> "}") (newLine <> ", ")
                    (map fieldPattern fieldPatterns)
                )
format (PApp _ qname patterns)
    | Atom.isSymbol qname =
        Format.intercalate " " ("(" <> Atom.qname qname <> ")" : map format patterns)
    | otherwise =
        Format.intercalate " " (Atom.qname qname : map format patterns)
format (PAsPat _ name pattern_) =
    Atom.name name <> "@" <> format pattern_
format p =
    error (show p)


fieldPattern :: PatField CommentedSrc -> Format
fieldPattern (PFieldPat _ qname pattern_) =
    Atom.qname qname <> " = " <> format pattern_
fieldPattern (PFieldPun _ qname) =
    Atom.qname qname
fieldPattern (PFieldWildcard _) =
    "_"
