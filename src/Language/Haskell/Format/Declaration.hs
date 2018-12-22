{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Format.Declaration
    ( group
    , format
    ) where

import Language.Haskell.Exts hiding (alt, binds, name)
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Nested as Nested
import Language.Haskell.Format.Types
import Prelude hiding (head)
import qualified Data.List as List
import qualified Language.Haskell.Format.Atom as Atom
import qualified Language.Haskell.Format.Literal as Literal
import qualified Language.Haskell.Format.Pattern as Pattern


group :: [Decl CommentedSrc] -> [[Decl CommentedSrc]]
group =
    List.groupBy signatureBinding
    where
        signatureBinding TypeSig{  } PatBind{  } =
            True
        signatureBinding TypeSig{  } FunBind{  } =
            True
        signatureBinding _ _ =
            False


format :: Decl CommentedSrc -> Format
format decl =
    case snd (ann decl) of
        [] ->
            format_ decl

        comments ->
            newLine
                <> Format.intercalate (newLine <> newLine <> newLine)
                    [ Format.intercalate newLine (map comment comments)
                    , format_ decl
                    ]


comment :: Comment -> Format
comment (Comment _ _ c) =
    "--" <> Format.fromString c


format_ :: Decl CommentedSrc -> Format
format_ (TypeSig _ names type_)
    | takesOneLine (ann type_) =
        typeNames <> " :: " <> type' type_
    | otherwise =
        Format.intercalate newLine
            [ typeNames <> " ::"
            , Format.indent (type' type_)
            ]
    where
        typeNames =
            Format.intercalate ", " (map name names)

        name n@(Ident _ _) =
            Atom.name n
        name n@(Symbol _ _) =
            "(" <> Atom.name n <> ")"
format_ (PatBind _ pattern' rhs_ maybeWhere) =
    mconcat
        [ Pattern.format pattern'
        , rhsInlined rhs_
        , maybe mempty ((newLine <>) . Format.indent . where_) maybeWhere
        ]
format_ (FunBind _ matches) =
    Format.intercalate newLine
        (map match matches)
format_ (TypeDecl src head_ type_)
    | takesOneLine src =
        "type " <> head head_ <> " = " <> type' type_
    | otherwise =
        "type " <> head head_ <> " =" <> newLine <> Format.indent (type' type_)
format_ (DataDecl _ dataOrNew _ head_ qualCons derivings) =
    Format.intercalate newLine $
        filter (mempty /=)
            [ instruction <> " " <> head head_
            , Format.indent $
                "= "
                    <> Format.intercalate (newLine <> "| ")
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
format_ (GDataDecl _ dataOrNew _ head_ _ gadtDeclarations derivings) =
    Format.intercalate newLine $
        filter (mempty /=)
            [ instruction <> " " <> head head_ <> " where"
            , Format.indent $
                Format.intercalate (newLine <> newLine)
                    (map gadtDeclaration gadtDeclarations)
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
format_ (ClassDecl _ maybeContext head_ _ maybeClassDeclarations) =
    Format.intercalate newLine $
        filter (mempty /=)
            [ Format.intercalate " " $
                filter (mempty /=)
                    [ "class"
                    , maybe mempty context maybeContext
                    , head head_
                    , "where"
                    ]
            , case maybeClassDeclarations of
                Just classDeclarations ->
                    Format.indent $
                        Format.intercalate newLine
                            (map classDeclaration classDeclarations)

                _ ->
                    ""
            ]
format_ (InstDecl _ _ instanceRule_ maybeInstanceDeclarations) =
    Format.intercalate newLine $
        filter (mempty /=)
            [ Format.intercalate " "
                [ "instance"
                , instanceRule instanceRule_
                , "where"
                ]
            , case maybeInstanceDeclarations of
                Just instanceDeclarations ->
                    Format.indent $
                        Format.intercalate (newLine <> newLine)
                            (map instanceDeclaration instanceDeclarations)

                _ ->
                    ""
            ]
format_ d =
    error (show d)


classDeclaration :: ClassDecl CommentedSrc -> Format
classDeclaration classDeclaration_ =
    case classDeclaration_ of
        ClsDecl _ declaration ->
            format declaration

        d ->
            error (show d)


instanceDeclaration :: InstDecl CommentedSrc -> Format
instanceDeclaration instanceDeclaration_ =
    case instanceDeclaration_ of
        InsDecl _ declaration ->
            format declaration

        InsType src left right ->
            Format.intercalate " "
                [ "type"
                , if takesOneLine src then
                    type' left <> " = " <> type' right

                  else
                    type' left <> newLine
                        <> Format.indent ("= " <> type' right)
                ]

        d ->
            error (show d)


match :: Match CommentedSrc -> Format
match (Match _ name patterns rhs_ maybeWhere) =
    Format.intercalate " "
        [ matchName
        , Format.intercalate " " (map Pattern.format patterns)
        ]
        <> rhsInlined rhs_
        <> maybe mempty ((newLine <>) . Format.indent . where_) maybeWhere
    where
        matchName =
            case name of
                Ident _ _ ->
                    Atom.name name

                Symbol _ _ ->
                    "(" <> Atom.name name <> ")"
match m =
    error (show m)


where_ :: Binds CommentedSrc -> Format
where_ binds_ =
    Format.intercalate newLine
        [ "where"
        , Format.indent (binds binds_)
        ]


rhsInlined :: Rhs CommentedSrc -> Format
rhsInlined (UnGuardedRhs _ expr) =
    " "
        <> (case expr of
            Do _ (_ : _ : _) ->
                Format.intercalate " "
                    [ "="
                    , expression expr
                    ]

            _ ->
                Format.intercalate newLine
                    [ "="
                    , Format.indent (expression expr)
                    ]
        )
rhsInlined (GuardedRhss _ guardedRhss) =
    newLine
        <> Format.indent (Format.intercalate newLine (map guardedRhs guardedRhss))


guardedRhs :: GuardedRhs CommentedSrc -> Format
guardedRhs (GuardedRhs _ [ stmt ] expr) =
    Format.intercalate newLine
        [ Format.intercalate " "
            [ "|"
            , statement stmt
            , "="
            ]
        , Format.indent (expression expr)
        ]
guardedRhs g =
    Format.fromString (show g)


binds :: Binds CommentedSrc -> Format
binds (BDecls _ declarations) =
    Format.intercalate (newLine <> newLine) $
        map (Format.intercalate newLine . map format) (group declarations)
binds b =
    Format.fromString $ show b


head :: DeclHead CommentedSrc -> Format
head (DHead _ name@(Ident _ _)) =
    Atom.name name
head (DHead _ name@(Symbol _ _)) =
    "(" <> Atom.name name <> ")"
head (DHApp _ head_ var) =
    head head_ <> " " <> typeVarBind var
head h =
    error $ show h


typeVarBind :: TyVarBind CommentedSrc -> Format
typeVarBind (UnkindedVar _ name) =
    Atom.name name
typeVarBind t =
    error $ show t


qualifiedConstructor :: QualConDecl CommentedSrc -> Format
qualifiedConstructor (QualConDecl _ _ _ con_) =
    constructor con_


constructor :: ConDecl CommentedSrc -> Format
constructor (ConDecl src name types)
    | takesOneLine src =
        Format.intercalate " " (Atom.name name : map type' types)
    | otherwise =
        Format.intercalate newLine
            [ Atom.name name
            , Format.indent $ Format.intercalate newLine (map type' types)
            ]
constructor (RecDecl src name fields)
    | takesOneLine src =
        Atom.name name <> " " <> Format.wrap "{ " " }" ", " (map field fields)
    | otherwise =
        Atom.name name <> newLine
            <> Format.indent
                (Format.wrap "{ " (newLine <> "}") (newLine <> ", ") (map field fields))
constructor con =
    error $ show con


gadtDeclaration :: GadtDecl CommentedSrc -> Format
gadtDeclaration (GadtDecl _ name maybeFields type_) =
    Format.intercalate " " $
        filter (mempty /=)
            [ Atom.name name
            , "::"
            , maybe
                mempty
                (\fields -> Format.wrap "{ " " }" ", " (map field fields) <> " ->")
                maybeFields
            , type' type_
            ]


field :: FieldDecl CommentedSrc -> Format
field (FieldDecl _ (name : _) type_) =
    Atom.name name <> " :: " <> type' type_
field f =
    error $ show f


deriving_ :: Deriving CommentedSrc -> Format
deriving_ (Deriving _ _ instanceRules) =
    case instanceRules of
        [ instanceRule_ ] ->
            instanceRule instanceRule_

        _ ->
            Format.wrap "(" ")" ", " (map instanceRule instanceRules)


instanceRule :: InstRule CommentedSrc -> Format
instanceRule (IRule _ _ _ instanceHead_) =
    instanceHead instanceHead_
instanceRule (IParen _ rule) =
    instanceRule rule


instanceHead :: InstHead CommentedSrc -> Format
instanceHead (IHCon _ qname) =
    Atom.qname qname
instanceHead (IHApp _ instanceHead_ type_) =
    instanceHead instanceHead_ <> " " <> type' type_
instanceHead instanceHead_ =
    error $ show instanceHead_


expression :: Exp CommentedSrc -> Format
expression (Var _ qname)
    | Atom.isSymbol qname =
        "(" <> Atom.qname qname <> ")"
    | otherwise =
        Atom.qname qname
expression (Con _ qname) =
    Atom.qname qname
expression (Lit _ literal') =
    Literal.format literal'
expression (App src e1 e2)
    | takesOneLine src =
        expression e1 <> " " <> expression e2
    | otherwise =
        expression e1 <> newLine <> Format.indent (expression e2)
expression (List _ []) =
    "[]"
expression (List src elements)
    | takesOneLine src =
        Format.wrap "[ " " ]" ", " (map expression elements)
    | otherwise =
        Format.wrap "[ " (newLine <> "]") (newLine <> ", ") (map listOrTupleElement elements)
expression (ListComp src expr qualStmts)
    | takesOneLine src =
        Format.intercalate " "
            [ "["
            , expression expr
            , "|"
            , Format.intercalate ", " (map qualStatement qualStmts)
            , "]"
            ]
    | otherwise =
        Format.intercalate newLine
            [ "["
            , Format.indent (expression expr)
            , Format.indent $
                "| " <> Format.intercalate (newLine <> ", ") (map qualStatement qualStmts)
            , "]"
            ]
expression (EnumFrom _ expr) =
    "[" <> expression expr <> "..]"
expression (Tuple src _ elements)
    | takesOneLine src =
        Format.wrap "( " " )" ", " (map expression elements)
    | otherwise =
        Format.wrap "( " (newLine <> ")") (newLine <> ", ") (map listOrTupleElement elements)
expression (RecConstr src qname fields)
    | takesOneLine src =
        Atom.qname qname
            <> Format.wrap "{ " " }" ", " (map fieldUpdate fields)
    | otherwise =
        Format.intercalate newLine
            [ Atom.qname qname
            , Format.indent $
                Format.wrap "{ " (newLine <> "}") (newLine <> ", ")
                    (map fieldUpdate fields)
            ]
expression (RecUpdate src expr fields)
    | takesOneLine src =
        Format.intercalate " "
            [ expression expr
            , Format.wrap "{ " " }" ", " (map fieldUpdate fields)
            ]
    | otherwise =
        Format.intercalate newLine
            [ expression expr
            , Format.indent $
                Format.wrap "{ " (newLine <> "}") (newLine <> ", ")
                    (map fieldUpdate fields)
            ]
expression (NegApp _ expr) =
    "-" <> expression expr
expression (InfixApp src left qop right)
    | takesOneLine src =
        Format.intercalate " "
            [ expression left
            , Atom.qop qop
            , expression right
            ]
    | otherwise =
        case qop of
            QVarOp _ (UnQual _ (Symbol _ "$")) ->
                case right of
                    Do _ (_ : _ : _) ->
                        expression left <> " " <> Atom.qop qop <> " " <> expression right

                    _ ->
                        expression left <> " " <> Atom.qop qop <> newLine
                            <> Format.indent (expression right)

            _ ->
                expression left <> newLine
                    <> (case right of
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
            ifThen cond
                ++ [ Format.indent (expression then_)
                   , newLine <> "else"
                   , Format.indent (expression else_)
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
    case statements of
        [ statement_ ] ->
            statement statement_

        _ ->
            Format.intercalate newLine
                [ "do"
                , Format.indent
                    (Format.intercalate (newLine <> newLine)
                        (map (Format.intercalate newLine . map statement)
                            (List.groupBy oneLiners statements)
                        )
                    )
                ]
    where
        oneLiners node1 node2 =
            takesOneLine (ann node1) && takesOneLine (ann node2)
expression (Let _ binds_ expr) =
    "let" <> newLine
        <> Format.intercalate newLine
            [ Format.indent (binds binds_)
            , "in"
            , Format.indent (expression expr)
            ]
expression (Lambda src patterns expr) =
    "\\" <> Format.intercalate " " (map Pattern.format patterns) <> " ->" <> body
    where
        body =
            if takesOneLine src then
                " " <> expression expr

            else
                case expr of
                    Do _ (_ : _ : _) ->
                        " " <> expression expr

                    _ ->
                        newLine <> Format.indent (expression expr)
expression (RightSection _ qop expr) =
    "(" <> Atom.qop qop <> " " <> expression expr <> ")"
expression (LeftSection _ expr qop) =
    "(" <> expression expr <> " " <> Atom.qop qop <> ")"
expression (Paren _ expr)
    | takesOneLine (ann expr) =
        "(" <> expression expr <> ")"
    | otherwise =
        "(" <> expression expr <> newLine <> ")"
expression (ExpTypeSig _ expr type_) =
    expression expr <> " :: " <> type' type_
expression e =
    error (show e)


statement :: Stmt CommentedSrc -> Format
statement (Generator src pattern_ expr)
    | takesOneLine src =
        Pattern.format pattern_ <> " <- " <> expression expr
    | otherwise =
        Format.intercalate newLine
            [ Pattern.format pattern_ <> " <-"
            , Format.indent (expression expr)
            ]
statement (Qualifier _ expr) =
    expression expr
statement (LetStmt _ binds_) =
    "let" <> newLine <> Format.indent (binds binds_)
statement s =
    error $ show s


qualStatement :: QualStmt CommentedSrc -> Format
qualStatement qualStmt =
    case qualStmt of
        QualStmt _ stmt ->
            statement stmt

        _ ->
            error (show qualStmt)


alt :: Alt CommentedSrc -> Format
alt (Alt _ pat_ (UnGuardedRhs _ expr) _) =
    Format.intercalate " "
        [ Pattern.format pat_
        , case expr of
            Do _ (_ : _ : _) ->
                "-> " <> expression expr

            _ ->
                Format.intercalate newLine
                    [ "->"
                    , Format.indent (expression expr)
                    ]
        ]
alt (Alt _ pat (GuardedRhss _ rhss) _) =
    Format.intercalate (newLine <> newLine) (map guardedRhs rhss)
    where
        guardedRhs (GuardedRhs _ [ stmt ] expr) =
            Format.intercalate newLine
                [ Format.intercalate " "
                    [ Pattern.format pat <> " | " <> statement stmt
                    , "->"
                    ]
                , Format.indent (expression expr)
                ]
        guardedRhs g =
            Format.fromString (show g)


fieldUpdate :: FieldUpdate CommentedSrc -> Format
fieldUpdate field =
    case field of
        FieldUpdate src qname expr ->
            if takesOneLine src then
                Atom.qname qname <> " = " <> expression expr

            else
                Atom.qname qname <> " =" <> newLine
                    <> Format.indent (expression expr)

        FieldPun _ qname ->
            Atom.qname qname

        FieldWildcard _ ->
            ".."


listOrTupleElement :: Exp CommentedSrc -> Format
listOrTupleElement expr =
    case expr of
        List{  } ->
            alignTail (expression expr)

        Tuple{  } ->
            alignTail (expression expr)

        If src cond then_ else_ ->
            if takesOneLine src then
                expression expr

            else
                let
                    alignedIfThen =
                        case ifThen cond of
                            [ if_, condition, then_ ] ->
                                [ if_
                                , condition
                                , "  " <> then_
                                ]

                            ifThen_ ->
                                ifThen_
                in
                    Format.intercalate newLine $
                        alignedIfThen
                            ++ [ Format.indent (expression then_)
                               , newLine <> "  else"
                               , Format.indent (expression else_)
                               ]

        _ ->
            expression expr
    where
        alignTail target =
            case Format.lines target of
                x : xs ->
                    Format.intercalate newLine (x : map ("  " <>) xs)

                _ ->
                    target


ifThen :: Exp CommentedSrc -> [Format]
ifThen cond
    | takesOneLine (ann cond) =
        [ Format.intercalate " " [ "if", expression cond, "then" ] ]
    | otherwise =
        [ "if"
        , Format.indent (expression cond)
        , "then"
        ]


context :: Context CommentedSrc -> Format
context ctx =
    case ctx of
        CxSingle _ assertion_ ->
            assertion assertion_ <> " =>"

        CxTuple _ assertions ->
            Format.wrap "(" ")" ", " (map assertion assertions) <> " =>"

        _ ->
            error (show ctx)


assertion :: Asst CommentedSrc -> Format
assertion asst =
    case asst of
        ClassA _ qname types ->
            Format.intercalate " " (Atom.qname qname : map type' types)

        ParenA _ parenAsst ->
            "(" <> assertion parenAsst <> ")"

        _ ->
            error (show asst)


type' :: Type CommentedSrc -> Format
type' (TyForall _ _ maybeContext type_) =
    Format.intercalate " " $
        filter (mempty /=)
            [ maybe mempty context maybeContext
            , type' type_
            ]
type' (TyApp _ t1 t2) =
    type' t1 <> " " <> type' t2
type' (TyCon _ qname') =
    Atom.qname qname'
type' (TyList _ t) =
    "[" <> type' t <> "]"
type' (TyTuple src _ types)
    | takesOneLine src =
        Format.wrap "( " " )" ", " (map type' types)
    | otherwise =
        Format.wrap "( " (newLine <> ")") (newLine <> ", ") (map type' types)
type' (TyVar _ name') =
    Atom.name name'
type' (TyFun src t1 t2)
    | takesOneLine src =
        type' t1 <> " -> " <> type' t2
    | otherwise =
        Format.intercalate newLine
            [ type' t1
            , "-> " <> type' t2
            ]
type' (TyParen src type_)
    | takesOneLine src =
        "(" <> type' type_ <> ")"
    | otherwise =
        "(" <> type' type_ <> newLine <> ")"
type' t =
    error (show t)
