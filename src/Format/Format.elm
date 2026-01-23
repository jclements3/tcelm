module Format.Format exposing
    ( format
    , formatModule
    , formatExpr, formatType, formatPattern
    )

{-| Format - Main formatting logic for the Elm formatter.

This module provides the main entry point for formatting Elm source code
according to the 94-column aligned style.

@docs format
@docs formatModule
@docs formatExpr, formatType, formatPattern

-}

import AST.Source as Src exposing (Located(..))
import Format.Align as Align
import Format.Box as Box exposing (Box(..), Line, line, punc, row, space)
import Format.ElmStructure as ES


{-| Format a complete module to a string.
-}
format : Src.Module -> String
format mod =
    Box.render (formatModule mod)


{-| Format a module to a Box.
-}
formatModule : Src.Module -> Box
formatModule mod =
    let
        header =
            formatModuleHeader mod

        imports =
            List.map formatImport mod.imports

        values =
            List.map formatValue mod.values

        unions =
            List.map formatUnion mod.unions

        aliases =
            List.map formatAlias mod.aliases

        ports =
            List.map formatPort mod.ports

        sections =
            [ [ header ]
            , imports
            , aliases
            , unions
            , values
            , ports
            ]
                |> List.filter (not << List.isEmpty)
                |> List.intersperse [ Box.blankLine ]
                |> List.concat
    in
    Box.stack1 sections



-- MODULE HEADER


formatModuleHeader : Src.Module -> Box
formatModuleHeader mod =
    let
        moduleName =
            case mod.name of
                Just (At _ name) ->
                    name

                Nothing ->
                    "Main"

        exposingClause =
            formatExposing mod.exports
    in
    case exposingClause of
        SingleLine exp ->
            line (row [ punc "module", space, Box.identifier moduleName, space, punc "exposing", space, exp ])

        _ ->
            ES.spaceSepOrIndented
                (line (row [ punc "module", space, Box.identifier moduleName, space, punc "exposing" ]))
                [ exposingClause ]


formatExposing : Located Src.Exposing -> Box
formatExposing (At _ exposing_) =
    case exposing_ of
        Src.Open ->
            line (punc "(..)")

        Src.Explicit exposed ->
            let
                items =
                    List.map formatExposed exposed
            in
            ES.group False "(" "," ")" False items


formatExposed : Src.Exposed -> Box
formatExposed exposed =
    case exposed of
        Src.Lower (At _ name) ->
            line (Box.identifier name)

        Src.Upper (At _ name) privacy ->
            case privacy of
                Src.Private ->
                    line (Box.identifier name)

                Src.Public _ ->
                    line (row [ Box.identifier name, punc "(..)" ])

        Src.Operator _ name ->
            line (row [ punc "(", Box.identifier name, punc ")" ])



-- IMPORTS


formatImport : Src.Import -> Box
formatImport imp =
    let
        (At _ name) =
            imp.name

        aliasClause =
            case imp.alias_ of
                Just alias ->
                    [ space, punc "as", space, Box.identifier alias ]

                Nothing ->
                    []

        exposingClause =
            case imp.exposing_ of
                Src.Open ->
                    [ space, punc "exposing", space, punc "(..)" ]

                Src.Explicit [] ->
                    []

                Src.Explicit exposed ->
                    let
                        items =
                            List.map formatExposed exposed

                        expBox =
                            ES.group False "(" "," ")" False items
                    in
                    case expBox of
                        SingleLine exp ->
                            [ space, punc "exposing", space, exp ]

                        _ ->
                            -- Will need to handle multiline case
                            [ space, punc "exposing", space, punc "(...)" ]
    in
    line (row ([ punc "import", space, Box.identifier name ] ++ aliasClause ++ exposingClause))



-- VALUES


formatValue : Located Src.Value -> Box
formatValue (At _ value) =
    let
        (At _ name) =
            value.name

        annotation =
            case value.type_ of
                Just typ ->
                    [ formatTypeAnnotation name typ ]

                Nothing ->
                    []

        args =
            List.map formatPattern value.args

        body =
            formatExpr value.body
    in
    Box.stack1
        (annotation
            ++ [ ES.definition "=" False (line (Box.identifier name)) args body ]
        )


formatTypeAnnotation : String -> Src.Type -> Box
formatTypeAnnotation name typ =
    ES.equalsPair ":" False (line (Box.identifier name)) (formatType typ)



-- UNIONS


formatUnion : Located Src.Union -> Box
formatUnion (At _ union) =
    let
        (At _ name) =
            union.name

        typeArgs =
            List.map (\(At _ arg) -> line (Box.identifier arg)) union.args

        nameWithArgs =
            ES.application (line (Box.identifier name)) typeArgs

        -- Use aligned variants for nicer formatting
        -- For union type constructor args, only wrap in parens if needed (type applications)
        variants =
            List.map
                (\( At _ ctorName, args ) -> ( ctorName, List.map formatTypeForCtor args ))
                union.ctors

        alignedCtors =
            Align.alignVariants variants

        ctorLines =
            case alignedCtors of
                [] ->
                    []

                first :: rest ->
                    Box.prefix (row [ punc "=", space ]) first
                        :: List.map (Box.prefix (row [ punc "|", space ])) rest
    in
    Box.stack1
        (ES.spaceSepOrStack (line (punc "type")) [ nameWithArgs ]
            :: List.map Box.indent ctorLines
        )



-- ALIASES


formatAlias : Located Src.Alias -> Box
formatAlias (At _ alias) =
    let
        (At _ name) =
            alias.name

        typeArgs =
            List.map (\(At _ arg) -> line (Box.identifier arg)) alias.args

        nameWithArgs =
            ES.application (line (Box.identifier name)) typeArgs
    in
    ES.definition "="
        False
        (line (row [ punc "type", space, punc "alias" ]))
        [ nameWithArgs ]
        (formatType alias.type_)



-- PORTS


formatPort : Src.Port -> Box
formatPort port_ =
    let
        (At _ name) =
            port_.name
    in
    ES.equalsPair ":"
        False
        (line (row [ punc "port", space, Box.identifier name ]))
        (formatType port_.type_)



-- EXPRESSIONS


{-| Format an expression.
-}
formatExpr : Src.Expr -> Box
formatExpr (At _ expr) =
    case expr of
        Src.Chr c ->
            line (row [ punc "'", Box.literal (escapeChar c), punc "'" ])

        Src.Str s ->
            formatString s

        Src.Int i ->
            line (Box.literal (String.fromInt i))

        Src.Float f ->
            line (Box.literal (String.fromFloat f))

        Src.Var _ name ->
            line (Box.identifier name)

        Src.VarQual _ qualifier name ->
            line (row [ Box.identifier qualifier, punc ".", Box.identifier name ])

        Src.List items ->
            formatList items

        Src.Op op ->
            line (row [ punc "(", Box.identifier op, punc ")" ])

        Src.Negate e ->
            Box.prefix (punc "-") (formatExprParens e)

        Src.Binops pairs final ->
            formatBinops pairs final

        Src.Lambda patterns body ->
            formatLambda patterns body

        Src.Call func args ->
            formatCall func args

        Src.If branches else_ ->
            formatIf branches else_

        Src.Let defs body ->
            formatLet defs body

        Src.Case subject branches ->
            formatCase subject branches

        Src.Accessor field ->
            line (row [ punc ".", Box.identifier field ])

        Src.Access e (At _ field) ->
            Box.addSuffix (row [ punc ".", Box.identifier field ]) (formatExprParens e)

        Src.Update (At _ name) fields ->
            formatRecordUpdate name fields

        Src.Record fields ->
            formatRecord fields

        Src.Unit ->
            line (punc "()")

        Src.Tuple a b rest ->
            formatTuple a b rest


formatExprParens : Src.Expr -> Box
formatExprParens expr =
    let
        needsParens =
            case Src.toValue expr of
                Src.Lambda _ _ ->
                    True

                Src.Binops _ _ ->
                    True

                Src.If _ _ ->
                    True

                Src.Let _ _ ->
                    True

                Src.Case _ _ ->
                    True

                _ ->
                    False
    in
    if needsParens then
        parens (formatExpr expr)

    else
        formatExpr expr


parens : Box -> Box
parens box =
    case box of
        SingleLine l ->
            line (row [ punc "(", l, punc ")" ])

        _ ->
            Box.stack1
                [ Box.prefix (punc "(") box
                , line (punc ")")
                ]


formatString : String -> Box
formatString s =
    if String.contains "\n" s then
        -- Multiline string
        line (row [ punc "\"\"\"", Box.literal s, punc "\"\"\"" ])

    else
        line (row [ punc "\"", Box.literal (escapeString s), punc "\"" ])


escapeString : String -> String
escapeString =
    String.replace "\\" "\\\\"
        >> String.replace "\"" "\\\""
        >> String.replace "\n" "\\n"
        >> String.replace "\t" "\\t"
        >> String.replace "\u{000D}" "\\r"


escapeChar : String -> String
escapeChar s =
    case s of
        "\\" ->
            "\\\\"

        "'" ->
            "\\'"

        "\n" ->
            "\\n"

        "\t" ->
            "\\t"

        "\u{000D}" ->
            "\\r"

        _ ->
            s


formatList : List Src.Expr -> Box
formatList items =
    if List.isEmpty items then
        line (punc "[]")

    else
        ES.group True "[" "," "]" False (List.map formatExpr items)


formatBinops : List ( Src.Expr, Located String ) -> Src.Expr -> Box
formatBinops pairs final =
    let
        formatPair ( e, At _ op ) =
            [ formatExpr e, line (Box.identifier op) ]

        allParts =
            List.concatMap formatPair pairs ++ [ formatExpr final ]
    in
    case allParts of
        first :: rest ->
            ES.spaceSepOrIndented first rest

        [] ->
            line (Box.literal "")


formatLambda : List Src.Pattern -> Src.Expr -> Box
formatLambda patterns body =
    let
        patternBoxes =
            List.map formatPattern patterns
    in
    case ( Box.allSingles patternBoxes, formatExpr body ) of
        ( Ok patternLines, SingleLine bodyLine ) ->
            let
                combined =
                    line
                        (row
                            ([ punc "\\" ]
                                ++ List.intersperse space patternLines
                                ++ [ space, punc "->", space, bodyLine ]
                            )
                        )
            in
            if ES.fitsOnLine 0 combined then
                combined

            else
                Box.stack1
                    [ line (row ([ punc "\\" ] ++ List.intersperse space patternLines ++ [ space, punc "->" ]))
                    , Box.indent (formatExpr body)
                    ]

        ( Ok patternLines, _ ) ->
            Box.stack1
                [ line (row ([ punc "\\" ] ++ List.intersperse space patternLines ++ [ space, punc "->" ]))
                , Box.indent (formatExpr body)
                ]

        _ ->
            Box.stack1
                [ Box.prefix (punc "\\") (Box.stack1 patternBoxes)
                , line (punc "->")
                , Box.indent (formatExpr body)
                ]


formatCall : Src.Expr -> List Src.Expr -> Box
formatCall func args =
    ES.application (formatExprParens func) (List.map formatExprParens args)


formatIf : List ( Src.Expr, Src.Expr ) -> Src.Expr -> Box
formatIf branches else_ =
    let
        formatBranch isFirst ( cond, body ) =
            let
                keyword =
                    if isFirst then
                        "if"

                    else
                        "else if"

                condBox =
                    formatExpr cond

                bodyBox =
                    formatExpr body
            in
            case condBox of
                SingleLine condLine ->
                    Box.stack1
                        [ line (row [ punc keyword, space, condLine, space, punc "then" ])
                        , Box.indent bodyBox
                        ]

                _ ->
                    Box.stack1
                        [ line (punc keyword)
                        , Box.indent condBox
                        , line (punc "then")
                        , Box.indent bodyBox
                        ]

        branchBoxes =
            List.indexedMap (\i b -> formatBranch (i == 0) b) branches

        elseBox =
            Box.stack1
                [ line (punc "else")
                , Box.indent (formatExpr else_)
                ]
    in
    Box.stack1 (branchBoxes ++ [ Box.blankLine, elseBox ])


formatLet : List (Located Src.Def) -> Src.Expr -> Box
formatLet defs body =
    let
        defBoxes =
            List.map formatDef defs
                |> List.intersperse Box.blankLine
    in
    Box.stack1
        ([ line (punc "let") ]
            ++ List.map Box.indent defBoxes
            ++ [ line (punc "in")
               , formatExpr body
               ]
        )


formatDef : Located Src.Def -> Box
formatDef (At _ def) =
    case def of
        Src.Define (At _ name) args body maybeType ->
            let
                annotation =
                    case maybeType of
                        Just typ ->
                            [ formatTypeAnnotation name typ ]

                        Nothing ->
                            []
            in
            Box.stack1
                (annotation
                    ++ [ ES.definition "=" False (line (Box.identifier name)) (List.map formatPattern args) (formatExpr body) ]
                )

        Src.Destruct pattern body ->
            ES.equalsPair "=" False (formatPattern pattern) (formatExpr body)


formatCase : Src.Expr -> List ( Src.Pattern, Src.Expr ) -> Box
formatCase subject branches =
    let
        subjectBox =
            formatExpr subject

        header =
            case subjectBox of
                SingleLine subjectLine ->
                    line (row [ punc "case", space, subjectLine, space, punc "of" ])

                _ ->
                    Box.stack1
                        [ line (punc "case")
                        , Box.indent subjectBox
                        , line (punc "of")
                        ]

        -- Convert branches to pattern/body box pairs for alignment
        branchPairs =
            List.map
                (\( pattern, body ) -> ( formatPattern pattern, formatExpr body ))
                branches

        -- Use aligned case branches for nice formatting
        alignedBranches =
            Align.alignCaseBranches branchPairs
    in
    Box.stack1 (header :: List.map Box.indent alignedBranches)


formatRecordUpdate : String -> List ( Located String, Src.Expr ) -> Box
formatRecordUpdate name fields =
    let
        alignedFields =
            List.map
                (\( At _ fieldName, value ) ->
                    Align.makeAlignedField fieldName (formatExpr value)
                )
                fields

        fieldBoxes =
            Align.alignFields "=" alignedFields
    in
    case fieldBoxes of
        [] ->
            line (row [ punc "{", space, Box.identifier name, space, punc "}" ])

        first :: rest ->
            ES.extensionGroup False (line (Box.identifier name)) first rest


formatRecord : List ( Located String, Src.Expr ) -> Box
formatRecord fields =
    if List.isEmpty fields then
        line (punc "{}")

    else
        let
            alignedFields =
                List.map
                    (\( At _ fieldName, value ) ->
                        Align.makeAlignedField fieldName (formatExpr value)
                    )
                    fields
        in
        Align.alignedRecord "=" alignedFields


formatTuple : Src.Expr -> Src.Expr -> List Src.Expr -> Box
formatTuple a b rest =
    ES.group True "(" "," ")" False (List.map formatExpr (a :: b :: rest))



-- PATTERNS


{-| Format a pattern.
-}
formatPattern : Src.Pattern -> Box
formatPattern (At _ pattern) =
    case pattern of
        Src.PAnything ->
            line (punc "_")

        Src.PVar name ->
            line (Box.identifier name)

        Src.PRecord fields ->
            ES.group True "{" "," "}" False (List.map (\(At _ f) -> line (Box.identifier f)) fields)

        Src.PAlias pat (At _ alias) ->
            ES.spaceSepOrStack (formatPatternParens pat) [ line (punc "as"), line (Box.identifier alias) ]

        Src.PUnit ->
            line (punc "()")

        Src.PTuple a b rest ->
            ES.group True "(" "," ")" False (List.map formatPattern (a :: b :: rest))

        Src.PCtor _ name args ->
            ES.application (line (Box.identifier name)) (List.map formatPatternParens args)

        Src.PCtorQual _ qualifier name args ->
            ES.application (line (row [ Box.identifier qualifier, punc ".", Box.identifier name ])) (List.map formatPatternParens args)

        Src.PList items ->
            if List.isEmpty items then
                line (punc "[]")

            else
                ES.group True "[" "," "]" False (List.map formatPattern items)

        Src.PCons head tail ->
            ES.spaceSepOrStack (formatPatternParens head) [ line (punc "::"), formatPatternParens tail ]

        Src.PChr c ->
            line (row [ punc "'", Box.literal (escapeChar c), punc "'" ])

        Src.PStr s ->
            line (row [ punc "\"", Box.literal (escapeString s), punc "\"" ])

        Src.PInt i ->
            line (Box.literal (String.fromInt i))


formatPatternParens : Src.Pattern -> Box
formatPatternParens pattern =
    let
        needsParens =
            case Src.toValue pattern of
                Src.PCtor _ _ (_ :: _) ->
                    True

                Src.PCtorQual _ _ _ (_ :: _) ->
                    True

                Src.PCons _ _ ->
                    True

                Src.PAlias _ _ ->
                    True

                _ ->
                    False
    in
    if needsParens then
        parens (formatPattern pattern)

    else
        formatPattern pattern



-- TYPES


{-| Format a type.
-}
formatType : Src.Type -> Box
formatType (At _ typ) =
    case typ of
        Src.TLambda from to ->
            formatFunctionType from to

        Src.TVar name ->
            line (Box.identifier name)

        Src.TType _ name args ->
            if List.isEmpty args then
                line (Box.identifier name)

            else
                ES.application (line (Box.identifier name)) (List.map formatTypeParens args)

        Src.TTypeQual _ qualifier name args ->
            let
                qualifiedName =
                    line (row [ Box.identifier qualifier, punc ".", Box.identifier name ])
            in
            if List.isEmpty args then
                qualifiedName

            else
                ES.application qualifiedName (List.map formatTypeParens args)

        Src.TRecord fields maybeExt ->
            formatRecordType fields maybeExt

        Src.TUnit ->
            line (punc "()")

        Src.TTuple a b rest ->
            ES.group True "(" "," ")" False (List.map formatType (a :: b :: rest))


formatFunctionType : Src.Type -> Src.Type -> Box
formatFunctionType from to =
    let
        fromBox =
            formatTypeParens from

        toBox =
            formatType to
    in
    case ( fromBox, toBox ) of
        ( SingleLine fromLine, SingleLine toLine ) ->
            let
                combined =
                    line (row [ fromLine, space, punc "->", space, toLine ])
            in
            if ES.fitsOnLine 0 combined then
                combined

            else
                Box.stack1
                    [ fromBox
                    , Box.prefix (row [ punc "->", space ]) toBox
                    ]

        _ ->
            Box.stack1
                [ fromBox
                , Box.prefix (row [ punc "->", space ]) toBox
                ]


formatTypeParens : Src.Type -> Box
formatTypeParens typ =
    let
        needsParens =
            case Src.toValue typ of
                Src.TLambda _ _ ->
                    True

                Src.TType _ _ (_ :: _) ->
                    True

                Src.TTypeQual _ _ _ (_ :: _) ->
                    True

                _ ->
                    False
    in
    if needsParens then
        parens (formatType typ)

    else
        formatType typ


{-| Format a type for use as a union constructor argument.
Only adds parentheses for function types, not type applications.
-}
formatTypeForCtor : Src.Type -> Box
formatTypeForCtor typ =
    let
        needsParens =
            case Src.toValue typ of
                Src.TLambda _ _ ->
                    True

                _ ->
                    False
    in
    if needsParens then
        parens (formatType typ)

    else
        formatType typ


formatRecordType : List ( Located String, Src.Type ) -> Maybe (Located String) -> Box
formatRecordType fields maybeExt =
    let
        alignedFields =
            List.map
                (\( At _ fieldName, fieldType ) ->
                    Align.makeAlignedField fieldName (formatType fieldType)
                )
                fields

        extName =
            Maybe.map (\(At _ n) -> n) maybeExt
    in
    Align.alignedRecordType alignedFields extName
