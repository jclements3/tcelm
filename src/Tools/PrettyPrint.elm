module Tools.PrettyPrint exposing
    ( prettyModule, prettyExpr, prettyType, prettyPattern
    , toDoc, Doc
    )

{-| AST Pretty Printer - Debug aid for viewing parsed AST structure.

@docs prettyModule, prettyExpr, prettyType, prettyPattern
@docs toDoc, Doc

-}

import AST.Source as Src exposing (Located(..))


{-| A pretty-printing document.
-}
type Doc
    = Text String
    | Line
    | Indent Int Doc
    | Concat (List Doc)


{-| Convert a Doc to a string.
-}
toDoc : Doc -> String
toDoc doc =
    render 0 doc


render : Int -> Doc -> String
render level doc =
    case doc of
        Text s ->
            s

        Line ->
            "\n" ++ String.repeat level " "

        Indent n d ->
            render (level + n) d

        Concat docs ->
            String.concat (List.map (render level) docs)



-- HELPERS


text : String -> Doc
text =
    Text


line : Doc
line =
    Line


indent : Doc -> Doc
indent =
    Indent 2


concat : List Doc -> Doc
concat =
    Concat


parens : Doc -> Doc
parens d =
    concat [ text "(", d, text ")" ]


brackets : Doc -> Doc
brackets d =
    concat [ text "[", d, text "]" ]


braces : Doc -> Doc
braces d =
    concat [ text "{", d, text "}" ]


commaSep : List Doc -> Doc
commaSep docs =
    concat (List.intersperse (text ", ") docs)


node : String -> List Doc -> Doc
node name children =
    case children of
        [] ->
            text name

        _ ->
            concat
                [ text name
                , line
                , indent (concat (List.intersperse line children))
                ]



-- MODULE


{-| Pretty print a module AST.
-}
prettyModule : Src.Module -> String
prettyModule mod =
    toDoc (moduleDoc mod)


moduleDoc : Src.Module -> Doc
moduleDoc mod =
    node "Module"
        [ concat [ text "name: ", text (Maybe.withDefault "Main" (Maybe.map locatedString mod.name)) ]
        , concat [ text "exports: ", exposingDoc (Src.toValue mod.exports) ]
        , node "imports" (List.map importDoc mod.imports)
        , node "values" (List.map (valueDoc << Src.toValue) mod.values)
        , node "unions" (List.map (unionDoc << Src.toValue) mod.unions)
        , node "aliases" (List.map (aliasDoc << Src.toValue) mod.aliases)
        , node "ports" (List.map portDoc mod.ports)
        ]


locatedString : Located String -> String
locatedString (At _ s) =
    s


exposingDoc : Src.Exposing -> Doc
exposingDoc exp =
    case exp of
        Src.Open ->
            text "(..)"

        Src.Explicit exposed ->
            parens (commaSep (List.map exposedDoc exposed))


exposedDoc : Src.Exposed -> Doc
exposedDoc exp =
    case exp of
        Src.Lower (At _ name) ->
            text name

        Src.Upper (At _ name) privacy ->
            case privacy of
                Src.Private ->
                    text name

                Src.Public _ ->
                    concat [ text name, text "(..)" ]

        Src.Operator _ name ->
            parens (text name)


importDoc : Src.Import -> Doc
importDoc imp =
    concat
        [ text "import "
        , text (locatedString imp.name)
        , case imp.alias_ of
            Just alias ->
                concat [ text " as ", text alias ]

            Nothing ->
                text ""
        , case imp.exposing_ of
            Src.Open ->
                text " exposing (..)"

            Src.Explicit [] ->
                text ""

            Src.Explicit exposed ->
                concat [ text " exposing ", parens (commaSep (List.map exposedDoc exposed)) ]
        ]


valueDoc : Src.Value -> Doc
valueDoc val =
    node "Value"
        [ concat [ text "name: ", text (locatedString val.name) ]
        , node "args" (List.map (patternDoc << Src.toValue) val.args)
        , concat [ text "body: ", line, indent (exprDoc (Src.toValue val.body)) ]
        , case val.type_ of
            Just t ->
                concat [ text "type: ", typeDoc (Src.toValue t) ]

            Nothing ->
                text "type: (inferred)"
        ]


unionDoc : Src.Union -> Doc
unionDoc union =
    node "Union"
        [ concat [ text "name: ", text (locatedString union.name) ]
        , concat [ text "args: ", brackets (commaSep (List.map (text << locatedString) union.args)) ]
        , node "constructors" (List.map ctorDoc union.ctors)
        ]


ctorDoc : ( Located String, List Src.Type ) -> Doc
ctorDoc ( At _ name, args ) =
    concat
        [ text name
        , if List.isEmpty args then
            text ""

          else
            concat [ text " ", commaSep (List.map (typeDoc << Src.toValue) args) ]
        ]


aliasDoc : Src.Alias -> Doc
aliasDoc alias =
    node "Alias"
        [ concat [ text "name: ", text (locatedString alias.name) ]
        , concat [ text "args: ", brackets (commaSep (List.map (text << locatedString) alias.args)) ]
        , concat [ text "type: ", typeDoc (Src.toValue alias.type_) ]
        ]


portDoc : Src.Port -> Doc
portDoc port_ =
    concat
        [ text "port "
        , text (locatedString port_.name)
        , text " : "
        , typeDoc (Src.toValue port_.type_)
        ]



-- EXPRESSIONS


{-| Pretty print an expression AST.
-}
prettyExpr : Src.Expr -> String
prettyExpr expr =
    toDoc (exprDoc (Src.toValue expr))


exprDoc : Src.Expr_ -> Doc
exprDoc expr =
    case expr of
        Src.Chr c ->
            concat [ text "Chr '", text c, text "'" ]

        Src.Str s ->
            concat [ text "Str \"", text (escapeString s), text "\"" ]

        Src.Int i ->
            concat [ text "Int ", text (String.fromInt i) ]

        Src.Float f ->
            concat [ text "Float ", text (String.fromFloat f) ]

        Src.Var vt name ->
            concat [ text "Var ", text (varTypeStr vt), text " ", text name ]

        Src.VarQual vt qual name ->
            concat [ text "VarQual ", text (varTypeStr vt), text " ", text qual, text ".", text name ]

        Src.List items ->
            node "List" (List.map (exprDoc << Src.toValue) items)

        Src.Op op ->
            concat [ text "Op ", text op ]

        Src.Negate e ->
            node "Negate" [ exprDoc (Src.toValue e) ]

        Src.Binops pairs final ->
            node "Binops"
                (List.map binopPairDoc pairs ++ [ exprDoc (Src.toValue final) ])

        Src.Lambda patterns body ->
            node "Lambda"
                [ node "patterns" (List.map (patternDoc << Src.toValue) patterns)
                , concat [ text "body: ", exprDoc (Src.toValue body) ]
                ]

        Src.Call func args ->
            node "Call"
                [ concat [ text "func: ", exprDoc (Src.toValue func) ]
                , node "args" (List.map (exprDoc << Src.toValue) args)
                ]

        Src.If branches else_ ->
            node "If"
                (List.map ifBranchDoc branches
                    ++ [ concat [ text "else: ", exprDoc (Src.toValue else_) ] ]
                )

        Src.Let defs body ->
            node "Let"
                [ node "defs" (List.map (defDoc << Src.toValue) defs)
                , concat [ text "body: ", exprDoc (Src.toValue body) ]
                ]

        Src.Case subject branches ->
            node "Case"
                [ concat [ text "subject: ", exprDoc (Src.toValue subject) ]
                , node "branches" (List.map caseBranchDoc branches)
                ]

        Src.Accessor field ->
            concat [ text "Accessor .", text field ]

        Src.Access e (At _ field) ->
            node "Access"
                [ exprDoc (Src.toValue e)
                , concat [ text ".", text field ]
                ]

        Src.Update (At _ name) fields ->
            node "Update"
                [ text name
                , node "fields" (List.map recordFieldDoc fields)
                ]

        Src.Record fields ->
            node "Record" (List.map recordFieldDoc fields)

        Src.Unit ->
            text "Unit"

        Src.Tuple a b rest ->
            node "Tuple" (List.map (exprDoc << Src.toValue) (a :: b :: rest))


varTypeStr : Src.VarType -> String
varTypeStr vt =
    case vt of
        Src.LowVar ->
            "low"

        Src.CapVar ->
            "cap"


binopPairDoc : ( Src.Expr, Located String ) -> Doc
binopPairDoc ( e, At _ op ) =
    concat [ exprDoc (Src.toValue e), text " ", text op ]


ifBranchDoc : ( Src.Expr, Src.Expr ) -> Doc
ifBranchDoc ( cond, body ) =
    concat
        [ text "if "
        , exprDoc (Src.toValue cond)
        , text " then "
        , exprDoc (Src.toValue body)
        ]


defDoc : Src.Def -> Doc
defDoc def =
    case def of
        Src.Define (At _ name) args body maybeType ->
            node "Define"
                [ text name
                , node "args" (List.map (patternDoc << Src.toValue) args)
                , concat [ text "body: ", exprDoc (Src.toValue body) ]
                , case maybeType of
                    Just t ->
                        concat [ text "type: ", typeDoc (Src.toValue t) ]

                    Nothing ->
                        text ""
                ]

        Src.Destruct pattern body ->
            node "Destruct"
                [ patternDoc (Src.toValue pattern)
                , exprDoc (Src.toValue body)
                ]


caseBranchDoc : ( Src.Pattern, Src.Expr ) -> Doc
caseBranchDoc ( pattern, body ) =
    concat
        [ patternDoc (Src.toValue pattern)
        , text " -> "
        , exprDoc (Src.toValue body)
        ]


recordFieldDoc : ( Located String, Src.Expr ) -> Doc
recordFieldDoc ( At _ name, value ) =
    concat [ text name, text " = ", exprDoc (Src.toValue value) ]


escapeString : String -> String
escapeString =
    String.replace "\\" "\\\\"
        >> String.replace "\"" "\\\""
        >> String.replace "\n" "\\n"
        >> String.replace "\t" "\\t"



-- PATTERNS


{-| Pretty print a pattern AST.
-}
prettyPattern : Src.Pattern -> String
prettyPattern pattern =
    toDoc (patternDoc (Src.toValue pattern))


patternDoc : Src.Pattern_ -> Doc
patternDoc pattern =
    case pattern of
        Src.PAnything ->
            text "_"

        Src.PVar name ->
            text name

        Src.PRecord fields ->
            braces (commaSep (List.map (text << locatedString) fields))

        Src.PAlias pat (At _ alias) ->
            concat [ patternDoc (Src.toValue pat), text " as ", text alias ]

        Src.PUnit ->
            text "()"

        Src.PTuple a b rest ->
            parens (commaSep (List.map (patternDoc << Src.toValue) (a :: b :: rest)))

        Src.PCtor _ name args ->
            if List.isEmpty args then
                text name

            else
                concat [ text name, text " ", commaSep (List.map (patternDoc << Src.toValue) args) ]

        Src.PCtorQual _ qual name args ->
            if List.isEmpty args then
                concat [ text qual, text ".", text name ]

            else
                concat [ text qual, text ".", text name, text " ", commaSep (List.map (patternDoc << Src.toValue) args) ]

        Src.PList items ->
            brackets (commaSep (List.map (patternDoc << Src.toValue) items))

        Src.PCons head tail ->
            concat [ patternDoc (Src.toValue head), text " :: ", patternDoc (Src.toValue tail) ]

        Src.PChr c ->
            concat [ text "'", text c, text "'" ]

        Src.PStr s ->
            concat [ text "\"", text s, text "\"" ]

        Src.PInt i ->
            text (String.fromInt i)



-- TYPES


{-| Pretty print a type AST.
-}
prettyType : Src.Type -> String
prettyType typ =
    toDoc (typeDoc (Src.toValue typ))


typeDoc : Src.Type_ -> Doc
typeDoc typ =
    case typ of
        Src.TLambda from to ->
            concat [ typeDoc (Src.toValue from), text " -> ", typeDoc (Src.toValue to) ]

        Src.TVar name ->
            text name

        Src.TType _ name args ->
            if List.isEmpty args then
                text name

            else
                concat [ text name, text " ", commaSep (List.map (typeDoc << Src.toValue) args) ]

        Src.TTypeQual _ qual name args ->
            if List.isEmpty args then
                concat [ text qual, text ".", text name ]

            else
                concat [ text qual, text ".", text name, text " ", commaSep (List.map (typeDoc << Src.toValue) args) ]

        Src.TRecord fields maybeExt ->
            let
                fieldDocs =
                    List.map recordTypeFieldDoc fields

                extDoc =
                    case maybeExt of
                        Just (At _ ext) ->
                            [ text ext, text " | " ]

                        Nothing ->
                            []
            in
            braces (concat (extDoc ++ [ commaSep fieldDocs ]))

        Src.TUnit ->
            text "()"

        Src.TTuple a b rest ->
            parens (commaSep (List.map (typeDoc << Src.toValue) (a :: b :: rest)))


recordTypeFieldDoc : ( Located String, Src.Type ) -> Doc
recordTypeFieldDoc ( At _ name, typ ) =
    concat [ text name, text " : ", typeDoc (Src.toValue typ) ]
