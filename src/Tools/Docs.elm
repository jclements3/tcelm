module Tools.Docs exposing
    ( generate, generateHtml, generateMarkdown, generateJson
    , ModuleDocs, ValueDoc, TypeDoc, AliasDoc
    , extractDocs
    )

{-| Documentation Generator - Extract and format documentation from Elm code.

@docs generate, generateHtml, generateMarkdown, generateJson
@docs ModuleDocs, ValueDoc, TypeDoc, AliasDoc
@docs extractDocs

-}

import AST.Source as Src exposing (Located(..))


{-| Documentation for a module.
-}
type alias ModuleDocs =
    { name : String
    , comment : Maybe String
    , values : List ValueDoc
    , types : List TypeDoc
    , aliases : List AliasDoc
    }


{-| Documentation for a value/function.
-}
type alias ValueDoc =
    { name : String
    , comment : Maybe String
    , type_ : Maybe String
    , exposed : Bool
    }


{-| Documentation for a union type.
-}
type alias TypeDoc =
    { name : String
    , comment : Maybe String
    , args : List String
    , constructors : List ( String, List String )
    , exposed : Bool
    }


{-| Documentation for a type alias.
-}
type alias AliasDoc =
    { name : String
    , comment : Maybe String
    , args : List String
    , type_ : String
    , exposed : Bool
    }



-- EXTRACTION


{-| Extract documentation from a module.
-}
extractDocs : Src.Module -> ModuleDocs
extractDocs mod =
    let
        moduleName =
            case mod.name of
                Just (At _ name) ->
                    name

                Nothing ->
                    "Main"

        exposedNames =
            getExposedNames mod.exports

        values =
            List.map (extractValueDoc exposedNames) mod.values

        types =
            List.map (extractTypeDoc exposedNames) mod.unions

        aliases =
            List.map (extractAliasDoc exposedNames) mod.aliases
    in
    { name = moduleName
    , comment = Nothing -- Would need access to module comment
    , values = values
    , types = types
    , aliases = aliases
    }


getExposedNames : Located Src.Exposing -> List String
getExposedNames (At _ exp) =
    case exp of
        Src.Open ->
            []

        -- All exposed
        Src.Explicit exposed ->
            List.filterMap
                (\e ->
                    case e of
                        Src.Lower (At _ name) ->
                            Just name

                        Src.Upper (At _ name) _ ->
                            Just name

                        Src.Operator _ name ->
                            Just name
                )
                exposed


isExposed : List String -> String -> Located Src.Exposing -> Bool
isExposed exposedNames name (At _ exp) =
    case exp of
        Src.Open ->
            True

        Src.Explicit _ ->
            List.member name exposedNames


extractValueDoc : List String -> Located Src.Value -> ValueDoc
extractValueDoc exposedNames (At _ value) =
    let
        (At _ name) =
            value.name
    in
    { name = name
    , comment = Nothing -- Would need access to doc comments
    , type_ = Maybe.map formatType value.type_
    , exposed = List.isEmpty exposedNames || List.member name exposedNames
    }


extractTypeDoc : List String -> Located Src.Union -> TypeDoc
extractTypeDoc exposedNames (At _ union) =
    let
        (At _ name) =
            union.name
    in
    { name = name
    , comment = Nothing
    , args = List.map (\(At _ a) -> a) union.args
    , constructors =
        List.map
            (\( At _ ctorName, args ) ->
                ( ctorName, List.map formatType args )
            )
            union.ctors
    , exposed = List.isEmpty exposedNames || List.member name exposedNames
    }


extractAliasDoc : List String -> Located Src.Alias -> AliasDoc
extractAliasDoc exposedNames (At _ alias) =
    let
        (At _ name) =
            alias.name
    in
    { name = name
    , comment = Nothing
    , args = List.map (\(At _ a) -> a) alias.args
    , type_ = formatType alias.type_
    , exposed = List.isEmpty exposedNames || List.member name exposedNames
    }


formatType : Src.Type -> String
formatType typ =
    typeToString typ


typeToString : Src.Type -> String
typeToString (At _ typ) =
    case typ of
        Src.TVar name ->
            name

        Src.TType _ name [] ->
            name

        Src.TType _ name args ->
            name ++ " " ++ String.join " " (List.map typeToStringParens args)

        Src.TTypeQual _ qual name [] ->
            qual ++ "." ++ name

        Src.TTypeQual _ qual name args ->
            qual ++ "." ++ name ++ " " ++ String.join " " (List.map typeToStringParens args)

        Src.TLambda from to ->
            typeToStringParens from ++ " -> " ++ typeToString to

        Src.TRecord fields maybeExt ->
            let
                fieldStrs =
                    List.map
                        (\( At _ fname, ftype ) ->
                            fname ++ " : " ++ typeToString ftype
                        )
                        fields

                extStr =
                    case maybeExt of
                        Just (At _ ext) ->
                            ext ++ " | "

                        Nothing ->
                            ""
            in
            "{ " ++ extStr ++ String.join ", " fieldStrs ++ " }"

        Src.TUnit ->
            "()"

        Src.TTuple a b rest ->
            "( " ++ String.join ", " (List.map typeToString (a :: b :: rest)) ++ " )"


typeToStringParens : Src.Type -> String
typeToStringParens typ =
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
        "(" ++ typeToString typ ++ ")"

    else
        typeToString typ



-- GENERATION


{-| Generate documentation in the default format (Markdown).
-}
generate : ModuleDocs -> String
generate =
    generateMarkdown


{-| Generate documentation as Markdown.
-}
generateMarkdown : ModuleDocs -> String
generateMarkdown docs =
    let
        header =
            "# " ++ docs.name ++ "\n\n"

        moduleComment =
            case docs.comment of
                Just c ->
                    c ++ "\n\n"

                Nothing ->
                    ""

        aliasesSection =
            if List.isEmpty (List.filter .exposed docs.aliases) then
                ""

            else
                "## Type Aliases\n\n"
                    ++ String.join "\n\n" (List.map aliasToMarkdown (List.filter .exposed docs.aliases))
                    ++ "\n\n"

        typesSection =
            if List.isEmpty (List.filter .exposed docs.types) then
                ""

            else
                "## Types\n\n"
                    ++ String.join "\n\n" (List.map typeToMarkdown (List.filter .exposed docs.types))
                    ++ "\n\n"

        valuesSection =
            if List.isEmpty (List.filter .exposed docs.values) then
                ""

            else
                "## Functions\n\n"
                    ++ String.join "\n\n" (List.map valueToMarkdown (List.filter .exposed docs.values))
                    ++ "\n\n"
    in
    header ++ moduleComment ++ aliasesSection ++ typesSection ++ valuesSection


valueToMarkdown : ValueDoc -> String
valueToMarkdown doc =
    let
        sig =
            case doc.type_ of
                Just t ->
                    "```elm\n" ++ doc.name ++ " : " ++ t ++ "\n```\n"

                Nothing ->
                    "```elm\n" ++ doc.name ++ "\n```\n"

        comment =
            case doc.comment of
                Just c ->
                    c ++ "\n"

                Nothing ->
                    ""
    in
    "### " ++ doc.name ++ "\n\n" ++ sig ++ comment


typeToMarkdown : TypeDoc -> String
typeToMarkdown doc =
    let
        args =
            if List.isEmpty doc.args then
                ""

            else
                " " ++ String.join " " doc.args

        ctors =
            List.map
                (\( name, ctorArgs ) ->
                    if List.isEmpty ctorArgs then
                        "    | " ++ name

                    else
                        "    | " ++ name ++ " " ++ String.join " " ctorArgs
                )
                doc.constructors

        sig =
            "```elm\ntype " ++ doc.name ++ args ++ "\n" ++ String.join "\n" ctors ++ "\n```\n"

        comment =
            case doc.comment of
                Just c ->
                    c ++ "\n"

                Nothing ->
                    ""
    in
    "### " ++ doc.name ++ "\n\n" ++ sig ++ comment


aliasToMarkdown : AliasDoc -> String
aliasToMarkdown doc =
    let
        args =
            if List.isEmpty doc.args then
                ""

            else
                " " ++ String.join " " doc.args

        sig =
            "```elm\ntype alias " ++ doc.name ++ args ++ " =\n    " ++ doc.type_ ++ "\n```\n"

        comment =
            case doc.comment of
                Just c ->
                    c ++ "\n"

                Nothing ->
                    ""
    in
    "### " ++ doc.name ++ "\n\n" ++ sig ++ comment


{-| Generate documentation as HTML.
-}
generateHtml : ModuleDocs -> String
generateHtml docs =
    let
        header =
            "<h1>" ++ escapeHtml docs.name ++ "</h1>\n"

        moduleComment =
            case docs.comment of
                Just c ->
                    "<p>" ++ escapeHtml c ++ "</p>\n"

                Nothing ->
                    ""

        aliasesSection =
            if List.isEmpty (List.filter .exposed docs.aliases) then
                ""

            else
                "<h2>Type Aliases</h2>\n"
                    ++ String.join "\n" (List.map aliasToHtml (List.filter .exposed docs.aliases))
                    ++ "\n"

        typesSection =
            if List.isEmpty (List.filter .exposed docs.types) then
                ""

            else
                "<h2>Types</h2>\n"
                    ++ String.join "\n" (List.map typeToHtml (List.filter .exposed docs.types))
                    ++ "\n"

        valuesSection =
            if List.isEmpty (List.filter .exposed docs.values) then
                ""

            else
                "<h2>Functions</h2>\n"
                    ++ String.join "\n" (List.map valueToHtml (List.filter .exposed docs.values))
                    ++ "\n"
    in
    "<!DOCTYPE html>\n<html>\n<head>\n<title>"
        ++ escapeHtml docs.name
        ++ "</title>\n<style>\n"
        ++ defaultCss
        ++ "\n</style>\n</head>\n<body>\n"
        ++ header
        ++ moduleComment
        ++ aliasesSection
        ++ typesSection
        ++ valuesSection
        ++ "</body>\n</html>"


valueToHtml : ValueDoc -> String
valueToHtml doc =
    let
        sig =
            case doc.type_ of
                Just t ->
                    "<pre><code>" ++ escapeHtml doc.name ++ " : " ++ escapeHtml t ++ "</code></pre>\n"

                Nothing ->
                    "<pre><code>" ++ escapeHtml doc.name ++ "</code></pre>\n"

        comment =
            case doc.comment of
                Just c ->
                    "<p>" ++ escapeHtml c ++ "</p>\n"

                Nothing ->
                    ""
    in
    "<div class=\"function\">\n<h3>" ++ escapeHtml doc.name ++ "</h3>\n" ++ sig ++ comment ++ "</div>\n"


typeToHtml : TypeDoc -> String
typeToHtml doc =
    let
        args =
            if List.isEmpty doc.args then
                ""

            else
                " " ++ String.join " " doc.args

        ctors =
            List.map
                (\( name, ctorArgs ) ->
                    if List.isEmpty ctorArgs then
                        "    | " ++ name

                    else
                        "    | " ++ name ++ " " ++ String.join " " ctorArgs
                )
                doc.constructors

        sig =
            "<pre><code>type "
                ++ escapeHtml doc.name
                ++ escapeHtml args
                ++ "\n"
                ++ escapeHtml (String.join "\n" ctors)
                ++ "</code></pre>\n"

        comment =
            case doc.comment of
                Just c ->
                    "<p>" ++ escapeHtml c ++ "</p>\n"

                Nothing ->
                    ""
    in
    "<div class=\"type\">\n<h3>" ++ escapeHtml doc.name ++ "</h3>\n" ++ sig ++ comment ++ "</div>\n"


aliasToHtml : AliasDoc -> String
aliasToHtml doc =
    let
        args =
            if List.isEmpty doc.args then
                ""

            else
                " " ++ String.join " " doc.args

        sig =
            "<pre><code>type alias "
                ++ escapeHtml doc.name
                ++ escapeHtml args
                ++ " =\n    "
                ++ escapeHtml doc.type_
                ++ "</code></pre>\n"

        comment =
            case doc.comment of
                Just c ->
                    "<p>" ++ escapeHtml c ++ "</p>\n"

                Nothing ->
                    ""
    in
    "<div class=\"alias\">\n<h3>" ++ escapeHtml doc.name ++ "</h3>\n" ++ sig ++ comment ++ "</div>\n"


defaultCss : String
defaultCss =
    """body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }
h1 { color: #1293d8; }
h2 { color: #5a6378; border-bottom: 1px solid #ddd; padding-bottom: 10px; }
h3 { color: #333; }
pre { background: #f5f5f5; padding: 15px; border-radius: 5px; overflow-x: auto; }
code { font-family: "SF Mono", Monaco, monospace; }
.function, .type, .alias { margin-bottom: 30px; }
p { color: #666; line-height: 1.6; }"""


escapeHtml : String -> String
escapeHtml =
    String.replace "&" "&amp;"
        >> String.replace "<" "&lt;"
        >> String.replace ">" "&gt;"
        >> String.replace "\"" "&quot;"


{-| Generate documentation as JSON.
-}
generateJson : ModuleDocs -> String
generateJson docs =
    let
        valueJson v =
            "{\"name\":\"" ++ escapeJson v.name ++ "\""
                ++ (case v.type_ of
                        Just t ->
                            ",\"type\":\"" ++ escapeJson t ++ "\""

                        Nothing ->
                            ""
                   )
                ++ (case v.comment of
                        Just c ->
                            ",\"comment\":\"" ++ escapeJson c ++ "\""

                        Nothing ->
                            ""
                   )
                ++ "}"

        typeJson t =
            "{\"name\":\"" ++ escapeJson t.name ++ "\""
                ++ ",\"args\":[" ++ String.join "," (List.map (\a -> "\"" ++ escapeJson a ++ "\"") t.args) ++ "]"
                ++ ",\"constructors\":["
                ++ String.join ","
                    (List.map
                        (\( n, args ) ->
                            "{\"name\":\"" ++ escapeJson n ++ "\",\"args\":[" ++ String.join "," (List.map (\a -> "\"" ++ escapeJson a ++ "\"") args) ++ "]}"
                        )
                        t.constructors
                    )
                ++ "]"
                ++ "}"

        aliasJson a =
            "{\"name\":\"" ++ escapeJson a.name ++ "\""
                ++ ",\"args\":[" ++ String.join "," (List.map (\arg -> "\"" ++ escapeJson arg ++ "\"") a.args) ++ "]"
                ++ ",\"type\":\"" ++ escapeJson a.type_ ++ "\""
                ++ "}"
    in
    "{\"name\":\"" ++ escapeJson docs.name ++ "\""
        ++ ",\"values\":[" ++ String.join "," (List.map valueJson (List.filter .exposed docs.values)) ++ "]"
        ++ ",\"types\":[" ++ String.join "," (List.map typeJson (List.filter .exposed docs.types)) ++ "]"
        ++ ",\"aliases\":[" ++ String.join "," (List.map aliasJson (List.filter .exposed docs.aliases)) ++ "]"
        ++ "}"


escapeJson : String -> String
escapeJson =
    String.replace "\\" "\\\\"
        >> String.replace "\"" "\\\""
        >> String.replace "\n" "\\n"
        >> String.replace "\t" "\\t"
        >> String.replace "\u{000D}" "\\r"
