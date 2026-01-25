module Codegen.Shared exposing
    ( ExprCtx
    , MainValue(..)
    , collectUserFunctionNames
    , collectAllFunctionNames
    , ctorListHasData
    , defaultExprCtx
    , escapeC
    , extractCtorName
    , generateCharPredCall
    , getModuleName
    , getModulePrefix
    , isSimpleLiteral
    , mainTypeIsString
    , mangle
    , mangleLocal
    , mangleWithPrefix
    , patternVars
    , sanitizeForCIdent
    , uniqueStrings
    )

{-| Shared types and utilities for code generation.

This module contains common types, helper functions, and constants
used across the various code generation modules.

-}

import AST.Source as Src


{-| Context passed through expression generation for tracking scope and names.
-}
type alias ExprCtx =
    { funcPrefix : String -- The enclosing function name for lifted local functions
    , modulePrefix : String -- Module name with dots replaced by underscores (for function prefixes)
    , userFunctions : List String -- Names of user-defined functions in this module
    }


{-| Default context for top-level or main expressions
-}
defaultExprCtx : ExprCtx
defaultExprCtx =
    { funcPrefix = "main"
    , modulePrefix = ""
    , userFunctions = []
    }


{-| Type representing the main value's type and code
-}
type MainValue
    = MainString String
    | MainInt Int
    | MainExpr String String -- (C type, C expression)
    | MainNone -- No main function in module (library module)


{-| Escape a string for C
-}
escapeC : String -> String
escapeC s =
    String.toList s
        |> List.map
            (\c ->
                case c of
                    '"' ->
                        "\\\""

                    '\'' ->
                        "\\'"

                    '\\' ->
                        "\\\\"

                    '\n' ->
                        "\\n"

                    '\t' ->
                        "\\t"

                    '\u{000D}' ->
                        "\\r"

                    _ ->
                        String.fromChar c
            )
        |> String.concat


{-| Generate inline code for Char predicates
-}
generateCharPredCall : String -> String -> String
generateCharPredCall fnName charExpr =
    case fnName of
        "isDigit" ->
            "(" ++ charExpr ++ " >= '0' && " ++ charExpr ++ " <= '9')"

        "isAlpha" ->
            "((" ++ charExpr ++ " >= 'a' && " ++ charExpr ++ " <= 'z') || (" ++ charExpr ++ " >= 'A' && " ++ charExpr ++ " <= 'Z'))"

        "isUpper" ->
            "(" ++ charExpr ++ " >= 'A' && " ++ charExpr ++ " <= 'Z')"

        "isLower" ->
            "(" ++ charExpr ++ " >= 'a' && " ++ charExpr ++ " <= 'z')"

        "isAlphaNum" ->
            "((" ++ charExpr ++ " >= 'a' && " ++ charExpr ++ " <= 'z') || (" ++ charExpr ++ " >= 'A' && " ++ charExpr ++ " <= 'Z') || (" ++ charExpr ++ " >= '0' && " ++ charExpr ++ " <= '9'))"

        "isHexDigit" ->
            "((" ++ charExpr ++ " >= '0' && " ++ charExpr ++ " <= '9') || (" ++ charExpr ++ " >= 'a' && " ++ charExpr ++ " <= 'f') || (" ++ charExpr ++ " >= 'A' && " ++ charExpr ++ " <= 'F'))"

        "isOctDigit" ->
            "(" ++ charExpr ++ " >= '0' && " ++ charExpr ++ " <= '7')"

        _ ->
            "/* unknown Char predicate: " ++ fnName ++ " */ 0"


{-| Extract variable names bound by a pattern
-}
patternVars : Src.Pattern -> List String
patternVars (Src.At _ pat) =
    case pat of
        Src.PVar name ->
            [ name ]

        Src.PCtor _ _ subPats ->
            List.concatMap patternVars subPats

        Src.PCtorQual _ _ _ subPats ->
            List.concatMap patternVars subPats

        Src.PList subPats ->
            List.concatMap patternVars subPats

        Src.PCons headPat tailPat ->
            patternVars headPat ++ patternVars tailPat

        Src.PTuple first second rest ->
            patternVars first ++ patternVars second ++ List.concatMap patternVars rest

        Src.PRecord fields ->
            List.map (\(Src.At _ name) -> name) fields

        Src.PAlias innerPat (Src.At _ aliasName) ->
            aliasName :: patternVars innerPat

        _ ->
            []


{-| Remove duplicates from a list
-}
uniqueStrings : List String -> List String
uniqueStrings list =
    List.foldl
        (\item acc ->
            if List.member item acc then
                acc

            else
                acc ++ [ item ]
        )
        []
        list


{-| Check if a union type has any constructors with data
-}
ctorListHasData : List ( Src.Located String, List Src.Type ) -> Bool
ctorListHasData ctors =
    case ctors of
        [] ->
            False

        ( _, args ) :: rest ->
            if List.isEmpty args then
                ctorListHasData rest

            else
                True


{-| Extract the module name from an AST module, defaulting to "Main"
-}
getModuleName : Src.Module -> String
getModuleName ast =
    ast.name
        |> Maybe.map (\(Src.At _ n) -> n)
        |> Maybe.withDefault "Main"


{-| Get the module prefix for C identifiers (module name with dots replaced by underscores)
E.g., "AST.Source" -> "AST_Source"
-}
getModulePrefix : Src.Module -> String
getModulePrefix ast =
    String.replace "." "_" (getModuleName ast)


{-| Check if a type annotation represents String type
-}
mainTypeIsString : Maybe Src.Type -> Bool
mainTypeIsString maybeType =
    case maybeType of
        Just (Src.At _ (Src.TType _ "String" [])) ->
            True

        _ ->
            False



-- NAME MANGLING


{-| Mangle an Elm identifier for C (adds elm\_ prefix, handles primes and dots)
-}
mangle : String -> String
mangle name =
    "elm_" ++ String.concat (List.map mangleChar (String.toList name))


{-| Mangle an Elm identifier with module prefix for cross-module linking
E.g., mangleWithPrefix "AST\_Source" "at" -> "elm\_AST\_Source\_at"
-}
mangleWithPrefix : String -> String -> String
mangleWithPrefix modulePrefix name =
    "elm_" ++ modulePrefix ++ "_" ++ String.concat (List.map mangleChar (String.toList name))


{-| Mangle a local variable name (from pattern bindings)
Local variables get a different prefix to distinguish from top-level functions.
-}
mangleLocal : String -> String
mangleLocal name =
    "_lv_" ++ String.concat (List.map mangleCharLocal (String.toList name))


{-| Replace special characters for C identifiers
-}
mangleChar : Char -> String
mangleChar c =
    case c of
        '\'' ->
            "_prime"

        '.' ->
            "_"

        _ ->
            String.fromChar c


{-| Replace special characters for local variables (no dot replacement needed)
-}
mangleCharLocal : Char -> String
mangleCharLocal c =
    case c of
        '\'' ->
            "_prime"

        _ ->
            String.fromChar c


{-| Extract the constructor name from a potentially qualified name.
E.g., "AST.Source.At" -> "At", "Just" -> "Just"
-}
extractCtorName : String -> String
extractCtorName name =
    case List.reverse (String.split "." name) of
        lastPart :: _ ->
            lastPart

        [] ->
            name


{-| Sanitize a string for use as a C identifier
-}
sanitizeForCIdent : String -> String
sanitizeForCIdent s =
    String.toList s
        |> List.map
            (\c ->
                if Char.isAlphaNum c then
                    String.fromChar c

                else
                    "_"
            )
        |> String.concat


{-| Check if an expression is a simple literal (can be used as static initializer)
-}
isSimpleLiteral : Src.Expr -> Bool
isSimpleLiteral (Src.At _ e) =
    case e of
        Src.Str _ -> True
        Src.Int _ -> True
        Src.Float _ -> True
        Src.Chr _ -> True
        _ -> False


{-| Collect user-defined function names from module values (excludes "main")
-}
collectUserFunctionNames : List (Src.Located Src.Value) -> List String
collectUserFunctionNames values =
    values
        |> List.filterMap
            (\(Src.At _ value) ->
                let
                    (Src.At _ name) = value.name
                in
                if name /= "main" && not (List.isEmpty value.args) then
                    Just name
                else
                    Nothing
            )


{-| Collect all function names from module values (includes "main")
-}
collectAllFunctionNames : List (Src.Located Src.Value) -> List String
collectAllFunctionNames values =
    values
        |> List.filterMap
            (\(Src.At _ value) ->
                let
                    (Src.At _ name) = value.name
                in
                if not (List.isEmpty value.args) then
                    Just name
                else
                    Nothing
            )
