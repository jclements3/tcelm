module Codegen.Expr exposing
    ( generateBinop
    , generateIfExpr
    , generateLiteral
    , generateRecordLiteral
    , generateTupleLiteral
    , generateListLiteral
    )

{-| Expression code generation utilities.

This module provides helper functions for generating C code from
various Elm expression types. The main expression generator remains
in Cli.elm but delegates to these utilities.

-}

import AST.Source as Src
import Codegen.Shared as Shared exposing (escapeC)


{-| Type alias for the expression generator callback.
-}
type alias GenExpr =
    Src.Expr -> String


{-| Generate C code for a literal expression.
Returns Just for literals we handle, Nothing otherwise.
-}
generateLiteral : Src.Expr_ -> Maybe String
generateLiteral expr =
    case expr of
        Src.Int n ->
            Just (String.fromInt n)

        Src.Float f ->
            Just (String.fromFloat f)

        Src.Str s ->
            Just ("\"" ++ escapeC s ++ "\"")

        Src.Chr c ->
            Just ("'" ++ escapeC c ++ "'")

        _ ->
            Nothing


{-| Generate C code for a binary operation.
-}
generateBinop : GenExpr -> String -> Src.Expr -> Src.Expr -> String
generateBinop genExpr op left right =
    let
        leftStr =
            genExpr left

        rightStr =
            genExpr right
    in
    case op of
        "+" ->
            "(" ++ leftStr ++ " + " ++ rightStr ++ ")"

        "-" ->
            "(" ++ leftStr ++ " - " ++ rightStr ++ ")"

        "*" ->
            "(" ++ leftStr ++ " * " ++ rightStr ++ ")"

        "/" ->
            "(" ++ leftStr ++ " / " ++ rightStr ++ ")"

        "//" ->
            "((int)" ++ leftStr ++ " / (int)" ++ rightStr ++ ")"

        "^" ->
            "elm_pow(" ++ leftStr ++ ", " ++ rightStr ++ ")"

        "==" ->
            "(" ++ leftStr ++ " == " ++ rightStr ++ ")"

        "/=" ->
            "(" ++ leftStr ++ " != " ++ rightStr ++ ")"

        "<" ->
            "(" ++ leftStr ++ " < " ++ rightStr ++ ")"

        ">" ->
            "(" ++ leftStr ++ " > " ++ rightStr ++ ")"

        "<=" ->
            "(" ++ leftStr ++ " <= " ++ rightStr ++ ")"

        ">=" ->
            "(" ++ leftStr ++ " >= " ++ rightStr ++ ")"

        "&&" ->
            "(" ++ leftStr ++ " && " ++ rightStr ++ ")"

        "||" ->
            "(" ++ leftStr ++ " || " ++ rightStr ++ ")"

        "++" ->
            "elm_str_append(" ++ leftStr ++ ", " ++ rightStr ++ ")"

        "::" ->
            -- Cons operator - prepend element to list
            "({ elm_list_t __cons_result = " ++ rightStr ++ "; "
                ++ "for (int __i = __cons_result.length; __i > 0; __i--) "
                ++ "__cons_result.data[__i] = __cons_result.data[__i - 1]; "
                ++ "__cons_result.data[0].d = " ++ leftStr ++ "; "
                ++ "__cons_result.length++; __cons_result; })"

        _ ->
            "/* unknown op: " ++ op ++ " */ 0"


{-| Generate C code for an if expression.
-}
generateIfExpr : GenExpr -> List ( Src.Expr, Src.Expr ) -> Src.Expr -> String
generateIfExpr genExpr branches elseExpr =
    case branches of
        [] ->
            genExpr elseExpr

        ( condition, thenExpr ) :: rest ->
            "("
                ++ genExpr condition
                ++ " ? "
                ++ genExpr thenExpr
                ++ " : "
                ++ generateIfExpr genExpr rest elseExpr
                ++ ")"


{-| Generate C code for a record literal.
-}
generateRecordLiteral : GenExpr -> List ( Src.Located String, Src.Expr ) -> String
generateRecordLiteral genExpr fields =
    let
        -- Infer field type from the value expression
        inferFieldType valueStr =
            if String.startsWith "\"" valueStr then
                "const char *"

            else if String.contains "elm_str_" valueStr || String.contains "elm_from_" valueStr then
                "const char *"

            else if String.contains "elm_list_t" valueStr || String.contains ".length" valueStr then
                "elm_list_t"

            else
                "double"

        fieldDefs =
            fields
                |> List.map
                    (\( Src.At _ fieldName, fieldValue ) ->
                        let
                            valueStr =
                                genExpr fieldValue
                        in
                        inferFieldType valueStr ++ " " ++ fieldName
                    )
                |> String.join "; "

        fieldValues =
            fields
                |> List.map
                    (\( Src.At _ fieldName, fieldValue ) ->
                        "." ++ fieldName ++ " = " ++ genExpr fieldValue
                    )
                |> String.join ", "
    in
    "((struct { " ++ fieldDefs ++ "; }){" ++ fieldValues ++ "})"


{-| Generate C code for a tuple literal.
-}
generateTupleLiteral : GenExpr -> Src.Expr -> Src.Expr -> List Src.Expr -> String
generateTupleLiteral genExpr first second rest =
    let
        elements =
            first :: second :: rest

        numElements =
            List.length elements

        tupleType =
            if numElements == 2 then
                "elm_tuple2_t"

            else if numElements == 3 then
                "elm_tuple3_t"

            else
                "struct { "
                    ++ (List.indexedMap (\i _ -> "elm_elem_t _" ++ String.fromInt i) elements |> String.join "; ")
                    ++ "; }"

        -- Generate each element, wrapped in elm_elem_t union initialization
        elemValues =
            elements
                |> List.map
                    (\e ->
                        let
                            valueStr =
                                genExpr e
                        in
                        "{.d = " ++ valueStr ++ "}"
                    )
                |> String.join ", "
    in
    "((" ++ tupleType ++ "){" ++ elemValues ++ "})"


{-| Generate C code for a list literal.
-}
generateListLiteral : GenExpr -> List Src.Expr -> String
generateListLiteral genExpr elements =
    if List.isEmpty elements then
        "((elm_list_t){ .length = 0 })"

    else
        let
            numElements =
                List.length elements

            elemValues =
                elements
                    |> List.map (\e -> "{.d = " ++ genExpr e ++ "}")
                    |> String.join ", "
        in
        "((elm_list_t){ .length = "
            ++ String.fromInt numElements
            ++ ", .data = {"
            ++ elemValues
            ++ "} })"
