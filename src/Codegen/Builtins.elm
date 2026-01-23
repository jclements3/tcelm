module Codegen.Builtins exposing
    ( generateBuiltinCall
    , generateBuiltinCallWithPipeArg
    )

{-| Built-in function code generation.

This module handles generation of C code for Elm's built-in functions
from modules like Basics, String, List, Maybe, Result, Tuple, Char, and Debug.

To break circular dependencies with expression generation, functions in this
module take a `genExpr` callback parameter for generating sub-expressions.

-}

import AST.Source as Src
import Codegen.Shared as Shared exposing (ExprCtx)


{-| Type alias for the expression generator callback.
This allows us to break the circular dependency between
expression generation and builtin handling.
-}
type alias GenExpr =
    Src.Expr -> String


{-| Type alias for context-aware expression generator callback.
-}
type alias GenExprCtx =
    ExprCtx -> Src.Expr -> String


{-| Generate C code for a built-in function call.

Takes:

  - genExpr: callback for generating sub-expressions
  - ctx: expression context for scope tracking
  - fn: the function being called
  - args: the arguments to the function

-}
generateBuiltinCall : GenExpr -> ExprCtx -> Src.Expr -> List Src.Expr -> Maybe String
generateBuiltinCall genExpr ctx fn args =
    case fn of
        -- Basics module functions (unqualified)
        Src.At _ (Src.Var _ "modBy") ->
            Just (generateModBy genExpr args)

        Src.At _ (Src.Var _ "remainderBy") ->
            Just (generateRemainderBy genExpr args)

        Src.At _ (Src.Var _ "abs") ->
            Just (generateAbs genExpr args)

        Src.At _ (Src.Var _ "negate") ->
            Just (generateNegate genExpr args)

        Src.At _ (Src.Var _ "min") ->
            Just (generateMin genExpr args)

        Src.At _ (Src.Var _ "max") ->
            Just (generateMax genExpr args)

        Src.At _ (Src.Var _ "identity") ->
            Just (generateIdentity genExpr args)

        Src.At _ (Src.Var _ "always") ->
            Just (generateAlways genExpr args)

        Src.At _ (Src.Var _ "never") ->
            Just (generateNever args)

        Src.At _ (Src.Var _ "not") ->
            Just (generateNot genExpr args)

        Src.At _ (Src.Var _ "clamp") ->
            Just (generateClamp genExpr args)

        Src.At _ (Src.Var _ "xor") ->
            Just (generateXor genExpr args)

        Src.At _ (Src.Var _ "compare") ->
            Just (generateCompare genExpr args)

        -- Basics module (qualified)
        Src.At _ (Src.VarQual _ "Basics" "modBy") ->
            Just (generateModBy genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "remainderBy") ->
            Just (generateRemainderBy genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "abs") ->
            Just (generateAbs genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "negate") ->
            Just (generateNegate genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "min") ->
            Just (generateMin genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "max") ->
            Just (generateMax genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "identity") ->
            Just (generateIdentity genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "always") ->
            Just (generateAlways genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "never") ->
            Just (generateNever args)

        Src.At _ (Src.VarQual _ "Basics" "not") ->
            Just (generateNot genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "clamp") ->
            Just (generateClamp genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "xor") ->
            Just (generateXor genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "compare") ->
            Just (generateCompare genExpr args)

        -- Tuple module
        Src.At _ (Src.VarQual _ "Tuple" "first") ->
            Just (generateTupleFirst genExpr args)

        Src.At _ (Src.VarQual _ "Tuple" "second") ->
            Just (generateTupleSecond genExpr args)

        Src.At _ (Src.VarQual _ "Tuple" "pair") ->
            Just (generateTuplePair genExpr args)

        -- Char module
        Src.At _ (Src.VarQual _ "Char" "toCode") ->
            Just (generateCharToCode genExpr args)

        Src.At _ (Src.VarQual _ "Char" "fromCode") ->
            Just (generateCharFromCode genExpr args)

        Src.At _ (Src.VarQual _ "Char" fnName) ->
            generateCharPredicate genExpr fnName args

        -- Not a builtin we handle here
        _ ->
            Nothing


{-| Generate code for builtin calls with pipe argument.
Returns Nothing if not a known builtin.
-}
generateBuiltinCallWithPipeArg : GenExpr -> ExprCtx -> Src.Expr -> List Src.Expr -> String -> Maybe String
generateBuiltinCallWithPipeArg genExpr ctx fn partialArgs pipeArg =
    -- This can be expanded as we migrate more pipe-specific builtins
    Nothing



-- BASICS MODULE HANDLERS


generateModBy : GenExpr -> List Src.Expr -> String
generateModBy genExpr args =
    case args of
        [ divisor, dividend ] ->
            "((((int)"
                ++ genExpr dividend
                ++ " % (int)"
                ++ genExpr divisor
                ++ " + (int)"
                ++ genExpr divisor
                ++ ") % (int)"
                ++ genExpr divisor
                ++ "))"

        [ _ ] ->
            "/* partial modBy */ 0"

        _ ->
            "/* modBy wrong arity */ 0"


generateRemainderBy : GenExpr -> List Src.Expr -> String
generateRemainderBy genExpr args =
    case args of
        [ divisor, dividend ] ->
            "((int)" ++ genExpr dividend ++ " % (int)" ++ genExpr divisor ++ ")"

        _ ->
            "/* remainderBy wrong arity */ 0"


generateAbs : GenExpr -> List Src.Expr -> String
generateAbs genExpr args =
    case args of
        [ value ] ->
            let
                v =
                    genExpr value
            in
            "((" ++ v ++ " < 0) ? -(" ++ v ++ ") : (" ++ v ++ "))"

        _ ->
            "/* abs wrong arity */ 0"


generateNegate : GenExpr -> List Src.Expr -> String
generateNegate genExpr args =
    case args of
        [ value ] ->
            "(-" ++ genExpr value ++ ")"

        _ ->
            "/* negate wrong arity */ 0"


generateMin : GenExpr -> List Src.Expr -> String
generateMin genExpr args =
    case args of
        [ a, b ] ->
            let
                aStr =
                    genExpr a

                bStr =
                    genExpr b
            in
            "((" ++ aStr ++ " < " ++ bStr ++ ") ? (" ++ aStr ++ ") : (" ++ bStr ++ "))"

        _ ->
            "/* min wrong arity */ 0"


generateMax : GenExpr -> List Src.Expr -> String
generateMax genExpr args =
    case args of
        [ a, b ] ->
            let
                aStr =
                    genExpr a

                bStr =
                    genExpr b
            in
            "((" ++ aStr ++ " > " ++ bStr ++ ") ? (" ++ aStr ++ ") : (" ++ bStr ++ "))"

        _ ->
            "/* max wrong arity */ 0"


generateIdentity : GenExpr -> List Src.Expr -> String
generateIdentity genExpr args =
    case args of
        [ value ] ->
            genExpr value

        _ ->
            "/* identity wrong arity */ 0"


generateAlways : GenExpr -> List Src.Expr -> String
generateAlways genExpr args =
    case args of
        [ value, _ ] ->
            genExpr value

        _ ->
            "/* always wrong arity */ 0"


generateNever : List Src.Expr -> String
generateNever args =
    case args of
        [ _ ] ->
            "({ while(1); 0; })"

        _ ->
            "/* never wrong arity */ 0"


generateNot : GenExpr -> List Src.Expr -> String
generateNot genExpr args =
    case args of
        [ value ] ->
            "(!" ++ genExpr value ++ ")"

        _ ->
            "/* not wrong arity */ 0"


generateClamp : GenExpr -> List Src.Expr -> String
generateClamp genExpr args =
    case args of
        [ low, high, value ] ->
            let
                lowStr =
                    genExpr low

                highStr =
                    genExpr high

                valStr =
                    genExpr value
            in
            "(("
                ++ valStr
                ++ " < "
                ++ lowStr
                ++ ") ? "
                ++ lowStr
                ++ " : (("
                ++ valStr
                ++ " > "
                ++ highStr
                ++ ") ? "
                ++ highStr
                ++ " : "
                ++ valStr
                ++ "))"

        _ ->
            "/* clamp wrong arity */ 0"


generateXor : GenExpr -> List Src.Expr -> String
generateXor genExpr args =
    case args of
        [ a, b ] ->
            "((" ++ genExpr a ++ ") != (" ++ genExpr b ++ "))"

        _ ->
            "/* xor wrong arity */ 0"


generateCompare : GenExpr -> List Src.Expr -> String
generateCompare genExpr args =
    case args of
        [ a, b ] ->
            let
                aStr =
                    genExpr a

                bStr =
                    genExpr b
            in
            "(("
                ++ aStr
                ++ " < "
                ++ bStr
                ++ ") ? ((elm_union_t){TAG_LT, 0}) : (("
                ++ aStr
                ++ " > "
                ++ bStr
                ++ ") ? ((elm_union_t){TAG_GT, 0}) : ((elm_union_t){TAG_EQ, 0})))"

        _ ->
            "/* compare wrong arity */ 0"



-- TUPLE MODULE HANDLERS


generateTupleFirst : GenExpr -> List Src.Expr -> String
generateTupleFirst genExpr args =
    case args of
        [ tuple ] ->
            "(" ++ genExpr tuple ++ "._0)"

        _ ->
            "/* Tuple.first wrong arity */ 0"


generateTupleSecond : GenExpr -> List Src.Expr -> String
generateTupleSecond genExpr args =
    case args of
        [ tuple ] ->
            "(" ++ genExpr tuple ++ "._1)"

        _ ->
            "/* Tuple.second wrong arity */ 0"


generateTuplePair : GenExpr -> List Src.Expr -> String
generateTuplePair genExpr args =
    case args of
        [ a, b ] ->
            "((elm_tuple2_t){" ++ genExpr a ++ ", " ++ genExpr b ++ "})"

        _ ->
            "/* Tuple.pair wrong arity */ 0"



-- CHAR MODULE HANDLERS


generateCharToCode : GenExpr -> List Src.Expr -> String
generateCharToCode genExpr args =
    case args of
        [ c ] ->
            "((int)" ++ genExpr c ++ ")"

        _ ->
            "/* Char.toCode wrong arity */ 0"


generateCharFromCode : GenExpr -> List Src.Expr -> String
generateCharFromCode genExpr args =
    case args of
        [ n ] ->
            "((char)" ++ genExpr n ++ ")"

        _ ->
            "/* Char.fromCode wrong arity */ 0"


generateCharPredicate : GenExpr -> String -> List Src.Expr -> Maybe String
generateCharPredicate genExpr fnName args =
    case args of
        [ c ] ->
            let
                cStr =
                    genExpr c
            in
            case fnName of
                "isDigit" ->
                    Just ("(" ++ cStr ++ " >= '0' && " ++ cStr ++ " <= '9')")

                "isAlpha" ->
                    Just ("((" ++ cStr ++ " >= 'a' && " ++ cStr ++ " <= 'z') || (" ++ cStr ++ " >= 'A' && " ++ cStr ++ " <= 'Z'))")

                "isUpper" ->
                    Just ("(" ++ cStr ++ " >= 'A' && " ++ cStr ++ " <= 'Z')")

                "isLower" ->
                    Just ("(" ++ cStr ++ " >= 'a' && " ++ cStr ++ " <= 'z')")

                "isAlphaNum" ->
                    Just ("((" ++ cStr ++ " >= 'a' && " ++ cStr ++ " <= 'z') || (" ++ cStr ++ " >= 'A' && " ++ cStr ++ " <= 'Z') || (" ++ cStr ++ " >= '0' && " ++ cStr ++ " <= '9'))")

                "isHexDigit" ->
                    Just ("((" ++ cStr ++ " >= '0' && " ++ cStr ++ " <= '9') || (" ++ cStr ++ " >= 'a' && " ++ cStr ++ " <= 'f') || (" ++ cStr ++ " >= 'A' && " ++ cStr ++ " <= 'F'))")

                "isOctDigit" ->
                    Just ("(" ++ cStr ++ " >= '0' && " ++ cStr ++ " <= '7')")

                "isSpace" ->
                    Just ("(" ++ cStr ++ " == ' ' || " ++ cStr ++ " == '\\t' || " ++ cStr ++ " == '\\n' || " ++ cStr ++ " == '\\r')")

                "toUpper" ->
                    Just ("((" ++ cStr ++ " >= 'a' && " ++ cStr ++ " <= 'z') ? " ++ cStr ++ " - 32 : " ++ cStr ++ ")")

                "toLower" ->
                    Just ("((" ++ cStr ++ " >= 'A' && " ++ cStr ++ " <= 'Z') ? " ++ cStr ++ " + 32 : " ++ cStr ++ ")")

                "toLocaleUpper" ->
                    Just ("((" ++ cStr ++ " >= 'a' && " ++ cStr ++ " <= 'z') ? " ++ cStr ++ " - 32 : " ++ cStr ++ ")")

                "toLocaleLower" ->
                    Just ("((" ++ cStr ++ " >= 'A' && " ++ cStr ++ " <= 'Z') ? " ++ cStr ++ " + 32 : " ++ cStr ++ ")")

                _ ->
                    Nothing

        _ ->
            Just ("/* Char." ++ fnName ++ " wrong arity */ 0")
