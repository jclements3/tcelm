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

        Src.At _ (Src.VarQual _ "Tuple" "mapFirst") ->
            Just (generateTupleMapFirst genExpr args)

        Src.At _ (Src.VarQual _ "Tuple" "mapSecond") ->
            Just (generateTupleMapSecond genExpr args)

        Src.At _ (Src.VarQual _ "Tuple" "mapBoth") ->
            Just (generateTupleMapBoth genExpr args)

        -- Char module
        Src.At _ (Src.VarQual _ "Char" "toCode") ->
            Just (generateCharToCode genExpr args)

        Src.At _ (Src.VarQual _ "Char" "fromCode") ->
            Just (generateCharFromCode genExpr args)

        Src.At _ (Src.VarQual _ "Char" fnName) ->
            generateCharPredicate genExpr fnName args

        -- String module
        Src.At _ (Src.VarQual _ "String" "fromChar") ->
            Just (generateStringFromChar genExpr args)

        Src.At _ (Src.VarQual _ "String" "cons") ->
            Just (generateStringCons genExpr args)

        Src.At _ (Src.VarQual _ "String" "length") ->
            Just (generateStringLength genExpr args)

        Src.At _ (Src.VarQual _ "String" "isEmpty") ->
            Just (generateStringIsEmpty genExpr args)

        Src.At _ (Src.VarQual _ "String" "reverse") ->
            Just (generateStringReverse genExpr args)

        Src.At _ (Src.VarQual _ "String" "toInt") ->
            Just (generateStringToInt genExpr args)

        Src.At _ (Src.VarQual _ "String" "fromInt") ->
            Just (generateStringFromInt genExpr args)

        Src.At _ (Src.VarQual _ "String" "fromFloat") ->
            Just (generateStringFromFloat genExpr args)

        Src.At _ (Src.VarQual _ "String" "toFloat") ->
            Just (generateStringToFloat genExpr args)

        Src.At _ (Src.VarQual _ "String" "left") ->
            Just (generateStringLeft genExpr args)

        Src.At _ (Src.VarQual _ "String" "right") ->
            Just (generateStringRight genExpr args)

        Src.At _ (Src.VarQual _ "String" "append") ->
            Just (generateStringAppend genExpr args)

        Src.At _ (Src.VarQual _ "String" "contains") ->
            Just (generateStringContains genExpr args)

        Src.At _ (Src.VarQual _ "String" "startsWith") ->
            Just (generateStringStartsWith genExpr args)

        Src.At _ (Src.VarQual _ "String" "endsWith") ->
            Just (generateStringEndsWith genExpr args)

        Src.At _ (Src.VarQual _ "String" "toUpper") ->
            Just (generateStringToUpper genExpr args)

        Src.At _ (Src.VarQual _ "String" "toLower") ->
            Just (generateStringToLower genExpr args)

        Src.At _ (Src.VarQual _ "String" "trim") ->
            Just (generateStringTrim genExpr args)

        Src.At _ (Src.VarQual _ "String" "trimLeft") ->
            Just (generateStringTrimLeft genExpr args)

        Src.At _ (Src.VarQual _ "String" "trimRight") ->
            Just (generateStringTrimRight genExpr args)

        Src.At _ (Src.VarQual _ "String" "repeat") ->
            Just (generateStringRepeat genExpr args)

        Src.At _ (Src.VarQual _ "String" "slice") ->
            Just (generateStringSlice genExpr args)

        Src.At _ (Src.VarQual _ "String" "dropLeft") ->
            Just (generateStringDropLeft genExpr args)

        Src.At _ (Src.VarQual _ "String" "dropRight") ->
            Just (generateStringDropRight genExpr args)

        Src.At _ (Src.VarQual _ "String" "padLeft") ->
            Just (generateStringPadLeft genExpr args)

        Src.At _ (Src.VarQual _ "String" "padRight") ->
            Just (generateStringPadRight genExpr args)

        Src.At _ (Src.VarQual _ "String" "replace") ->
            Just (generateStringReplace genExpr args)

        -- Note: String.join has inline implementation in Cli.elm, not handled here

        -- Bitwise module
        Src.At _ (Src.VarQual _ "Bitwise" "and") ->
            Just (generateBitwiseAnd genExpr args)

        Src.At _ (Src.VarQual _ "Bitwise" "or") ->
            Just (generateBitwiseOr genExpr args)

        Src.At _ (Src.VarQual _ "Bitwise" "xor") ->
            Just (generateBitwiseXor genExpr args)

        Src.At _ (Src.VarQual _ "Bitwise" "complement") ->
            Just (generateBitwiseComplement genExpr args)

        Src.At _ (Src.VarQual _ "Bitwise" "shiftLeftBy") ->
            Just (generateBitwiseShiftLeftBy genExpr args)

        Src.At _ (Src.VarQual _ "Bitwise" "shiftRightBy") ->
            Just (generateBitwiseShiftRightBy genExpr args)

        Src.At _ (Src.VarQual _ "Bitwise" "shiftRightZfBy") ->
            Just (generateBitwiseShiftRightZfBy genExpr args)

        -- Debug module
        Src.At _ (Src.VarQual _ "Debug" "log") ->
            Just (generateDebugLog genExpr args)

        Src.At _ (Src.VarQual _ "Debug" "todo") ->
            Just (generateDebugTodo genExpr args)

        -- Maybe module (simple functions only - map/andThen have lambda handling in Cli.elm)
        Src.At _ (Src.VarQual _ "Maybe" "withDefault") ->
            Just (generateMaybeWithDefault genExpr args)

        -- Result module (simple functions only - map/mapError/andThen have lambda handling in Cli.elm)
        Src.At _ (Src.VarQual _ "Result" "withDefault") ->
            Just (generateResultWithDefault genExpr args)

        Src.At _ (Src.VarQual _ "Result" "toMaybe") ->
            Just (generateResultToMaybe genExpr args)

        Src.At _ (Src.VarQual _ "Result" "fromMaybe") ->
            Just (generateResultFromMaybe genExpr args)

        -- Math functions (Basics module, unqualified)
        Src.At _ (Src.Var _ "floor") ->
            Just (generateFloor genExpr args)

        Src.At _ (Src.Var _ "ceiling") ->
            Just (generateCeiling genExpr args)

        Src.At _ (Src.Var _ "round") ->
            Just (generateRound genExpr args)

        Src.At _ (Src.Var _ "truncate") ->
            Just (generateTruncate genExpr args)

        Src.At _ (Src.Var _ "sqrt") ->
            Just (generateSqrt genExpr args)

        Src.At _ (Src.Var _ "logBase") ->
            Just (generateLogBase genExpr args)

        Src.At _ (Src.Var _ "toFloat") ->
            Just (generateToFloat genExpr args)

        Src.At _ (Src.Var _ "isEven") ->
            Just (generateIsEven genExpr args)

        Src.At _ (Src.Var _ "isOdd") ->
            Just (generateIsOdd genExpr args)

        -- List module (simple functions - map/filter/foldl/foldr/etc remain in Cli.elm)
        Src.At _ (Src.VarQual _ "List" "head") ->
            Just (generateListHead genExpr args)

        Src.At _ (Src.VarQual _ "List" "tail") ->
            Just (generateListTail genExpr args)

        Src.At _ (Src.VarQual _ "List" "last") ->
            Just (generateListLast genExpr args)

        Src.At _ (Src.VarQual _ "List" "sum") ->
            Just (generateListSum genExpr args)

        Src.At _ (Src.VarQual _ "List" "product") ->
            Just (generateListProduct genExpr args)

        Src.At _ (Src.VarQual _ "List" "maximum") ->
            Just (generateListMaximum genExpr args)

        Src.At _ (Src.VarQual _ "List" "minimum") ->
            Just (generateListMinimum genExpr args)

        Src.At _ (Src.VarQual _ "List" "reverse") ->
            Just (generateListReverse genExpr args)

        Src.At _ (Src.VarQual _ "List" "member") ->
            Just (generateListMember genExpr args)

        Src.At _ (Src.VarQual _ "List" "range") ->
            Just (generateListRange genExpr args)

        Src.At _ (Src.VarQual _ "List" "take") ->
            Just (generateListTake genExpr args)

        Src.At _ (Src.VarQual _ "List" "drop") ->
            Just (generateListDrop genExpr args)

        Src.At _ (Src.VarQual _ "List" "append") ->
            Just (generateListAppend genExpr args)

        Src.At _ (Src.VarQual _ "List" "repeat") ->
            Just (generateListRepeat genExpr args)

        Src.At _ (Src.VarQual _ "List" "intersperse") ->
            Just (generateListIntersperse genExpr args)

        -- Math functions (Basics module, qualified)
        Src.At _ (Src.VarQual _ "Basics" "floor") ->
            Just (generateFloor genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "ceiling") ->
            Just (generateCeiling genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "round") ->
            Just (generateRound genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "truncate") ->
            Just (generateTruncate genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "sqrt") ->
            Just (generateSqrt genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "logBase") ->
            Just (generateLogBase genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "toFloat") ->
            Just (generateToFloat genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "isEven") ->
            Just (generateIsEven genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "isOdd") ->
            Just (generateIsOdd genExpr args)

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


generateTupleMapFirst : GenExpr -> List Src.Expr -> String
generateTupleMapFirst genExpr args =
    case args of
        [ fnExpr, tupleExpr ] ->
            let
                tupleStr =
                    genExpr tupleExpr

                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __tuple_in._0; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__tuple_in._0)"
            in
            "({ elm_tuple2_t __tuple_in = " ++ tupleStr ++ "; elm_tuple2_t __tuple_out; __tuple_out._0 = " ++ fnAppStr ++ "; __tuple_out._1 = __tuple_in._1; __tuple_out; })"

        _ ->
            "/* Tuple.mapFirst wrong arity */ 0"


generateTupleMapSecond : GenExpr -> List Src.Expr -> String
generateTupleMapSecond genExpr args =
    case args of
        [ fnExpr, tupleExpr ] ->
            let
                tupleStr =
                    genExpr tupleExpr

                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __tuple_in._1; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__tuple_in._1)"
            in
            "({ elm_tuple2_t __tuple_in = " ++ tupleStr ++ "; elm_tuple2_t __tuple_out; __tuple_out._0 = __tuple_in._0; __tuple_out._1 = " ++ fnAppStr ++ "; __tuple_out; })"

        _ ->
            "/* Tuple.mapSecond wrong arity */ 0"


generateTupleMapBoth : GenExpr -> List Src.Expr -> String
generateTupleMapBoth genExpr args =
    case args of
        [ fnFirst, fnSecond, tupleExpr ] ->
            let
                tupleStr =
                    genExpr tupleExpr

                fnFirstApp =
                    case fnFirst of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __tuple_in._0; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnFirst ++ "(__tuple_in._0)"

                fnSecondApp =
                    case fnSecond of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __tuple_in._1; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnSecond ++ "(__tuple_in._1)"
            in
            "({ elm_tuple2_t __tuple_in = " ++ tupleStr ++ "; elm_tuple2_t __tuple_out; __tuple_out._0 = " ++ fnFirstApp ++ "; __tuple_out._1 = " ++ fnSecondApp ++ "; __tuple_out; })"

        _ ->
            "/* Tuple.mapBoth wrong arity */ 0"



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



-- STRING MODULE HANDLERS


generateStringFromChar : GenExpr -> List Src.Expr -> String
generateStringFromChar genExpr args =
    case args of
        [ c ] ->
            "elm_str_from_char(" ++ genExpr c ++ ")"

        _ ->
            "/* String.fromChar wrong arity */ 0"


generateStringCons : GenExpr -> List Src.Expr -> String
generateStringCons genExpr args =
    case args of
        [ c, s ] ->
            "elm_str_cons(" ++ genExpr c ++ ", " ++ genExpr s ++ ")"

        _ ->
            "/* String.cons wrong arity */ 0"


generateStringLength : GenExpr -> List Src.Expr -> String
generateStringLength genExpr args =
    case args of
        [ s ] ->
            "elm_strlen(" ++ genExpr s ++ ")"

        _ ->
            "/* String.length wrong arity */ 0"


generateStringIsEmpty : GenExpr -> List Src.Expr -> String
generateStringIsEmpty genExpr args =
    case args of
        [ s ] ->
            "(*(" ++ genExpr s ++ ") == '\\0')"

        _ ->
            "/* String.isEmpty wrong arity */ 0"


generateStringReverse : GenExpr -> List Src.Expr -> String
generateStringReverse genExpr args =
    case args of
        [ s ] ->
            "elm_str_reverse(" ++ genExpr s ++ ")"

        _ ->
            "/* String.reverse wrong arity */ 0"


generateStringToInt : GenExpr -> List Src.Expr -> String
generateStringToInt genExpr args =
    case args of
        [ s ] ->
            "elm_str_to_int(" ++ genExpr s ++ ")"

        _ ->
            "/* String.toInt wrong arity */ 0"


generateStringFromInt : GenExpr -> List Src.Expr -> String
generateStringFromInt genExpr args =
    case args of
        [ n ] ->
            "elm_from_int(" ++ genExpr n ++ ")"

        _ ->
            "/* String.fromInt wrong arity */ 0"


generateStringFromFloat : GenExpr -> List Src.Expr -> String
generateStringFromFloat genExpr args =
    case args of
        [ f ] ->
            "elm_from_float(" ++ genExpr f ++ ")"

        _ ->
            "/* String.fromFloat wrong arity */ 0"


generateStringToFloat : GenExpr -> List Src.Expr -> String
generateStringToFloat genExpr args =
    case args of
        [ s ] ->
            "elm_str_to_float(" ++ genExpr s ++ ")"

        _ ->
            "/* String.toFloat wrong arity */ 0"


generateStringLeft : GenExpr -> List Src.Expr -> String
generateStringLeft genExpr args =
    case args of
        [ n, s ] ->
            "elm_str_left(" ++ genExpr n ++ ", " ++ genExpr s ++ ")"

        _ ->
            "/* String.left wrong arity */ 0"


generateStringRight : GenExpr -> List Src.Expr -> String
generateStringRight genExpr args =
    case args of
        [ n, s ] ->
            "elm_str_right(" ++ genExpr n ++ ", " ++ genExpr s ++ ")"

        _ ->
            "/* String.right wrong arity */ 0"


generateStringAppend : GenExpr -> List Src.Expr -> String
generateStringAppend genExpr args =
    case args of
        [ a, b ] ->
            "elm_str_append(" ++ genExpr a ++ ", " ++ genExpr b ++ ")"

        _ ->
            "/* String.append wrong arity */ 0"


generateStringContains : GenExpr -> List Src.Expr -> String
generateStringContains genExpr args =
    case args of
        [ needle, haystack ] ->
            "elm_str_contains(" ++ genExpr needle ++ ", " ++ genExpr haystack ++ ")"

        _ ->
            "/* String.contains wrong arity */ 0"


generateStringStartsWith : GenExpr -> List Src.Expr -> String
generateStringStartsWith genExpr args =
    case args of
        [ prefix, s ] ->
            "elm_str_starts_with(" ++ genExpr prefix ++ ", " ++ genExpr s ++ ")"

        _ ->
            "/* String.startsWith wrong arity */ 0"


generateStringEndsWith : GenExpr -> List Src.Expr -> String
generateStringEndsWith genExpr args =
    case args of
        [ suffix, s ] ->
            "elm_str_ends_with(" ++ genExpr suffix ++ ", " ++ genExpr s ++ ")"

        _ ->
            "/* String.endsWith wrong arity */ 0"


generateStringToUpper : GenExpr -> List Src.Expr -> String
generateStringToUpper genExpr args =
    case args of
        [ s ] ->
            "elm_str_to_upper(" ++ genExpr s ++ ")"

        _ ->
            "/* String.toUpper wrong arity */ 0"


generateStringToLower : GenExpr -> List Src.Expr -> String
generateStringToLower genExpr args =
    case args of
        [ s ] ->
            "elm_str_to_lower(" ++ genExpr s ++ ")"

        _ ->
            "/* String.toLower wrong arity */ 0"


generateStringTrim : GenExpr -> List Src.Expr -> String
generateStringTrim genExpr args =
    case args of
        [ s ] ->
            "elm_str_trim(" ++ genExpr s ++ ")"

        _ ->
            "/* String.trim wrong arity */ 0"


generateStringTrimLeft : GenExpr -> List Src.Expr -> String
generateStringTrimLeft genExpr args =
    case args of
        [ s ] ->
            "elm_str_trim_left(" ++ genExpr s ++ ")"

        _ ->
            "/* String.trimLeft wrong arity */ 0"


generateStringTrimRight : GenExpr -> List Src.Expr -> String
generateStringTrimRight genExpr args =
    case args of
        [ s ] ->
            "elm_str_trim_right(" ++ genExpr s ++ ")"

        _ ->
            "/* String.trimRight wrong arity */ 0"


generateStringRepeat : GenExpr -> List Src.Expr -> String
generateStringRepeat genExpr args =
    case args of
        [ n, s ] ->
            "elm_str_repeat(" ++ genExpr n ++ ", " ++ genExpr s ++ ")"

        _ ->
            "/* String.repeat wrong arity */ 0"


generateStringSlice : GenExpr -> List Src.Expr -> String
generateStringSlice genExpr args =
    case args of
        [ start, end, s ] ->
            "elm_str_slice(" ++ genExpr start ++ ", " ++ genExpr end ++ ", " ++ genExpr s ++ ")"

        _ ->
            "/* String.slice wrong arity */ 0"


generateStringDropLeft : GenExpr -> List Src.Expr -> String
generateStringDropLeft genExpr args =
    case args of
        [ n, s ] ->
            let
                nStr =
                    genExpr n

                sStr =
                    genExpr s
            in
            "elm_str_slice(" ++ nStr ++ ", elm_strlen(" ++ sStr ++ "), " ++ sStr ++ ")"

        _ ->
            "/* String.dropLeft wrong arity */ 0"


generateStringDropRight : GenExpr -> List Src.Expr -> String
generateStringDropRight genExpr args =
    case args of
        [ n, s ] ->
            let
                nStr =
                    genExpr n

                sStr =
                    genExpr s
            in
            "elm_str_slice(0, elm_strlen(" ++ sStr ++ ") - " ++ nStr ++ ", " ++ sStr ++ ")"

        _ ->
            "/* String.dropRight wrong arity */ 0"


generateStringPadLeft : GenExpr -> List Src.Expr -> String
generateStringPadLeft genExpr args =
    case args of
        [ n, c, s ] ->
            "elm_str_pad_left(" ++ genExpr n ++ ", " ++ genExpr c ++ ", " ++ genExpr s ++ ")"

        _ ->
            "/* String.padLeft wrong arity */ 0"


generateStringPadRight : GenExpr -> List Src.Expr -> String
generateStringPadRight genExpr args =
    case args of
        [ n, c, s ] ->
            "elm_str_pad_right(" ++ genExpr n ++ ", " ++ genExpr c ++ ", " ++ genExpr s ++ ")"

        _ ->
            "/* String.padRight wrong arity */ 0"


generateStringReplace : GenExpr -> List Src.Expr -> String
generateStringReplace genExpr args =
    case args of
        [ from, to, s ] ->
            "elm_str_replace(" ++ genExpr from ++ ", " ++ genExpr to ++ ", " ++ genExpr s ++ ")"

        _ ->
            "/* String.replace wrong arity */ 0"



-- BITWISE MODULE HANDLERS


generateBitwiseAnd : GenExpr -> List Src.Expr -> String
generateBitwiseAnd genExpr args =
    case args of
        [ a, b ] ->
            "(" ++ genExpr a ++ " & " ++ genExpr b ++ ")"

        _ ->
            "/* Bitwise.and wrong arity */ 0"


generateBitwiseOr : GenExpr -> List Src.Expr -> String
generateBitwiseOr genExpr args =
    case args of
        [ a, b ] ->
            "(" ++ genExpr a ++ " | " ++ genExpr b ++ ")"

        _ ->
            "/* Bitwise.or wrong arity */ 0"


generateBitwiseXor : GenExpr -> List Src.Expr -> String
generateBitwiseXor genExpr args =
    case args of
        [ a, b ] ->
            "(" ++ genExpr a ++ " ^ " ++ genExpr b ++ ")"

        _ ->
            "/* Bitwise.xor wrong arity */ 0"


generateBitwiseComplement : GenExpr -> List Src.Expr -> String
generateBitwiseComplement genExpr args =
    case args of
        [ a ] ->
            "(~" ++ genExpr a ++ ")"

        _ ->
            "/* Bitwise.complement wrong arity */ 0"


generateBitwiseShiftLeftBy : GenExpr -> List Src.Expr -> String
generateBitwiseShiftLeftBy genExpr args =
    case args of
        [ n, x ] ->
            "(" ++ genExpr x ++ " << " ++ genExpr n ++ ")"

        _ ->
            "/* Bitwise.shiftLeftBy wrong arity */ 0"


generateBitwiseShiftRightBy : GenExpr -> List Src.Expr -> String
generateBitwiseShiftRightBy genExpr args =
    case args of
        [ n, x ] ->
            "(" ++ genExpr x ++ " >> " ++ genExpr n ++ ")"

        _ ->
            "/* Bitwise.shiftRightBy wrong arity */ 0"


generateBitwiseShiftRightZfBy : GenExpr -> List Src.Expr -> String
generateBitwiseShiftRightZfBy genExpr args =
    case args of
        [ n, x ] ->
            "((unsigned int)" ++ genExpr x ++ " >> " ++ genExpr n ++ ")"

        _ ->
            "/* Bitwise.shiftRightZfBy wrong arity */ 0"



-- DEBUG MODULE HANDLERS


generateDebugLog : GenExpr -> List Src.Expr -> String
generateDebugLog genExpr args =
    case args of
        -- Debug.log just returns the value (embedded-friendly, no printing)
        [ _, value ] ->
            genExpr value

        _ ->
            "/* Debug.log wrong arity */ 0"


generateDebugTodo : GenExpr -> List Src.Expr -> String
generateDebugTodo _ args =
    case args of
        -- Debug.todo halts execution (infinite loop for embedded targets)
        [ _ ] ->
            "({ while(1); 0; })"

        _ ->
            "/* Debug.todo wrong arity */ 0"



-- MAYBE MODULE HANDLERS


generateMaybeWithDefault : GenExpr -> List Src.Expr -> String
generateMaybeWithDefault genExpr args =
    case args of
        [ defaultVal, maybeVal ] ->
            let
                defStr =
                    genExpr defaultVal

                maybeStr =
                    genExpr maybeVal
            in
            "((" ++ maybeStr ++ ").tag == TAG_Just ? (" ++ maybeStr ++ ").data : " ++ defStr ++ ")"

        _ ->
            "/* Maybe.withDefault wrong arity */ 0"



-- RESULT MODULE HANDLERS


generateResultWithDefault : GenExpr -> List Src.Expr -> String
generateResultWithDefault genExpr args =
    case args of
        [ def, result ] ->
            let
                defStr =
                    genExpr def

                resultStr =
                    genExpr result
            in
            "((" ++ resultStr ++ ").tag == TAG_Ok ? (" ++ resultStr ++ ").data : " ++ defStr ++ ")"

        _ ->
            "/* Result.withDefault wrong arity */ 0"


generateResultToMaybe : GenExpr -> List Src.Expr -> String
generateResultToMaybe genExpr args =
    case args of
        [ resultExpr ] ->
            let
                resultStr =
                    genExpr resultExpr
            in
            "((" ++ resultStr ++ ").tag == TAG_Ok ? ((elm_union_t){TAG_Just, (" ++ resultStr ++ ").data}) : ((elm_union_t){TAG_Nothing, 0}))"

        _ ->
            "/* Result.toMaybe wrong arity */ 0"


generateResultFromMaybe : GenExpr -> List Src.Expr -> String
generateResultFromMaybe genExpr args =
    case args of
        [ errExpr, maybeExpr ] ->
            let
                errStr =
                    genExpr errExpr

                maybeStr =
                    genExpr maybeExpr
            in
            "((" ++ maybeStr ++ ").tag == TAG_Just ? ((elm_union_t){TAG_Ok, (" ++ maybeStr ++ ").data}) : ((elm_union_t){TAG_Err, " ++ errStr ++ "}))"

        _ ->
            "/* Result.fromMaybe wrong arity */ 0"



-- MATH/BASICS FUNCTIONS


generateFloor : GenExpr -> List Src.Expr -> String
generateFloor genExpr args =
    case args of
        [ x ] ->
            "((int)" ++ genExpr x ++ ")"

        _ ->
            "/* floor wrong arity */ 0"


generateCeiling : GenExpr -> List Src.Expr -> String
generateCeiling genExpr args =
    case args of
        [ x ] ->
            let
                xStr =
                    genExpr x
            in
            "((int)" ++ xStr ++ " + (" ++ xStr ++ " > (int)" ++ xStr ++ " ? 1 : 0))"

        _ ->
            "/* ceiling wrong arity */ 0"


generateRound : GenExpr -> List Src.Expr -> String
generateRound genExpr args =
    case args of
        [ x ] ->
            let
                xStr =
                    genExpr x
            in
            "(" ++ xStr ++ " >= 0 ? (int)(" ++ xStr ++ " + 0.5) : (int)(" ++ xStr ++ " - 0.5))"

        _ ->
            "/* round wrong arity */ 0"


generateTruncate : GenExpr -> List Src.Expr -> String
generateTruncate genExpr args =
    case args of
        [ x ] ->
            "((int)" ++ genExpr x ++ ")"

        _ ->
            "/* truncate wrong arity */ 0"


generateSqrt : GenExpr -> List Src.Expr -> String
generateSqrt genExpr args =
    case args of
        [ x ] ->
            "sqrt(" ++ genExpr x ++ ")"

        _ ->
            "/* sqrt wrong arity */ 0"


generateLogBase : GenExpr -> List Src.Expr -> String
generateLogBase genExpr args =
    case args of
        [ base, x ] ->
            let
                baseStr =
                    genExpr base

                xStr =
                    genExpr x
            in
            "({ int __b = " ++ baseStr ++ ", __x = " ++ xStr ++ ", __r = 0; if (__b > 1 && __x > 0) { while (__x >= __b) { __x /= __b; __r++; } } __r; })"

        _ ->
            "/* logBase wrong arity */ 0"


generateToFloat : GenExpr -> List Src.Expr -> String
generateToFloat genExpr args =
    case args of
        [ n ] ->
            "((double)" ++ genExpr n ++ ")"

        _ ->
            "/* toFloat wrong arity */ 0"


generateIsEven : GenExpr -> List Src.Expr -> String
generateIsEven genExpr args =
    case args of
        [ n ] ->
            "((" ++ genExpr n ++ " % 2) == 0)"

        _ ->
            "/* isEven wrong arity */ 0"


generateIsOdd : GenExpr -> List Src.Expr -> String
generateIsOdd genExpr args =
    case args of
        [ n ] ->
            "((" ++ genExpr n ++ " % 2) != 0)"

        _ ->
            "/* isOdd wrong arity */ 0"



-- LIST MODULE HANDLERS


generateListHead : GenExpr -> List Src.Expr -> String
generateListHead genExpr args =
    case args of
        [ listExpr ] ->
            let
                listStr =
                    genExpr listExpr
            in
            "({ typeof(" ++ listStr ++ ") __lst = " ++ listStr ++ "; __lst.length > 0 ? ((elm_union_t){TAG_Just, {.ptr = (void*)&__lst.data[0]}}) : ((elm_union_t){TAG_Nothing, {.num = 0}}); })"

        _ ->
            "/* List.head wrong arity */ 0"


generateListTail : GenExpr -> List Src.Expr -> String
generateListTail genExpr args =
    case args of
        [ listExpr ] ->
            let
                listStr =
                    genExpr listExpr
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; elm_union_t __result; if (__lst.length == 0) { __result = (elm_union_t){TAG_Nothing, 0}; } else { __result = (elm_union_t){TAG_Just, __lst.length - 1}; } __result; })"

        _ ->
            "/* List.tail wrong arity */ 0"


generateListLast : GenExpr -> List Src.Expr -> String
generateListLast genExpr args =
    case args of
        [ listExpr ] ->
            let
                listStr =
                    genExpr listExpr
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; __lst.length > 0 ? ((elm_union_t){TAG_Just, __lst.data[__lst.length - 1]}) : ((elm_union_t){TAG_Nothing, 0}); })"

        _ ->
            "/* List.last wrong arity */ 0"


generateListSum : GenExpr -> List Src.Expr -> String
generateListSum genExpr args =
    case args of
        [ listExpr ] ->
            let
                listStr =
                    genExpr listExpr
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; double __sum = 0; for (int __i = 0; __i < __lst.length; __i++) __sum += __lst.data[__i].d; __sum; })"

        _ ->
            "/* List.sum wrong arity */ 0"


generateListProduct : GenExpr -> List Src.Expr -> String
generateListProduct genExpr args =
    case args of
        [ listExpr ] ->
            let
                listStr =
                    genExpr listExpr
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; double __prod = 1; for (int __i = 0; __i < __lst.length; __i++) __prod *= __lst.data[__i].d; __prod; })"

        _ ->
            "/* List.product wrong arity */ 0"


generateListMaximum : GenExpr -> List Src.Expr -> String
generateListMaximum genExpr args =
    case args of
        [ listExpr ] ->
            let
                listStr =
                    genExpr listExpr
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; elm_union_t __result; if (__lst.length == 0) { __result = (elm_union_t){TAG_Nothing, 0}; } else { int __max = __lst.data[0]; for (int __i = 1; __i < __lst.length; __i++) if (__lst.data[__i] > __max) __max = __lst.data[__i]; __result = (elm_union_t){TAG_Just, __max}; } __result; })"

        _ ->
            "/* List.maximum wrong arity */ 0"


generateListMinimum : GenExpr -> List Src.Expr -> String
generateListMinimum genExpr args =
    case args of
        [ listExpr ] ->
            let
                listStr =
                    genExpr listExpr
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; elm_union_t __result; if (__lst.length == 0) { __result = (elm_union_t){TAG_Nothing, 0}; } else { int __min = __lst.data[0]; for (int __i = 1; __i < __lst.length; __i++) if (__lst.data[__i] < __min) __min = __lst.data[__i]; __result = (elm_union_t){TAG_Just, __min}; } __result; })"

        _ ->
            "/* List.minimum wrong arity */ 0"


generateListReverse : GenExpr -> List Src.Expr -> String
generateListReverse genExpr args =
    case args of
        [ listExpr ] ->
            let
                listStr =
                    genExpr listExpr
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; elm_list_t __rev; __rev.length = __lst.length; for (int __i = 0; __i < __lst.length; __i++) __rev.data[__i] = __lst.data[__lst.length - 1 - __i]; __rev; })"

        _ ->
            "/* List.reverse wrong arity */ 0"


generateListMember : GenExpr -> List Src.Expr -> String
generateListMember genExpr args =
    case args of
        [ elemExpr, listExpr ] ->
            let
                elemStr =
                    genExpr elemExpr

                listStr =
                    genExpr listExpr
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; int __elem = " ++ elemStr ++ "; int __found = 0; for (int __i = 0; __i < __lst.length && !__found; __i++) if (__lst.data[__i] == __elem) __found = 1; __found; })"

        _ ->
            "/* List.member wrong arity */ 0"


generateListRange : GenExpr -> List Src.Expr -> String
generateListRange genExpr args =
    case args of
        [ loExpr, hiExpr ] ->
            let
                loStr =
                    genExpr loExpr

                hiStr =
                    genExpr hiExpr
            in
            "({ int __lo = " ++ loStr ++ ", __hi = " ++ hiStr ++ "; elm_list_t __lst; __lst.length = __hi >= __lo ? __hi - __lo + 1 : 0; if (__lst.length > ELM_LIST_MAX) __lst.length = ELM_LIST_MAX; for (int __i = 0; __i < __lst.length; __i++) __lst.data[__i].d = __lo + __i; __lst; })"

        _ ->
            "/* List.range wrong arity */ 0"


generateListTake : GenExpr -> List Src.Expr -> String
generateListTake genExpr args =
    case args of
        [ nExpr, listExpr ] ->
            let
                nStr =
                    genExpr nExpr

                listStr =
                    genExpr listExpr
            in
            "({ int __n = " ++ nStr ++ "; elm_list_t __lst = " ++ listStr ++ "; elm_list_t __result; __result.length = __n < __lst.length ? __n : __lst.length; if (__result.length < 0) __result.length = 0; for (int __i = 0; __i < __result.length; __i++) __result.data[__i] = __lst.data[__i]; __result; })"

        _ ->
            "/* List.take wrong arity */ 0"


generateListDrop : GenExpr -> List Src.Expr -> String
generateListDrop genExpr args =
    case args of
        [ nExpr, listExpr ] ->
            let
                nStr =
                    genExpr nExpr

                listStr =
                    genExpr listExpr
            in
            "({ int __n = " ++ nStr ++ "; elm_list_t __lst = " ++ listStr ++ "; elm_list_t __result; int __start = __n < __lst.length ? __n : __lst.length; if (__start < 0) __start = 0; __result.length = __lst.length - __start; for (int __i = 0; __i < __result.length; __i++) __result.data[__i] = __lst.data[__start + __i]; __result; })"

        _ ->
            "/* List.drop wrong arity */ 0"


generateListAppend : GenExpr -> List Src.Expr -> String
generateListAppend genExpr args =
    case args of
        [ listAExpr, listBExpr ] ->
            let
                listAStr =
                    genExpr listAExpr

                listBStr =
                    genExpr listBExpr
            in
            "({ elm_list_t __a = " ++ listAStr ++ ", __b = " ++ listBStr ++ "; elm_list_t __result; __result.length = __a.length + __b.length; if (__result.length > ELM_LIST_MAX) __result.length = ELM_LIST_MAX; int __i; for (__i = 0; __i < __a.length && __i < __result.length; __i++) __result.data[__i] = __a.data[__i]; for (int __j = 0; __i < __result.length; __i++, __j++) __result.data[__i] = __b.data[__j]; __result; })"

        _ ->
            "/* List.append wrong arity */ 0"


generateListRepeat : GenExpr -> List Src.Expr -> String
generateListRepeat genExpr args =
    case args of
        [ nExpr, elemExpr ] ->
            let
                nStr =
                    genExpr nExpr

                elemStr =
                    genExpr elemExpr
            in
            "({ int __n = " ++ nStr ++ ", __elem = " ++ elemStr ++ "; elm_list_t __lst; __lst.length = __n > 0 ? (__n > ELM_LIST_MAX ? ELM_LIST_MAX : __n) : 0; for (int __i = 0; __i < __lst.length; __i++) __lst.data[__i] = __elem; __lst; })"

        _ ->
            "/* List.repeat wrong arity */ 0"


generateListIntersperse : GenExpr -> List Src.Expr -> String
generateListIntersperse genExpr args =
    case args of
        [ sepExpr, listExpr ] ->
            let
                sepStr =
                    genExpr sepExpr

                listStr =
                    genExpr listExpr
            in
            "({ int __sep = " ++ sepStr ++ "; elm_list_t __lst = " ++ listStr ++ "; elm_list_t __result; if (__lst.length == 0) { __result.length = 0; } else { __result.length = __lst.length * 2 - 1; if (__result.length > ELM_LIST_MAX) __result.length = ELM_LIST_MAX; int __j = 0; for (int __i = 0; __i < __lst.length && __j < __result.length; __i++) { if (__i > 0 && __j < __result.length) __result.data[__j++] = __sep; if (__j < __result.length) __result.data[__j++] = __lst.data[__i]; } __result.length = __j; } __result; })"

        _ ->
            "/* List.intersperse wrong arity */ 0"
