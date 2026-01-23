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

        Src.At _ (Src.VarQual _ "String" "join") ->
            Just (generateStringJoin genExpr args)

        Src.At _ (Src.VarQual _ "String" "any") ->
            Just (generateStringAny genExpr args)

        Src.At _ (Src.VarQual _ "String" "all") ->
            Just (generateStringAll genExpr args)

        Src.At _ (Src.VarQual _ "String" "foldl") ->
            Just (generateStringFoldl genExpr args)

        Src.At _ (Src.VarQual _ "String" "foldr") ->
            Just (generateStringFoldr genExpr args)

        Src.At _ (Src.VarQual _ "String" "filter") ->
            Just (generateStringFilter genExpr args)

        Src.At _ (Src.VarQual _ "String" "map") ->
            Just (generateStringMap genExpr args)

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

        -- Maybe module
        Src.At _ (Src.VarQual _ "Maybe" "withDefault") ->
            Just (generateMaybeWithDefault genExpr args)

        Src.At _ (Src.VarQual _ "Maybe" "map") ->
            Just (generateMaybeMap genExpr args)

        Src.At _ (Src.VarQual _ "Maybe" "andThen") ->
            Just (generateMaybeAndThen genExpr args)

        Src.At _ (Src.VarQual _ "Maybe" "map2") ->
            Just (generateMaybeMap2 genExpr args)

        Src.At _ (Src.VarQual _ "Maybe" "map3") ->
            Just (generateMaybeMap3 genExpr args)

        Src.At _ (Src.VarQual _ "Maybe" "map4") ->
            Just (generateMaybeMap4 genExpr args)

        Src.At _ (Src.VarQual _ "Maybe" "map5") ->
            Just (generateMaybeMap5 genExpr args)

        -- Result module
        Src.At _ (Src.VarQual _ "Result" "withDefault") ->
            Just (generateResultWithDefault genExpr args)

        Src.At _ (Src.VarQual _ "Result" "toMaybe") ->
            Just (generateResultToMaybe genExpr args)

        Src.At _ (Src.VarQual _ "Result" "fromMaybe") ->
            Just (generateResultFromMaybe genExpr args)

        Src.At _ (Src.VarQual _ "Result" "map") ->
            Just (generateResultMap genExpr args)

        Src.At _ (Src.VarQual _ "Result" "mapError") ->
            Just (generateResultMapError genExpr args)

        Src.At _ (Src.VarQual _ "Result" "andThen") ->
            Just (generateResultAndThen genExpr args)

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

        Src.At _ (Src.VarQual _ "List" "sort") ->
            Just (generateListSort genExpr args)

        Src.At _ (Src.VarQual _ "List" "foldl") ->
            Just (generateListFoldl genExpr args)

        Src.At _ (Src.VarQual _ "List" "foldr") ->
            Just (generateListFoldr genExpr args)

        Src.At _ (Src.VarQual _ "List" "all") ->
            Just (generateListAll genExpr args)

        Src.At _ (Src.VarQual _ "List" "any") ->
            Just (generateListAny genExpr args)

        Src.At _ (Src.VarQual _ "List" "length") ->
            Just (generateListLength genExpr args)

        Src.At _ (Src.VarQual _ "List" "isEmpty") ->
            Just (generateListIsEmpty genExpr args)

        Src.At _ (Src.VarQual _ "List" "singleton") ->
            Just (generateListSingleton genExpr args)

        Src.At _ (Src.VarQual _ "List" "map") ->
            Just (generateListMap genExpr args)

        Src.At _ (Src.VarQual _ "List" "map2") ->
            Just (generateListMap2 genExpr args)

        Src.At _ (Src.VarQual _ "List" "map3") ->
            Just (generateListMap3 genExpr args)

        Src.At _ (Src.VarQual _ "List" "filter") ->
            Just (generateListFilter genExpr args)

        Src.At _ (Src.VarQual _ "List" "partition") ->
            Just (generateListPartition genExpr args)

        Src.At _ (Src.VarQual _ "List" "sortBy") ->
            Just (generateListSortBy genExpr args)

        Src.At _ (Src.VarQual _ "List" "indexedMap") ->
            Just (generateListIndexedMap genExpr args)

        Src.At _ (Src.VarQual _ "List" "concat") ->
            Just (generateListConcat genExpr args)

        Src.At _ (Src.VarQual _ "List" "filterMap") ->
            Just (generateListFilterMap genExpr args)

        Src.At _ (Src.VarQual _ "List" "concatMap") ->
            Just (generateListConcatMap genExpr args)

        Src.At _ (Src.VarQual _ "List" "unzip") ->
            Just (generateListUnzip genExpr args)

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

        -- Trig functions (unqualified)
        Src.At _ (Src.Var _ "sin") ->
            Just (generateSin genExpr args)

        Src.At _ (Src.Var _ "cos") ->
            Just (generateCos genExpr args)

        Src.At _ (Src.Var _ "tan") ->
            Just (generateTan genExpr args)

        Src.At _ (Src.Var _ "asin") ->
            Just (generateAsin genExpr args)

        Src.At _ (Src.Var _ "acos") ->
            Just (generateAcos genExpr args)

        Src.At _ (Src.Var _ "atan") ->
            Just (generateAtan genExpr args)

        Src.At _ (Src.Var _ "atan2") ->
            Just (generateAtan2 genExpr args)

        -- Trig functions (qualified)
        Src.At _ (Src.VarQual _ "Basics" "sin") ->
            Just (generateSin genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "cos") ->
            Just (generateCos genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "tan") ->
            Just (generateTan genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "asin") ->
            Just (generateAsin genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "acos") ->
            Just (generateAcos genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "atan") ->
            Just (generateAtan genExpr args)

        Src.At _ (Src.VarQual _ "Basics" "atan2") ->
            Just (generateAtan2 genExpr args)

        -- Built-in constructors
        Src.At _ (Src.Var Src.CapVar "Just") ->
            Just (generateJustCtor genExpr args)

        Src.At _ (Src.Var _ "Ok") ->
            Just (generateOkCtor genExpr args)

        Src.At _ (Src.Var _ "Err") ->
            Just (generateErrCtor genExpr args)

        -- Not a builtin we handle here
        _ ->
            Nothing


{-| Generate code for builtin calls with pipe argument.
Returns Nothing if not a known builtin.
-}
generateBuiltinCallWithPipeArg : GenExpr -> ExprCtx -> Src.Expr -> List Src.Expr -> String -> Maybe String
generateBuiltinCallWithPipeArg genExpr ctx fn partialArgs pipeArg =
    case fn of
        -- List.map with function and pipe arg (list)
        Src.At _ (Src.VarQual _ "List" "map") ->
            Just (generateListMapPipe genExpr partialArgs pipeArg)

        -- List.filter with predicate and pipe arg (list)
        Src.At _ (Src.VarQual _ "List" "filter") ->
            Just (generateListFilterPipe genExpr partialArgs pipeArg)

        -- List.take with count and pipe arg (list)
        Src.At _ (Src.VarQual _ "List" "take") ->
            Just (generateListTakePipe genExpr partialArgs pipeArg)

        -- List.indexedMap with function and pipe arg (list)
        Src.At _ (Src.VarQual _ "List" "indexedMap") ->
            Just (generateListIndexedMapPipe genExpr partialArgs pipeArg)

        -- List.filterMap with function and pipe arg (list)
        Src.At _ (Src.VarQual _ "List" "filterMap") ->
            Just (generateListFilterMapPipe genExpr partialArgs pipeArg)

        -- List.concatMap with function and pipe arg (list)
        Src.At _ (Src.VarQual _ "List" "concatMap") ->
            Just (generateListConcatMapPipe genExpr partialArgs pipeArg)

        -- List.sortBy with function and pipe arg (list)
        Src.At _ (Src.VarQual _ "List" "sortBy") ->
            Just (generateListSortByPipe genExpr partialArgs pipeArg)

        -- List.partition with function and pipe arg (list)
        Src.At _ (Src.VarQual _ "List" "partition") ->
            Just (generateListPartitionPipe genExpr partialArgs pipeArg)

        -- List.sum with pipe arg (list)
        Src.At _ (Src.VarQual _ "List" "sum") ->
            Just ("({ elm_list_t __lst = " ++ pipeArg ++ "; double __sum = 0; for (int __i = 0; __i < __lst.length; __i++) __sum += __lst.data[__i].d; __sum; })")

        -- List.product with pipe arg (list)
        Src.At _ (Src.VarQual _ "List" "product") ->
            Just ("({ elm_list_t __lst = " ++ pipeArg ++ "; double __prod = 1; for (int __i = 0; __i < __lst.length; __i++) __prod *= __lst.data[__i].d; __prod; })")

        -- List.reverse with pipe arg (list)
        Src.At _ (Src.VarQual _ "List" "reverse") ->
            Just ("({ elm_list_t __lst = " ++ pipeArg ++ "; elm_list_t __rev; __rev.length = __lst.length; for (int __i = 0; __i < __lst.length; __i++) __rev.data[__i] = __lst.data[__lst.length - 1 - __i]; __rev; })")

        -- List.length with pipe arg (list)
        Src.At _ (Src.VarQual _ "List" "length") ->
            Just ("({ elm_list_t __lst = " ++ pipeArg ++ "; __lst.length; })")

        -- List.head with pipe arg (list)
        Src.At _ (Src.VarQual _ "List" "head") ->
            Just ("({ typeof(" ++ pipeArg ++ ") __lst = " ++ pipeArg ++ "; __lst.length > 0 ? ((elm_union_t){TAG_Just, {.ptr = (void*)&__lst.data[0]}}) : ((elm_union_t){TAG_Nothing, {.num = 0}}); })")

        -- List.last with pipe arg (list)
        Src.At _ (Src.VarQual _ "List" "last") ->
            Just ("({ elm_list_t __lst = " ++ pipeArg ++ "; __lst.length > 0 ? ((elm_union_t){TAG_Just, {.num = __lst.data[__lst.length - 1].d}}) : ((elm_union_t){TAG_Nothing, {.num = 0}}); })")

        -- Maybe.map with function and pipe arg (Maybe)
        Src.At _ (Src.VarQual _ "Maybe" "map") ->
            Just (generateMaybeMapPipe genExpr partialArgs pipeArg)

        -- Maybe.andThen with function and pipe arg (Maybe)
        Src.At _ (Src.VarQual _ "Maybe" "andThen") ->
            Just (generateMaybeAndThenPipe genExpr partialArgs pipeArg)

        _ ->
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


generateStringJoin : GenExpr -> List Src.Expr -> String
generateStringJoin genExpr args =
    case args of
        [ sepExpr, listExpr ] ->
            let
                sepStr =
                    genExpr sepExpr

                listStr =
                    genExpr listExpr
            in
            "({ static char __join_buf[1024]; elm_list_t __lst = " ++ listStr ++ "; const char *__sep = " ++ sepStr ++ "; int __pos = 0; for (int __i = 0; __i < __lst.length && __pos < 1023; __i++) { if (__i > 0) { int __seplen = 0; while (__sep[__seplen]) __seplen++; for (int __j = 0; __j < __seplen && __pos < 1023; __j++) __join_buf[__pos++] = __sep[__j]; } const char *__s = (const char *)(long)__lst.data[__i]; while (*__s && __pos < 1023) __join_buf[__pos++] = *__s++; } __join_buf[__pos] = 0; __join_buf; })"

        _ ->
            "/* String.join wrong arity */ 0"


generateStringAny : GenExpr -> List Src.Expr -> String
generateStringAny genExpr args =
    case args of
        [ predExpr, strExpr ] ->
            let
                strStr =
                    genExpr strExpr

                predAppStr =
                    case predExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __str[__i]; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.VarQual _ "Char" fnName) ->
                            Shared.generateCharPredCall fnName "__str[__i]"

                        _ ->
                            genExpr predExpr ++ "(__str[__i])"
            in
            "({ const char *__str = " ++ strStr ++ "; int __result = 0; for (int __i = 0; __str[__i]; __i++) { if (" ++ predAppStr ++ ") { __result = 1; break; } } __result; })"

        _ ->
            "/* String.any wrong arity */ 0"


generateStringAll : GenExpr -> List Src.Expr -> String
generateStringAll genExpr args =
    case args of
        [ predExpr, strExpr ] ->
            let
                strStr =
                    genExpr strExpr

                predAppStr =
                    case predExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __str[__i]; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.VarQual _ "Char" fnName) ->
                            Shared.generateCharPredCall fnName "__str[__i]"

                        _ ->
                            genExpr predExpr ++ "(__str[__i])"
            in
            "({ const char *__str = " ++ strStr ++ "; int __result = 1; for (int __i = 0; __str[__i]; __i++) { if (!(" ++ predAppStr ++ ")) { __result = 0; break; } } __result; })"

        _ ->
            "/* String.all wrong arity */ 0"


generateStringFoldl : GenExpr -> List Src.Expr -> String
generateStringFoldl genExpr args =
    case args of
        [ fnExpr, initExpr, strExpr ] ->
            let
                strStr =
                    genExpr strExpr

                initStr =
                    genExpr initExpr

                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar charName), Src.At _ (Src.PVar accName) ] lambdaBody) ->
                            "({ double elm_" ++ charName ++ " = __str[__i]; double elm_" ++ accName ++ " = __acc; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__str[__i], __acc)"
            in
            "({ const char *__str = " ++ strStr ++ "; int __acc = " ++ initStr ++ "; for (int __i = 0; __str[__i]; __i++) { __acc = " ++ fnAppStr ++ "; } __acc; })"

        _ ->
            "/* String.foldl wrong arity */ 0"


generateStringFoldr : GenExpr -> List Src.Expr -> String
generateStringFoldr genExpr args =
    case args of
        [ fnExpr, initExpr, strExpr ] ->
            let
                strStr =
                    genExpr strExpr

                initStr =
                    genExpr initExpr

                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar charName), Src.At _ (Src.PVar accName) ] lambdaBody) ->
                            "({ double elm_" ++ charName ++ " = __str[__i]; double elm_" ++ accName ++ " = __acc; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__str[__i], __acc)"
            in
            "({ const char *__str = " ++ strStr ++ "; int __len = 0; while (__str[__len]) __len++; int __acc = " ++ initStr ++ "; for (int __i = __len - 1; __i >= 0; __i--) { __acc = " ++ fnAppStr ++ "; } __acc; })"

        _ ->
            "/* String.foldr wrong arity */ 0"


generateStringFilter : GenExpr -> List Src.Expr -> String
generateStringFilter genExpr args =
    case args of
        [ predExpr, strExpr ] ->
            let
                strStr =
                    genExpr strExpr

                predAppStr =
                    case predExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __str[__i]; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.VarQual _ "Char" fnName) ->
                            Shared.generateCharPredCall fnName "__str[__i]"

                        _ ->
                            genExpr predExpr ++ "(__str[__i])"
            in
            "({ static char __filter_buf[256]; const char *__str = " ++ strStr ++ "; int __j = 0; for (int __i = 0; __str[__i] && __j < 255; __i++) { if (" ++ predAppStr ++ ") { __filter_buf[__j++] = __str[__i]; } } __filter_buf[__j] = 0; __filter_buf; })"

        _ ->
            "/* String.filter wrong arity */ 0"


generateStringMap : GenExpr -> List Src.Expr -> String
generateStringMap genExpr args =
    case args of
        [ fnExpr, strExpr ] ->
            let
                strStr =
                    genExpr strExpr

                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __str[__i]; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.VarQual _ "Char" "toUpper") ->
                            "((__str[__i] >= 'a' && __str[__i] <= 'z') ? __str[__i] - 32 : __str[__i])"

                        Src.At _ (Src.VarQual _ "Char" "toLower") ->
                            "((__str[__i] >= 'A' && __str[__i] <= 'Z') ? __str[__i] + 32 : __str[__i])"

                        _ ->
                            genExpr fnExpr ++ "(__str[__i])"
            in
            "({ static char __map_buf[256]; const char *__str = " ++ strStr ++ "; int __i; for (__i = 0; __str[__i] && __i < 255; __i++) __map_buf[__i] = " ++ fnAppStr ++ "; __map_buf[__i] = 0; __map_buf; })"

        _ ->
            "/* String.map wrong arity */ 0"



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


generateMaybeMap : GenExpr -> List Src.Expr -> String
generateMaybeMap genExpr args =
    case args of
        [ fnExpr, maybeVal ] ->
            let
                maybeStr = genExpr maybeVal
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar varName) ] body) ->
                            "({ double elm_" ++ varName ++ " = __maybe_val.data.num; " ++ genExpr body ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] body) ->
                            "({ elm_union_t __inner = __maybe_val.data; double elm_" ++ innerName ++ " = __inner.data.num; " ++ genExpr body ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] body) ->
                            "({ elm_union_t __inner = __maybe_val.data; double elm_" ++ innerName ++ " = __inner.data.num; " ++ genExpr body ++ "; })"

                        Src.At _ (Src.Accessor fieldName) ->
                            "((struct { const char *" ++ fieldName ++ "; } *)__maybe_val.data.ptr)->" ++ fieldName

                        _ ->
                            genExpr fnExpr ++ "(__maybe_val.data)"
            in
            "({ elm_union_t __maybe_val = " ++ maybeStr ++ "; elm_union_t __result; if (__maybe_val.tag == TAG_Just) { elm_union_t __inner = {0}; __inner.data.num = " ++ fnAppStr ++ "; __result = elm_Just(__inner); } else { __result = elm_Nothing(); } __result; })"

        _ ->
            "/* Maybe.map wrong arity */ 0"


generateMaybeAndThen : GenExpr -> List Src.Expr -> String
generateMaybeAndThen genExpr args =
    case args of
        [ fnExpr, maybeVal ] ->
            let
                maybeStr = genExpr maybeVal
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar varName) ] body) ->
                            "({ double elm_" ++ varName ++ " = __maybe_val.data.child->data.num; " ++ genExpr body ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PTuple (Src.At _ first) (Src.At _ second) rest) ] body) ->
                            let
                                extractBinding idx pat =
                                    case pat of
                                        Src.PVar vn -> "double elm_" ++ vn ++ " = __tuple._" ++ String.fromInt idx ++ ".d;"
                                        Src.PAnything -> ""
                                        _ -> "/* unsupported nested pattern */"

                                firstBinding = extractBinding 0 first
                                secondBinding = extractBinding 1 second
                                restBindings = rest |> List.indexedMap (\i (Src.At _ p) -> extractBinding (i + 2) p)

                                allBindings = [ firstBinding, secondBinding ] ++ restBindings
                                    |> List.filter (\s -> s /= "")
                                    |> String.join " "

                                tupleType = if List.isEmpty rest then "elm_tuple2_t" else "elm_tuple3_t"
                            in
                            "({ " ++ tupleType ++ " __tuple = *(" ++ tupleType ++ "*)&(__maybe_val.data.child->data.num); " ++ allBindings ++ " " ++ genExpr body ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PRecord fieldNames) ] body) ->
                            let
                                bindings = fieldNames
                                    |> List.map (\(Src.At _ fld) -> "double elm_" ++ fld ++ " = __rec." ++ fld ++ ";")
                                    |> String.join " "
                            in
                            "({ typeof(__maybe_val.data.child->data.num) __rec = __maybe_val.data.child->data.num; " ++ bindings ++ " " ++ genExpr body ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__maybe_val.data)"
            in
            "({ elm_union_t __maybe_val = " ++ maybeStr ++ "; __maybe_val.tag == TAG_Just ? " ++ fnAppStr ++ " : ((elm_union_t){TAG_Nothing, 0}); })"

        _ ->
            "/* Maybe.andThen wrong arity */ 0"


generateMaybeMap2 : GenExpr -> List Src.Expr -> String
generateMaybeMap2 genExpr args =
    case args of
        [ fnExpr, maybeA, maybeB ] ->
            let
                maybeAStr = genExpr maybeA
                maybeBStr = genExpr maybeB
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PVar pname2) ] lambdaBody) ->
                            "({ double elm_" ++ pname1 ++ " = __maybe_a.data; double elm_" ++ pname2 ++ " = __maybe_b.data; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__maybe_a.data, __maybe_b.data)"
            in
            "({ elm_union_t __maybe_a = " ++ maybeAStr ++ "; elm_union_t __maybe_b = " ++ maybeBStr ++ "; (__maybe_a.tag == TAG_Just && __maybe_b.tag == TAG_Just) ? ((elm_union_t){TAG_Just, " ++ fnAppStr ++ "}) : ((elm_union_t){TAG_Nothing, 0}); })"

        _ ->
            "/* Maybe.map2 wrong arity */ 0"


generateMaybeMap3 : GenExpr -> List Src.Expr -> String
generateMaybeMap3 genExpr args =
    case args of
        [ fnExpr, maybeA, maybeB, maybeC ] ->
            let
                maybeAStr = genExpr maybeA
                maybeBStr = genExpr maybeB
                maybeCStr = genExpr maybeC
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar p1), Src.At _ (Src.PVar p2), Src.At _ (Src.PVar p3) ] lambdaBody) ->
                            "({ double elm_" ++ p1 ++ " = __maybe_a.data; double elm_" ++ p2 ++ " = __maybe_b.data; double elm_" ++ p3 ++ " = __maybe_c.data; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__maybe_a.data, __maybe_b.data, __maybe_c.data)"
            in
            "({ elm_union_t __maybe_a = " ++ maybeAStr ++ "; elm_union_t __maybe_b = " ++ maybeBStr ++ "; elm_union_t __maybe_c = " ++ maybeCStr ++ "; (__maybe_a.tag == TAG_Just && __maybe_b.tag == TAG_Just && __maybe_c.tag == TAG_Just) ? ((elm_union_t){TAG_Just, " ++ fnAppStr ++ "}) : ((elm_union_t){TAG_Nothing, 0}); })"

        _ ->
            "/* Maybe.map3 wrong arity */ 0"


generateMaybeMap4 : GenExpr -> List Src.Expr -> String
generateMaybeMap4 genExpr args =
    case args of
        [ fnExpr, maybeA, maybeB, maybeC, maybeD ] ->
            let
                maybeAStr = genExpr maybeA
                maybeBStr = genExpr maybeB
                maybeCStr = genExpr maybeC
                maybeDStr = genExpr maybeD
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar p1), Src.At _ (Src.PVar p2), Src.At _ (Src.PVar p3), Src.At _ (Src.PVar p4) ] lambdaBody) ->
                            "({ double elm_" ++ p1 ++ " = __maybe_a.data; double elm_" ++ p2 ++ " = __maybe_b.data; double elm_" ++ p3 ++ " = __maybe_c.data; double elm_" ++ p4 ++ " = __maybe_d.data; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__maybe_a.data, __maybe_b.data, __maybe_c.data, __maybe_d.data)"
            in
            "({ elm_union_t __maybe_a = " ++ maybeAStr ++ "; elm_union_t __maybe_b = " ++ maybeBStr ++ "; elm_union_t __maybe_c = " ++ maybeCStr ++ "; elm_union_t __maybe_d = " ++ maybeDStr ++ "; (__maybe_a.tag == TAG_Just && __maybe_b.tag == TAG_Just && __maybe_c.tag == TAG_Just && __maybe_d.tag == TAG_Just) ? ((elm_union_t){TAG_Just, " ++ fnAppStr ++ "}) : ((elm_union_t){TAG_Nothing, 0}); })"

        _ ->
            "/* Maybe.map4 wrong arity */ 0"


generateMaybeMap5 : GenExpr -> List Src.Expr -> String
generateMaybeMap5 genExpr args =
    case args of
        [ fnExpr, maybeA, maybeB, maybeC, maybeD, maybeE ] ->
            let
                maybeAStr = genExpr maybeA
                maybeBStr = genExpr maybeB
                maybeCStr = genExpr maybeC
                maybeDStr = genExpr maybeD
                maybeEStr = genExpr maybeE
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar p1), Src.At _ (Src.PVar p2), Src.At _ (Src.PVar p3), Src.At _ (Src.PVar p4), Src.At _ (Src.PVar p5) ] lambdaBody) ->
                            "({ double elm_" ++ p1 ++ " = __maybe_a.data; double elm_" ++ p2 ++ " = __maybe_b.data; double elm_" ++ p3 ++ " = __maybe_c.data; double elm_" ++ p4 ++ " = __maybe_d.data; double elm_" ++ p5 ++ " = __maybe_e.data; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__maybe_a.data, __maybe_b.data, __maybe_c.data, __maybe_d.data, __maybe_e.data)"
            in
            "({ elm_union_t __maybe_a = " ++ maybeAStr ++ "; elm_union_t __maybe_b = " ++ maybeBStr ++ "; elm_union_t __maybe_c = " ++ maybeCStr ++ "; elm_union_t __maybe_d = " ++ maybeDStr ++ "; elm_union_t __maybe_e = " ++ maybeEStr ++ "; (__maybe_a.tag == TAG_Just && __maybe_b.tag == TAG_Just && __maybe_c.tag == TAG_Just && __maybe_d.tag == TAG_Just && __maybe_e.tag == TAG_Just) ? ((elm_union_t){TAG_Just, " ++ fnAppStr ++ "}) : ((elm_union_t){TAG_Nothing, 0}); })"

        _ ->
            "/* Maybe.map5 wrong arity */ 0"



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


generateResultMap : GenExpr -> List Src.Expr -> String
generateResultMap genExpr args =
    case args of
        [ fnExpr, resultExpr ] ->
            let
                resultStr = genExpr resultExpr
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __result_val.data; " ++ genExpr lambdaBody ++ "; })"
                        _ ->
                            genExpr fnExpr ++ "(__result_val.data)"
            in
            "({ elm_union_t __result_val = " ++ resultStr ++ "; __result_val.tag == TAG_Ok ? ((elm_union_t){TAG_Ok, " ++ fnAppStr ++ "}) : __result_val; })"

        _ ->
            "/* Result.map wrong arity */ 0"


generateResultMapError : GenExpr -> List Src.Expr -> String
generateResultMapError genExpr args =
    case args of
        [ fnExpr, resultExpr ] ->
            let
                resultStr = genExpr resultExpr
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __result_val.data; " ++ genExpr lambdaBody ++ "; })"
                        _ ->
                            genExpr fnExpr ++ "(__result_val.data)"
            in
            "({ elm_union_t __result_val = " ++ resultStr ++ "; __result_val.tag == TAG_Err ? ((elm_union_t){TAG_Err, " ++ fnAppStr ++ "}) : __result_val; })"

        _ ->
            "/* Result.mapError wrong arity */ 0"


generateResultAndThen : GenExpr -> List Src.Expr -> String
generateResultAndThen genExpr args =
    case args of
        [ fnExpr, resultExpr ] ->
            let
                resultStr = genExpr resultExpr
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __result_val.data; " ++ genExpr lambdaBody ++ "; })"
                        _ ->
                            genExpr fnExpr ++ "(__result_val.data)"
            in
            "({ elm_union_t __result_val = " ++ resultStr ++ "; __result_val.tag == TAG_Ok ? " ++ fnAppStr ++ " : __result_val; })"

        _ ->
            "/* Result.andThen wrong arity */ 0"



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


generateListSort : GenExpr -> List Src.Expr -> String
generateListSort genExpr args =
    case args of
        [ listExpr ] ->
            let
                listStr =
                    genExpr listExpr
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; for (int __i = 1; __i < __lst.length; __i++) { int __key = __lst.data[__i], __j = __i - 1; while (__j >= 0 && __lst.data[__j] > __key) { __lst.data[__j + 1] = __lst.data[__j]; __j--; } __lst.data[__j + 1] = __key; } __lst; })"

        _ ->
            "/* List.sort wrong arity */ 0"


generateListFoldl : GenExpr -> List Src.Expr -> String
generateListFoldl genExpr args =
    case args of
        [ fnExpr, initExpr, listExpr ] ->
            let
                initStr =
                    genExpr initExpr

                listStr =
                    genExpr listExpr

                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PVar pname2) ] lambdaBody) ->
                            "({ double elm_" ++ pname1 ++ " = __lst.data[__i], elm_" ++ pname2 ++ " = __acc; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__lst.data[__i], __acc)"
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; int __acc = " ++ initStr ++ "; for (int __i = 0; __i < __lst.length; __i++) __acc = " ++ fnAppStr ++ "; __acc; })"

        _ ->
            "/* List.foldl wrong arity */ 0"


generateListFoldr : GenExpr -> List Src.Expr -> String
generateListFoldr genExpr args =
    case args of
        [ fnExpr, initExpr, listExpr ] ->
            let
                initStr =
                    genExpr initExpr

                listStr =
                    genExpr listExpr

                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PVar pname2) ] lambdaBody) ->
                            "({ double elm_" ++ pname1 ++ " = __lst.data[__i], elm_" ++ pname2 ++ " = __acc; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__lst.data[__i], __acc)"
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; int __acc = " ++ initStr ++ "; for (int __i = __lst.length - 1; __i >= 0; __i--) __acc = " ++ fnAppStr ++ "; __acc; })"

        _ ->
            "/* List.foldr wrong arity */ 0"


generateListAll : GenExpr -> List Src.Expr -> String
generateListAll genExpr args =
    case args of
        [ fnExpr, listExpr ] ->
            let
                listStr =
                    genExpr listExpr

                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __lst.data[__i].d; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PTuple (Src.At _ (Src.PVar pname1)) (Src.At _ (Src.PVar pname2)) []) ] lambdaBody) ->
                            "({ elm_tuple2_t __elem = __lst.data[__i].t2; double elm_" ++ pname1 ++ " = __elem._0; double elm_" ++ pname2 ++ " = __elem._1; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__lst.data[__i].d)"
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; int __result = 1; for (int __i = 0; __i < __lst.length && __result; __i++) if (!(" ++ fnAppStr ++ ")) __result = 0; __result; })"

        _ ->
            "/* List.all wrong arity */ 0"


generateListAny : GenExpr -> List Src.Expr -> String
generateListAny genExpr args =
    case args of
        [ fnExpr, listExpr ] ->
            let
                listStr =
                    genExpr listExpr

                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __lst.data[__i].d; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PTuple (Src.At _ (Src.PVar pname1)) (Src.At _ (Src.PVar pname2)) []) ] lambdaBody) ->
                            "({ elm_tuple2_t __elem = __lst.data[__i].t2; elm_elem_t elm_" ++ pname1 ++ " = __elem._0; elm_elem_t elm_" ++ pname2 ++ " = __elem._1; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__lst.data[__i].d)"
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; int __result = 0; for (int __i = 0; __i < __lst.length && !__result; __i++) if (" ++ fnAppStr ++ ") __result = 1; __result; })"

        _ ->
            "/* List.any wrong arity */ 0"



-- TRIG FUNCTIONS


generateSin : GenExpr -> List Src.Expr -> String
generateSin genExpr args =
    case args of
        [ x ] ->
            "sin(" ++ genExpr x ++ ")"

        _ ->
            "/* sin wrong arity */ 0"


generateCos : GenExpr -> List Src.Expr -> String
generateCos genExpr args =
    case args of
        [ x ] ->
            "cos(" ++ genExpr x ++ ")"

        _ ->
            "/* cos wrong arity */ 0"


generateTan : GenExpr -> List Src.Expr -> String
generateTan genExpr args =
    case args of
        [ x ] ->
            "tan(" ++ genExpr x ++ ")"

        _ ->
            "/* tan wrong arity */ 0"


generateAsin : GenExpr -> List Src.Expr -> String
generateAsin genExpr args =
    case args of
        [ x ] ->
            "asin(" ++ genExpr x ++ ")"

        _ ->
            "/* asin wrong arity */ 0"


generateAcos : GenExpr -> List Src.Expr -> String
generateAcos genExpr args =
    case args of
        [ x ] ->
            "acos(" ++ genExpr x ++ ")"

        _ ->
            "/* acos wrong arity */ 0"


generateAtan : GenExpr -> List Src.Expr -> String
generateAtan genExpr args =
    case args of
        [ x ] ->
            "atan(" ++ genExpr x ++ ")"

        _ ->
            "/* atan wrong arity */ 0"


generateAtan2 : GenExpr -> List Src.Expr -> String
generateAtan2 genExpr args =
    case args of
        [ y, x ] ->
            "atan2(" ++ genExpr y ++ ", " ++ genExpr x ++ ")"

        _ ->
            "/* atan2 wrong arity */ 0"



-- MORE LIST FUNCTIONS


generateListLength : GenExpr -> List Src.Expr -> String
generateListLength genExpr args =
    case args of
        [ listExpr ] ->
            let
                listStr = genExpr listExpr
            in
            "(" ++ listStr ++ ").length"

        _ ->
            "/* List.length wrong arity */ 0"


generateListIsEmpty : GenExpr -> List Src.Expr -> String
generateListIsEmpty genExpr args =
    case args of
        [ listExpr ] ->
            let
                listStr = genExpr listExpr
            in
            "((" ++ listStr ++ ").length == 0)"

        _ ->
            "/* List.isEmpty wrong arity */ 0"


generateListSingleton : GenExpr -> List Src.Expr -> String
generateListSingleton genExpr args =
    case args of
        [ elemExpr ] ->
            let
                elemStr = genExpr elemExpr
                isUnionExpr =
                    case elemExpr of
                        Src.At _ (Src.Var Src.CapVar _) -> True
                        Src.At _ (Src.VarQual Src.CapVar _ _) -> True
                        Src.At _ (Src.Call (Src.At _ (Src.Var Src.CapVar _)) _) -> True
                        Src.At _ (Src.Call (Src.At _ (Src.VarQual Src.CapVar _ _)) _) -> True
                        Src.At _ (Src.Var Src.LowVar name) ->
                            String.contains "Expr" name ||
                            String.contains "expr" name ||
                            String.contains "Pat" name ||
                            String.contains "pat" name ||
                            String.contains "Type" name
                        _ -> False
                isUnion = isUnionExpr ||
                          String.startsWith "((elm_union_t)" elemStr ||
                          String.contains "elm_union_t" elemStr
                wrapElem =
                    if isUnion then
                        "{.u = " ++ elemStr ++ "}"
                    else if String.startsWith "\"" elemStr then
                        "{.str = " ++ elemStr ++ "}"
                    else
                        elemStr
            in
            "((elm_list_t){ .length = 1, .data = { " ++ wrapElem ++ " } })"

        _ ->
            "/* List.singleton wrong arity */ 0"


generateListMap : GenExpr -> List Src.Expr -> String
generateListMap genExpr args =
    case args of
        [ fnExpr, listExpr ] ->
            let
                listStr = genExpr listExpr
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __lst.data[__i].d; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ double elm_" ++ innerName ++ " = ((elm_union_t)__lst.data[__i].u).data.num; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ double elm_" ++ innerName ++ " = ((elm_union_t)__lst.data[__i].u).data.num; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PTuple (Src.At _ (Src.PVar pname1)) (Src.At _ (Src.PVar pname2)) []) ] lambdaBody) ->
                            "({ elm_tuple2_t __elem = __lst.data[__i].t2; double elm_" ++ pname1 ++ " = __elem._0; double elm_" ++ pname2 ++ " = __elem._1; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Accessor fieldName) ->
                            "(__lst.data[__i]." ++ fieldName ++ ")"

                        _ ->
                            genExpr fnExpr ++ "(__lst.data[__i].d)"
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; elm_list_t __result; __result.length = __lst.length; for (int __i = 0; __i < __lst.length; __i++) __result.data[__i].d = " ++ fnAppStr ++ "; __result; })"

        _ ->
            "/* List.map wrong arity */ 0"


generateListMap2 : GenExpr -> List Src.Expr -> String
generateListMap2 genExpr args =
    case args of
        [ fnExpr, listAExpr, listBExpr ] ->
            let
                listAStr = genExpr listAExpr
                listBStr = genExpr listBExpr
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PVar pname2) ] lambdaBody) ->
                            "({ double elm_" ++ pname1 ++ " = __lstA.data[__i], elm_" ++ pname2 ++ " = __lstB.data[__i]; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar pname1) ]), Src.At _ (Src.PVar pname2) ] lambdaBody) ->
                            "({ elm_union_t __elem1 = (elm_union_t)__lstA.data[__i].d; typeof(__elem1.data) elm_" ++ pname1 ++ " = __elem1.data; typeof(__lstB.data[__i].d) elm_" ++ pname2 ++ " = __lstB.data[__i].d; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar pname1) ]), Src.At _ (Src.PVar pname2) ] lambdaBody) ->
                            "({ elm_union_t __elem1 = (elm_union_t)__lstA.data[__i].d; typeof(__elem1.data) elm_" ++ pname1 ++ " = __elem1.data; typeof(__lstB.data[__i].d) elm_" ++ pname2 ++ " = __lstB.data[__i].d; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__lstA.data[__i], __lstB.data[__i])"
            in
            "({ elm_list_t __lstA = " ++ listAStr ++ ", __lstB = " ++ listBStr ++ "; elm_list_t __result; __result.length = __lstA.length < __lstB.length ? __lstA.length : __lstB.length; for (int __i = 0; __i < __result.length; __i++) __result.data[__i] = " ++ fnAppStr ++ "; __result; })"

        _ ->
            "/* List.map2 wrong arity */ 0"


generateListMap3 : GenExpr -> List Src.Expr -> String
generateListMap3 genExpr args =
    case args of
        [ fnExpr, listAExpr, listBExpr, listCExpr ] ->
            let
                listAStr = genExpr listAExpr
                listBStr = genExpr listBExpr
                listCStr = genExpr listCExpr
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PVar pname2), Src.At _ (Src.PVar pname3) ] lambdaBody) ->
                            "({ double elm_" ++ pname1 ++ " = __lstA.data[__i], elm_" ++ pname2 ++ " = __lstB.data[__i], elm_" ++ pname3 ++ " = __lstC.data[__i]; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__lstA.data[__i], __lstB.data[__i], __lstC.data[__i])"
            in
            "({ elm_list_t __lstA = " ++ listAStr ++ ", __lstB = " ++ listBStr ++ ", __lstC = " ++ listCStr ++ "; elm_list_t __result; int __minLen = __lstA.length; if (__lstB.length < __minLen) __minLen = __lstB.length; if (__lstC.length < __minLen) __minLen = __lstC.length; __result.length = __minLen; for (int __i = 0; __i < __result.length; __i++) __result.data[__i] = " ++ fnAppStr ++ "; __result; })"

        _ ->
            "/* List.map3 wrong arity */ 0"


generateListFilter : GenExpr -> List Src.Expr -> String
generateListFilter genExpr args =
    case args of
        [ fnExpr, listExpr ] ->
            let
                listStr = genExpr listExpr
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ typeof(__lst.data[0]) elm_" ++ pname ++ " = __lst.data[__i]; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Binops [ ( Src.At _ (Src.Var _ "not"), Src.At _ "<<" ) ] (Src.At _ (Src.VarQual _ "String" "isEmpty"))) ->
                            "(*(const char *)(long)__lst.data[__i] != '\\0')"

                        Src.At _ (Src.Binops [ ( Src.At _ (Src.Var _ "not"), Src.At _ "<<" ) ] innerFn) ->
                            "!(" ++ genExpr innerFn ++ "((const char *)(long)__lst.data[__i]))"

                        _ ->
                            genExpr fnExpr ++ "(__lst.data[__i])"
            in
            "({ typeof(" ++ listStr ++ ") __lst = " ++ listStr ++ "; typeof(__lst) __result; __result.length = 0; for (int __i = 0; __i < __lst.length; __i++) if (" ++ fnAppStr ++ ") __result.data[__result.length++] = __lst.data[__i]; __result; })"

        _ ->
            "/* List.filter wrong arity */ 0"


generateListPartition : GenExpr -> List Src.Expr -> String
generateListPartition genExpr args =
    case args of
        [ fnExpr, listExpr ] ->
            let
                listStr = genExpr listExpr
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __lst.data[__i]; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__lst.data[__i])"
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; struct { elm_list_t _0; elm_list_t _1; } __result; __result._0.length = 0; __result._1.length = 0; for (int __i = 0; __i < __lst.length; __i++) { if (" ++ fnAppStr ++ ") __result._0.data[__result._0.length++] = __lst.data[__i]; else __result._1.data[__result._1.length++] = __lst.data[__i]; } __result; })"

        _ ->
            "/* List.partition wrong arity */ 0"


generateListSortBy : GenExpr -> List Src.Expr -> String
generateListSortBy genExpr args =
    case args of
        [ fnExpr, listExpr ] ->
            let
                listStr = genExpr listExpr
                fnAppStr elemVar =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = " ++ elemVar ++ "; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PTuple (Src.At _ first) (Src.At _ second) rest) ] lambdaBody) ->
                            let
                                elemType = if List.isEmpty rest then "elm_tuple2_t" else "elm_tuple3_t"
                                bindOne idx pat =
                                    case pat of
                                        Src.PVar vn ->
                                            "double elm_" ++ vn ++ " = __sort_telem._" ++ String.fromInt idx ++ ".d;"
                                        Src.PAnything -> ""
                                        _ -> ""
                                bindings = [ bindOne 0 first, bindOne 1 second ]
                                    ++ (rest |> List.indexedMap (\i (Src.At _ p) -> bindOne (i + 2) p))
                                    |> List.filter (\s -> s /= "")
                                    |> String.join " "
                            in
                            "({ " ++ elemType ++ " __sort_telem = *(" ++ elemType ++ "*)&(" ++ elemVar ++ "); " ++ bindings ++ " " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ elm_union_t __sort_inner = (elm_union_t)" ++ elemVar ++ "; double elm_" ++ innerName ++ " = __sort_inner.data.num; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ elm_union_t __sort_inner = (elm_union_t)" ++ elemVar ++ "; double elm_" ++ innerName ++ " = __sort_inner.data.num; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(" ++ elemVar ++ ")"
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; elm_list_t __sorted; __sorted.length = __lst.length; for (int __i = 0; __i < __lst.length; __i++) __sorted.data[__i] = __lst.data[__i]; for (int __i = 1; __i < __sorted.length; __i++) { elm_data_t __key = __sorted.data[__i]; typeof(" ++ fnAppStr "__sorted.data[__i].d" ++ ") __key_val = " ++ fnAppStr "__key.d" ++ "; int __j = __i - 1; while (__j >= 0 && strcmp(" ++ fnAppStr "__sorted.data[__j].d" ++ ", __key_val) > 0) { __sorted.data[__j + 1] = __sorted.data[__j]; __j--; } __sorted.data[__j + 1] = __key; } __sorted; })"

        _ ->
            "/* List.sortBy wrong arity */ 0"


generateListIndexedMap : GenExpr -> List Src.Expr -> String
generateListIndexedMap genExpr args =
    case args of
        [ fnExpr, listExpr ] ->
            let
                listStr = genExpr listExpr
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PVar pname2) ] lambdaBody) ->
                            "({ double elm_" ++ pname1 ++ " = __i, elm_" ++ pname2 ++ " = __lst.data[__i]; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ double elm_" ++ pname1 ++ " = __i; double elm_" ++ innerName ++ " = ((elm_union_t)__lst.data[__i]).data; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ double elm_" ++ pname1 ++ " = __i; double elm_" ++ innerName ++ " = ((elm_union_t)__lst.data[__i]).data; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PTuple (Src.At _ (Src.PVar pname2)) (Src.At _ (Src.PVar pname3)) []) ] lambdaBody) ->
                            "({ double elm_" ++ pname1 ++ " = __i; elm_tuple2_t __elem = __lst.data[__i]; double elm_" ++ pname2 ++ " = __elem._0; double elm_" ++ pname3 ++ " = __elem._1; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PTuple (Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ])) (Src.At _ (Src.PVar pname3)) []) ] lambdaBody) ->
                            "({ double elm_" ++ pname1 ++ " = __i; elm_tuple2_t __elem = __lst.data[__i]; elm_union_t __loc = __elem._0; double elm_" ++ innerName ++ " = __loc.data; double elm_" ++ pname3 ++ " = __elem._1; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ Src.PAnything, Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ double elm_" ++ innerName ++ " = ((elm_union_t)__lst.data[__i]).data; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ Src.PAnything, Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ double elm_" ++ innerName ++ " = ((elm_union_t)__lst.data[__i]).data; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__i, __lst.data[__i])"
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; elm_list_t __result; __result.length = __lst.length; for (int __i = 0; __i < __lst.length; __i++) __result.data[__i] = " ++ fnAppStr ++ "; __result; })"

        _ ->
            "/* List.indexedMap wrong arity */ 0"


generateListConcat : GenExpr -> List Src.Expr -> String
generateListConcat _ args =
    case args of
        [ _ ] ->
            "/* List.concat not fully supported */ ((elm_list_t){ .length = 0 })"

        _ ->
            "/* List.concat wrong arity */ 0"


generateListFilterMap : GenExpr -> List Src.Expr -> String
generateListFilterMap genExpr args =
    case args of
        [ fnExpr, listExpr ] ->
            let
                listStr = genExpr listExpr
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __lst.data[__i]; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ elm_union_t __elem = __lst.data[__i]; double elm_" ++ innerName ++ " = __elem.data; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ elm_union_t __elem = __lst.data[__i]; double elm_" ++ innerName ++ " = __elem.data; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__lst.data[__i])"
            in
            "({ elm_list_t __lst = " ++ listStr ++ "; elm_list_t __result; __result.length = 0; for (int __i = 0; __i < __lst.length; __i++) { elm_union_t __maybe = " ++ fnAppStr ++ "; if (__maybe.tag == TAG_Just) __result.data[__result.length++] = __maybe.data; } __result; })"

        _ ->
            "/* List.filterMap wrong arity */ 0"


generateListConcatMap : GenExpr -> List Src.Expr -> String
generateListConcatMap genExpr args =
    case args of
        [ fnExpr, listExpr ] ->
            let
                listStr = genExpr listExpr
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __src.data[__i]; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ elm_union_t __elem = __src.data[__i]; double elm_" ++ innerName ++ " = __elem.data; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ elm_union_t __elem = __src.data[__i]; double elm_" ++ innerName ++ " = __elem.data; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__src.data[__i])"
            in
            "({ elm_list_t __src = " ++ listStr ++ "; elm_list_t __result; __result.length = 0; for (int __i = 0; __i < __src.length; __i++) { elm_list_t __sub = " ++ fnAppStr ++ "; for (int __j = 0; __j < __sub.length; __j++) __result.data[__result.length++] = __sub.data[__j]; } __result; })"

        _ ->
            "/* List.concatMap wrong arity */ 0"


generateListUnzip : GenExpr -> List Src.Expr -> String
generateListUnzip _ args =
    case args of
        [ _ ] ->
            "/* List.unzip not fully supported */ 0"

        _ ->
            "/* List.unzip wrong arity */ 0"



-- PIPE ARGUMENT HANDLERS
-- These handle the case where a builtin function is used with |> pipe operator


generateListMapPipe : GenExpr -> List Src.Expr -> String -> String
generateListMapPipe genExpr partialArgs pipeArg =
    case partialArgs of
        [ fnExpr ] ->
            let
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __lst.data[__i].d; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Accessor fieldName) ->
                            "(__lst.data[__i]." ++ fieldName ++ ")"

                        _ ->
                            genExpr fnExpr ++ "(__lst.data[__i].d)"
            in
            "({ elm_list_t __lst = " ++ pipeArg ++ "; elm_list_t __result; __result.length = __lst.length; for (int __i = 0; __i < __lst.length; __i++) __result.data[__i].d = " ++ fnAppStr ++ "; __result; })"

        _ ->
            "/* List.map partial wrong arity */ 0"


generateListFilterPipe : GenExpr -> List Src.Expr -> String -> String
generateListFilterPipe genExpr partialArgs pipeArg =
    case partialArgs of
        [ fnExpr ] ->
            let
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ typeof(__lst.data[0]) elm_" ++ pname ++ " = __lst.data[__i]; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__lst.data[__i])"
            in
            "({ typeof(" ++ pipeArg ++ ") __lst = " ++ pipeArg ++ "; typeof(__lst) __result; __result.length = 0; for (int __i = 0; __i < __lst.length; __i++) { if (" ++ fnAppStr ++ ") __result.data[__result.length++] = __lst.data[__i]; } __result; })"

        _ ->
            "/* List.filter partial wrong arity */ 0"


generateListTakePipe : GenExpr -> List Src.Expr -> String -> String
generateListTakePipe genExpr partialArgs pipeArg =
    case partialArgs of
        [ nExpr ] ->
            let
                nStr = genExpr nExpr
            in
            "({ int __n = " ++ nStr ++ "; elm_list_t __lst = " ++ pipeArg ++ "; elm_list_t __result; __result.length = __n < __lst.length ? __n : __lst.length; for (int __i = 0; __i < __result.length; __i++) __result.data[__i] = __lst.data[__i]; __result; })"

        _ ->
            "/* List.take partial wrong arity */ 0"


generateListIndexedMapPipe : GenExpr -> List Src.Expr -> String -> String
generateListIndexedMapPipe genExpr partialArgs pipeArg =
    case partialArgs of
        [ fnExpr ] ->
            let
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PVar pname2) ] lambdaBody) ->
                            "({ double elm_" ++ pname1 ++ " = __i; double elm_" ++ pname2 ++ " = __lst.data[__i]; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ double elm_" ++ pname1 ++ " = __i; double elm_" ++ innerName ++ " = ((elm_union_t)__lst.data[__i]).data; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ double elm_" ++ pname1 ++ " = __i; double elm_" ++ innerName ++ " = ((elm_union_t)__lst.data[__i]).data; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__i, __lst.data[__i])"
            in
            "({ elm_list_t __lst = " ++ pipeArg ++ "; elm_list_t __result; __result.length = __lst.length; for (int __i = 0; __i < __lst.length; __i++) __result.data[__i] = " ++ fnAppStr ++ "; __result; })"

        _ ->
            "/* List.indexedMap partial wrong arity */ 0"


generateListFilterMapPipe : GenExpr -> List Src.Expr -> String -> String
generateListFilterMapPipe genExpr partialArgs pipeArg =
    case partialArgs of
        [ fnExpr ] ->
            let
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __lst.data[__i]; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ double elm_" ++ innerName ++ " = ((elm_union_t)__lst.data[__i]).data; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ double elm_" ++ innerName ++ " = ((elm_union_t)__lst.data[__i]).data; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Var _ "identity") ->
                            "__lst.data[__i]"

                        _ ->
                            genExpr fnExpr ++ "(__lst.data[__i])"
            in
            "({ elm_list_t __lst = " ++ pipeArg ++ "; elm_list_t __result; __result.length = 0; for (int __i = 0; __i < __lst.length; __i++) { elm_union_t __maybe = " ++ fnAppStr ++ "; if (__maybe.tag == TAG_Just) __result.data[__result.length++] = __maybe.data; } __result; })"

        _ ->
            "/* List.filterMap partial wrong arity */ 0"


generateListConcatMapPipe : GenExpr -> List Src.Expr -> String -> String
generateListConcatMapPipe genExpr partialArgs pipeArg =
    case partialArgs of
        [ fnExpr ] ->
            let
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __src.data[__i]; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__src.data[__i])"
            in
            "({ elm_list_t __src = " ++ pipeArg ++ "; elm_list_t __result; __result.length = 0; for (int __i = 0; __i < __src.length; __i++) { elm_list_t __sub = " ++ fnAppStr ++ "; for (int __j = 0; __j < __sub.length; __j++) __result.data[__result.length++] = __sub.data[__j]; } __result; })"

        _ ->
            "/* List.concatMap partial wrong arity */ 0"


generateListSortByPipe : GenExpr -> List Src.Expr -> String -> String
generateListSortByPipe genExpr partialArgs pipeArg =
    case partialArgs of
        [ fnExpr ] ->
            let
                fnAppStr elemVar =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = " ++ elemVar ++ "; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PTuple (Src.At _ first) (Src.At _ second) rest) ] lambdaBody) ->
                            let
                                elemType = if List.isEmpty rest then "elm_tuple2_t" else "elm_tuple3_t"
                                bindOne idx pat =
                                    case pat of
                                        Src.PVar vn ->
                                            "double elm_" ++ vn ++ " = __sort_telem._" ++ String.fromInt idx ++ ".d;"
                                        Src.PAnything -> ""
                                        _ -> ""
                                bindings = [ bindOne 0 first, bindOne 1 second ]
                                    ++ (rest |> List.indexedMap (\i (Src.At _ p) -> bindOne (i + 2) p))
                                    |> List.filter (\s -> s /= "")
                                    |> String.join " "
                            in
                            "({ " ++ elemType ++ " __sort_telem = *(" ++ elemType ++ "*)&(" ++ elemVar ++ "); " ++ bindings ++ " " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ elm_union_t __sort_inner = (elm_union_t)" ++ elemVar ++ "; double elm_" ++ innerName ++ " = __sort_inner.data.num; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ elm_union_t __sort_inner = (elm_union_t)" ++ elemVar ++ "; double elm_" ++ innerName ++ " = __sort_inner.data.num; " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(" ++ elemVar ++ ")"
            in
            "({ elm_list_t __lst = " ++ pipeArg ++ "; elm_list_t __sorted; __sorted.length = __lst.length; for (int __i = 0; __i < __lst.length; __i++) __sorted.data[__i] = __lst.data[__i]; for (int __i = 1; __i < __sorted.length; __i++) { elm_data_t __key = __sorted.data[__i]; typeof(" ++ fnAppStr "__sorted.data[__i].d" ++ ") __key_val = " ++ fnAppStr "__key.d" ++ "; int __j = __i - 1; while (__j >= 0 && strcmp(" ++ fnAppStr "__sorted.data[__j].d" ++ ", __key_val) > 0) { __sorted.data[__j + 1] = __sorted.data[__j]; __j--; } __sorted.data[__j + 1] = __key; } __sorted; })"

        _ ->
            "/* List.sortBy partial wrong arity */ 0"


generateListPartitionPipe : GenExpr -> List Src.Expr -> String -> String
generateListPartitionPipe genExpr partialArgs pipeArg =
    case partialArgs of
        [ fnExpr ] ->
            let
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __lst.data[__i]; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ elm_union_t __elem = (elm_union_t)__lst.data[__i]; typeof(__elem.data) elm_" ++ innerName ++ " = __elem.data; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ elm_union_t __elem = (elm_union_t)__lst.data[__i]; typeof(__elem.data) elm_" ++ innerName ++ " = __elem.data; " ++ genExpr lambdaBody ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PTuple (Src.At _ first) (Src.At _ second) rest) ] lambdaBody) ->
                            let
                                elemType = if List.isEmpty rest then "elm_tuple2_t" else "elm_tuple3_t"
                                bindOne idx pat =
                                    case pat of
                                        Src.PVar vn ->
                                            "typeof(__part_telem._" ++ String.fromInt idx ++ ".d) elm_" ++ vn ++ " = __part_telem._" ++ String.fromInt idx ++ ".d;"
                                        Src.PAnything -> ""
                                        _ -> ""
                                bindings = [ bindOne 0 first, bindOne 1 second ]
                                    ++ (rest |> List.indexedMap (\i (Src.At _ p) -> bindOne (i + 2) p))
                                    |> List.filter (\s -> s /= "")
                                    |> String.join " "
                            in
                            "({ " ++ elemType ++ " __part_telem = *(" ++ elemType ++ "*)&(__lst.data[__i]); " ++ bindings ++ " " ++ genExpr lambdaBody ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__lst.data[__i])"
            in
            "({ elm_list_t __lst = " ++ pipeArg ++ "; struct { elm_list_t _0; elm_list_t _1; } __result; __result._0.length = 0; __result._1.length = 0; for (int __i = 0; __i < __lst.length; __i++) { if (" ++ fnAppStr ++ ") __result._0.data[__result._0.length++] = __lst.data[__i]; else __result._1.data[__result._1.length++] = __lst.data[__i]; } __result; })"

        _ ->
            "/* List.partition partial wrong arity */ 0"


generateMaybeMapPipe : GenExpr -> List Src.Expr -> String -> String
generateMaybeMapPipe genExpr partialArgs pipeArg =
    case partialArgs of
        [ fnExpr ] ->
            let
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Accessor fieldName) ->
                            "((struct { const char *" ++ fieldName ++ "; } *)__maybe_val.data.ptr)->" ++ fieldName

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar varName) ] body) ->
                            "({ double elm_" ++ varName ++ " = __maybe_val.data.num; " ++ genExpr body ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] body) ->
                            "({ elm_union_t __inner = __maybe_val.data; double elm_" ++ innerName ++ " = __inner.data.num; " ++ genExpr body ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] body) ->
                            "({ elm_union_t __inner = __maybe_val.data; double elm_" ++ innerName ++ " = __inner.data.num; " ++ genExpr body ++ "; })"

                        Src.At _ (Src.Call (Src.At _ (Src.Op opName)) [ Src.At _ (Src.Int n) ]) ->
                            if opName == "-" then
                                "((" ++ String.fromInt n ++ ") - (__maybe_val.data.num))"
                            else
                                let
                                    cOp = case opName of
                                        "+" -> "+"
                                        "*" -> "*"
                                        "/" -> "/"
                                        _ -> opName
                                in
                                "((__maybe_val.data.num) " ++ cOp ++ " (" ++ String.fromInt n ++ "))"

                        _ ->
                            genExpr fnExpr ++ "(__maybe_val.data.num)"

                isStringResult =
                    case fnExpr of
                        Src.At _ (Src.Accessor _) -> True
                        _ -> False
            in
            if isStringResult then
                "({ elm_union_t __maybe_val = " ++ pipeArg ++ "; elm_union_t __result; if (__maybe_val.tag == TAG_Just) { elm_union_t __inner = {0}; __inner.data.str = " ++ fnAppStr ++ "; __result = elm_Just(__inner); } else { __result = elm_Nothing(); } __result; })"
            else
                "({ elm_union_t __maybe_val = " ++ pipeArg ++ "; elm_union_t __result; if (__maybe_val.tag == TAG_Just) { elm_union_t __inner = {0}; __inner.data.num = " ++ fnAppStr ++ "; __result = elm_Just(__inner); } else { __result = elm_Nothing(); } __result; })"

        _ ->
            "/* Maybe.map partial wrong arity */ 0"


generateMaybeAndThenPipe : GenExpr -> List Src.Expr -> String -> String
generateMaybeAndThenPipe genExpr partialArgs pipeArg =
    case partialArgs of
        [ fnExpr ] ->
            let
                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar varName) ] body) ->
                            "({ double elm_" ++ varName ++ " = __maybe_val.data.child->data.num; " ++ genExpr body ++ "; })"

                        Src.At _ (Src.Lambda [ Src.At _ (Src.PTuple (Src.At _ first) (Src.At _ second) rest) ] body) ->
                            let
                                extractBinding idx pat =
                                    case pat of
                                        Src.PVar vn -> "double elm_" ++ vn ++ " = __tuple._" ++ String.fromInt idx ++ ".d;"
                                        Src.PAnything -> ""
                                        _ -> "/* unsupported nested pattern */"
                                firstBinding = extractBinding 0 first
                                secondBinding = extractBinding 1 second
                                restBindings = rest |> List.indexedMap (\i (Src.At _ p) -> extractBinding (i + 2) p)
                                allBindings = [ firstBinding, secondBinding ] ++ restBindings
                                    |> List.filter (\s -> s /= "")
                                    |> String.join " "
                                tupleType = if List.isEmpty rest then "elm_tuple2_t" else "elm_tuple3_t"
                            in
                            "({ " ++ tupleType ++ " __tuple = *(" ++ tupleType ++ "*)&(__maybe_val.data.child->data.num); " ++ allBindings ++ " " ++ genExpr body ++ "; })"

                        _ ->
                            genExpr fnExpr ++ "(__maybe_val.data.child->data.num)"
            in
            "({ elm_union_t __maybe_val = " ++ pipeArg ++ "; __maybe_val.tag == TAG_Just ? " ++ fnAppStr ++ " : ((elm_union_t){TAG_Nothing, 0}); })"

        _ ->
            "/* Maybe.andThen partial wrong arity */ 0"



-- HELPER FUNCTIONS FOR UNION CONSTRUCTION


{-| Check if an expression generates a union value
-}
isUnionValue : String -> Bool
isUnionValue exprStr =
    String.startsWith "((elm_union_t)" exprStr
        || String.startsWith "elm_alloc_union" exprStr
        || String.contains "elm_union_t" exprStr


{-| Check if an expression generates a string value
-}
isStringValue : String -> Bool
isStringValue exprStr =
    String.startsWith "\"" exprStr
        || String.startsWith "elm_str_" exprStr
        || String.startsWith "elm_from_int" exprStr
        || String.startsWith "elm_from_float" exprStr
        || String.contains "elm_str_append" exprStr


{-| Check if an expression generates a record/struct value
-}
isRecordValue : String -> Bool
isRecordValue exprStr =
    String.startsWith "((struct " exprStr
        || String.startsWith "{." exprStr
        || String.startsWith "elm_" exprStr && String.contains "struct" exprStr


{-| Wrap a value for use in elm_union_t constructor
-}
wrapUnionData : String -> String
wrapUnionData valueStr =
    if isUnionValue valueStr then
        "{.child = elm_alloc_union(" ++ valueStr ++ ")}"
    else if isStringValue valueStr then
        "{.str = " ++ valueStr ++ "}"
    else if isRecordValue valueStr then
        "{.child = (elm_union_t*)malloc(sizeof(elm_union_t))}"
    else
        "{.num = " ++ valueStr ++ "}"


{-| Generate a union constructor expression
-}
makeUnionCtor : String -> String -> String
makeUnionCtor tag dataValue =
    if dataValue == "0" || dataValue == "" then
        "((elm_union_t){" ++ tag ++ ", {.num = 0}})"
    else
        "((elm_union_t){" ++ tag ++ ", " ++ wrapUnionData dataValue ++ "})"


{-| Generate Just constructor call
-}
generateJustCtor : GenExpr -> List Src.Expr -> String
generateJustCtor genExpr args =
    case args of
        [ value ] ->
            let
                valueStr = genExpr value
                wrappedValue =
                    if isUnionValue valueStr then
                        valueStr
                    else if isStringValue valueStr then
                        "((elm_union_t){0, {.str = " ++ valueStr ++ "}, 0})"
                    else if isRecordValue valueStr then
                        let
                            startIdx = String.indexes "((struct {" valueStr |> List.head |> Maybe.withDefault 0
                            typeStart = startIdx + 2
                            typeEndMarker = "})"
                            afterStart = String.dropLeft typeStart valueStr
                            typeEndIdx = String.indexes typeEndMarker afterStart |> List.head |> Maybe.withDefault 0
                            structType = String.left (typeEndIdx + 1) afterStart
                        in
                        "({ void *__ptr = malloc(sizeof(" ++ structType ++ ")); memcpy(__ptr, &" ++ valueStr ++ ", sizeof(" ++ structType ++ ")); ((elm_union_t){TAG_Just, {.ptr = __ptr}, 0}); })"
                    else
                        "((elm_union_t){0, {.num = " ++ valueStr ++ "}, 0})"
            in
            if isRecordValue valueStr then
                wrappedValue
            else
                "elm_Just(" ++ wrappedValue ++ ")"

        _ ->
            "/* Just wrong arity */ 0"


{-| Generate Ok constructor call
-}
generateOkCtor : GenExpr -> List Src.Expr -> String
generateOkCtor genExpr args =
    case args of
        [ value ] ->
            let
                valueStr = genExpr value
                wrappedValue =
                    if isUnionValue valueStr then
                        valueStr
                    else if isStringValue valueStr then
                        "((elm_union_t){0, {.str = " ++ valueStr ++ "}, 0})"
                    else
                        "((elm_union_t){0, {.num = " ++ valueStr ++ "}, 0})"
            in
            "elm_Ok(" ++ wrappedValue ++ ")"

        _ ->
            "/* Ok wrong arity */ 0"


{-| Generate Err constructor call
-}
generateErrCtor : GenExpr -> List Src.Expr -> String
generateErrCtor genExpr args =
    case args of
        [ value ] ->
            let
                valueStr = genExpr value
                wrappedValue =
                    if isUnionValue valueStr then
                        valueStr
                    else if isStringValue valueStr then
                        "((elm_union_t){0, {.str = " ++ valueStr ++ "}, 0})"
                    else
                        "((elm_union_t){0, {.num = " ++ valueStr ++ "}, 0})"
            in
            "elm_Err(" ++ wrappedValue ++ ")"

        _ ->
            "/* Err wrong arity */ 0"
