module Codegen.Pattern exposing
    ( generateCaseExpr
    , patternBindings
    )

{-| Pattern matching code generation.

This module handles generation of C code for Elm case expressions
and pattern matching.

To break circular dependencies with expression generation, functions in this
module take a `genExpr` callback parameter for generating sub-expressions.

-}

import AST.Source as Src
import Codegen.Shared as Shared exposing (ExprCtx, escapeC)


{-| Type alias for the expression generator callback.
-}
type alias GenExpr =
    Src.Expr -> String


{-| Generate C code for a case expression.
Returns Just if this module can handle the case, Nothing otherwise.
The fallback logic remains in Cli.elm for complex patterns.
-}
generateCaseExpr : GenExpr -> Src.Expr -> List ( Src.Pattern, Maybe Src.Expr, Src.Expr ) -> Maybe String
generateCaseExpr genExpr scrutinee branches =
    let
        -- Check what kind of case this is
        patternType =
            detectPatternType branches
    in
    case patternType of
        BoolCase ->
            Just (generateBoolCase genExpr scrutinee branches)

        IntLiteralCase ->
            Just (generateIntLiteralCase genExpr scrutinee branches)

        StringLiteralCase ->
            Just (generateStringLiteralCase genExpr scrutinee branches)

        CharLiteralCase ->
            Just (generateCharLiteralCase genExpr scrutinee branches)

        _ ->
            -- Complex patterns handled by fallback in Cli.elm
            Nothing


{-| Pattern type detection
-}
type PatternType
    = BoolCase -- True/False patterns
    | IntLiteralCase -- Integer literal patterns
    | StringLiteralCase -- String literal patterns
    | CharLiteralCase -- Character literal patterns
    | CtorCase -- Custom type constructor patterns
    | ListCase -- List patterns (empty, cons)
    | TupleCase -- Tuple patterns
    | RecordCase -- Record patterns
    | WildcardCase -- Just wildcards/variables
    | MixedCase -- Mixed or complex patterns


detectPatternType : List ( Src.Pattern, Maybe Src.Expr, Src.Expr ) -> PatternType
detectPatternType branches =
    let
        patterns =
            List.map (\( p, _, _ ) -> p) branches
    in
    if List.all isBoolPattern patterns then
        BoolCase

    else if List.all isIntLiteralPattern patterns then
        IntLiteralCase

    else if List.all isStringLiteralPattern patterns then
        StringLiteralCase

    else if List.all isCharLiteralPattern patterns then
        CharLiteralCase

    else
        MixedCase


isBoolPattern : Src.Pattern -> Bool
isBoolPattern (Src.At _ pat) =
    case pat of
        Src.PCtor _ "True" [] ->
            True

        Src.PCtor _ "False" [] ->
            True

        Src.PAnything ->
            True

        Src.PVar _ ->
            True

        _ ->
            False


isIntLiteralPattern : Src.Pattern -> Bool
isIntLiteralPattern (Src.At _ pat) =
    case pat of
        Src.PInt _ ->
            True

        Src.PAnything ->
            True

        Src.PVar _ ->
            True

        _ ->
            False


isStringLiteralPattern : Src.Pattern -> Bool
isStringLiteralPattern (Src.At _ pat) =
    case pat of
        Src.PStr _ ->
            True

        Src.PAnything ->
            True

        Src.PVar _ ->
            True

        _ ->
            False


isCharLiteralPattern : Src.Pattern -> Bool
isCharLiteralPattern (Src.At _ pat) =
    case pat of
        Src.PChr _ ->
            True

        Src.PAnything ->
            True

        Src.PVar _ ->
            True

        _ ->
            False



-- BOOL CASE GENERATION


generateBoolCase : GenExpr -> Src.Expr -> List ( Src.Pattern, Maybe Src.Expr, Src.Expr ) -> String
generateBoolCase genExpr scrutinee branches =
    let
        scrutineeStr =
            genExpr scrutinee

        -- Find True and False branches
        findBranch name =
            branches
                |> List.filterMap
                    (\( Src.At _ pat, guard, result ) ->
                        case pat of
                            Src.PCtor _ n [] ->
                                if n == name then
                                    Just ( guard, result )

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
                |> List.head

        -- Find wildcard/variable branch as fallback
        findWildcard =
            branches
                |> List.filterMap
                    (\( Src.At _ pat, guard, result ) ->
                        case pat of
                            Src.PAnything ->
                                Just ( guard, result )

                            Src.PVar _ ->
                                Just ( guard, result )

                            _ ->
                                Nothing
                    )
                |> List.head

        trueBranch =
            findBranch "True"

        falseBranch =
            findBranch "False"
    in
    case ( trueBranch, falseBranch ) of
        ( Just ( trueGuard, trueResult ), Just ( falseGuard, falseResult ) ) ->
            let
                trueStr =
                    applyGuard genExpr trueGuard (genExpr trueResult) "0"

                falseStr =
                    applyGuard genExpr falseGuard (genExpr falseResult) "0"
            in
            "(" ++ scrutineeStr ++ " ? " ++ trueStr ++ " : " ++ falseStr ++ ")"

        ( Just ( trueGuard, trueResult ), Nothing ) ->
            case findWildcard of
                Just ( wildGuard, wildResult ) ->
                    let
                        trueStr =
                            applyGuard genExpr trueGuard (genExpr trueResult) (genExpr wildResult)

                        falseStr =
                            applyGuard genExpr wildGuard (genExpr wildResult) "0"
                    in
                    "(" ++ scrutineeStr ++ " ? " ++ trueStr ++ " : " ++ falseStr ++ ")"

                Nothing ->
                    "(" ++ scrutineeStr ++ " ? " ++ genExpr trueResult ++ " : 0)"

        ( Nothing, Just ( falseGuard, falseResult ) ) ->
            case findWildcard of
                Just ( wildGuard, wildResult ) ->
                    let
                        trueStr =
                            applyGuard genExpr wildGuard (genExpr wildResult) "0"

                        falseStr =
                            applyGuard genExpr falseGuard (genExpr falseResult) (genExpr wildResult)
                    in
                    "(" ++ scrutineeStr ++ " ? " ++ trueStr ++ " : " ++ falseStr ++ ")"

                Nothing ->
                    "(" ++ scrutineeStr ++ " ? 0 : " ++ genExpr falseResult ++ ")"

        ( Nothing, Nothing ) ->
            -- Only wildcards
            case findWildcard of
                Just ( _, result ) ->
                    genExpr result

                Nothing ->
                    "0 /* no match */"



-- INT LITERAL CASE GENERATION


generateIntLiteralCase : GenExpr -> Src.Expr -> List ( Src.Pattern, Maybe Src.Expr, Src.Expr ) -> String
generateIntLiteralCase genExpr scrutinee branches =
    let
        scrutineeStr =
            genExpr scrutinee

        generateBranches bs =
            case bs of
                [] ->
                    "0 /* no match */"

                ( Src.At _ pat, maybeGuard, result ) :: rest ->
                    case pat of
                        Src.PInt n ->
                            let
                                resultStr =
                                    applyGuard genExpr maybeGuard (genExpr result) (generateBranches rest)
                            in
                            "("
                                ++ scrutineeStr
                                ++ " == "
                                ++ String.fromInt n
                                ++ " ? "
                                ++ resultStr
                                ++ " : "
                                ++ generateBranches rest
                                ++ ")"

                        Src.PVar varName ->
                            let
                                binding =
                                    "double elm_" ++ varName ++ " = " ++ scrutineeStr ++ ";"

                                resultStr =
                                    applyGuard genExpr maybeGuard (genExpr result) (generateBranches rest)
                            in
                            "({ " ++ binding ++ " " ++ resultStr ++ "; })"

                        Src.PAnything ->
                            applyGuard genExpr maybeGuard (genExpr result) (generateBranches rest)

                        _ ->
                            generateBranches rest
    in
    generateBranches branches



-- STRING LITERAL CASE GENERATION


generateStringLiteralCase : GenExpr -> Src.Expr -> List ( Src.Pattern, Maybe Src.Expr, Src.Expr ) -> String
generateStringLiteralCase genExpr scrutinee branches =
    let
        scrutineeStr =
            genExpr scrutinee

        generateBranches : List ( Src.Pattern, Maybe Src.Expr, Src.Expr ) -> String
        generateBranches bs =
            case bs of
                [] ->
                    "\"\" /* no match */"

                ( Src.At _ pat, maybeGuard, result ) :: rest ->
                    case pat of
                        Src.PStr s ->
                            let
                                resultStr =
                                    applyGuard genExpr maybeGuard (genExpr result) (generateBranches rest)
                            in
                            "(strcmp("
                                ++ scrutineeStr
                                ++ ", \""
                                ++ escapeC s
                                ++ "\") == 0 ? "
                                ++ resultStr
                                ++ " : "
                                ++ generateBranches rest
                                ++ ")"

                        Src.PVar varName ->
                            let
                                binding =
                                    "const char *elm_" ++ varName ++ " = " ++ scrutineeStr ++ ";"

                                resultStr =
                                    applyGuard genExpr maybeGuard (genExpr result) (generateBranches rest)
                            in
                            "({ " ++ binding ++ " " ++ resultStr ++ "; })"

                        Src.PAnything ->
                            applyGuard genExpr maybeGuard (genExpr result) (generateBranches rest)

                        _ ->
                            generateBranches rest
    in
    generateBranches branches



-- CHAR LITERAL CASE GENERATION


generateCharLiteralCase : GenExpr -> Src.Expr -> List ( Src.Pattern, Maybe Src.Expr, Src.Expr ) -> String
generateCharLiteralCase genExpr scrutinee branches =
    let
        scrutineeStr =
            genExpr scrutinee

        generateBranches : List ( Src.Pattern, Maybe Src.Expr, Src.Expr ) -> String
        generateBranches bs =
            case bs of
                [] ->
                    "'\\0' /* no match */"

                ( Src.At _ pat, maybeGuard, result ) :: rest ->
                    case pat of
                        Src.PChr c ->
                            let
                                resultStr =
                                    applyGuard genExpr maybeGuard (genExpr result) (generateBranches rest)
                            in
                            "("
                                ++ scrutineeStr
                                ++ " == '"
                                ++ escapeC c
                                ++ "' ? "
                                ++ resultStr
                                ++ " : "
                                ++ generateBranches rest
                                ++ ")"

                        Src.PVar varName ->
                            let
                                binding =
                                    "char elm_" ++ varName ++ " = " ++ scrutineeStr ++ ";"

                                resultStr =
                                    applyGuard genExpr maybeGuard (genExpr result) (generateBranches rest)
                            in
                            "({ " ++ binding ++ " " ++ resultStr ++ "; })"

                        Src.PAnything ->
                            applyGuard genExpr maybeGuard (genExpr result) (generateBranches rest)

                        _ ->
                            generateBranches rest
    in
    generateBranches branches



-- HELPER FUNCTIONS


applyGuard : GenExpr -> Maybe Src.Expr -> String -> String -> String
applyGuard genExpr maybeGuard thenStr elseStr =
    case maybeGuard of
        Nothing ->
            thenStr

        Just guardExpr ->
            "(" ++ genExpr guardExpr ++ " ? " ++ thenStr ++ " : " ++ elseStr ++ ")"


{-| Generate variable bindings for a pattern.
Returns a list of C variable declarations.
-}
patternBindings : GenExpr -> Src.Pattern -> String -> List String
patternBindings genExpr (Src.At _ pat) scrutineeStr =
    case pat of
        Src.PVar varName ->
            [ "double elm_" ++ varName ++ " = " ++ scrutineeStr ++ ";" ]

        Src.PAnything ->
            []

        Src.PTuple (Src.At _ first) (Src.At _ second) rest ->
            let
                bindOne idx p =
                    case p of
                        Src.PVar vn ->
                            [ "double elm_" ++ vn ++ " = " ++ scrutineeStr ++ "._" ++ String.fromInt idx ++ ";" ]

                        Src.PAnything ->
                            []

                        _ ->
                            []
            in
            bindOne 0 first
                ++ bindOne 1 second
                ++ (rest
                        |> List.indexedMap (\i (Src.At _ p) -> bindOne (i + 2) p)
                        |> List.concat
                   )

        Src.PRecord fields ->
            fields
                |> List.map (\(Src.At _ fieldName) -> "double elm_" ++ fieldName ++ " = " ++ scrutineeStr ++ "." ++ fieldName ++ ";")

        _ ->
            []
