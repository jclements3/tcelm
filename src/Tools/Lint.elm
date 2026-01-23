module Tools.Lint exposing
    ( lint, lintModule
    , Rule, Severity(..), Issue
    , defaultRules, allRules
    , unusedImports, unusedVariables, unusedTypeVariables
    , missingTypeAnnotations, todoComments, debugUsage
    , namingConventions, simplifiableExpressions
    )

{-| Linter - Static analysis for Elm code.

@docs lint, lintModule
@docs Rule, Severity, Issue
@docs defaultRules, allRules
@docs unusedImports, unusedVariables, unusedTypeVariables
@docs missingTypeAnnotations, todoComments, debugUsage
@docs namingConventions, simplifiableExpressions

-}

import AST.Source as Src exposing (Located(..))
import Set exposing (Set)


{-| Severity level of an issue.
-}
type Severity
    = Error
    | Warning
    | Info


{-| A lint issue found in the code.
-}
type alias Issue =
    { rule : String
    , message : String
    , severity : Severity
    , region : Src.Region
    , suggestion : Maybe String
    }


{-| A lint rule.
-}
type alias Rule =
    { name : String
    , description : String
    , check : Src.Module -> List Issue
    }



-- LINTING


{-| Lint a module with the default rules.
-}
lint : Src.Module -> List Issue
lint =
    lintModule defaultRules


{-| Lint a module with specific rules.
-}
lintModule : List Rule -> Src.Module -> List Issue
lintModule rules mod =
    List.concatMap (\rule -> rule.check mod) rules
        |> List.sortBy (\issue -> ( issue.region.start.row, issue.region.start.col ))



-- RULE SETS


{-| Default set of rules.
-}
defaultRules : List Rule
defaultRules =
    [ unusedImports
    , unusedVariables
    , missingTypeAnnotations
    , debugUsage
    , todoComments
    ]


{-| All available rules.
-}
allRules : List Rule
allRules =
    [ unusedImports
    , unusedVariables
    , unusedTypeVariables
    , missingTypeAnnotations
    , todoComments
    , debugUsage
    , namingConventions
    , simplifiableExpressions
    ]



-- RULES


{-| Check for unused imports.
-}
unusedImports : Rule
unusedImports =
    { name = "NoUnusedImports"
    , description = "Reports imports that are not used in the module"
    , check = checkUnusedImports
    }


checkUnusedImports : Src.Module -> List Issue
checkUnusedImports mod =
    let
        usedNames =
            collectUsedNames mod

        checkImport : Src.Import -> Maybe Issue
        checkImport imp =
            let
                (At region name) =
                    imp.name

                isUsed =
                    case imp.alias_ of
                        Just alias ->
                            Set.member alias usedNames || Set.member name usedNames

                        Nothing ->
                            Set.member name usedNames
                                || (case imp.exposing_ of
                                        Src.Open ->
                                            True

                                        Src.Explicit exposed ->
                                            List.any (exposedIsUsed usedNames) exposed
                                   )
            in
            if isUsed then
                Nothing

            else
                Just
                    { rule = "NoUnusedImports"
                    , message = "Import `" ++ name ++ "` is not used"
                    , severity = Warning
                    , region = region
                    , suggestion = Just ("Remove unused import: " ++ name)
                    }
    in
    List.filterMap checkImport mod.imports


exposedIsUsed : Set String -> Src.Exposed -> Bool
exposedIsUsed usedNames exposed =
    case exposed of
        Src.Lower (At _ name) ->
            Set.member name usedNames

        Src.Upper (At _ name) _ ->
            Set.member name usedNames

        Src.Operator _ name ->
            Set.member name usedNames


{-| Check for unused variables.
-}
unusedVariables : Rule
unusedVariables =
    { name = "NoUnusedVariables"
    , description = "Reports variables that are defined but never used"
    , check = checkUnusedVariables
    }


checkUnusedVariables : Src.Module -> List Issue
checkUnusedVariables mod =
    let
        -- Get all top-level definitions
        topLevelNames =
            List.map (\(At _ v) -> locatedString v.name) mod.values

        -- Get all used names in the module
        usedNames =
            collectUsedNames mod

        -- Check each definition
        checkValue : Located Src.Value -> List Issue
        checkValue (At _ value) =
            let
                (At nameRegion name) =
                    value.name

                isExported =
                    case Src.toValue mod.exports of
                        Src.Open ->
                            True

                        Src.Explicit exposed ->
                            List.any
                                (\e ->
                                    case e of
                                        Src.Lower (At _ n) ->
                                            n == name

                                        _ ->
                                            False
                                )
                                exposed
            in
            if isExported || Set.member name usedNames then
                -- Check function arguments
                checkPatternList value.args (collectExprNames value.body)

            else
                { rule = "NoUnusedVariables"
                , message = "Function `" ++ name ++ "` is defined but never used"
                , severity = Warning
                , region = nameRegion
                , suggestion = Just ("Remove unused function or export it")
                }
                    :: checkPatternList value.args (collectExprNames value.body)
    in
    List.concatMap checkValue mod.values


checkPatternList : List Src.Pattern -> Set String -> List Issue
checkPatternList patterns usedNames =
    List.concatMap (\p -> checkPatternUsed p usedNames) patterns


checkPatternUsed : Src.Pattern -> Set String -> List Issue
checkPatternUsed (At region pattern) usedNames =
    case pattern of
        Src.PVar name ->
            if name == "_" || String.startsWith "_" name || Set.member name usedNames then
                []

            else
                [ { rule = "NoUnusedVariables"
                  , message = "Variable `" ++ name ++ "` is not used"
                  , severity = Warning
                  , region = region
                  , suggestion = Just ("Prefix with underscore: _" ++ name)
                  }
                ]

        Src.PAlias pat (At aliasRegion alias) ->
            let
                aliasIssue =
                    if Set.member alias usedNames then
                        []

                    else
                        [ { rule = "NoUnusedVariables"
                          , message = "Alias `" ++ alias ++ "` is not used"
                          , severity = Warning
                          , region = aliasRegion
                          , suggestion = Just ("Remove alias or use it")
                          }
                        ]
            in
            aliasIssue ++ checkPatternUsed pat usedNames

        Src.PTuple a b rest ->
            List.concatMap (\p -> checkPatternUsed p usedNames) (a :: b :: rest)

        Src.PCtor _ _ args ->
            List.concatMap (\p -> checkPatternUsed p usedNames) args

        Src.PCtorQual _ _ _ args ->
            List.concatMap (\p -> checkPatternUsed p usedNames) args

        Src.PList items ->
            List.concatMap (\p -> checkPatternUsed p usedNames) items

        Src.PCons head tail ->
            checkPatternUsed head usedNames ++ checkPatternUsed tail usedNames

        Src.PRecord fields ->
            List.concatMap
                (\(At r n) ->
                    if Set.member n usedNames then
                        []

                    else
                        [ { rule = "NoUnusedVariables"
                          , message = "Field `" ++ n ++ "` is destructured but not used"
                          , severity = Warning
                          , region = r
                          , suggestion = Just ("Remove from pattern or use it")
                          }
                        ]
                )
                fields

        _ ->
            []


{-| Check for unused type variables.
-}
unusedTypeVariables : Rule
unusedTypeVariables =
    { name = "NoUnusedTypeVariables"
    , description = "Reports type variables that are declared but not used"
    , check = checkUnusedTypeVariables
    }


checkUnusedTypeVariables : Src.Module -> List Issue
checkUnusedTypeVariables mod =
    let
        checkAlias : Located Src.Alias -> List Issue
        checkAlias (At _ alias) =
            let
                usedVars =
                    collectTypeVars alias.type_

                checkArg : Located String -> Maybe Issue
                checkArg (At region name) =
                    if Set.member name usedVars then
                        Nothing

                    else
                        Just
                            { rule = "NoUnusedTypeVariables"
                            , message = "Type variable `" ++ name ++ "` is not used"
                            , severity = Warning
                            , region = region
                            , suggestion = Just ("Remove unused type variable")
                            }
            in
            List.filterMap checkArg alias.args

        checkUnion : Located Src.Union -> List Issue
        checkUnion (At _ union) =
            let
                usedVars =
                    List.concatMap (List.map collectTypeVars << Tuple.second) union.ctors
                        |> List.foldl Set.union Set.empty

                checkArg : Located String -> Maybe Issue
                checkArg (At region name) =
                    if Set.member name usedVars then
                        Nothing

                    else
                        Just
                            { rule = "NoUnusedTypeVariables"
                            , message = "Type variable `" ++ name ++ "` is not used"
                            , severity = Warning
                            , region = region
                            , suggestion = Just ("Remove unused type variable")
                            }
            in
            List.filterMap checkArg union.args
    in
    List.concatMap checkAlias mod.aliases ++ List.concatMap checkUnion mod.unions


{-| Check for missing type annotations.
-}
missingTypeAnnotations : Rule
missingTypeAnnotations =
    { name = "NoMissingTypeAnnotations"
    , description = "Reports top-level functions without type annotations"
    , check = checkMissingTypeAnnotations
    }


checkMissingTypeAnnotations : Src.Module -> List Issue
checkMissingTypeAnnotations mod =
    let
        checkValue : Located Src.Value -> Maybe Issue
        checkValue (At _ value) =
            case value.type_ of
                Just _ ->
                    Nothing

                Nothing ->
                    let
                        (At region name) =
                            value.name
                    in
                    Just
                        { rule = "NoMissingTypeAnnotations"
                        , message = "Function `" ++ name ++ "` is missing a type annotation"
                        , severity = Info
                        , region = region
                        , suggestion = Just ("Add a type annotation")
                        }
    in
    List.filterMap checkValue mod.values


{-| Check for TODO comments.
-}
todoComments : Rule
todoComments =
    { name = "NoTodoComments"
    , description = "Reports TODO, FIXME, and XXX comments"
    , check = \_ -> []

    -- Note: Would need source text to check comments
    }


{-| Check for Debug module usage.
-}
debugUsage : Rule
debugUsage =
    { name = "NoDebug"
    , description = "Reports usage of Debug module functions"
    , check = checkDebugUsage
    }


checkDebugUsage : Src.Module -> List Issue
checkDebugUsage mod =
    let
        checkExpr : Src.Expr -> List Issue
        checkExpr (At region expr) =
            case expr of
                Src.VarQual _ "Debug" name ->
                    [ { rule = "NoDebug"
                      , message = "Debug." ++ name ++ " should not be used in production"
                      , severity = Warning
                      , region = region
                      , suggestion = Just ("Remove Debug." ++ name ++ " call")
                      }
                    ]

                Src.Call func args ->
                    checkExpr func ++ List.concatMap checkExpr args

                Src.If branches else_ ->
                    List.concatMap (\( c, b ) -> checkExpr c ++ checkExpr b) branches
                        ++ checkExpr else_

                Src.Let defs body ->
                    List.concatMap checkDef defs ++ checkExpr body

                Src.Case subject branches ->
                    checkExpr subject
                        ++ List.concatMap (\( _, b ) -> checkExpr b) branches

                Src.Lambda _ body ->
                    checkExpr body

                Src.List items ->
                    List.concatMap checkExpr items

                Src.Tuple a b rest ->
                    List.concatMap checkExpr (a :: b :: rest)

                Src.Record fields ->
                    List.concatMap (checkExpr << Tuple.second) fields

                Src.Update _ fields ->
                    List.concatMap (checkExpr << Tuple.second) fields

                Src.Binops pairs final ->
                    List.concatMap (checkExpr << Tuple.first) pairs ++ checkExpr final

                Src.Negate e ->
                    checkExpr e

                Src.Access e _ ->
                    checkExpr e

                _ ->
                    []

        checkDef : Located Src.Def -> List Issue
        checkDef (At _ def) =
            case def of
                Src.Define _ _ body _ ->
                    checkExpr body

                Src.Destruct _ body ->
                    checkExpr body
    in
    List.concatMap (\(At _ v) -> checkExpr v.body) mod.values


{-| Check naming conventions.
-}
namingConventions : Rule
namingConventions =
    { name = "NamingConventions"
    , description = "Reports names that don't follow Elm conventions"
    , check = checkNamingConventions
    }


checkNamingConventions : Src.Module -> List Issue
checkNamingConventions mod =
    let
        checkValue : Located Src.Value -> List Issue
        checkValue (At _ value) =
            let
                (At region name) =
                    value.name
            in
            if Char.isUpper (String.uncons name |> Maybe.map Tuple.first |> Maybe.withDefault '_') then
                [ { rule = "NamingConventions"
                  , message = "Function `" ++ name ++ "` should start with lowercase"
                  , severity = Warning
                  , region = region
                  , suggestion = Just ("Rename to start with lowercase letter")
                  }
                ]

            else
                []
    in
    List.concatMap checkValue mod.values


{-| Check for simplifiable expressions.
-}
simplifiableExpressions : Rule
simplifiableExpressions =
    { name = "Simplify"
    , description = "Reports expressions that can be simplified"
    , check = checkSimplifiable
    }


checkSimplifiable : Src.Module -> List Issue
checkSimplifiable mod =
    let
        checkExpr : Src.Expr -> List Issue
        checkExpr (At region expr) =
            case expr of
                Src.If [ ( cond, thenBranch ) ] elseBranch ->
                    -- Check for `if x then True else False`
                    case ( Src.toValue thenBranch, Src.toValue elseBranch ) of
                        ( Src.Var Src.CapVar "True", Src.Var Src.CapVar "False" ) ->
                            [ { rule = "Simplify"
                              , message = "`if x then True else False` can be simplified to `x`"
                              , severity = Info
                              , region = region
                              , suggestion = Just "Use the condition directly"
                              }
                            ]

                        ( Src.Var Src.CapVar "False", Src.Var Src.CapVar "True" ) ->
                            [ { rule = "Simplify"
                              , message = "`if x then False else True` can be simplified to `not x`"
                              , severity = Info
                              , region = region
                              , suggestion = Just "Use `not` instead"
                              }
                            ]

                        _ ->
                            checkExpr cond ++ checkExpr thenBranch ++ checkExpr elseBranch

                Src.Call func args ->
                    checkExpr func ++ List.concatMap checkExpr args

                Src.If branches else_ ->
                    List.concatMap (\( c, b ) -> checkExpr c ++ checkExpr b) branches
                        ++ checkExpr else_

                Src.Let defs body ->
                    List.concatMap checkDef defs ++ checkExpr body

                Src.Case subject branches ->
                    checkExpr subject
                        ++ List.concatMap (\( _, b ) -> checkExpr b) branches

                Src.Lambda _ body ->
                    checkExpr body

                Src.List items ->
                    List.concatMap checkExpr items

                Src.Tuple a b rest ->
                    List.concatMap checkExpr (a :: b :: rest)

                Src.Record fields ->
                    List.concatMap (checkExpr << Tuple.second) fields

                Src.Update _ fields ->
                    List.concatMap (checkExpr << Tuple.second) fields

                Src.Binops pairs final ->
                    List.concatMap (checkExpr << Tuple.first) pairs ++ checkExpr final

                Src.Negate e ->
                    checkExpr e

                Src.Access e _ ->
                    checkExpr e

                _ ->
                    []

        checkDef : Located Src.Def -> List Issue
        checkDef (At _ def) =
            case def of
                Src.Define _ _ body _ ->
                    checkExpr body

                Src.Destruct _ body ->
                    checkExpr body
    in
    List.concatMap (\(At _ v) -> checkExpr v.body) mod.values



-- HELPERS


locatedString : Located String -> String
locatedString (At _ s) =
    s


collectUsedNames : Src.Module -> Set String
collectUsedNames mod =
    List.map (\(At _ v) -> collectExprNames v.body) mod.values
        |> List.foldl Set.union Set.empty


collectExprNames : Src.Expr -> Set String
collectExprNames (At _ expr) =
    case expr of
        Src.Var _ name ->
            Set.singleton name

        Src.VarQual _ qual name ->
            Set.fromList [ qual, name ]

        Src.Call func args ->
            Set.union (collectExprNames func) (List.foldl (\e acc -> Set.union (collectExprNames e) acc) Set.empty args)

        Src.If branches else_ ->
            List.foldl
                (\( c, b ) acc -> Set.union acc (Set.union (collectExprNames c) (collectExprNames b)))
                (collectExprNames else_)
                branches

        Src.Let defs body ->
            List.foldl
                (\(At _ def) acc ->
                    case def of
                        Src.Define _ _ e _ ->
                            Set.union acc (collectExprNames e)

                        Src.Destruct _ e ->
                            Set.union acc (collectExprNames e)
                )
                (collectExprNames body)
                defs

        Src.Case subject branches ->
            List.foldl
                (\( _, b ) acc -> Set.union acc (collectExprNames b))
                (collectExprNames subject)
                branches

        Src.Lambda _ body ->
            collectExprNames body

        Src.List items ->
            List.foldl (\e acc -> Set.union (collectExprNames e) acc) Set.empty items

        Src.Tuple a b rest ->
            List.foldl (\e acc -> Set.union (collectExprNames e) acc) Set.empty (a :: b :: rest)

        Src.Record fields ->
            List.foldl (\( _, e ) acc -> Set.union (collectExprNames e) acc) Set.empty fields

        Src.Update (At _ name) fields ->
            List.foldl (\( _, e ) acc -> Set.union (collectExprNames e) acc) (Set.singleton name) fields

        Src.Binops pairs final ->
            List.foldl (\( e, _ ) acc -> Set.union (collectExprNames e) acc) (collectExprNames final) pairs

        Src.Negate e ->
            collectExprNames e

        Src.Access e _ ->
            collectExprNames e

        _ ->
            Set.empty


collectTypeVars : Src.Type -> Set String
collectTypeVars (At _ typ) =
    case typ of
        Src.TVar name ->
            Set.singleton name

        Src.TLambda from to ->
            Set.union (collectTypeVars from) (collectTypeVars to)

        Src.TType _ _ args ->
            List.foldl (\t acc -> Set.union (collectTypeVars t) acc) Set.empty args

        Src.TTypeQual _ _ _ args ->
            List.foldl (\t acc -> Set.union (collectTypeVars t) acc) Set.empty args

        Src.TRecord fields _ ->
            List.foldl (\( _, t ) acc -> Set.union (collectTypeVars t) acc) Set.empty fields

        Src.TTuple a b rest ->
            List.foldl (\t acc -> Set.union (collectTypeVars t) acc) Set.empty (a :: b :: rest)

        Src.TUnit ->
            Set.empty
