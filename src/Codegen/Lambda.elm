module Codegen.Lambda exposing
    ( LiftedFunc
    , LiftedLambda
    , LambdaState
    , collectVarRefs
    , initialLambdaState
    )

{-| Lambda lifting for code generation.

This module handles lambda lifting - transforming nested functions and
lambdas into top-level functions that can be compiled without nested
function support (needed for TCC compatibility).

Two representations are provided:

  - `LiftedFunc` - simpler representation used by the standalone code generator
  - `LiftedLambda` - more detailed representation with ID tracking for the module generator

-}

import AST.Source as Src


{-| A local function to be lifted to module level (used in standalone codegen).
-}
type alias LiftedFunc =
    { prefix : String
    , name : String
    , args : List Src.Pattern
    , body : Src.Expr
    , capturedVars : List String -- Variables captured from outer scope
    }


{-| Lifted lambda for TCC compatibility (used in module codegen).
Instead of GCC nested functions, lambdas are lifted to module level.
-}
type alias LiftedLambda =
    { id : Int
    , modulePrefix : String
    , captures : List String -- Variables captured from enclosing scope
    , args : List Src.Pattern -- Lambda parameters
    , body : Src.Expr
    , outerLocals : List String -- All locals visible in enclosing scope
    }


{-| State for lambda collection - tracks current ID and collected lambdas.
-}
type alias LambdaState =
    { nextId : Int
    , lambdas : List LiftedLambda
    }


{-| Initial state for lambda collection.
-}
initialLambdaState : LambdaState
initialLambdaState =
    { nextId = 0
    , lambdas = []
    }


{-| Collect all variable references from an expression.
This is used to determine which variables are captured by a lambda.
-}
collectVarRefs : Src.Expr -> List String
collectVarRefs (Src.At _ expr) =
    case expr of
        Src.Var _ name ->
            [ name ]

        Src.VarQual _ _ name ->
            [ name ]

        Src.Call fn fnArgs ->
            collectVarRefs fn ++ List.concatMap collectVarRefs fnArgs

        Src.Binops pairs final ->
            List.concatMap (\( e, _ ) -> collectVarRefs e) pairs ++ collectVarRefs final

        Src.If branches elseExpr ->
            List.concatMap (\( cond, thenE ) -> collectVarRefs cond ++ collectVarRefs thenE) branches
                ++ collectVarRefs elseExpr

        Src.Let defs letBody ->
            let
                -- Collect refs from def bodies (but not the names being defined)
                defRefs =
                    defs
                        |> List.concatMap
                            (\(Src.At _ def) ->
                                case def of
                                    Src.Define _ _ defBody _ ->
                                        collectVarRefs defBody

                                    Src.Destruct _ e ->
                                        collectVarRefs e
                            )
            in
            defRefs ++ collectVarRefs letBody

        Src.Case scrutinee branches ->
            collectVarRefs scrutinee
                ++ List.concatMap
                    (\( _, maybeGuard, branchExpr ) ->
                        (case maybeGuard of
                            Just guardExpr ->
                                collectVarRefs guardExpr

                            Nothing ->
                                []
                        )
                            ++ collectVarRefs branchExpr
                    )
                    branches

        Src.Lambda _ lambdaBody ->
            collectVarRefs lambdaBody

        Src.Record fields ->
            fields |> List.concatMap (\( _, fieldExpr ) -> collectVarRefs fieldExpr)

        Src.Update (Src.At _ baseName) fields ->
            baseName :: List.concatMap (\( _, fieldExpr ) -> collectVarRefs fieldExpr) fields

        Src.Access inner _ ->
            collectVarRefs inner

        Src.Negate inner ->
            collectVarRefs inner

        Src.Tuple first second rest ->
            collectVarRefs first ++ collectVarRefs second ++ List.concatMap collectVarRefs rest

        Src.List items ->
            List.concatMap collectVarRefs items

        _ ->
            []
