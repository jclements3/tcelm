module Infer exposing
    ( infer
    , inferModule
    , Env
    , TypeError(..)
    , Substitution
    , emptyEnv
    , extendEnv
    , lookupEnv
    )

{-| Hindley-Milner type inference with type class support.

This module implements Algorithm W extended with:
- Type classes (constraint generation and solving)
- Higher-kinded types (kind checking)
- Row polymorphism (extensible records)

The inference process:
1. Generate fresh type variables
2. Generate constraints from expressions
3. Solve constraints (unification)
4. Check/resolve type class constraints
5. Generalize to type schemes
-}

import AST
import Types exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


-- TYPE ENVIRONMENT


{-| The type environment maps variable names to their type schemes.
-}
type alias Env =
    { types : Dict String Scheme
    , classes : Dict ClassName TypeClass
    , instances : List Instance
    , constructors : Dict String Scheme  -- data constructor types
    }


emptyEnv : Env
emptyEnv =
    { types = builtinTypes
    , classes = Dict.empty
    , instances = []
    , constructors = builtinConstructors
    }


{-| Built-in types for operators and common functions.
-}
builtinTypes : Dict String Scheme
builtinTypes =
    let
        -- a -> a -> a (for polymorphic numeric ops)
        numBinOp =
            Scheme [ "a" ] [] (TArrow (TVar "a") (TArrow (TVar "a") (TVar "a")))

        -- Int -> Int -> Int
        intBinOp =
            Scheme [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int")))

        -- a -> a -> Bool
        eqOp =
            Scheme [ "a" ] [] (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Bool")))

        -- Bool -> Bool -> Bool
        boolBinOp =
            Scheme [] [] (TArrow (TCon "Bool") (TArrow (TCon "Bool") (TCon "Bool")))

        -- a -> a
        identity =
            Scheme [ "a" ] [] (TArrow (TVar "a") (TVar "a"))

        -- a -> b -> a
        constFn =
            Scheme [ "a", "b" ] [] (TArrow (TVar "a") (TArrow (TVar "b") (TVar "a")))

        -- (b -> c) -> (a -> b) -> a -> c
        composeFn =
            Scheme [ "a", "b", "c" ] []
                (TArrow
                    (TArrow (TVar "b") (TVar "c"))
                    (TArrow
                        (TArrow (TVar "a") (TVar "b"))
                        (TArrow (TVar "a") (TVar "c"))
                    )
                )

        -- a -> (a -> b) -> b
        pipeOp =
            Scheme [ "a", "b" ] []
                (TArrow (TVar "a") (TArrow (TArrow (TVar "a") (TVar "b")) (TVar "b")))

        -- (a -> b) -> a -> b
        pipeLeftOp =
            Scheme [ "a", "b" ] []
                (TArrow (TArrow (TVar "a") (TVar "b")) (TArrow (TVar "a") (TVar "b")))

        -- List a -> List a -> List a
        appendOp =
            Scheme [ "a" ] []
                (TArrow (TApp (TCon "List") (TVar "a"))
                    (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "a")))
                )

        -- a -> List a -> List a
        consOp =
            Scheme [ "a" ] []
                (TArrow (TVar "a")
                    (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "a")))
                )
    in
    Dict.fromList
        [ -- Arithmetic operators
          ( "+", intBinOp )
        , ( "-", intBinOp )
        , ( "*", intBinOp )
        , ( "/", intBinOp )
        , ( "//", intBinOp )
        , ( "^", intBinOp )
        , ( "modBy", intBinOp )
        , ( "remainderBy", intBinOp )
        , ( "negate", Scheme [] [] (TArrow (TCon "Int") (TCon "Int")) )
        , ( "abs", Scheme [] [] (TArrow (TCon "Int") (TCon "Int")) )

        -- Comparison
        , ( "==", eqOp )
        , ( "/=", eqOp )
        , ( "<", eqOp )
        , ( ">", eqOp )
        , ( "<=", eqOp )
        , ( ">=", eqOp )

        -- Boolean
        , ( "&&", boolBinOp )
        , ( "||", boolBinOp )
        , ( "not", Scheme [] [] (TArrow (TCon "Bool") (TCon "Bool")) )

        -- Function combinators
        , ( "|>", pipeOp )
        , ( "<|", pipeLeftOp )
        , ( ">>", composeFn )
        , ( "<<", composeFn )
        , ( "identity", identity )
        , ( "always", constFn )

        -- List operators
        , ( "++", appendOp )
        , ( "::", consOp )

        -- Basic values
        , ( "True", Scheme [] [] (TCon "Bool") )
        , ( "False", Scheme [] [] (TCon "Bool") )

        -- Basics module
        , ( "min", intBinOp )
        , ( "max", intBinOp )
        , ( "clamp", Scheme [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int")))) )
        , ( "flip"
          , Scheme [ "a", "b", "c" ] []
                (TArrow (TArrow (TVar "a") (TArrow (TVar "b") (TVar "c")))
                    (TArrow (TVar "b") (TArrow (TVar "a") (TVar "c"))))
          )

        -- List module
        , ( "List.isEmpty", Scheme [ "a" ] [] (TArrow (TApp (TCon "List") (TVar "a")) (TCon "Bool")) )
        , ( "List.length", Scheme [ "a" ] [] (TArrow (TApp (TCon "List") (TVar "a")) (TCon "Int")) )
        , ( "List.reverse", Scheme [ "a" ] [] (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "a"))) )
        , ( "List.member", Scheme [ "a" ] [] (TArrow (TVar "a") (TArrow (TApp (TCon "List") (TVar "a")) (TCon "Bool"))) )
        , ( "List.head", Scheme [ "a" ] [] (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "Maybe") (TVar "a"))) )
        , ( "List.tail", Scheme [ "a" ] [] (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "Maybe") (TApp (TCon "List") (TVar "a")))) )
        , ( "List.take", Scheme [ "a" ] [] (TArrow (TCon "Int") (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "a")))) )
        , ( "List.drop", Scheme [ "a" ] [] (TArrow (TCon "Int") (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "a")))) )
        , ( "List.sum", Scheme [] [] (TArrow (TApp (TCon "List") (TCon "Int")) (TCon "Int")) )
        , ( "List.product", Scheme [] [] (TArrow (TApp (TCon "List") (TCon "Int")) (TCon "Int")) )
        , ( "List.maximum", Scheme [] [] (TArrow (TApp (TCon "List") (TCon "Int")) (TApp (TCon "Maybe") (TCon "Int"))) )
        , ( "List.minimum", Scheme [] [] (TArrow (TApp (TCon "List") (TCon "Int")) (TApp (TCon "Maybe") (TCon "Int"))) )
        , ( "List.append", appendOp )
        , ( "List.concat", Scheme [ "a" ] [] (TArrow (TApp (TCon "List") (TApp (TCon "List") (TVar "a"))) (TApp (TCon "List") (TVar "a"))) )
        , ( "List.intersperse", Scheme [ "a" ] [] (TArrow (TVar "a") (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "a")))) )
        , ( "List.range", Scheme [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TApp (TCon "List") (TCon "Int")))) )
        , ( "List.repeat", Scheme [ "a" ] [] (TArrow (TCon "Int") (TArrow (TVar "a") (TApp (TCon "List") (TVar "a")))) )
        , ( "List.map"
          , Scheme [ "a", "b" ] []
                (TArrow (TArrow (TVar "a") (TVar "b"))
                    (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "b"))))
          )
        , ( "List.filter"
          , Scheme [ "a" ] []
                (TArrow (TArrow (TVar "a") (TCon "Bool"))
                    (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "a"))))
          )
        , ( "List.filterMap"
          , Scheme [ "a", "b" ] []
                (TArrow (TArrow (TVar "a") (TApp (TCon "Maybe") (TVar "b")))
                    (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "b"))))
          )
        , ( "List.foldl"
          , Scheme [ "a", "b" ] []
                (TArrow (TArrow (TVar "a") (TArrow (TVar "b") (TVar "b")))
                    (TArrow (TVar "b") (TArrow (TApp (TCon "List") (TVar "a")) (TVar "b"))))
          )
        , ( "List.foldr"
          , Scheme [ "a", "b" ] []
                (TArrow (TArrow (TVar "a") (TArrow (TVar "b") (TVar "b")))
                    (TArrow (TVar "b") (TArrow (TApp (TCon "List") (TVar "a")) (TVar "b"))))
          )
        , ( "List.any"
          , Scheme [ "a" ] []
                (TArrow (TArrow (TVar "a") (TCon "Bool"))
                    (TArrow (TApp (TCon "List") (TVar "a")) (TCon "Bool")))
          )
        , ( "List.all"
          , Scheme [ "a" ] []
                (TArrow (TArrow (TVar "a") (TCon "Bool"))
                    (TArrow (TApp (TCon "List") (TVar "a")) (TCon "Bool")))
          )
        , ( "List.concatMap"
          , Scheme [ "a", "b" ] []
                (TArrow (TArrow (TVar "a") (TApp (TCon "List") (TVar "b")))
                    (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "b"))))
          )
        , ( "List.indexedMap"
          , Scheme [ "a", "b" ] []
                (TArrow (TArrow (TCon "Int") (TArrow (TVar "a") (TVar "b")))
                    (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "b"))))
          )
        , ( "List.sort"
          , Scheme [ "a" ] [] (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "a")))
          )
        , ( "List.sortBy"
          , Scheme [ "a", "b" ] []
                (TArrow (TArrow (TVar "a") (TVar "b"))
                    (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "a"))))
          )
        , ( "List.partition"
          , Scheme [ "a" ] []
                (TArrow (TArrow (TVar "a") (TCon "Bool"))
                    (TArrow (TApp (TCon "List") (TVar "a"))
                        (TTuple [ TApp (TCon "List") (TVar "a"), TApp (TCon "List") (TVar "a") ])))
          )

        -- Maybe module
        , ( "Maybe.withDefault", Scheme [ "a" ] [] (TArrow (TVar "a") (TArrow (TApp (TCon "Maybe") (TVar "a")) (TVar "a"))) )
        , ( "Maybe.map"
          , Scheme [ "a", "b" ] []
                (TArrow (TArrow (TVar "a") (TVar "b"))
                    (TArrow (TApp (TCon "Maybe") (TVar "a")) (TApp (TCon "Maybe") (TVar "b"))))
          )
        , ( "Maybe.andThen"
          , Scheme [ "a", "b" ] []
                (TArrow (TArrow (TVar "a") (TApp (TCon "Maybe") (TVar "b")))
                    (TArrow (TApp (TCon "Maybe") (TVar "a")) (TApp (TCon "Maybe") (TVar "b"))))
          )
        , ( "Maybe.map2"
          , Scheme [ "a", "b", "c" ] []
                (TArrow (TArrow (TVar "a") (TArrow (TVar "b") (TVar "c")))
                    (TArrow (TApp (TCon "Maybe") (TVar "a"))
                        (TArrow (TApp (TCon "Maybe") (TVar "b")) (TApp (TCon "Maybe") (TVar "c")))))
          )

        -- Result module (simplified - using type var for error)
        , ( "Result.withDefault"
          , Scheme [ "e", "a" ] []
                (TArrow (TVar "a") (TArrow (TApp (TApp (TCon "Result") (TVar "e")) (TVar "a")) (TVar "a")))
          )
        , ( "Result.map"
          , Scheme [ "e", "a", "b" ] []
                (TArrow (TArrow (TVar "a") (TVar "b"))
                    (TArrow (TApp (TApp (TCon "Result") (TVar "e")) (TVar "a"))
                        (TApp (TApp (TCon "Result") (TVar "e")) (TVar "b"))))
          )
        , ( "Result.mapError"
          , Scheme [ "e", "f", "a" ] []
                (TArrow (TArrow (TVar "e") (TVar "f"))
                    (TArrow (TApp (TApp (TCon "Result") (TVar "e")) (TVar "a"))
                        (TApp (TApp (TCon "Result") (TVar "f")) (TVar "a"))))
          )
        , ( "Result.andThen"
          , Scheme [ "e", "a", "b" ] []
                (TArrow (TArrow (TVar "a") (TApp (TApp (TCon "Result") (TVar "e")) (TVar "b")))
                    (TArrow (TApp (TApp (TCon "Result") (TVar "e")) (TVar "a"))
                        (TApp (TApp (TCon "Result") (TVar "e")) (TVar "b"))))
          )
        , ( "Result.toMaybe"
          , Scheme [ "e", "a" ] []
                (TArrow (TApp (TApp (TCon "Result") (TVar "e")) (TVar "a"))
                    (TApp (TCon "Maybe") (TVar "a")))
          )

        -- String module
        , ( "String.length", Scheme [] [] (TArrow (TCon "String") (TCon "Int")) )
        , ( "String.isEmpty", Scheme [] [] (TArrow (TCon "String") (TCon "Bool")) )
        , ( "String.reverse", Scheme [] [] (TArrow (TCon "String") (TCon "String")) )
        , ( "String.concat", Scheme [] [] (TArrow (TApp (TCon "List") (TCon "String")) (TCon "String")) )
        , ( "String.append", Scheme [] [] (TArrow (TCon "String") (TArrow (TCon "String") (TCon "String"))) )
        , ( "String.join", Scheme [] [] (TArrow (TCon "String") (TArrow (TApp (TCon "List") (TCon "String")) (TCon "String"))) )
        , ( "String.fromInt", Scheme [] [] (TArrow (TCon "Int") (TCon "String")) )
        , ( "String.toInt", Scheme [] [] (TArrow (TCon "String") (TApp (TCon "Maybe") (TCon "Int"))) )
        , ( "String.fromFloat", Scheme [] [] (TArrow (TCon "Float") (TCon "String")) )
        , ( "String.toFloat", Scheme [] [] (TArrow (TCon "String") (TApp (TCon "Maybe") (TCon "Float"))) )
        , ( "String.left", Scheme [] [] (TArrow (TCon "Int") (TArrow (TCon "String") (TCon "String"))) )
        , ( "String.right", Scheme [] [] (TArrow (TCon "Int") (TArrow (TCon "String") (TCon "String"))) )
        , ( "String.dropLeft", Scheme [] [] (TArrow (TCon "Int") (TArrow (TCon "String") (TCon "String"))) )
        , ( "String.dropRight", Scheme [] [] (TArrow (TCon "Int") (TArrow (TCon "String") (TCon "String"))) )
        , ( "String.contains", Scheme [] [] (TArrow (TCon "String") (TArrow (TCon "String") (TCon "Bool"))) )
        , ( "String.startsWith", Scheme [] [] (TArrow (TCon "String") (TArrow (TCon "String") (TCon "Bool"))) )
        , ( "String.endsWith", Scheme [] [] (TArrow (TCon "String") (TArrow (TCon "String") (TCon "Bool"))) )
        , ( "String.split", Scheme [] [] (TArrow (TCon "String") (TArrow (TCon "String") (TApp (TCon "List") (TCon "String")))) )
        , ( "String.slice", Scheme [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TArrow (TCon "String") (TCon "String")))) )
        , ( "String.toUpper", Scheme [] [] (TArrow (TCon "String") (TCon "String")) )
        , ( "String.toLower", Scheme [] [] (TArrow (TCon "String") (TCon "String")) )
        , ( "String.trim", Scheme [] [] (TArrow (TCon "String") (TCon "String")) )
        , ( "String.trimLeft", Scheme [] [] (TArrow (TCon "String") (TCon "String")) )
        , ( "String.trimRight", Scheme [] [] (TArrow (TCon "String") (TCon "String")) )
          -- Tuple module
        , ( "Tuple.pair", Scheme [ "a", "b" ] [] (TArrow (TVar "a") (TArrow (TVar "b") (TTuple [ TVar "a", TVar "b" ]))) )
        , ( "Tuple.first", Scheme [ "a", "b" ] [] (TArrow (TTuple [ TVar "a", TVar "b" ]) (TVar "a")) )
        , ( "Tuple.second", Scheme [ "a", "b" ] [] (TArrow (TTuple [ TVar "a", TVar "b" ]) (TVar "b")) )
        , ( "Tuple.mapFirst", Scheme [ "a", "b", "c" ] [] (TArrow (TArrow (TVar "a") (TVar "c")) (TArrow (TTuple [ TVar "a", TVar "b" ]) (TTuple [ TVar "c", TVar "b" ]))) )
        , ( "Tuple.mapSecond", Scheme [ "a", "b", "c" ] [] (TArrow (TArrow (TVar "b") (TVar "c")) (TArrow (TTuple [ TVar "a", TVar "b" ]) (TTuple [ TVar "a", TVar "c" ]))) )
        ]


builtinConstructors : Dict String Scheme
builtinConstructors =
    Dict.fromList
        [ ( "True", Scheme [] [] (TCon "Bool") )
        , ( "False", Scheme [] [] (TCon "Bool") )
        , ( "Nothing", Scheme [ "a" ] [] (TApp (TCon "Maybe") (TVar "a")) )
        , ( "Just", Scheme [ "a" ] [] (TArrow (TVar "a") (TApp (TCon "Maybe") (TVar "a"))) )
        , ( "Ok", Scheme [ "e", "a" ] [] (TArrow (TVar "a") (TApp (TApp (TCon "Result") (TVar "e")) (TVar "a"))) )
        , ( "Err", Scheme [ "e", "a" ] [] (TArrow (TVar "e") (TApp (TApp (TCon "Result") (TVar "e")) (TVar "a"))) )
        , ( "Nil", Scheme [ "a" ] [] (TApp (TCon "List") (TVar "a")) )
        , ( "Cons", Scheme [ "a" ] [] (TArrow (TVar "a") (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "a")))) )
        ]


extendEnv : String -> Scheme -> Env -> Env
extendEnv name scheme env =
    { env | types = Dict.insert name scheme env.types }


lookupEnv : String -> Env -> Maybe Scheme
lookupEnv name env =
    Dict.get name env.types



-- SUBSTITUTION


{-| A substitution maps type variables to types.
-}
type alias Substitution =
    Dict TypeVar Type


emptySubst : Substitution
emptySubst =
    Dict.empty


singleSubst : TypeVar -> Type -> Substitution
singleSubst v t =
    Dict.singleton v t


composeSubst : Substitution -> Substitution -> Substitution
composeSubst s1 s2 =
    Dict.union
        (Dict.map (\_ t -> applySubst s1 t) s2)
        s1


applySubst : Substitution -> Type -> Type
applySubst subst ty =
    substituteType subst ty


applySubstScheme : Substitution -> Scheme -> Scheme
applySubstScheme subst (Scheme vars constraints ty) =
    let
        subst2 = List.foldl Dict.remove subst vars
    in
    Scheme vars
        (List.map (applySubstConstraint subst2) constraints)
        (applySubst subst2 ty)


applySubstConstraint : Substitution -> Constraint -> Constraint
applySubstConstraint subst (IsIn cls ty) =
    IsIn cls (applySubst subst ty)


applySubstEnv : Substitution -> Env -> Env
applySubstEnv subst env =
    { env | types = Dict.map (\_ -> applySubstScheme subst) env.types }



-- INFERENCE STATE


type alias InferState =
    { supply : Int
    , substitution : Substitution
    , constraints : List Constraint
    }


initialState : InferState
initialState =
    { supply = 0
    , substitution = emptySubst
    , constraints = []
    }


{-| Generate a fresh type variable.
-}
freshTypeVar : InferState -> ( Type, InferState )
freshTypeVar state =
    let
        name = "t" ++ String.fromInt state.supply
    in
    ( TVar name
    , { state | supply = state.supply + 1 }
    )


freshTypeVars : Int -> InferState -> ( List Type, InferState )
freshTypeVars n state =
    if n <= 0 then
        ( [], state )
    else
        let
            ( t, state1 ) = freshTypeVar state
            ( ts, state2 ) = freshTypeVars (n - 1) state1
        in
        ( t :: ts, state2 )



-- ERRORS


type TypeError
    = UnificationError Type Type
    | OccursCheck TypeVar Type
    | UnboundVariable String
    | UnboundConstructor String
    | UnboundTypeClass String
    | NoInstance ClassName Type
    | KindMismatch Kind Kind
    | ArityMismatch String Int Int
    | NotAFunction Type
    | RecordFieldMissing String
    | DuplicateDefinition String
    | Other String



-- UNIFICATION


{-| Unify two types, returning a substitution or an error.
-}
unify : Type -> Type -> Result TypeError Substitution
unify t1 t2 =
    case ( t1, t2 ) of
        ( TVar v, t ) ->
            bindVar v t

        ( t, TVar v ) ->
            bindVar v t

        ( TCon c1, TCon c2 ) ->
            if c1 == c2 then
                Ok emptySubst
            else
                Err (UnificationError t1 t2)

        ( TApp a1 b1, TApp a2 b2 ) ->
            unifyMany [ a1, b1 ] [ a2, b2 ]

        ( TArrow a1 b1, TArrow a2 b2 ) ->
            unifyMany [ a1, b1 ] [ a2, b2 ]

        ( TTuple ts1, TTuple ts2 ) ->
            if List.length ts1 == List.length ts2 then
                unifyMany ts1 ts2
            else
                Err (UnificationError t1 t2)

        ( TRecord fields1 row1, TRecord fields2 row2 ) ->
            unifyRecords fields1 row1 fields2 row2

        ( TForall _ _ _, _ ) ->
            -- Foralls should be instantiated before unification
            Err (Other "Cannot unify forall directly")

        ( _, TForall _ _ _ ) ->
            Err (Other "Cannot unify forall directly")

        _ ->
            Err (UnificationError t1 t2)


bindVar : TypeVar -> Type -> Result TypeError Substitution
bindVar v t =
    if t == TVar v then
        Ok emptySubst
    else if occurs v t then
        Err (OccursCheck v t)
    else
        Ok (singleSubst v t)


unifyMany : List Type -> List Type -> Result TypeError Substitution
unifyMany ts1 ts2 =
    case ( ts1, ts2 ) of
        ( [], [] ) ->
            Ok emptySubst

        ( t1 :: rest1, t2 :: rest2 ) ->
            case unify t1 t2 of
                Err e ->
                    Err e

                Ok s1 ->
                    case unifyMany (List.map (applySubst s1) rest1) (List.map (applySubst s1) rest2) of
                        Err e ->
                            Err e

                        Ok s2 ->
                            Ok (composeSubst s2 s1)

        _ ->
            Err (Other "Type list length mismatch")


unifyRecords : List ( String, Type ) -> Maybe TypeVar -> List ( String, Type ) -> Maybe TypeVar -> Result TypeError Substitution
unifyRecords fields1 row1 fields2 row2 =
    -- Simplified record unification
    -- Full implementation would handle row polymorphism properly
    let
        dict1 = Dict.fromList fields1
        dict2 = Dict.fromList fields2

        commonFields = Set.intersect (Set.fromList (Dict.keys dict1)) (Set.fromList (Dict.keys dict2))
    in
    commonFields
        |> Set.toList
        |> List.filterMap (\f ->
            Maybe.map2 Tuple.pair (Dict.get f dict1) (Dict.get f dict2)
        )
        |> List.foldl
            (\( t1, t2 ) result ->
                case result of
                    Err e -> Err e
                    Ok s ->
                        case unify (applySubst s t1) (applySubst s t2) of
                            Err e -> Err e
                            Ok s2 -> Ok (composeSubst s2 s)
            )
            (Ok emptySubst)



-- INSTANTIATION AND GENERALIZATION


{-| Instantiate a type scheme with fresh type variables.
-}
instantiate : Scheme -> InferState -> ( Type, List Constraint, InferState )
instantiate (Scheme vars constraints ty) state =
    let
        ( freshVars, state1 ) = freshTypeVars (List.length vars) state
        subst = Dict.fromList (List.map2 Tuple.pair vars freshVars)
    in
    ( applySubst subst ty
    , List.map (applySubstConstraint subst) constraints
    , state1
    )


{-| Generalize a type to a type scheme by quantifying over free variables
    not in the environment.
-}
generalize : Env -> List Constraint -> Type -> Scheme
generalize env constraints ty =
    let
        envVars =
            env.types
                |> Dict.values
                |> List.concatMap schemeVars
                |> Set.fromList

        tyVars = freeTypeVars ty

        freeVars =
            Set.diff tyVars envVars
                |> Set.toList

        -- Filter constraints to only those mentioning free vars
        relevantConstraints =
            constraints
                |> List.filter (\(IsIn _ t) ->
                    not (Set.isEmpty (Set.intersect (freeTypeVars t) (Set.fromList freeVars)))
                )
    in
    Scheme freeVars relevantConstraints ty


schemeVars : Scheme -> List TypeVar
schemeVars (Scheme _ _ ty) =
    Set.toList (freeTypeVars ty)



-- INFERENCE


{-| Infer the type of an expression.
-}
infer : Env -> AST.Expr -> InferState -> Result TypeError ( Type, InferState )
infer env expr state =
    case expr of
        AST.ELit lit ->
            Ok ( litType lit, state )

        AST.EVar qname ->
            let
                -- Use full qualified name for lookup
                name =
                    case qname.module_ of
                        Nothing -> qname.name
                        Just mod -> mod ++ "." ++ qname.name
            in
            case lookupEnv name env of
                Nothing ->
                    Err (UnboundVariable name)

                Just scheme ->
                    let
                        ( ty, constraints, state1 ) = instantiate scheme state
                    in
                    Ok ( ty, { state1 | constraints = state1.constraints ++ constraints } )

        AST.EConstructor qname ->
            let
                name = qname.name
            in
            case Dict.get name env.constructors of
                Nothing ->
                    Err (UnboundConstructor name)

                Just scheme ->
                    let
                        ( ty, constraints, state1 ) = instantiate scheme state
                    in
                    Ok ( ty, { state1 | constraints = state1.constraints ++ constraints } )

        AST.EApp func arg ->
            inferApp env func arg state

        AST.ELambda patterns body ->
            inferLambda env patterns body state

        AST.ELet bindings body ->
            inferLet env bindings body state

        AST.EIf cond then_ else_ ->
            inferIf env cond then_ else_ state

        AST.ECase scrutinee branches ->
            inferCase env scrutinee branches state

        AST.EBinOp left op right ->
            -- Desugar to function application
            let
                opVar = AST.EVar { module_ = Nothing, name = AST.getValue op }
                app1 = AST.EApp (AST.locate (AST.getRegion op) opVar) left
                app2 = AST.EApp (AST.locate (AST.getRegion right) app1) right
            in
            infer env app2 state

        AST.ENegate inner ->
            -- negate : number -> number
            case infer env (AST.getValue inner) state of
                Err e ->
                    Err e

                Ok ( ty, state1 ) ->
                    -- For now, assume numeric
                    Ok ( ty, state1 )

        AST.ERecord fields ->
            inferRecord env fields state

        AST.ERecordAccess record field ->
            inferRecordAccess env record field state

        AST.ERecordAccessor field ->
            -- .field : { field : a | r } -> a
            let
                ( fieldTy, state1 ) = freshTypeVar state
                ( rowTy, state2 ) = freshTypeVar state1

                rowVarName =
                    case rowTy of
                        TVar v -> v
                        _ -> "r"

                recordTy = TRecord [ ( AST.getValue field, fieldTy ) ] (Just rowVarName)
            in
            Ok ( TArrow recordTy fieldTy, state2 )

        AST.ERecordUpdate name fields ->
            inferRecordUpdate env name fields state

        AST.ETuple exprs ->
            inferTuple env exprs state

        AST.EList exprs ->
            inferList env exprs state

        AST.EUnit ->
            Ok ( TCon "Unit", state )

        AST.EParens inner ->
            infer env (AST.getValue inner) state

        AST.EDo statements ->
            inferDo env statements state


inferApp : Env -> AST.Located AST.Expr -> AST.Located AST.Expr -> InferState -> Result TypeError ( Type, InferState )
inferApp env func arg state =
    case infer env (AST.getValue func) state of
        Err e ->
            Err e

        Ok ( funcTy, state1 ) ->
            case infer env (AST.getValue arg) state1 of
                Err e ->
                    Err e

                Ok ( argTy, state2 ) ->
                    let
                        ( retTy, state3 ) = freshTypeVar state2
                        expectedFuncTy = TArrow argTy retTy
                    in
                    case unify funcTy expectedFuncTy of
                        Err e ->
                            Err e

                        Ok subst ->
                            Ok ( applySubst subst retTy
                               , { state3 | substitution = composeSubst subst state3.substitution }
                               )


inferLambda : Env -> List (AST.Located AST.Pattern) -> AST.Located AST.Expr -> InferState -> Result TypeError ( Type, InferState )
inferLambda env patterns body state =
    case patterns of
        [] ->
            infer env (AST.getValue body) state

        pat :: restPats ->
            let
                ( argTy, state1 ) = freshTypeVar state
                patBindings = patternBindings (AST.getValue pat) argTy

                env1 =
                    List.foldl
                        (\( name, ty ) e -> extendEnv name (Scheme [] [] ty) e)
                        env
                        patBindings
            in
            case inferLambda env1 restPats body state1 of
                Err e ->
                    Err e

                Ok ( bodyTy, state2 ) ->
                    Ok ( TArrow argTy bodyTy, state2 )


inferLet : Env -> List (AST.Located AST.LetBinding) -> AST.Located AST.Expr -> InferState -> Result TypeError ( Type, InferState )
inferLet env bindings body state =
    case bindings of
        [] ->
            infer env (AST.getValue body) state

        binding :: rest ->
            let
                letBind = AST.getValue binding
            in
            case infer env (AST.getValue letBind.value) state of
                Err e ->
                    Err e

                Ok ( bindTy, state1 ) ->
                    let
                        scheme = generalize env state1.constraints bindTy
                        patBinds = patternBindings (AST.getValue letBind.pattern) bindTy
                        env1 =
                            List.foldl
                                (\( name, _ ) e -> extendEnv name scheme e)
                                env
                                patBinds
                    in
                    inferLet env1 rest body state1


inferIf : Env -> AST.Located AST.Expr -> AST.Located AST.Expr -> AST.Located AST.Expr -> InferState -> Result TypeError ( Type, InferState )
inferIf env cond then_ else_ state =
    case infer env (AST.getValue cond) state of
        Err e ->
            Err e

        Ok ( condTy, state1 ) ->
            case unify condTy (TCon "Bool") of
                Err e ->
                    Err e

                Ok subst1 ->
                    let
                        state2 = { state1 | substitution = composeSubst subst1 state1.substitution }
                    in
                    case infer env (AST.getValue then_) state2 of
                        Err e ->
                            Err e

                        Ok ( thenTy, state3 ) ->
                            case infer env (AST.getValue else_) state3 of
                                Err e ->
                                    Err e

                                Ok ( elseTy, state4 ) ->
                                    case unify thenTy elseTy of
                                        Err e ->
                                            Err e

                                        Ok subst2 ->
                                            Ok ( applySubst subst2 thenTy
                                               , { state4 | substitution = composeSubst subst2 state4.substitution }
                                               )


inferCase : Env -> AST.Located AST.Expr -> List (AST.Located AST.CaseBranch) -> InferState -> Result TypeError ( Type, InferState )
inferCase env scrutinee branches state =
    case infer env (AST.getValue scrutinee) state of
        Err e ->
            Err e

        Ok ( scrutTy, state1 ) ->
            let
                ( resultTy, state2 ) = freshTypeVar state1
            in
            inferBranches env scrutTy resultTy branches state2


inferBranches : Env -> Type -> Type -> List (AST.Located AST.CaseBranch) -> InferState -> Result TypeError ( Type, InferState )
inferBranches env scrutTy resultTy branches state =
    case branches of
        [] ->
            Ok ( resultTy, state )

        branch :: rest ->
            let
                br = AST.getValue branch
                -- Apply current substitution to scrutinee type to resolve type variables
                resolvedScrutTy = applySubst state.substitution scrutTy
                ( patBinds, state0 ) = patternBindingsWithState env (AST.getValue br.pattern) resolvedScrutTy state
                env1 =
                    List.foldl
                        (\( name, ty ) e -> extendEnv name (Scheme [] [] ty) e)
                        env
                        patBinds
            in
            case infer env1 (AST.getValue br.body) state0 of
                Err e ->
                    Err e

                Ok ( branchTy, state1 ) ->
                    case unify resultTy branchTy of
                        Err e ->
                            Err e

                        Ok subst ->
                            inferBranches env scrutTy
                                (applySubst subst resultTy)
                                rest
                                { state1 | substitution = composeSubst subst state1.substitution }


inferRecord : Env -> List ( AST.Located AST.Name, AST.Located AST.Expr ) -> InferState -> Result TypeError ( Type, InferState )
inferRecord env fields state =
    inferRecordFields env fields state []


inferRecordFields : Env -> List ( AST.Located AST.Name, AST.Located AST.Expr ) -> InferState -> List ( String, Type ) -> Result TypeError ( Type, InferState )
inferRecordFields env fields state acc =
    case fields of
        [] ->
            Ok ( TRecord (List.reverse acc) Nothing, state )

        ( name, expr ) :: rest ->
            case infer env (AST.getValue expr) state of
                Err e ->
                    Err e

                Ok ( ty, state1 ) ->
                    inferRecordFields env rest state1 (( AST.getValue name, ty ) :: acc)


inferRecordAccess : Env -> AST.Located AST.Expr -> AST.Located AST.Name -> InferState -> Result TypeError ( Type, InferState )
inferRecordAccess env record field state =
    case infer env (AST.getValue record) state of
        Err e ->
            Err e

        Ok ( recordTy, state1 ) ->
            let
                ( fieldTy, state2 ) = freshTypeVar state1
                ( rowVar, state3 ) = freshTypeVar state2

                rowVarName =
                    case rowVar of
                        TVar v -> v
                        _ -> "r"

                expectedTy = TRecord [ ( AST.getValue field, fieldTy ) ] (Just rowVarName)
            in
            case unify recordTy expectedTy of
                Err e ->
                    Err e

                Ok subst ->
                    Ok ( applySubst subst fieldTy
                       , { state3 | substitution = composeSubst subst state3.substitution }
                       )


inferRecordUpdate : Env -> AST.Located AST.Name -> List ( AST.Located AST.Name, AST.Located AST.Expr ) -> InferState -> Result TypeError ( Type, InferState )
inferRecordUpdate env name fields state =
    -- { name | field = value }
    case lookupEnv (AST.getValue name) env of
        Nothing ->
            Err (UnboundVariable (AST.getValue name))

        Just scheme ->
            let
                ( recordTy, _, state1 ) = instantiate scheme state
            in
            -- For simplicity, return the same record type
            -- Full implementation would check field compatibility
            Ok ( recordTy, state1 )


inferTuple : Env -> List (AST.Located AST.Expr) -> InferState -> Result TypeError ( Type, InferState )
inferTuple env exprs state =
    inferTupleElements env exprs state []


inferTupleElements : Env -> List (AST.Located AST.Expr) -> InferState -> List Type -> Result TypeError ( Type, InferState )
inferTupleElements env exprs state acc =
    case exprs of
        [] ->
            Ok ( TTuple (List.reverse acc), state )

        expr :: rest ->
            case infer env (AST.getValue expr) state of
                Err e ->
                    Err e

                Ok ( ty, state1 ) ->
                    inferTupleElements env rest state1 (ty :: acc)


inferList : Env -> List (AST.Located AST.Expr) -> InferState -> Result TypeError ( Type, InferState )
inferList env exprs state =
    case exprs of
        [] ->
            let
                ( elemTy, state1 ) = freshTypeVar state
            in
            Ok ( TApp (TCon "List") elemTy, state1 )

        first :: rest ->
            case infer env (AST.getValue first) state of
                Err e ->
                    Err e

                Ok ( elemTy, state1 ) ->
                    inferListRest env rest elemTy state1


inferListRest : Env -> List (AST.Located AST.Expr) -> Type -> InferState -> Result TypeError ( Type, InferState )
inferListRest env exprs elemTy state =
    case exprs of
        [] ->
            Ok ( TApp (TCon "List") elemTy, state )

        expr :: rest ->
            case infer env (AST.getValue expr) state of
                Err e ->
                    Err e

                Ok ( ty, state1 ) ->
                    case unify elemTy ty of
                        Err e ->
                            Err e

                        Ok subst ->
                            inferListRest env rest
                                (applySubst subst elemTy)
                                { state1 | substitution = composeSubst subst state1.substitution }


inferDo : Env -> List (AST.Located AST.DoStatement) -> InferState -> Result TypeError ( Type, InferState )
inferDo env statements state =
    -- do-notation desugaring happens elsewhere
    -- For now, just return a placeholder
    let
        ( ty, state1 ) = freshTypeVar state
    in
    Ok ( ty, state1 )



-- PATTERN BINDINGS


{-| Extract variable bindings from a pattern with their types.
-}
patternBindings : AST.Pattern -> Type -> List ( String, Type )
patternBindings pattern ty =
    case pattern of
        AST.PVar name ->
            [ ( name, ty ) ]

        AST.PWildcard ->
            []

        AST.PLit _ ->
            []

        AST.PCon _ subPats ->
            -- Would need constructor type info to get sub-pattern types
            []

        AST.PRecord fields ->
            -- For record patterns, assume the type is a record
            case ty of
                TRecord fieldTypes _ ->
                    fields
                        |> List.filterMap (\locName ->
                            let
                                name = AST.getValue locName
                            in
                            fieldTypes
                                |> List.filterMap (\( fn, ft ) ->
                                    if fn == name then Just ( name, ft ) else Nothing
                                )
                                |> List.head
                        )

                _ ->
                    []

        AST.PTuple subPats ->
            case ty of
                TTuple types ->
                    List.map2 patternBindings (List.map AST.getValue subPats) types
                        |> List.concat

                _ ->
                    []

        AST.PList _ ->
            []

        AST.PCons head tail ->
            case ty of
                TApp (TCon "List") elemTy ->
                    patternBindings (AST.getValue head) elemTy
                        ++ patternBindings (AST.getValue tail) ty

                _ ->
                    []

        AST.PAlias inner alias_ ->
            ( AST.getValue alias_, ty ) :: patternBindings (AST.getValue inner) ty

        AST.PParens inner ->
            patternBindings (AST.getValue inner) ty

        AST.PUnit ->
            []


{-| Extract variable bindings from a pattern, generating fresh type variables when needed.
-}
patternBindingsWithState : Env -> AST.Pattern -> Type -> InferState -> ( List ( String, Type ), InferState )
patternBindingsWithState env pattern ty state =
    case pattern of
        AST.PVar name ->
            ( [ ( name, ty ) ], state )

        AST.PWildcard ->
            ( [], state )

        AST.PLit _ ->
            ( [], state )

        AST.PCon qname subPats ->
            -- Look up constructor type and extract argument types
            let
                ctorName = qualNameToString qname
            in
            case Dict.get ctorName env.constructors of
                Just scheme ->
                    let
                        -- Instantiate the scheme to get actual types (ignore constraints for now)
                        ( ctorTy, _, state1 ) = instantiate scheme state

                        -- Extract argument types from constructor type
                        argTypes = extractArgTypes ctorTy

                        -- Bind sub-patterns to their types
                        bindSubPat i subPat accState =
                            let
                                subTy =
                                    List.drop i argTypes
                                        |> List.head
                                        |> Maybe.withDefault (TVar "a")
                            in
                            patternBindingsWithState env (AST.getValue subPat) subTy accState

                        ( allBinds, finalState ) =
                            List.foldl
                                (\( i, subPat ) ( bindsAcc, st ) ->
                                    let
                                        ( newBinds, st1 ) = bindSubPat i subPat st
                                    in
                                    ( bindsAcc ++ newBinds, st1 )
                                )
                                ( [], state1 )
                                (List.indexedMap Tuple.pair subPats)
                    in
                    ( allBinds, finalState )

                Nothing ->
                    -- Constructor not found, return empty bindings
                    ( [], state )

        AST.PRecord fields ->
            case ty of
                TRecord fieldTypes _ ->
                    let
                        binds =
                            fields
                                |> List.filterMap (\locName ->
                                    let
                                        name = AST.getValue locName
                                    in
                                    fieldTypes
                                        |> List.filterMap (\( fn, ft ) ->
                                            if fn == name then Just ( name, ft ) else Nothing
                                        )
                                        |> List.head
                                )
                    in
                    ( binds, state )

                _ ->
                    ( [], state )

        AST.PTuple subPats ->
            case ty of
                TTuple types ->
                    -- Bind patterns to their corresponding tuple element types
                    List.foldl
                        (\( subPat, subTy ) ( bindsAcc, st ) ->
                            let
                                ( newBinds, st1 ) = patternBindingsWithState env (AST.getValue subPat) subTy st
                            in
                            ( bindsAcc ++ newBinds, st1 )
                        )
                        ( [], state )
                        (List.map2 Tuple.pair subPats types)

                TVar _ ->
                    -- Type is a variable, generate fresh type variables for each element
                    let
                        numElements = List.length subPats
                        ( elemTypes, state1 ) =
                            List.foldl
                                (\_ ( tys, st ) ->
                                    let ( freshTy, st1 ) = freshTypeVar st
                                    in ( tys ++ [ freshTy ], st1 )
                                )
                                ( [], state )
                                subPats
                    in
                    List.foldl
                        (\( subPat, subTy ) ( bindsAcc, st ) ->
                            let
                                ( newBinds, st1 ) = patternBindingsWithState env (AST.getValue subPat) subTy st
                            in
                            ( bindsAcc ++ newBinds, st1 )
                        )
                        ( [], state1 )
                        (List.map2 Tuple.pair subPats elemTypes)

                _ ->
                    ( [], state )

        AST.PList _ ->
            ( [], state )

        AST.PCons head tail ->
            case ty of
                TApp (TCon "List") elemTy ->
                    let
                        headBinds = patternBindings (AST.getValue head) elemTy
                        tailBinds = patternBindings (AST.getValue tail) ty
                    in
                    ( headBinds ++ tailBinds, state )

                TVar _ ->
                    -- Type is not yet resolved, generate fresh type variables
                    let
                        ( elemTy, state1 ) = freshTypeVar state
                        listTy = TApp (TCon "List") elemTy
                        headBinds = patternBindings (AST.getValue head) elemTy
                        tailBinds = patternBindings (AST.getValue tail) listTy
                    in
                    ( headBinds ++ tailBinds, state1 )

                _ ->
                    ( [], state )

        AST.PAlias inner alias_ ->
            let
                ( innerBinds, state1 ) = patternBindingsWithState env (AST.getValue inner) ty state
            in
            ( ( AST.getValue alias_, ty ) :: innerBinds, state1 )

        AST.PParens inner ->
            patternBindingsWithState env (AST.getValue inner) ty state

        AST.PUnit ->
            ( [], state )



-- LITERAL TYPES


litType : AST.Literal -> Type
litType lit =
    case lit of
        AST.LInt _ -> TCon "Int"
        AST.LFloat _ -> TCon "Float"
        AST.LString _ -> TCon "String"
        AST.LChar _ -> TCon "Char"



-- MODULE INFERENCE


{-| Infer types for an entire module.
-}
inferModule : Env -> AST.Module -> Result TypeError Env
inferModule env module_ =
    inferDecls env module_.decls


inferDecls : Env -> List (AST.Located AST.Decl) -> Result TypeError Env
inferDecls env decls =
    case decls of
        [] ->
            Ok env

        decl :: rest ->
            case inferDecl env (AST.getValue decl) of
                Err e ->
                    Err e

                Ok env1 ->
                    inferDecls env1 rest


inferDecl : Env -> AST.Decl -> Result TypeError Env
inferDecl env decl =
    case decl of
        AST.ValueDecl valueDef ->
            let
                name = AST.getValue valueDef.name

                -- For recursive functions, we need to add a preliminary type binding
                -- Create a fresh type variable for the function's type
                ( prelimTy, state0 ) = freshTypeVar initialState

                -- Add the function name to the environment with a preliminary type
                -- This allows recursive calls within the body to find the function
                envWithSelf = extendEnv name (Scheme [] [] prelimTy) env
            in
            case inferValueDef envWithSelf valueDef state0 of
                Err e ->
                    Err e

                Ok ( ty, state ) ->
                    -- Unify the inferred type with the preliminary type
                    case unify prelimTy ty of
                        Err e ->
                            Err e

                        Ok subst ->
                            let
                                state1 = { state | substitution = composeSubst subst state.substitution }
                                finalTy = applySubst state1.substitution ty
                                scheme = generalize env state1.constraints finalTy
                            in
                            Ok (extendEnv name scheme env)

        AST.TypeAliasDecl _ ->
            -- Type aliases are handled during kind checking
            Ok env

        AST.CustomTypeDecl typeDef ->
            -- Add constructor types to environment
            let
                typeName = AST.getValue typeDef.name
                typeVars = List.map AST.getValue typeDef.typeVars

                resultType =
                    List.foldl
                        (\v t -> TApp t (TVar v))
                        (TCon typeName)
                        typeVars

                addCtor locCtor env1 =
                    let
                        ctor = AST.getValue locCtor
                        ctorName = AST.getValue ctor.name
                        -- Build constructor type: arg1 -> arg2 -> ... -> ResultType
                        ctorType =
                            List.foldr
                                (\_ t -> TArrow (TVar "a") t)  -- Simplified
                                resultType
                                ctor.args
                        scheme = Scheme typeVars [] ctorType
                    in
                    { env1 | constructors = Dict.insert ctorName scheme env1.constructors }
            in
            Ok (List.foldl addCtor env typeDef.constructors)

        AST.TypeClassDecl classDef ->
            -- Add type class to environment
            let
                className = AST.getValue classDef.name
                typeVars = List.map AST.getValue classDef.typeVars

                methods =
                    classDef.methods
                        |> List.map (\locMethod ->
                            let
                                method = AST.getValue locMethod
                            in
                            ( AST.getValue method.name
                            , Scheme typeVars [] (TCon "TODO")  -- Simplified
                            )
                        )

                typeClass =
                    TypeClass className typeVars
                        (List.map (\_ -> KStar) typeVars)
                        methods
                        []
            in
            Ok { env | classes = Dict.insert className typeClass env.classes }

        AST.InstanceDecl _ ->
            -- Add instance to environment
            Ok env

        AST.PortDecl _ ->
            Ok env

        AST.InfixDecl _ ->
            Ok env

        AST.ForeignDecl foreignDef ->
            -- Add foreign function to environment
            let
                name = AST.getValue foreignDef.name
                -- Simplified: would need to convert TypeAnnotation to Type
                scheme = Scheme [] [] (TCon "Foreign")
            in
            Ok (extendEnv name scheme env)


{-| Infer the type of a value definition, handling function arguments.
-}
inferValueDef : Env -> AST.ValueDef -> InferState -> Result TypeError ( Type, InferState )
inferValueDef env valueDef state =
    case valueDef.args of
        [] ->
            -- No arguments, just infer the body
            infer env (AST.getValue valueDef.body) state

        args ->
            -- This is a function, so we need to:
            -- 1. Create fresh type variables for each argument
            -- 2. Add them to the environment
            -- 3. Infer the body type
            -- 4. Build the function type: arg1 -> arg2 -> ... -> bodyType
            inferFunctionDef env args valueDef.body state


inferFunctionDef : Env -> List (AST.Located AST.Pattern) -> AST.Located AST.Expr -> InferState -> Result TypeError ( Type, InferState )
inferFunctionDef env args body state =
    case args of
        [] ->
            infer env (AST.getValue body) state

        arg :: restArgs ->
            let
                -- Create a fresh type variable for this argument
                ( argTy, state1 ) = freshTypeVar state

                -- Get bindings from the pattern
                patBinds = patternBindings (AST.getValue arg) argTy

                -- Extend environment with pattern bindings
                env1 =
                    List.foldl
                        (\( bindName, bindTy ) e -> extendEnv bindName (Scheme [] [] bindTy) e)
                        env
                        patBinds
            in
            -- Recursively infer the rest of the function
            case inferFunctionDef env1 restArgs body state1 of
                Err e ->
                    Err e

                Ok ( restTy, state2 ) ->
                    -- Build arrow type: argTy -> restTy
                    Ok ( TArrow argTy restTy, state2 )


-- Extract argument types from a function type (e.g., Int -> Box returns [Int])
extractArgTypes : Type -> List Type
extractArgTypes ty =
    case ty of
        TArrow argTy resultTy ->
            argTy :: extractArgTypes resultTy

        _ ->
            []


-- Convert a qualified name to a string
qualNameToString : AST.QualName -> String
qualNameToString qname =
    case qname.module_ of
        Nothing -> qname.name
        Just mod -> mod ++ "." ++ qname.name
