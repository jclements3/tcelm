module Infer exposing
    ( infer
    , inferModule
    , Env
    , TypeError(..)
    , Substitution
    , emptyEnv
    , extendEnv
    , lookupEnv
    , resolveConstraint
    , checkConstraints
    , isComparable
    , isAppendable
    , isNumber
    )

{-| Hindley-Milner type inference with type class support.

UPGRADED VERSION using do-notation for cleaner monadic code.
Compare to src2/Infer.elm for the original nested-case version.

Reduction: ~3,013 lines -> ~2,100 lines (-30%)
-}

import AST
import Types exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


-- INFER MONAD


{-| The Infer monad combines state threading with error handling.
    InferState -> Result TypeError (a, InferState)

    tcelm do-notation desugars:
        do { x <- ma; f x }
    to:
        ma |> andThen (\x -> f x)
-}
type alias Infer a =
    InferState -> Result TypeError ( a, InferState )


{-| Core monadic bind for Infer monad.
-}
andThen : (a -> Infer b) -> Infer a -> Infer b
andThen f ma state =
    case ma state of
        Err e -> Err e
        Ok ( a, state1 ) -> f a state1


{-| Lift a pure value into Infer monad.
-}
pure : a -> Infer a
pure a state =
    Ok ( a, state )


{-| Lift a Result into Infer monad (state unchanged).
-}
liftResult : Result TypeError a -> Infer a
liftResult result state =
    case result of
        Err e -> Err e
        Ok a -> Ok ( a, state )


{-| Get current state.
-}
get : Infer InferState
get state =
    Ok ( state, state )


{-| Modify state.
-}
modify : (InferState -> InferState) -> Infer ()
modify f state =
    Ok ( (), f state )


{-| Run inference and apply accumulated substitution.
-}
runInfer : Infer a -> InferState -> Result TypeError ( a, InferState )
runInfer = identity


{-| Map over the result.
-}
map : (a -> b) -> Infer a -> Infer b
map f ma = do
    a <- ma
    pure (f a)


{-| Sequence two inferences.
-}
map2 : (a -> b -> c) -> Infer a -> Infer b -> Infer c
map2 f ma mb = do
    a <- ma
    b <- mb
    pure (f a b)


{-| Fold over a list with an Infer action.
-}
foldM : (b -> a -> Infer b) -> b -> List a -> Infer b
foldM f acc list =
    case list of
        [] -> pure acc
        x :: xs -> do
            acc1 <- f acc x
            foldM f acc1 xs


{-| Map an Infer action over a list.
-}
mapM : (a -> Infer b) -> List a -> Infer (List b)
mapM f list =
    case list of
        [] -> pure []
        x :: xs -> do
            y <- f x
            ys <- mapM f xs
            pure (y :: ys)



-- TYPE ENVIRONMENT


type alias Env =
    { types : Dict String Scheme
    , classes : Dict ClassName TypeClass
    , instances : List Instance
    , constructors : Dict String Scheme
    , typeAliases : Dict String ( List String, Type )
    }


emptyEnv : Env
emptyEnv =
    { types = builtinTypes
    , classes = builtinClasses
    , instances = builtinInstances
    , constructors = builtinConstructors
    , typeAliases = Dict.empty
    }


builtinClasses : Dict ClassName TypeClass
builtinClasses =
    Dict.fromList
        [ ( "comparable"
          , TypeClass "comparable" [ "a" ] [ KStar ]
                [ ( "compare", Scheme [ "a" ] [ IsIn "comparable" (TVar "a") ]
                    (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Order")))
                  )
                ]
                []
          )
        , ( "appendable"
          , TypeClass "appendable" [ "a" ] [ KStar ]
                [ ( "append", Scheme [ "a" ] [ IsIn "appendable" (TVar "a") ]
                    (TArrow (TVar "a") (TArrow (TVar "a") (TVar "a")))
                  )
                ]
                []
          )
        , ( "number"
          , TypeClass "number" [ "a" ] [ KStar ]
                [ ( "add", Scheme [ "a" ] [ IsIn "number" (TVar "a") ]
                    (TArrow (TVar "a") (TArrow (TVar "a") (TVar "a")))
                  )
                , ( "sub", Scheme [ "a" ] [ IsIn "number" (TVar "a") ]
                    (TArrow (TVar "a") (TArrow (TVar "a") (TVar "a")))
                  )
                , ( "mul", Scheme [ "a" ] [ IsIn "number" (TVar "a") ]
                    (TArrow (TVar "a") (TArrow (TVar "a") (TVar "a")))
                  )
                , ( "negate", Scheme [ "a" ] [ IsIn "number" (TVar "a") ]
                    (TArrow (TVar "a") (TVar "a"))
                  )
                ]
                []
          )
        ]


builtinInstances : List Instance
builtinInstances =
    [ Instance "comparable" [] [ TCon "Int" ]
    , Instance "comparable" [] [ TCon "Float" ]
    , Instance "comparable" [] [ TCon "Char" ]
    , Instance "comparable" [] [ TCon "String" ]
    , Instance "appendable" [] [ TCon "String" ]
    , Instance "number" [] [ TCon "Int" ]
    , Instance "number" [] [ TCon "Float" ]
    ]


builtinTypes : Dict String Scheme
builtinTypes =
    Dict.fromList
        [ ( "True", Scheme [] [] (TCon "Bool") )
        , ( "False", Scheme [] [] (TCon "Bool") )
        , ( "+", Scheme [ "a" ] [ IsIn "number" (TVar "a") ]
            (TArrow (TVar "a") (TArrow (TVar "a") (TVar "a"))) )
        , ( "-", Scheme [ "a" ] [ IsIn "number" (TVar "a") ]
            (TArrow (TVar "a") (TArrow (TVar "a") (TVar "a"))) )
        , ( "*", Scheme [ "a" ] [ IsIn "number" (TVar "a") ]
            (TArrow (TVar "a") (TArrow (TVar "a") (TVar "a"))) )
        , ( "/", Scheme [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Float"))) )
        , ( "//", Scheme [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int"))) )
        , ( "^", Scheme [ "a" ] [ IsIn "number" (TVar "a") ]
            (TArrow (TVar "a") (TArrow (TVar "a") (TVar "a"))) )
        , ( "++", Scheme [ "a" ] [ IsIn "appendable" (TVar "a") ]
            (TArrow (TVar "a") (TArrow (TVar "a") (TVar "a"))) )
        , ( "::", Scheme [ "a" ] [] (TArrow (TVar "a") (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "a")))) )
        , ( "==", Scheme [ "a" ] [] (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Bool"))) )
        , ( "/=", Scheme [ "a" ] [] (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Bool"))) )
        , ( "<", Scheme [ "a" ] [ IsIn "comparable" (TVar "a") ]
            (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Bool"))) )
        , ( ">", Scheme [ "a" ] [ IsIn "comparable" (TVar "a") ]
            (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Bool"))) )
        , ( "<=", Scheme [ "a" ] [ IsIn "comparable" (TVar "a") ]
            (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Bool"))) )
        , ( ">=", Scheme [ "a" ] [ IsIn "comparable" (TVar "a") ]
            (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Bool"))) )
        , ( "&&", Scheme [] [] (TArrow (TCon "Bool") (TArrow (TCon "Bool") (TCon "Bool"))) )
        , ( "||", Scheme [] [] (TArrow (TCon "Bool") (TArrow (TCon "Bool") (TCon "Bool"))) )
        , ( "|>", Scheme [ "a", "b" ] [] (TArrow (TVar "a") (TArrow (TArrow (TVar "a") (TVar "b")) (TVar "b"))) )
        , ( "<|", Scheme [ "a", "b" ] [] (TArrow (TArrow (TVar "a") (TVar "b")) (TArrow (TVar "a") (TVar "b"))) )
        , ( ">>", Scheme [ "a", "b", "c" ] []
            (TArrow (TArrow (TVar "a") (TVar "b")) (TArrow (TArrow (TVar "b") (TVar "c")) (TArrow (TVar "a") (TVar "c")))) )
        , ( "<<", Scheme [ "a", "b", "c" ] []
            (TArrow (TArrow (TVar "b") (TVar "c")) (TArrow (TArrow (TVar "a") (TVar "b")) (TArrow (TVar "a") (TVar "c")))) )
        , ( "modBy", Scheme [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int"))) )
        , ( "remainderBy", Scheme [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int"))) )
        , ( "not", Scheme [] [] (TArrow (TCon "Bool") (TCon "Bool")) )
        , ( "negate", Scheme [ "a" ] [ IsIn "number" (TVar "a") ] (TArrow (TVar "a") (TVar "a")) )
        , ( "identity", Scheme [ "a" ] [] (TArrow (TVar "a") (TVar "a")) )
        , ( "always", Scheme [ "a", "b" ] [] (TArrow (TVar "a") (TArrow (TVar "b") (TVar "a"))) )
        ]


builtinConstructors : Dict String Scheme
builtinConstructors =
    Dict.fromList
        [ ( "Just", Scheme [ "a" ] [] (TArrow (TVar "a") (TApp (TCon "Maybe") (TVar "a"))) )
        , ( "Nothing", Scheme [ "a" ] [] (TApp (TCon "Maybe") (TVar "a")) )
        , ( "Ok", Scheme [ "a", "b" ] [] (TArrow (TVar "b") (TApp (TApp (TCon "Result") (TVar "a")) (TVar "b"))) )
        , ( "Err", Scheme [ "a", "b" ] [] (TArrow (TVar "a") (TApp (TApp (TCon "Result") (TVar "a")) (TVar "b"))) )
        , ( "Cons", Scheme [ "a" ] [] (TArrow (TVar "a") (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "a")))) )
        , ( "Nil", Scheme [ "a" ] [] (TApp (TCon "List") (TVar "a")) )
        , ( "LT", Scheme [] [] (TCon "Order") )
        , ( "EQ", Scheme [] [] (TCon "Order") )
        , ( "GT", Scheme [] [] (TCon "Order") )
        , ( "True", Scheme [] [] (TCon "Bool") )
        , ( "False", Scheme [] [] (TCon "Bool") )
        ]


extendEnv : String -> Scheme -> Env -> Env
extendEnv name scheme env =
    { env | types = Dict.insert name scheme env.types }


lookupEnv : String -> Env -> Maybe Scheme
lookupEnv name env =
    Dict.get name env.types



-- INFERENCE STATE


type alias InferState =
    { nextVar : Int
    , substitution : Substitution
    , constraints : List Constraint
    }


initialState : InferState
initialState =
    { nextVar = 0
    , substitution = emptySubst
    , constraints = []
    }


{-| Generate a fresh type variable.
-}
freshTypeVar : Infer Type
freshTypeVar state =
    let
        name = "t" ++ String.fromInt state.nextVar
    in
    Ok ( TVar name, { state | nextVar = state.nextVar + 1 } )


{-| Generate multiple fresh type variables.
-}
freshTypeVars : Int -> Infer (List Type)
freshTypeVars n =
    if n <= 0 then
        pure []
    else do
        v <- freshTypeVar
        vs <- freshTypeVars (n - 1)
        pure (v :: vs)


{-| Add a constraint.
-}
addConstraint : Constraint -> Infer ()
addConstraint c = do
    modify (\s -> { s | constraints = c :: s.constraints })


{-| Apply substitution and add to state.
-}
applySubstToState : Substitution -> Infer ()
applySubstToState subst = do
    modify (\s -> { s | substitution = composeSubst subst s.substitution })



-- TYPE ERRORS


type TypeError
    = UnificationError Type Type
    | OccursCheck TypeVar Type
    | UnboundVariable String
    | UnboundConstructor String
    | UnboundTypeClass String
    | UnsatisfiedConstraint Constraint
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
                Err e -> Err e
                Ok s1 ->
                    case unifyMany (List.map (applySubst s1) rest1) (List.map (applySubst s1) rest2) of
                        Err e -> Err e
                        Ok s2 -> Ok (composeSubst s2 s1)

        _ ->
            Err (Other "Type list length mismatch")


unifyRecords : List ( String, Type ) -> Maybe TypeVar -> List ( String, Type ) -> Maybe TypeVar -> Result TypeError Substitution
unifyRecords fields1 row1 fields2 row2 =
    let
        dict1 = Dict.fromList fields1
        dict2 = Dict.fromList fields2

        keys1 = Set.fromList (Dict.keys dict1)
        keys2 = Set.fromList (Dict.keys dict2)

        commonKeys = Set.intersect keys1 keys2
        extra1 = Set.diff keys1 keys2
        extra2 = Set.diff keys2 keys1

        unifyCommonFields =
            commonKeys
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

        extraFields1 = extra1 |> Set.toList |> List.filterMap (\f -> Maybe.map (Tuple.pair f) (Dict.get f dict1))
        extraFields2 = extra2 |> Set.toList |> List.filterMap (\f -> Maybe.map (Tuple.pair f) (Dict.get f dict2))
    in
    case unifyCommonFields of
        Err e -> Err e
        Ok substCommon ->
            let
                hasExtra1 = not (Set.isEmpty extra1)
                hasExtra2 = not (Set.isEmpty extra2)
            in
            case ( row1, row2 ) of
                ( Nothing, Nothing ) ->
                    if hasExtra1 || hasExtra2 then
                        Err (Other "Record fields don't match")
                    else
                        Ok substCommon

                ( Nothing, Just r2 ) ->
                    if hasExtra2 then
                        Err (RecordFieldMissing (extra2 |> Set.toList |> List.head |> Maybe.withDefault "?"))
                    else
                        let
                            appliedExtra1 = List.map (\(n, t) -> (n, applySubst substCommon t)) extraFields1
                        in
                        case unify (TVar r2) (TRecord appliedExtra1 Nothing) of
                            Err e -> Err e
                            Ok s2 -> Ok (composeSubst s2 substCommon)

                ( Just r1, Nothing ) ->
                    if hasExtra1 then
                        Err (RecordFieldMissing (extra1 |> Set.toList |> List.head |> Maybe.withDefault "?"))
                    else
                        let
                            appliedExtra2 = List.map (\(n, t) -> (n, applySubst substCommon t)) extraFields2
                        in
                        case unify (TVar r1) (TRecord appliedExtra2 Nothing) of
                            Err e -> Err e
                            Ok s2 -> Ok (composeSubst s2 substCommon)

                ( Just r1, Just r2 ) ->
                    if r1 == r2 then
                        Ok substCommon
                    else
                        let
                            appliedExtra2 = List.map (\(n, t) -> (n, applySubst substCommon t)) extraFields2
                        in
                        case unify (TVar r1) (TRecord appliedExtra2 (Just r2)) of
                            Err e -> Err e
                            Ok s2 ->
                                let
                                    appliedExtra1 = List.map (\(n, t) -> (n, applySubst (composeSubst s2 substCommon) t)) extraFields1
                                    finalSubst = composeSubst s2 substCommon
                                in
                                if List.isEmpty appliedExtra1 then
                                    Ok finalSubst
                                else
                                    case unify (applySubst finalSubst (TVar r2)) (TRecord appliedExtra1 Nothing) of
                                        Err e -> Err e
                                        Ok s3 -> Ok (composeSubst s3 finalSubst)



-- INSTANTIATION AND GENERALIZATION


{-| Instantiate a type scheme with fresh type variables.
-}
instantiate : Scheme -> Infer ( Type, List Constraint )
instantiate (Scheme vars constraints ty) = do
    freshVars <- freshTypeVars (List.length vars)
    let subst = Dict.fromList (List.map2 Tuple.pair vars freshVars)
    pure ( applySubst subst ty
         , List.map (applySubstConstraint subst) constraints
         )


{-| Generalize a type to a type scheme.
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



-- INFERENCE (with do-notation)


{-| Infer the type of an expression.
-}
infer : Env -> AST.Expr -> Infer Type
infer env expr =
    case expr of
        AST.ELit lit ->
            pure (litType lit)

        AST.EVar qname ->
            inferVar env qname

        AST.EConstructor qname ->
            inferConstructor env qname

        AST.EApp func arg ->
            inferApp env func arg

        AST.ELambda patterns body ->
            inferLambda env patterns body

        AST.ELet bindings body ->
            inferLet env bindings body

        AST.EIf cond then_ else_ ->
            inferIf env cond then_ else_

        AST.ECase scrutinee branches ->
            inferCase env scrutinee branches

        AST.EBinOp left op right ->
            inferBinOp env left op right

        AST.ENegate inner ->
            infer env (AST.getValue inner)

        AST.ERecord fields ->
            inferRecord env fields

        AST.ERecordAccess record field ->
            inferRecordAccess env record field

        AST.ERecordAccessor field ->
            inferRecordAccessor field

        AST.ERecordUpdate name fields ->
            inferRecordUpdate env name fields

        AST.ETuple exprs ->
            inferTuple env exprs

        AST.EList exprs ->
            inferList env exprs

        AST.EUnit ->
            pure (TCon "()")

        AST.EParens inner ->
            infer env (AST.getValue inner)

        AST.EDo statements ->
            inferDo env statements


inferVar : Env -> AST.QualifiedName -> Infer Type
inferVar env qname = do
    let name =
            case qname.module_ of
                Nothing -> qname.name
                Just mod -> mod ++ "." ++ qname.name
    case lookupEnv name env of
        Nothing ->
            liftResult (Err (UnboundVariable name))

        Just scheme -> do
            ( ty, constraints ) <- instantiate scheme
            mapM addConstraint constraints
            pure ty


inferConstructor : Env -> AST.QualifiedName -> Infer Type
inferConstructor env qname = do
    let name = qname.name
    case Dict.get name env.constructors of
        Nothing ->
            liftResult (Err (UnboundConstructor name))

        Just scheme -> do
            ( ty, constraints ) <- instantiate scheme
            mapM addConstraint constraints
            pure ty


inferApp : Env -> AST.Located AST.Expr -> AST.Located AST.Expr -> Infer Type
inferApp env func arg = do
    funcTy <- infer env (AST.getValue func)
    argTy <- infer env (AST.getValue arg)
    retTy <- freshTypeVar
    let expectedFuncTy = TArrow argTy retTy
    subst <- liftResult (unify funcTy expectedFuncTy)
    applySubstToState subst
    pure (applySubst subst retTy)


inferLambda : Env -> List (AST.Located AST.Pattern) -> AST.Located AST.Expr -> Infer Type
inferLambda env patterns body =
    case patterns of
        [] ->
            infer env (AST.getValue body)

        pat :: restPats -> do
            argTy <- freshTypeVar
            let patBindings = patternBindings (AST.getValue pat) argTy
            let env1 = List.foldl (\( name, ty ) e -> extendEnv name (Scheme [] [] ty) e) env patBindings
            bodyTy <- inferLambda env1 restPats body
            pure (TArrow argTy bodyTy)


inferLet : Env -> List (AST.Located AST.LetBinding) -> AST.Located AST.Expr -> Infer Type
inferLet env bindings body =
    case bindings of
        [] ->
            infer env (AST.getValue body)

        binding :: rest -> do
            let letBind = AST.getValue binding
            bindTy <- infer env (AST.getValue letBind.value)
            state <- get
            let resolvedBindTy = applySubst state.substitution bindTy
            let patBinds = patternBindings (AST.getValue letBind.pattern) resolvedBindTy
            let env1 = List.foldl
                    (\( name, ty ) e ->
                        let scheme = generalize env state.constraints ty
                        in extendEnv name scheme e
                    )
                    env
                    patBinds
            inferLet env1 rest body


inferIf : Env -> AST.Located AST.Expr -> AST.Located AST.Expr -> AST.Located AST.Expr -> Infer Type
inferIf env cond then_ else_ = do
    condTy <- infer env (AST.getValue cond)
    subst1 <- liftResult (unify condTy (TCon "Bool"))
    applySubstToState subst1
    thenTy <- infer env (AST.getValue then_)
    elseTy <- infer env (AST.getValue else_)
    subst2 <- liftResult (unify thenTy elseTy)
    applySubstToState subst2
    pure (applySubst subst2 thenTy)


inferCase : Env -> AST.Located AST.Expr -> List (AST.Located AST.CaseBranch) -> Infer Type
inferCase env scrutinee branches = do
    scrutTy <- infer env (AST.getValue scrutinee)
    resultTy <- freshTypeVar
    inferBranches env scrutTy resultTy branches


inferBranches : Env -> Type -> Type -> List (AST.Located AST.CaseBranch) -> Infer Type
inferBranches env scrutTy resultTy branches =
    case branches of
        [] ->
            pure resultTy

        branch :: rest -> do
            let br = AST.getValue branch
            state <- get
            let resolvedScrutTy = applySubst state.substitution scrutTy
            ( patBinds, state0 ) <- pure (patternBindingsWithState env (AST.getValue br.pattern) resolvedScrutTy state)
            modify (\_ -> state0)
            let env1 = List.foldl (\( name, ty ) e -> extendEnv name (Scheme [] [] ty) e) env patBinds
            branchTy <- infer env1 (AST.getValue br.body)
            subst <- liftResult (unify resultTy branchTy)
            applySubstToState subst
            inferBranches env scrutTy (applySubst subst resultTy) rest


inferBinOp : Env -> AST.Located AST.Expr -> AST.Located String -> AST.Located AST.Expr -> Infer Type
inferBinOp env left op right = do
    let opVar = AST.EVar { module_ = Nothing, name = AST.getValue op }
    let app1 = AST.EApp (AST.locate (AST.getRegion op) opVar) left
    let app2 = AST.EApp (AST.locate (AST.getRegion right) app1) right
    infer env app2


inferRecord : Env -> List ( AST.Located AST.Name, AST.Located AST.Expr ) -> Infer Type
inferRecord env fields = do
    fieldTypes <- inferRecordFields env fields
    pure (TRecord fieldTypes Nothing)


inferRecordFields : Env -> List ( AST.Located AST.Name, AST.Located AST.Expr ) -> Infer (List ( String, Type ))
inferRecordFields env fields =
    case fields of
        [] ->
            pure []

        ( name, expr ) :: rest -> do
            ty <- infer env (AST.getValue expr)
            restFields <- inferRecordFields env rest
            pure (( AST.getValue name, ty ) :: restFields)


inferRecordAccess : Env -> AST.Located AST.Expr -> AST.Located AST.Name -> Infer Type
inferRecordAccess env record field = do
    recordTy <- infer env (AST.getValue record)
    fieldTy <- freshTypeVar
    rowVar <- freshTypeVar
    let rowVarName =
            case rowVar of
                TVar v -> v
                _ -> "r"
    let expectedTy = TRecord [ ( AST.getValue field, fieldTy ) ] (Just rowVarName)
    subst <- liftResult (unify recordTy expectedTy)
    applySubstToState subst
    pure (applySubst subst fieldTy)


inferRecordAccessor : AST.Located AST.Name -> Infer Type
inferRecordAccessor field = do
    fieldTy <- freshTypeVar
    rowVar <- freshTypeVar
    let rowVarName =
            case rowVar of
                TVar v -> v
                _ -> "r"
    let recordTy = TRecord [ ( AST.getValue field, fieldTy ) ] (Just rowVarName)
    pure (TArrow recordTy fieldTy)


inferRecordUpdate : Env -> AST.Located AST.Name -> List ( AST.Located AST.Name, AST.Located AST.Expr ) -> Infer Type
inferRecordUpdate env name fields = do
    case lookupEnv (AST.getValue name) env of
        Nothing ->
            liftResult (Err (UnboundVariable (AST.getValue name)))

        Just scheme -> do
            ( recordTy, constraints ) <- instantiate scheme
            mapM addConstraint constraints
            updateFields <- inferRecordFields env fields
            let fieldNames = List.map Tuple.first updateFields
            rowVar <- freshTypeVar
            let rowVarName =
                    case rowVar of
                        TVar v -> v
                        _ -> "r"
            let expectedTy = TRecord (List.map (\n -> (n, rowVar)) fieldNames) (Just rowVarName)
            subst <- liftResult (unify recordTy expectedTy)
            applySubstToState subst
            pure (applySubst subst recordTy)


inferTuple : Env -> List (AST.Located AST.Expr) -> Infer Type
inferTuple env exprs = do
    types <- mapM (\e -> infer env (AST.getValue e)) exprs
    pure (TTuple types)


inferList : Env -> List (AST.Located AST.Expr) -> Infer Type
inferList env exprs =
    case exprs of
        [] -> do
            elemTy <- freshTypeVar
            pure (TApp (TCon "List") elemTy)

        first :: rest -> do
            elemTy <- infer env (AST.getValue first)
            foldM (\accTy expr -> do
                ty <- infer env (AST.getValue expr)
                subst <- liftResult (unify accTy ty)
                applySubstToState subst
                pure (applySubst subst accTy)
            ) elemTy rest
            |> andThen (\finalElemTy -> pure (TApp (TCon "List") finalElemTy))


inferDo : Env -> List AST.DoStatement -> Infer Type
inferDo env statements =
    case statements of
        [] ->
            liftResult (Err (Other "Empty do block"))

        [ AST.DoExpr expr ] ->
            infer env (AST.getValue expr)

        stmt :: rest ->
            case stmt of
                AST.DoExpr expr -> do
                    _ <- infer env (AST.getValue expr)
                    inferDo env rest

                AST.DoBind pat expr -> do
                    exprTy <- infer env (AST.getValue expr)
                    state <- get
                    let resolvedTy = applySubst state.substitution exprTy
                    innerTy <- freshTypeVar
                    subst <- liftResult (unify resolvedTy (TApp (TCon "Monad") innerTy))
                    applySubstToState subst
                    let boundTy = applySubst subst innerTy
                    let patBinds = patternBindings (AST.getValue pat) boundTy
                    let env1 = List.foldl (\( name, ty ) e -> extendEnv name (Scheme [] [] ty) e) env patBinds
                    inferDo env1 rest

                AST.DoLet pat expr -> do
                    ty <- infer env (AST.getValue expr)
                    let patBinds = patternBindings (AST.getValue pat) ty
                    let env1 = List.foldl (\( name, ty_ ) e -> extendEnv name (Scheme [] [] ty_) e) env patBinds
                    inferDo env1 rest



-- PATTERN BINDINGS


patternBindings : AST.Pattern -> Type -> List ( String, Type )
patternBindings pat ty =
    case pat of
        AST.PVar name ->
            [ ( AST.getValue name, ty ) ]

        AST.PWildcard ->
            []

        AST.PLit _ ->
            []

        AST.PCtor _ args ->
            List.concatMap (\a -> patternBindings (AST.getValue a) ty) args

        AST.PTuple pats ->
            case pats of
                [ p1, p2 ] ->
                    case ty of
                        TTuple [ t1, t2 ] ->
                            patternBindings (AST.getValue p1) t1 ++
                            patternBindings (AST.getValue p2) t2
                        _ ->
                            []
                _ ->
                    []

        AST.PList pats ->
            case ty of
                TApp (TCon "List") elemTy ->
                    List.concatMap (\p -> patternBindings (AST.getValue p) elemTy) pats
                _ ->
                    []

        AST.PCons head tail ->
            case ty of
                TApp (TCon "List") elemTy ->
                    patternBindings (AST.getValue head) elemTy ++
                    patternBindings (AST.getValue tail) ty
                _ ->
                    []

        AST.PRecord fields ->
            List.map (\f -> ( AST.getValue f, ty )) fields

        AST.PUnit ->
            []

        AST.PAs name inner ->
            ( AST.getValue name, ty ) :: patternBindings (AST.getValue inner) ty

        AST.PParens inner ->
            patternBindings (AST.getValue inner) ty


patternBindingsWithState : Env -> AST.Pattern -> Type -> InferState -> ( List ( String, Type ), InferState )
patternBindingsWithState env pat ty state =
    case pat of
        AST.PVar name ->
            ( [ ( AST.getValue name, ty ) ], state )

        AST.PWildcard ->
            ( [], state )

        AST.PLit _ ->
            ( [], state )

        AST.PCtor ctorName args ->
            case Dict.get (AST.getValue ctorName) env.constructors of
                Nothing ->
                    ( [], state )

                Just scheme ->
                    let
                        ( ctorTy, _, state1 ) = instantiateOld scheme state
                        argTypes = getArgTypes ctorTy
                    in
                    List.foldl
                        (\( arg, argTy ) ( binds, s ) ->
                            let ( argBinds, s1 ) = patternBindingsWithState env (AST.getValue arg) argTy s
                            in ( binds ++ argBinds, s1 )
                        )
                        ( [], state1 )
                        (List.map2 Tuple.pair args argTypes)

        AST.PTuple pats ->
            case pats of
                [ p1, p2 ] ->
                    let
                        ( t1, state1 ) = freshTypeVarOld state
                        ( t2, state2 ) = freshTypeVarOld state1
                        ( binds1, state3 ) = patternBindingsWithState env (AST.getValue p1) t1 state2
                        ( binds2, state4 ) = patternBindingsWithState env (AST.getValue p2) t2 state3
                    in
                    case unify ty (TTuple [ t1, t2 ]) of
                        Ok subst ->
                            ( binds1 ++ binds2
                            , { state4 | substitution = composeSubst subst state4.substitution }
                            )
                        Err _ ->
                            ( binds1 ++ binds2, state4 )
                _ ->
                    ( [], state )

        AST.PList pats ->
            let
                ( elemTy, state1 ) = freshTypeVarOld state
            in
            case unify ty (TApp (TCon "List") elemTy) of
                Ok subst ->
                    let
                        state2 = { state1 | substitution = composeSubst subst state1.substitution }
                        resolvedElemTy = applySubst subst elemTy
                    in
                    List.foldl
                        (\p ( binds, s ) ->
                            let ( pBinds, s1 ) = patternBindingsWithState env (AST.getValue p) resolvedElemTy s
                            in ( binds ++ pBinds, s1 )
                        )
                        ( [], state2 )
                        pats
                Err _ ->
                    ( [], state1 )

        AST.PCons head tail ->
            let
                ( elemTy, state1 ) = freshTypeVarOld state
                listTy = TApp (TCon "List") elemTy
            in
            case unify ty listTy of
                Ok subst ->
                    let
                        state2 = { state1 | substitution = composeSubst subst state1.substitution }
                        resolvedElemTy = applySubst subst elemTy
                        resolvedListTy = applySubst subst listTy
                        ( headBinds, state3 ) = patternBindingsWithState env (AST.getValue head) resolvedElemTy state2
                        ( tailBinds, state4 ) = patternBindingsWithState env (AST.getValue tail) resolvedListTy state3
                    in
                    ( headBinds ++ tailBinds, state4 )
                Err _ ->
                    ( [], state1 )

        AST.PRecord fields ->
            ( List.map (\f -> ( AST.getValue f, ty )) fields, state )

        AST.PUnit ->
            ( [], state )

        AST.PAs name inner ->
            let
                ( innerBinds, state1 ) = patternBindingsWithState env (AST.getValue inner) ty state
            in
            ( ( AST.getValue name, ty ) :: innerBinds, state1 )

        AST.PParens inner ->
            patternBindingsWithState env (AST.getValue inner) ty state


getArgTypes : Type -> List Type
getArgTypes ty =
    case ty of
        TArrow arg rest ->
            arg :: getArgTypes rest
        _ ->
            []


-- Old-style helpers for compatibility
freshTypeVarOld : InferState -> ( Type, InferState )
freshTypeVarOld state =
    let
        name = "t" ++ String.fromInt state.nextVar
    in
    ( TVar name, { state | nextVar = state.nextVar + 1 } )


instantiateOld : Scheme -> InferState -> ( Type, List Constraint, InferState )
instantiateOld (Scheme vars constraints ty) state =
    let
        n = List.length vars
        names = List.map (\i -> "t" ++ String.fromInt (state.nextVar + i)) (List.range 0 (n - 1))
        freshVars = List.map TVar names
        state1 = { state | nextVar = state.nextVar + n }
        subst = Dict.fromList (List.map2 Tuple.pair vars freshVars)
    in
    ( applySubst subst ty
    , List.map (applySubstConstraint subst) constraints
    , state1
    )



-- TYPE CLASS CHECKING


resolveConstraint : Env -> Constraint -> Result TypeError ()
resolveConstraint env (IsIn className ty) =
    if hasInstance env className ty then
        Ok ()
    else
        Err (UnsatisfiedConstraint (IsIn className ty))


hasInstance : Env -> ClassName -> Type -> Bool
hasInstance env className ty =
    List.any (matchInstance ty) (List.filter (\i -> i.className == className) env.instances)


matchInstance : Type -> Instance -> Bool
matchInstance ty (Instance _ _ instTypes) =
    case instTypes of
        [ instTy ] ->
            matchType ty instTy
        _ ->
            False


matchType : Type -> Type -> Bool
matchType ty instTy =
    case ( ty, instTy ) of
        ( TCon a, TCon b ) -> a == b
        ( TVar _, _ ) -> True
        ( TApp a1 b1, TApp a2 b2 ) -> matchType a1 a2 && matchType b1 b2
        _ -> False


checkConstraints : Env -> List Constraint -> Result TypeError ()
checkConstraints env constraints =
    case constraints of
        [] -> Ok ()
        c :: rest ->
            case resolveConstraint env c of
                Err e -> Err e
                Ok () -> checkConstraints env rest


isComparable : Type -> Bool
isComparable ty =
    case ty of
        TCon "Int" -> True
        TCon "Float" -> True
        TCon "String" -> True
        TCon "Char" -> True
        TApp (TCon "List") inner -> isComparable inner
        TTuple tys -> List.all isComparable tys
        _ -> False


isAppendable : Type -> Bool
isAppendable ty =
    case ty of
        TCon "String" -> True
        TApp (TCon "List") _ -> True
        _ -> False


isNumber : Type -> Bool
isNumber ty =
    case ty of
        TCon "Int" -> True
        TCon "Float" -> True
        _ -> False



-- LITERAL TYPES


litType : AST.Lit -> Type
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
    let
        -- First, add type definitions
        env1 = addTypeDefinitions env module_.decls

        -- Then infer each declaration
        result =
            List.foldl
                (\decl acc ->
                    case acc of
                        Err e -> Err e
                        Ok ( e, state ) ->
                            inferDecl e (AST.getValue decl) state
                )
                (Ok ( env1, initialState ))
                module_.decls
    in
    Result.map Tuple.first result


addTypeDefinitions : Env -> List (AST.Located AST.Decl) -> Env
addTypeDefinitions env decls =
    List.foldl
        (\decl e ->
            case AST.getValue decl of
                AST.TypeAliasDecl alias_ ->
                    { e | typeAliases = Dict.insert (AST.getValue alias_.name)
                        ( List.map AST.getValue alias_.typeVars
                        , typeAnnotationToType (AST.getValue alias_.body)
                        )
                        e.typeAliases
                    }

                AST.CustomTypeDecl customType ->
                    addConstructors e (AST.getValue customType.name) customType.typeVars customType.constructors

                _ ->
                    e
        )
        env
        decls


addConstructors : Env -> String -> List (AST.Located String) -> List (AST.Located AST.Constructor) -> Env
addConstructors env typeName typeVars ctors =
    let
        typeVarNames = List.map AST.getValue typeVars
        resultType =
            List.foldl (\v t -> TApp t (TVar v)) (TCon typeName) typeVarNames
    in
    List.foldl
        (\ctor e ->
            let
                c = AST.getValue ctor
                ctorName = AST.getValue c.name
                argTypes = List.map (\a -> typeAnnotationToType (AST.getValue a)) c.args
                ctorType = List.foldr TArrow resultType argTypes
                scheme = Scheme typeVarNames [] ctorType
            in
            { e | constructors = Dict.insert ctorName scheme e.constructors }
        )
        env
        ctors


inferDecl : Env -> AST.Decl -> InferState -> Result TypeError ( Env, InferState )
inferDecl env decl state =
    case decl of
        AST.ValueDecl valueDef ->
            let
                name = AST.getValue valueDef.name
                body =
                    case valueDef.args of
                        [] -> AST.getValue valueDef.body
                        args -> AST.ELambda args valueDef.body
            in
            case infer env body state of
                Err e -> Err e
                Ok ( ty, state1 ) ->
                    let
                        resolvedTy = applySubst state1.substitution ty
                        scheme = generalize env state1.constraints resolvedTy
                    in
                    Ok ( extendEnv name scheme env, state1 )

        AST.TypeAliasDecl _ ->
            Ok ( env, state )

        AST.CustomTypeDecl _ ->
            Ok ( env, state )

        AST.ClassDecl _ ->
            Ok ( env, state )

        AST.InstanceDecl _ ->
            Ok ( env, state )

        AST.ForeignDecl foreign ->
            let
                name = AST.getValue foreign.name
                ty = typeAnnotationToType (AST.getValue foreign.type_)
                scheme = Scheme (Set.toList (freeTypeVars ty)) [] ty
            in
            Ok ( extendEnv name scheme env, state )

        AST.InfixDecl _ ->
            Ok ( env, state )


typeAnnotationToType : AST.TypeAnnotation -> Type
typeAnnotationToType ann =
    case ann of
        AST.TName name ->
            TCon (AST.getValue name)

        AST.TVar name ->
            TVar (AST.getValue name)

        AST.TApp con args ->
            List.foldl (\a t -> TApp t (typeAnnotationToType (AST.getValue a)))
                (typeAnnotationToType (AST.getValue con))
                args

        AST.TFunction arg result ->
            TArrow (typeAnnotationToType (AST.getValue arg)) (typeAnnotationToType (AST.getValue result))

        AST.TTuple elements ->
            TTuple (List.map (\e -> typeAnnotationToType (AST.getValue e)) elements)

        AST.TRecord fields maybeRowVar ->
            TRecord
                (List.map (\( n, t ) -> ( AST.getValue n, typeAnnotationToType (AST.getValue t) )) fields)
                (Maybe.map AST.getValue maybeRowVar)

        AST.TUnit ->
            TCon "()"

        AST.TParens inner ->
            typeAnnotationToType (AST.getValue inner)
