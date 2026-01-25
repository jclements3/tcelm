module Types exposing
    ( Type(..)
    , Scheme(..)
    , Kind(..)
    , TypeClass(..)
    , Instance(..)
    , Constraint(..)
    , TypeVar
    , TyConName
    , ClassName
    , freeTypeVars
    , substituteType
    , occurs
    , kindOf
    )

{-| Core type system types.

This module defines the type language for tcelm, including:
- Types (monotypes)
- Type schemes (polytypes with forall)
- Kinds (types of types, for HKTs)
- Type classes and instances
- Constraints

The design supports:
- Hindley-Milner type inference
- Higher-kinded types (for Functor, Monad, etc.)
- Type classes (for overloading)
- Future: Effect types, linear types
-}

import Set exposing (Set)
import Dict exposing (Dict)


-- TYPE VARIABLES AND NAMES


{-| Type variable name (e.g., "a", "b", "m")
-}
type alias TypeVar =
    String


{-| Type constructor name (e.g., "Int", "List", "Maybe")
-}
type alias TyConName =
    String


{-| Type class name (e.g., "Eq", "Functor", "Monad")
-}
type alias ClassName =
    String



-- KINDS (Types of Types)


{-| Kinds classify types.

    * : the kind of concrete types (Int, String, Maybe Int)
    * -> * : the kind of type constructors (Maybe, List)
    * -> * -> * : binary type constructors (Either, Tuple)

Examples:
    Int : *
    Maybe : * -> *
    Either : * -> * -> *
    Functor : (* -> *) -> Constraint
-}
type Kind
    = KStar                     -- * (concrete type)
    | KArrow Kind Kind          -- k1 -> k2 (type constructor)
    | KConstraint               -- Constraint (for type classes)



-- TYPES (Monotypes)


{-| The type language.

TVar: Type variable (a, b, m)
TCon: Type constructor (Int, String, Bool)
TApp: Type application (Maybe Int, List a, m a)
TArrow: Function type (a -> b), sugar for TApp (TApp Arrow a) b
TRecord: Record type { name : String, age : Int }
TTuple: Tuple type (a, b) or (a, b, c)
TForall: Explicit forall (used after generalization)
-}
type Type
    = TVar TypeVar
    | TCon TyConName
    | TApp Type Type
    | TArrow Type Type
    | TRecord (List ( String, Type )) (Maybe TypeVar)  -- fields + optional row variable
    | TTuple (List Type)
    | TForall (List TypeVar) (List Constraint) Type    -- forall a b. (C a, D b) => t



-- TYPE SCHEMES (Polytypes)


{-| A type scheme is a type with universally quantified variables.

    Scheme ["a"] [] (TArrow (TVar "a") (TVar "a"))
    represents: forall a. a -> a

    Scheme ["a"] [IsIn "Eq" (TVar "a")] (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Bool")))
    represents: forall a. Eq a => a -> a -> Bool
-}
type Scheme
    = Scheme (List TypeVar) (List Constraint) Type



-- TYPE CLASSES


{-| A type class constraint.

    IsIn "Eq" (TVar "a")        -- Eq a
    IsIn "Functor" (TVar "f")   -- Functor f (where f : * -> *)
    IsIn "Monad" (TVar "m")     -- Monad m
-}
type Constraint
    = IsIn ClassName Type


{-| A type class definition.

    class Eq a where
        eq : a -> a -> Bool
        neq : a -> a -> Bool

Becomes:
    TypeClass "Eq" ["a"] [KStar]
        [ ("eq", Scheme ["a"] [IsIn "Eq" (TVar "a")] (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Bool"))))
        , ("neq", Scheme ["a"] [IsIn "Eq" (TVar "a")] (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Bool"))))
        ]
        []  -- no superclasses

    class Eq a => Ord a where ...

Has superclass [IsIn "Eq" (TVar "a")]
-}
type TypeClass
    = TypeClass
        ClassName                           -- class name
        (List TypeVar)                      -- type parameters
        (List Kind)                         -- kinds of parameters
        (List ( String, Scheme ))           -- method signatures
        (List Constraint)                   -- superclass constraints


{-| A type class instance.

    instance Eq Int where
        eq = primEqInt
        neq x y = not (eq x y)

Becomes:
    Instance [] [IsIn "Eq" (TCon "Int")]
        [ ("eq", expr for primEqInt)
        , ("neq", expr for lambda)
        ]

    instance Eq a => Eq (List a) where ...

Has context [IsIn "Eq" (TVar "a")]
-}
type Instance
    = Instance
        (List TypeVar)                      -- universally quantified vars
        (List Constraint)                   -- instance context (prerequisites)
        Constraint                          -- the instance head (what we're implementing)
        (List ( String, () ))               -- method implementations (placeholder for Expr)



-- TYPE OPERATIONS


{-| Get free type variables in a type.
-}
freeTypeVars : Type -> Set TypeVar
freeTypeVars ty =
    case ty of
        TVar v ->
            Set.singleton v

        TCon _ ->
            Set.empty

        TApp t1 t2 ->
            Set.union (freeTypeVars t1) (freeTypeVars t2)

        TArrow t1 t2 ->
            Set.union (freeTypeVars t1) (freeTypeVars t2)

        TRecord fields maybeRow ->
            let
                fieldVars =
                    fields
                        |> List.map (\( _, t ) -> freeTypeVars t)
                        |> List.foldl Set.union Set.empty

                rowVar =
                    case maybeRow of
                        Just v -> Set.singleton v
                        Nothing -> Set.empty
            in
            Set.union fieldVars rowVar

        TTuple types ->
            types
                |> List.map freeTypeVars
                |> List.foldl Set.union Set.empty

        TForall vars _ body ->
            Set.diff (freeTypeVars body) (Set.fromList vars)


{-| Substitute type variables in a type.
-}
substituteType : Dict TypeVar Type -> Type -> Type
substituteType subst ty =
    case ty of
        TVar v ->
            Dict.get v subst |> Maybe.withDefault ty

        TCon _ ->
            ty

        TApp t1 t2 ->
            TApp (substituteType subst t1) (substituteType subst t2)

        TArrow t1 t2 ->
            TArrow (substituteType subst t1) (substituteType subst t2)

        TRecord fields maybeRow ->
            let
                newFields =
                    List.map (\( name, t ) -> ( name, substituteType subst t )) fields

                newRow =
                    case maybeRow of
                        Just v ->
                            case Dict.get v subst of
                                Just (TVar v2) -> Just v2
                                _ -> maybeRow
                        Nothing ->
                            Nothing
            in
            TRecord newFields newRow

        TTuple types ->
            TTuple (List.map (substituteType subst) types)

        TForall vars constraints body ->
            -- Remove bound variables from substitution
            let
                subst2 =
                    List.foldl Dict.remove subst vars
            in
            TForall vars constraints (substituteType subst2 body)


{-| Occurs check: does variable v occur in type t?
Used to prevent infinite types like a = List a
-}
occurs : TypeVar -> Type -> Bool
occurs v ty =
    Set.member v (freeTypeVars ty)


{-| Get the kind of a type (simplified - assumes all type constructors have known kinds)
-}
kindOf : Type -> Kind
kindOf ty =
    case ty of
        TVar _ ->
            KStar  -- default, should be looked up in kind environment

        TCon name ->
            -- Built-in kinds
            case name of
                "Int" -> KStar
                "Float" -> KStar
                "String" -> KStar
                "Bool" -> KStar
                "Char" -> KStar
                "List" -> KArrow KStar KStar
                "Maybe" -> KArrow KStar KStar
                "Result" -> KArrow KStar (KArrow KStar KStar)
                "IO" -> KArrow KStar KStar
                "Task" -> KArrow KStar (KArrow KStar KStar)
                _ -> KStar  -- default

        TApp t1 _ ->
            -- Result of application: if t1 : k1 -> k2, then t1 t2 : k2
            case kindOf t1 of
                KArrow _ k2 -> k2
                _ -> KStar

        TArrow _ _ ->
            KStar

        TRecord _ _ ->
            KStar

        TTuple _ ->
            KStar

        TForall _ _ body ->
            kindOf body
