module Core exposing
    ( Module
    , Decl(..)
    , Expr(..)
    , Alt(..)
    , Pattern(..)
    , Literal(..)
    , Var
    , DataCon
    , TypedVar
    , FuncDef
    , DataDef
    , DataConDef
    , ClassDef
    , InstDef
    , exprType
    , freeVars
    , substitute
    )

{-| Core IR (Intermediate Representation).

Core is a small, explicitly-typed lambda calculus that serves as the
intermediate representation between the source AST and code generation.

Design goals:
- Simple enough for optimization passes
- Explicit types everywhere (no inference needed after this point)
- Explicit dictionary passing for type classes
- No syntactic sugar (do-notation, operators desugared)

Based on GHC's Core but simplified for embedded targets.
-}

import Types exposing (Type, Scheme, Constraint)
import Set exposing (Set)
import Dict exposing (Dict)


-- IDENTIFIERS


{-| A variable name
-}
type alias Var =
    String


{-| A data constructor name (e.g., "Just", "Cons", "True")
-}
type alias DataCon =
    String


{-| A variable with its type
-}
type alias TypedVar =
    { name : Var
    , type_ : Type
    }



-- MODULE


type alias Module =
    { name : String
    , decls : List Decl
    }



-- DECLARATIONS


type Decl
    = FuncDecl FuncDef
    | DataDecl DataDef
    | ClassDecl ClassDef
    | InstDecl InstDef


type alias FuncDef =
    { name : Var
    , type_ : Scheme
    , args : List TypedVar
    , body : Expr
    }


type alias DataDef =
    { name : String
    , typeVars : List String
    , constructors : List DataConDef
    }


type alias DataConDef =
    { name : DataCon
    , tag : Int              -- runtime tag for pattern matching
    , fields : List Type     -- types of constructor fields
    }


type alias ClassDef =
    { name : String
    , typeVars : List String
    , superclasses : List Constraint
    , methods : List ( Var, Scheme )
    }


type alias InstDef =
    { className : String
    , typeArgs : List Type
    , context : List Constraint
    , methods : List ( Var, Expr )
    }



-- EXPRESSIONS


{-| Core expressions.

All expressions are explicitly typed. Type class dictionaries are
explicit lambda arguments.

Design choices:
- EVar: Variables (includes dictionary variables)
- ELit: Literals
- EApp: Application (always single argument, curried)
- ELam: Lambda (always single argument)
- ELet: Let binding (non-recursive by default)
- ELetRec: Recursive let
- ECase: Pattern matching
- ECon: Data constructor application
- ETyApp: Type application (for polymorphism)
- ETyLam: Type abstraction (big lambda)
- EDictApp: Dictionary application (for type classes)
- EDictLam: Dictionary abstraction
- EDict: Dictionary literal (for instances)
-}
type Expr
    = EVar TypedVar
    | ELit Literal Type
    | EApp Expr Expr Type                              -- f x : result_type
    | ELam TypedVar Expr Type                          -- \x -> e : type
    | ELet Var Expr Expr Type                          -- let x = e1 in e2 : type
    | ELetRec (List ( Var, Expr )) Expr Type           -- let rec ... in e : type
    | ECase Expr (List Alt) Type                       -- case e of alts : type
    | ECon DataCon (List Expr) Type                    -- Constructor arg1 arg2 : type
    | ETuple (List Expr) Type                          -- (e1, e2, ...) : type
    | ERecord (List ( String, Expr )) Type             -- { x = e1, y = e2 } : type
    | ERecordAccess Expr String Type                   -- e.field : type
    | ERecordUpdate Expr (List ( String, Expr )) Type  -- { e | x = e1 } : type
    -- For polymorphism
    | ETyApp Expr Type Type                            -- e @t : result_type
    | ETyLam String Expr Type                          -- /\a -> e : forall a. type
    -- For type classes
    | EDictApp Expr Expr Type                          -- e dict : result_type
    | EDictLam TypedVar Expr Type                      -- \dict -> e : type
    | EDict String (List Expr) Type                    -- dictionary for instance


{-| Case alternatives.

Alt pattern guard? body
-}
type Alt
    = Alt Pattern (Maybe Expr) Expr



-- PATTERNS


type Pattern
    = PVar TypedVar
    | PWildcard Type
    | PLit Literal Type
    | PCon DataCon (List Pattern) Type
    | PTuple (List Pattern) Type
    | PRecord (List ( String, Pattern )) Type
    | PAlias Pattern String Type  -- inner pattern, alias name, type



-- LITERALS


type Literal
    = LInt Int
    | LFloat Float
    | LString String
    | LChar Char



-- UTILITY FUNCTIONS


{-| Get the type of an expression.
All Core expressions are explicitly typed, so this is O(1).
-}
exprType : Expr -> Type
exprType expr =
    case expr of
        EVar tv -> tv.type_
        ELit _ t -> t
        EApp _ _ t -> t
        ELam _ _ t -> t
        ELet _ _ _ t -> t
        ELetRec _ _ t -> t
        ECase _ _ t -> t
        ECon _ _ t -> t
        ETuple _ t -> t
        ERecord _ t -> t
        ERecordAccess _ _ t -> t
        ERecordUpdate _ _ t -> t
        ETyApp _ _ t -> t
        ETyLam _ _ t -> t
        EDictApp _ _ t -> t
        EDictLam _ _ t -> t
        EDict _ _ t -> t


{-| Get free variables in an expression.
-}
freeVars : Expr -> Set Var
freeVars expr =
    case expr of
        EVar tv ->
            Set.singleton tv.name

        ELit _ _ ->
            Set.empty

        EApp e1 e2 _ ->
            Set.union (freeVars e1) (freeVars e2)

        ELam tv body _ ->
            Set.remove tv.name (freeVars body)

        ELet v e1 e2 _ ->
            Set.union (freeVars e1) (Set.remove v (freeVars e2))

        ELetRec bindings body _ ->
            let
                boundVars =
                    List.map (\( v, _ ) -> v) bindings
                        |> Set.fromList

                bodyVars =
                    bindings
                        |> List.map (\( _, e ) -> freeVars e)
                        |> List.foldl Set.union (freeVars body)
            in
            Set.diff bodyVars boundVars

        ECase scrutinee alts _ ->
            let
                altVars =
                    List.map freeVarsAlt alts
                        |> List.foldl Set.union Set.empty
            in
            Set.union (freeVars scrutinee) altVars

        ECon _ args _ ->
            args
                |> List.map freeVars
                |> List.foldl Set.union Set.empty

        ETuple exprs _ ->
            exprs
                |> List.map freeVars
                |> List.foldl Set.union Set.empty

        ERecord fields _ ->
            fields
                |> List.map (\( _, e ) -> freeVars e)
                |> List.foldl Set.union Set.empty

        ERecordAccess e _ _ ->
            freeVars e

        ERecordUpdate e fields _ ->
            let
                fieldVars =
                    fields
                        |> List.map (\( _, fe ) -> freeVars fe)
                        |> List.foldl Set.union Set.empty
            in
            Set.union (freeVars e) fieldVars

        ETyApp e _ _ ->
            freeVars e

        ETyLam _ e _ ->
            freeVars e

        EDictApp e1 e2 _ ->
            Set.union (freeVars e1) (freeVars e2)

        EDictLam tv e _ ->
            Set.remove tv.name (freeVars e)

        EDict _ args _ ->
            args
                |> List.map freeVars
                |> List.foldl Set.union Set.empty


freeVarsAlt : Alt -> Set Var
freeVarsAlt (Alt pat guard body) =
    let
        patVars =
            patternVars pat

        guardVars =
            guard
                |> Maybe.map freeVars
                |> Maybe.withDefault Set.empty

        bodyVars =
            freeVars body
    in
    Set.diff (Set.union guardVars bodyVars) patVars


patternVars : Pattern -> Set Var
patternVars pat =
    case pat of
        PVar tv ->
            Set.singleton tv.name

        PWildcard _ ->
            Set.empty

        PLit _ _ ->
            Set.empty

        PCon _ pats _ ->
            pats
                |> List.map patternVars
                |> List.foldl Set.union Set.empty

        PTuple pats _ ->
            pats
                |> List.map patternVars
                |> List.foldl Set.union Set.empty

        PRecord fields _ ->
            fields
                |> List.map (\( _, p ) -> patternVars p)
                |> List.foldl Set.union Set.empty

        PAlias inner alias_ _ ->
            Set.insert alias_ (patternVars inner)


{-| Substitute a variable with an expression.
-}
substitute : Var -> Expr -> Expr -> Expr
substitute var replacement expr =
    case expr of
        EVar tv ->
            if tv.name == var then
                replacement
            else
                expr

        ELit _ _ ->
            expr

        EApp e1 e2 t ->
            EApp (substitute var replacement e1) (substitute var replacement e2) t

        ELam tv body t ->
            if tv.name == var then
                expr  -- shadowed
            else
                ELam tv (substitute var replacement body) t

        ELet v e1 e2 t ->
            let
                newE1 = substitute var replacement e1
            in
            if v == var then
                ELet v newE1 e2 t  -- shadowed in e2
            else
                ELet v newE1 (substitute var replacement e2) t

        ELetRec bindings body t ->
            let
                boundVars = List.map (\( v, _ ) -> v) bindings
            in
            if List.member var boundVars then
                expr  -- shadowed
            else
                ELetRec
                    (List.map (\( v, e ) -> ( v, substitute var replacement e )) bindings)
                    (substitute var replacement body)
                    t

        ECase scrutinee alts t ->
            ECase
                (substitute var replacement scrutinee)
                (List.map (substituteAlt var replacement) alts)
                t

        ECon dc args t ->
            ECon dc (List.map (substitute var replacement) args) t

        ETuple exprs t ->
            ETuple (List.map (substitute var replacement) exprs) t

        ERecord fields t ->
            ERecord (List.map (\( n, e ) -> ( n, substitute var replacement e )) fields) t

        ERecordAccess e field t ->
            ERecordAccess (substitute var replacement e) field t

        ERecordUpdate e fields t ->
            ERecordUpdate
                (substitute var replacement e)
                (List.map (\( n, fe ) -> ( n, substitute var replacement fe )) fields)
                t

        ETyApp e ty t ->
            ETyApp (substitute var replacement e) ty t

        ETyLam a e t ->
            ETyLam a (substitute var replacement e) t

        EDictApp e1 e2 t ->
            EDictApp (substitute var replacement e1) (substitute var replacement e2) t

        EDictLam tv e t ->
            if tv.name == var then
                expr
            else
                EDictLam tv (substitute var replacement e) t

        EDict cls args t ->
            EDict cls (List.map (substitute var replacement) args) t


substituteAlt : Var -> Expr -> Alt -> Alt
substituteAlt var replacement (Alt pat guard body) =
    if Set.member var (patternVars pat) then
        Alt pat guard body  -- shadowed
    else
        Alt pat
            (Maybe.map (substitute var replacement) guard)
            (substitute var replacement body)
