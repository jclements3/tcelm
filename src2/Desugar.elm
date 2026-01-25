module Desugar exposing
    ( desugarModule
    , desugarExpr
    )

{-| Desugar source AST to Core IR.

This module transforms the high-level source AST into the simpler Core IR.
Transformations include:
- Pattern matching compilation
- do-notation desugaring to andThen calls
- Operator desugaring to function applications
- Record accessor desugaring
- Type class dictionary passing insertion
- Lambda lifting preparation
-}

import AST
import Core
import Types exposing (Type(..), Scheme(..), Constraint(..))
import Dict exposing (Dict)


-- CONTEXT


type alias DesugarCtx =
    { types : Dict String Type      -- Variable types from inference
    , constructors : Dict String ConstructorInfo
    , currentModule : String
    , freshId : Int
    }


type alias ConstructorInfo =
    { tag : Int
    , arity : Int
    , type_ : Type
    }


emptyCtx : DesugarCtx
emptyCtx =
    { types = Dict.empty
    , constructors = Dict.empty
    , currentModule = "Main"
    , freshId = 0
    }


freshVar : String -> DesugarCtx -> ( String, DesugarCtx )
freshVar prefix ctx =
    ( prefix ++ "_" ++ String.fromInt ctx.freshId
    , { ctx | freshId = ctx.freshId + 1 }
    )



-- MODULE DESUGARING


desugarModule : AST.Module -> Core.Module
desugarModule ast =
    let
        moduleName =
            ast.name
                |> Maybe.map AST.getValue
                |> Maybe.withDefault "Main"

        ctx =
            { emptyCtx | currentModule = moduleName }

        decls =
            ast.decls
                |> List.filterMap (\loc -> desugarDecl ctx (AST.getValue loc))
    in
    { name = moduleName
    , decls = decls
    }



-- DECLARATION DESUGARING


desugarDecl : DesugarCtx -> AST.Decl -> Maybe Core.Decl
desugarDecl ctx decl =
    case decl of
        AST.ValueDecl valueDef ->
            Just (Core.FuncDecl (desugarValueDef ctx valueDef))

        AST.CustomTypeDecl typeDef ->
            Just (Core.DataDecl (desugarCustomType ctx typeDef))

        AST.TypeClassDecl classDef ->
            Just (Core.ClassDecl (desugarTypeClass ctx classDef))

        AST.InstanceDecl instDef ->
            Just (Core.InstDecl (desugarInstance ctx instDef))

        AST.TypeAliasDecl _ ->
            -- Type aliases are erased at this stage
            Nothing

        AST.PortDecl _ ->
            -- Ports become foreign imports
            Nothing

        AST.InfixDecl _ ->
            -- Infix declarations are used during parsing only
            Nothing

        AST.ForeignDecl foreignDef ->
            Just (Core.FuncDecl (desugarForeign ctx foreignDef))


desugarValueDef : DesugarCtx -> AST.ValueDef -> Core.FuncDef
desugarValueDef ctx def =
    let
        name = AST.getValue def.name

        -- Convert patterns to typed variables
        ( args, ctx1 ) =
            List.foldl
                (\locPat ( acc, c ) ->
                    let
                        ( typedVar, c1 ) = patternToTypedVar c (AST.getValue locPat)
                    in
                    ( acc ++ [ typedVar ], c1 )
                )
                ( [], ctx )
                def.args

        body = desugarExpr ctx1 (AST.getValue def.body)
    in
    { name = name
    , type_ = Scheme [] [] (TCon "TODO")  -- Type comes from inference
    , args = args
    , body = body
    }


desugarCustomType : DesugarCtx -> AST.CustomTypeDef -> Core.DataDef
desugarCustomType _ typeDef =
    let
        name = AST.getValue typeDef.name
        typeVars = List.map AST.getValue typeDef.typeVars

        constructors =
            typeDef.constructors
                |> List.indexedMap (\i locCtor ->
                    let
                        ctor = AST.getValue locCtor
                    in
                    { name = AST.getValue ctor.name
                    , tag = i
                    , fields = List.map (\_ -> TCon "TODO") ctor.args
                    }
                )
    in
    { name = name
    , typeVars = typeVars
    , constructors = constructors
    }


desugarTypeClass : DesugarCtx -> AST.TypeClassDef -> Core.ClassDef
desugarTypeClass _ classDef =
    let
        name = AST.getValue classDef.name
        typeVars = List.map AST.getValue classDef.typeVars

        superclasses =
            classDef.superclasses
                |> List.map (\loc ->
                    let
                        c = AST.getValue loc
                    in
                    IsIn (AST.getValue c.className) (TVar "a")
                )

        methods =
            classDef.methods
                |> List.map (\loc ->
                    let
                        m = AST.getValue loc
                    in
                    ( AST.getValue m.name, Scheme [] [] (TCon "TODO") )
                )
    in
    { name = name
    , typeVars = typeVars
    , superclasses = superclasses
    , methods = methods
    }


desugarInstance : DesugarCtx -> AST.InstanceDef -> Core.InstDef
desugarInstance ctx instDef =
    let
        className = AST.getValue instDef.className

        context =
            instDef.context
                |> List.map (\loc ->
                    let
                        c = AST.getValue loc
                    in
                    IsIn (AST.getValue c.className) (TVar "a")
                )

        typeArgs =
            instDef.typeArgs
                |> List.map (\_ -> TCon "TODO")

        methods =
            instDef.methods
                |> List.map (\loc ->
                    let
                        m = AST.getValue loc
                        body = desugarExpr ctx (AST.getValue m.body)
                    in
                    ( AST.getValue m.name, body )
                )
    in
    { className = className
    , typeArgs = typeArgs
    , context = context
    , methods = methods
    }


desugarForeign : DesugarCtx -> AST.ForeignDef -> Core.FuncDef
desugarForeign _ def =
    let
        name = AST.getValue def.name
    in
    { name = name
    , type_ = Scheme [] [] (TCon "Foreign")
    , args = []
    , body = Core.EVar { name = def.cName, type_ = TCon "Foreign" }
    }



-- EXPRESSION DESUGARING


desugarExpr : DesugarCtx -> AST.Expr -> Core.Expr
desugarExpr ctx expr =
    case expr of
        AST.ELit lit ->
            Core.ELit (desugarLiteral lit) (litType lit)

        AST.EVar qname ->
            Core.EVar
                { name = qualNameToString qname
                , type_ = Dict.get qname.name ctx.types |> Maybe.withDefault (TVar "a")
                }

        AST.EConstructor qname ->
            Core.ECon (qualNameToString qname) [] (TVar "a")

        AST.EApp func arg ->
            let
                funcCore = desugarExpr ctx (AST.getValue func)
                argCore = desugarExpr ctx (AST.getValue arg)
                resultType = TVar "r"  -- Would come from type inference
            in
            Core.EApp funcCore argCore resultType

        AST.ELambda patterns body ->
            desugarLambda ctx patterns body

        AST.ELet bindings body ->
            desugarLet ctx bindings body

        AST.EIf cond then_ else_ ->
            desugarIf ctx cond then_ else_

        AST.ECase scrutinee branches ->
            desugarCase ctx scrutinee branches

        AST.EBinOp left op right ->
            desugarBinOp ctx left op right

        AST.ENegate inner ->
            let
                innerCore = desugarExpr ctx (AST.getValue inner)
                negateVar = Core.EVar { name = "negate", type_ = TArrow (TCon "Int") (TCon "Int") }
            in
            Core.EApp negateVar innerCore (TCon "Int")

        AST.ERecord fields ->
            let
                fieldCores =
                    fields
                        |> List.map (\( name, value ) ->
                            ( AST.getValue name, desugarExpr ctx (AST.getValue value) )
                        )
            in
            Core.ERecord fieldCores (TRecord (List.map (\( n, _ ) -> ( n, TVar "a" )) fieldCores) Nothing)

        AST.ERecordAccess record field ->
            let
                recordCore = desugarExpr ctx (AST.getValue record)
            in
            Core.ERecordAccess recordCore (AST.getValue field) (TVar "a")

        AST.ERecordAccessor field ->
            -- .field becomes \r -> r.field
            let
                ( varName, _ ) = freshVar "r" ctx
                recordVar = { name = varName, type_ = TRecord [ ( AST.getValue field, TVar "a" ) ] Nothing }
            in
            Core.ELam
                recordVar
                (Core.ERecordAccess (Core.EVar recordVar) (AST.getValue field) (TVar "a"))
                (TArrow recordVar.type_ (TVar "a"))

        AST.ERecordUpdate name fields ->
            let
                recordVar = { name = AST.getValue name, type_ = TVar "r" }
                fieldCores =
                    fields
                        |> List.map (\( fn, fv ) ->
                            ( AST.getValue fn, desugarExpr ctx (AST.getValue fv) )
                        )
            in
            Core.ERecordUpdate (Core.EVar recordVar) fieldCores (TVar "r")

        AST.ETuple exprs ->
            let
                exprCores = List.map (\e -> desugarExpr ctx (AST.getValue e)) exprs
            in
            Core.ETuple exprCores (TTuple (List.map (\_ -> TVar "a") exprCores))

        AST.EList exprs ->
            desugarList ctx exprs

        AST.EUnit ->
            Core.ECon "Unit" [] (TCon "Unit")

        AST.EParens inner ->
            desugarExpr ctx (AST.getValue inner)

        AST.EDo statements ->
            desugarDo ctx statements



-- LAMBDA DESUGARING


desugarLambda : DesugarCtx -> List (AST.Located AST.Pattern) -> AST.Located AST.Expr -> Core.Expr
desugarLambda ctx patterns body =
    case patterns of
        [] ->
            desugarExpr ctx (AST.getValue body)

        pat :: rest ->
            let
                ( typedVar, ctx1 ) = patternToTypedVar ctx (AST.getValue pat)
                innerBody = desugarLambda ctx1 rest body
                resultType = Core.exprType innerBody
            in
            Core.ELam typedVar innerBody (TArrow typedVar.type_ resultType)



-- LET DESUGARING


desugarLet : DesugarCtx -> List (AST.Located AST.LetBinding) -> AST.Located AST.Expr -> Core.Expr
desugarLet ctx bindings body =
    case bindings of
        [] ->
            desugarExpr ctx (AST.getValue body)

        binding :: rest ->
            let
                bind = AST.getValue binding
                bindName = patternName (AST.getValue bind.pattern)
                bindExpr = desugarExpr ctx (AST.getValue bind.value)
                innerExpr = desugarLet ctx rest body
            in
            Core.ELet bindName bindExpr innerExpr (Core.exprType innerExpr)



-- IF DESUGARING (to case)


desugarIf : DesugarCtx -> AST.Located AST.Expr -> AST.Located AST.Expr -> AST.Located AST.Expr -> Core.Expr
desugarIf ctx cond then_ else_ =
    let
        condCore = desugarExpr ctx (AST.getValue cond)
        thenCore = desugarExpr ctx (AST.getValue then_)
        elseCore = desugarExpr ctx (AST.getValue else_)
        resultType = Core.exprType thenCore

        trueAlt = Core.Alt (Core.PCon "True" [] (TCon "Bool")) Nothing thenCore
        falseAlt = Core.Alt (Core.PCon "False" [] (TCon "Bool")) Nothing elseCore
    in
    Core.ECase condCore [ trueAlt, falseAlt ] resultType



-- CASE DESUGARING


desugarCase : DesugarCtx -> AST.Located AST.Expr -> List (AST.Located AST.CaseBranch) -> Core.Expr
desugarCase ctx scrutinee branches =
    let
        scrutCore = desugarExpr ctx (AST.getValue scrutinee)

        alts =
            branches
                |> List.map (\loc ->
                    let
                        br = AST.getValue loc
                        pat = desugarPattern ctx (AST.getValue br.pattern)
                        guard = Maybe.map (\g -> desugarExpr ctx (AST.getValue g)) br.guard
                        body = desugarExpr ctx (AST.getValue br.body)
                    in
                    Core.Alt pat guard body
                )

        resultType =
            case alts of
                (Core.Alt _ _ body) :: _ -> Core.exprType body
                [] -> TVar "a"
    in
    Core.ECase scrutCore alts resultType



-- BINARY OPERATOR DESUGARING


desugarBinOp : DesugarCtx -> AST.Located AST.Expr -> AST.Located AST.Name -> AST.Located AST.Expr -> Core.Expr
desugarBinOp ctx left op right =
    let
        opName = AST.getValue op
        leftCore = desugarExpr ctx (AST.getValue left)
        rightCore = desugarExpr ctx (AST.getValue right)
    in
    case opName of
        -- Pipeline operators desugar directly to function application
        "|>" ->
            -- a |> f  ==>  f a
            Core.EApp rightCore leftCore (TVar "a")

        "<|" ->
            -- f <| a  ==>  f a
            Core.EApp leftCore rightCore (TVar "a")

        _ ->
            -- Other operators: generate function call
            let
                ( opFuncName, resultType ) =
                    case opName of
                        "+" -> ( "add", TCon "Int" )
                        "-" -> ( "sub", TCon "Int" )
                        "*" -> ( "mul", TCon "Int" )
                        "/" -> ( "div", TCon "Int" )
                        "//" -> ( "intDiv", TCon "Int" )
                        "^" -> ( "pow", TCon "Int" )
                        "%" -> ( "mod", TCon "Int" )
                        "==" -> ( "eq", TCon "Bool" )
                        "/=" -> ( "neq", TCon "Bool" )
                        "<" -> ( "lt", TCon "Bool" )
                        ">" -> ( "gt", TCon "Bool" )
                        "<=" -> ( "lte", TCon "Bool" )
                        ">=" -> ( "gte", TCon "Bool" )
                        "&&" -> ( "and", TCon "Bool" )
                        "||" -> ( "or", TCon "Bool" )
                        "++" -> ( "append", TVar "a" )
                        "::" -> ( "cons", TVar "a" )
                        ">>" -> ( "compose", TVar "a" )
                        "<<" -> ( "composeLeft", TVar "a" )
                        _ -> ( opName, TVar "a" )

                opVar = Core.EVar { name = opFuncName, type_ = TArrow (TVar "a") (TArrow (TVar "b") resultType) }

                -- f x y = (f x) y
                app1 = Core.EApp opVar leftCore (TArrow (TVar "b") resultType)
            in
            Core.EApp app1 rightCore resultType



-- LIST DESUGARING


desugarList : DesugarCtx -> List (AST.Located AST.Expr) -> Core.Expr
desugarList ctx exprs =
    let
        listType = TApp (TCon "List") (TVar "a")

        nil = Core.ECon "Nil" [] listType

        cons elem rest =
            Core.ECon "Cons" [ elem, rest ] listType
    in
    List.foldr
        (\loc acc -> cons (desugarExpr ctx (AST.getValue loc)) acc)
        nil
        exprs



-- DO-NOTATION DESUGARING


desugarDo : DesugarCtx -> List (AST.Located AST.DoStatement) -> Core.Expr
desugarDo ctx statements =
    case statements of
        [] ->
            -- Empty do block is an error, but return unit for now
            Core.ECon "Unit" [] (TCon "Unit")

        [ stmt ] ->
            -- Last statement must be an expression
            case AST.getValue stmt of
                AST.DoExpr expr ->
                    desugarExpr ctx (AST.getValue expr)

                AST.DoBind _ expr ->
                    -- Bind at end is also an expression
                    desugarExpr ctx (AST.getValue expr)

                AST.DoLet bindings ->
                    -- Let at end is weird but handle it
                    desugarLet ctx bindings (AST.locate (AST.getRegion stmt) AST.EUnit)

        stmt :: rest ->
            case AST.getValue stmt of
                AST.DoExpr expr ->
                    -- expr; rest  =>  expr >> desugar(rest)
                    let
                        exprCore = desugarExpr ctx (AST.getValue expr)
                        restCore = desugarDo ctx rest

                        -- andThen_ : m a -> m b -> m b
                        andThen_ = Core.EVar { name = "andThen_", type_ = TVar "m" }
                    in
                    Core.EApp (Core.EApp andThen_ exprCore (TVar "m")) restCore (TVar "m")

                AST.DoBind pat expr ->
                    -- x <- expr; rest  =>  expr >>= \x -> desugar(rest)
                    let
                        exprCore = desugarExpr ctx (AST.getValue expr)
                        restCore = desugarDo ctx rest

                        ( typedVar, _ ) = patternToTypedVar ctx (AST.getValue pat)

                        -- Build the lambda: \x -> rest
                        lambda = Core.ELam typedVar restCore (TArrow typedVar.type_ (Core.exprType restCore))

                        -- andThen : m a -> (a -> m b) -> m b
                        andThen = Core.EVar { name = "andThen", type_ = TVar "m" }
                    in
                    Core.EApp (Core.EApp andThen exprCore (TVar "m")) lambda (TVar "m")

                AST.DoLet bindings ->
                    -- let x = expr; rest  =>  let x = expr in desugar(rest)
                    desugarLet ctx bindings
                        (AST.locate (AST.getRegion stmt) (AST.EDo rest))



-- PATTERN DESUGARING


desugarPattern : DesugarCtx -> AST.Pattern -> Core.Pattern
desugarPattern ctx pattern =
    case pattern of
        AST.PVar name ->
            Core.PVar { name = name, type_ = TVar "a" }

        AST.PWildcard ->
            Core.PWildcard (TVar "a")

        AST.PLit lit ->
            Core.PLit (desugarLiteral lit) (litType lit)

        AST.PCon qname subPats ->
            let
                conName = qualNameToString qname
                subPatsCore = List.map (\loc -> desugarPattern ctx (AST.getValue loc)) subPats

                -- Look up the constructor's result type from context
                conType =
                    case Dict.get conName ctx.constructors of
                        Just info ->
                            -- Extract the result type (last arrow in the constructor type)
                            getResultType info.type_

                        Nothing ->
                            TVar "a"
            in
            Core.PCon conName subPatsCore conType

        AST.PRecord fields ->
            let
                fieldPats =
                    fields
                        |> List.map (\loc ->
                            let
                                name = AST.getValue loc
                            in
                            ( name, Core.PVar { name = name, type_ = TVar "a" } )
                        )
            in
            Core.PRecord fieldPats (TVar "r")

        AST.PTuple subPats ->
            let
                subPatsCore = List.map (\loc -> desugarPattern ctx (AST.getValue loc)) subPats
            in
            Core.PTuple subPatsCore (TTuple (List.map (\_ -> TVar "a") subPatsCore))

        AST.PList subPats ->
            -- [p1, p2, p3] => Cons p1 (Cons p2 (Cons p3 Nil))
            let
                listType = TApp (TCon "List") (TVar "a")
            in
            List.foldr
                (\loc acc ->
                    Core.PCon "Cons"
                        [ desugarPattern ctx (AST.getValue loc), acc ]
                        listType
                )
                (Core.PCon "Nil" [] listType)
                subPats

        AST.PCons head tail ->
            let
                headCore = desugarPattern ctx (AST.getValue head)
                tailCore = desugarPattern ctx (AST.getValue tail)
            in
            Core.PCon "Cons" [ headCore, tailCore ] (TApp (TCon "List") (TVar "a"))

        AST.PAlias inner alias_ ->
            -- Pattern aliases need special handling in Core
            -- For now, just return the inner pattern
            desugarPattern ctx (AST.getValue inner)

        AST.PParens inner ->
            desugarPattern ctx (AST.getValue inner)

        AST.PUnit ->
            Core.PCon "Unit" [] (TCon "Unit")



-- HELPERS


desugarLiteral : AST.Literal -> Core.Literal
desugarLiteral lit =
    case lit of
        AST.LInt n -> Core.LInt n
        AST.LFloat f -> Core.LFloat f
        AST.LString s -> Core.LString s
        AST.LChar c -> Core.LChar c


litType : AST.Literal -> Type
litType lit =
    case lit of
        AST.LInt _ -> TCon "Int"
        AST.LFloat _ -> TCon "Float"
        AST.LString _ -> TCon "String"
        AST.LChar _ -> TCon "Char"


-- Extract the result type from a constructor's function type
-- For `Red : Color`, returns `Color`
-- For `Just : a -> Maybe a`, returns `Maybe a`
getResultType : Type -> Type
getResultType ty =
    case ty of
        TArrow _ result ->
            getResultType result

        _ ->
            ty


qualNameToString : AST.QualName -> String
qualNameToString qname =
    case qname.module_ of
        Nothing -> qname.name
        Just mod -> mod ++ "." ++ qname.name


patternToTypedVar : DesugarCtx -> AST.Pattern -> ( Core.TypedVar, DesugarCtx )
patternToTypedVar ctx pattern =
    case pattern of
        AST.PVar name ->
            ( { name = name, type_ = TVar "a" }, ctx )

        AST.PWildcard ->
            let
                ( name, ctx1 ) = freshVar "_" ctx
            in
            ( { name = name, type_ = TVar "a" }, ctx1 )

        _ ->
            -- Complex patterns need case expressions
            let
                ( name, ctx1 ) = freshVar "arg" ctx
            in
            ( { name = name, type_ = TVar "a" }, ctx1 )


patternName : AST.Pattern -> String
patternName pattern =
    case pattern of
        AST.PVar name -> name
        AST.PAlias _ alias_ -> AST.getValue alias_
        _ -> "_"
