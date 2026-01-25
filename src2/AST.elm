module AST exposing
    ( Module
    , Decl(..)
    , Expr(..)
    , Pattern(..)
    , TypeAnnotation(..)
    , Literal(..)
    , Name
    , QualName
    , Located
    , Region
    , Position
    , ValueDef
    , TypeAliasDef
    , CustomTypeDef
    , Constructor
    , TypeClassDef
    , ClassConstraint
    , MethodSig
    , InstanceDef
    , MethodImpl
    , PortDef
    , InfixDef
    , Associativity(..)
    , ForeignDef
    , LetBinding
    , CaseBranch
    , DoStatement(..)
    , Exposing
    , ExposedItem(..)
    , ExposedConstructors(..)
    , Import
    , at
    , locate
    , getValue
    , getRegion
    )

{-| Source AST for tcelm.

This is the AST produced by the parser, before type checking.
It closely mirrors the source syntax.

After type checking, this is transformed into TypedAST.
-}


-- LOCATION TRACKING


type alias Position =
    { line : Int
    , column : Int
    }


type alias Region =
    { start : Position
    , end : Position
    }


type alias Located a =
    { region : Region
    , value : a
    }


at : Position -> Position -> a -> Located a
at start end value =
    { region = { start = start, end = end }
    , value = value
    }


locate : Region -> a -> Located a
locate region value =
    { region = region
    , value = value
    }


getValue : Located a -> a
getValue loc =
    loc.value


getRegion : Located a -> Region
getRegion loc =
    loc.region



-- NAMES


type alias Name =
    String


type alias QualName =
    { module_ : Maybe String
    , name : String
    }



-- MODULE STRUCTURE


type alias Module =
    { name : Maybe (Located Name)
    , exports : Maybe (Located Exposing)
    , imports : List (Located Import)
    , decls : List (Located Decl)
    }


type alias Exposing =
    List ExposedItem


type ExposedItem
    = ExposedValue Name
    | ExposedType Name ExposedConstructors
    | ExposedOperator Name


type ExposedConstructors
    = ExposedAll             -- Type(..)
    | ExposedSome (List Name) -- Type(A, B)
    | ExposedNone            -- Type


type alias Import =
    { module_ : Located Name
    , alias_ : Maybe Name
    , exposing_ : Maybe Exposing
    }



-- DECLARATIONS


type Decl
    = ValueDecl ValueDef
    | TypeAliasDecl TypeAliasDef
    | CustomTypeDecl CustomTypeDef
    | TypeClassDecl TypeClassDef
    | InstanceDecl InstanceDef
    | PortDecl PortDef
    | InfixDecl InfixDef
    | ForeignDecl ForeignDef


type alias ValueDef =
    { name : Located Name
    , typeAnnotation : Maybe (Located TypeAnnotation)
    , args : List (Located Pattern)
    , body : Located Expr
    }


type alias TypeAliasDef =
    { name : Located Name
    , typeVars : List (Located Name)
    , body : Located TypeAnnotation
    }


type alias CustomTypeDef =
    { name : Located Name
    , typeVars : List (Located Name)
    , constructors : List (Located Constructor)
    }


type alias Constructor =
    { name : Located Name
    , args : List (Located TypeAnnotation)
    }


type alias TypeClassDef =
    { name : Located Name
    , typeVars : List (Located Name)
    , superclasses : List (Located ClassConstraint)
    , methods : List (Located MethodSig)
    }


type alias ClassConstraint =
    { className : Located Name
    , args : List (Located TypeAnnotation)
    }


type alias MethodSig =
    { name : Located Name
    , type_ : Located TypeAnnotation
    }


type alias InstanceDef =
    { context : List (Located ClassConstraint)
    , className : Located Name
    , typeArgs : List (Located TypeAnnotation)
    , methods : List (Located MethodImpl)
    }


type alias MethodImpl =
    { name : Located Name
    , args : List (Located Pattern)
    , body : Located Expr
    }


type alias PortDef =
    { name : Located Name
    , type_ : Located TypeAnnotation
    }


type alias InfixDef =
    { operator : Located Name
    , associativity : Associativity
    , precedence : Int
    , function : Located Name
    }


type Associativity
    = LeftAssoc
    | RightAssoc
    | NonAssoc


type alias ForeignDef =
    { name : Located Name
    , cName : String
    , type_ : Located TypeAnnotation
    }



-- TYPE ANNOTATIONS (source level)


type TypeAnnotation
    = TAVar Name                                        -- a
    | TACon QualName                                    -- Int, Maybe, List
    | TAApp (Located TypeAnnotation) (Located TypeAnnotation)  -- Maybe Int, f a
    | TAArrow (Located TypeAnnotation) (Located TypeAnnotation) -- a -> b
    | TARecord (List ( Located Name, Located TypeAnnotation )) (Maybe (Located Name))  -- { x : Int | r }
    | TATuple (List (Located TypeAnnotation))           -- (a, b, c)
    | TAUnit                                            -- ()
    | TAParens (Located TypeAnnotation)                 -- (a -> b)



-- EXPRESSIONS


type Expr
    = ELit Literal
    | EVar QualName
    | EConstructor QualName
    | EApp (Located Expr) (Located Expr)
    | ELambda (List (Located Pattern)) (Located Expr)
    | ELet (List (Located LetBinding)) (Located Expr)
    | EIf (Located Expr) (Located Expr) (Located Expr)
    | ECase (Located Expr) (List (Located CaseBranch))
    | EBinOp (Located Expr) (Located Name) (Located Expr)
    | ENegate (Located Expr)
    | ERecord (List ( Located Name, Located Expr ))
    | ERecordAccess (Located Expr) (Located Name)
    | ERecordAccessor (Located Name)                    -- .field
    | ERecordUpdate (Located Name) (List ( Located Name, Located Expr ))
    | ETuple (List (Located Expr))
    | EList (List (Located Expr))
    | EUnit
    | EParens (Located Expr)
    | EDo (List (Located DoStatement))                  -- do-notation


type Literal
    = LInt Int
    | LFloat Float
    | LString String
    | LChar Char


type alias LetBinding =
    { pattern : Located Pattern
    , typeAnnotation : Maybe (Located TypeAnnotation)
    , value : Located Expr
    }


type alias CaseBranch =
    { pattern : Located Pattern
    , guard : Maybe (Located Expr)
    , body : Located Expr
    }


type DoStatement
    = DoBind (Located Pattern) (Located Expr)          -- x <- action
    | DoLet (List (Located LetBinding))                 -- let x = expr
    | DoExpr (Located Expr)                             -- action (last must be this)



-- PATTERNS


type Pattern
    = PVar Name
    | PWildcard
    | PLit Literal
    | PCon QualName (List (Located Pattern))
    | PRecord (List (Located Name))
    | PTuple (List (Located Pattern))
    | PList (List (Located Pattern))
    | PCons (Located Pattern) (Located Pattern)
    | PAlias (Located Pattern) (Located Name)
    | PParens (Located Pattern)
    | PUnit
