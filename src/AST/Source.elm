module AST.Source exposing
    ( Module, Import, Exposing(..), Exposed(..), Privacy(..)
    , Expr, Expr_(..), VarType(..), DoStatement(..)
    , Pattern, Pattern_(..)
    , Type, Type_(..)
    , Def(..), Value, Union, Alias, Infix, Port, Associativity(..)
    , TypeClass, Instance, Foreign, ClassConstraint, MethodSig, MethodImpl
    , DocComment
    , Located(..), Region, Position
    , at, merge, toValue, toRegion
    )

{-| Source AST - The abstract syntax tree produced by the parser.
This mirrors the Haskell compiler's AST/Source.hs structure.
-}


-- DOCUMENTATION COMMENTS


{-| A documentation comment `{-| ... -}` attached to a declaration.
-}
type alias DocComment =
    String


-- LOCATION


type alias Position =
    { row : Int
    , col : Int
    }


type alias Region =
    { start : Position
    , end : Position
    }


type Located a
    = At Region a


at : Position -> Position -> a -> Located a
at start end value =
    At { start = start, end = end } value


merge : Located a -> Located b -> c -> Located c
merge loc1 loc2 value =
    At (mergeRegions (toRegion loc1) (toRegion loc2)) value


mergeRegions : Region -> Region -> Region
mergeRegions r1 r2 =
    { start = r1.start, end = r2.end }


toValue : Located a -> a
toValue loc =
    case loc of
        At _ value -> value


toRegion : Located a -> Region
toRegion loc =
    case loc of
        At r _ -> r



-- EXPRESSIONS


type alias Expr =
    Located Expr_


type Expr_
    = Chr String
    | Str String
    | Int Int
    | Float Float
    | Var VarType String
    | VarQual VarType String String
    | List (List Expr)
    | Op String
    | Negate Expr
    | Binops (List ( Expr, Located String )) Expr
    | Lambda (List Pattern) Expr
    | Call Expr (List Expr)
    | If (List ( Expr, Expr )) Expr
    | Let (List (Located Def)) Expr
    | Case Expr (List ( Pattern, Maybe Expr, Expr ))  -- (pattern, maybe guard, body)
    | Accessor String
    | Access Expr (Located String)
    | Update (Located String) (List ( Located String, Expr ))
    | Record (List ( Located String, Expr ))
    | Unit
    | Tuple Expr Expr (List Expr)
    | Do (List DoStatement)  -- do-notation


{-| Statements within a do-block.
-}
type DoStatement
    = DoBind Pattern Expr           -- pattern <- expr
    | DoLet (List (Located Def))    -- let x = expr
    | DoExpr Expr                   -- expr (usually last)


type VarType
    = LowVar
    | CapVar



-- DEFINITIONS


type Def
    = Define (Located String) (List Pattern) Expr (Maybe Type)
    | Destruct Pattern Expr



-- PATTERNS


type alias Pattern =
    Located Pattern_


type Pattern_
    = PAnything
    | PVar String
    | PRecord (List (Located String))
    | PAlias Pattern (Located String)
    | PUnit
    | PTuple Pattern Pattern (List Pattern)
    | PCtor Region String (List Pattern)
    | PCtorQual Region String String (List Pattern)
    | PList (List Pattern)
    | PCons Pattern Pattern
    | PChr String
    | PStr String
    | PInt Int



-- TYPES


type alias Type =
    Located Type_


type Type_
    = TLambda Type Type
    | TVar String
    | TType Region String (List Type)
    | TTypeQual Region String String (List Type)
    | TRecord (List ( Located String, Type )) (Maybe (Located String))
    | TUnit
    | TTuple Type Type (List Type)



-- MODULE


type alias Module =
    { name : Maybe (Located String)
    , exports : Located Exposing
    , docs : Maybe DocComment
    , imports : List Import
    , values : List (Located Value)
    , unions : List (Located Union)
    , aliases : List (Located Alias)
    , binops : List (Located Infix)
    , ports : List Port
    , classes : List (Located TypeClass)
    , instances : List (Located Instance)
    , foreigns : List (Located Foreign)
    }


type alias Import =
    { name : Located String
    , alias_ : Maybe String
    , exposing_ : Exposing
    }


type alias Value =
    { name : Located String
    , args : List Pattern
    , body : Expr
    , type_ : Maybe Type
    , docs : Maybe DocComment
    }


type alias Union =
    { name : Located String
    , args : List (Located String)
    , ctors : List ( Located String, List Type )
    , docs : Maybe DocComment
    }


type alias Alias =
    { name : Located String
    , args : List (Located String)
    , type_ : Type
    , docs : Maybe DocComment
    }


type alias Infix =
    { op : String
    , associativity : Associativity
    , precedence : Int
    , function : String
    }


type Associativity
    = Left
    | Right
    | Non


type alias Port =
    { name : Located String
    , type_ : Type
    , docs : Maybe DocComment
    }


{-| Type class declaration: class Eq a where eq : a -> a -> Bool
-}
type alias TypeClass =
    { name : Located String
    , args : List (Located String)
    , supers : List ClassConstraint
    , methods : List MethodSig
    , docs : Maybe DocComment
    }


{-| Class constraint like `Eq a` or `Functor f`
-}
type alias ClassConstraint =
    { class_ : String
    , args : List Type
    }


{-| Method signature in a type class
-}
type alias MethodSig =
    { name : Located String
    , type_ : Type
    }


{-| Instance declaration: instance Eq Int where eq = primEqInt
-}
type alias Instance =
    { context : List ClassConstraint
    , class_ : String
    , args : List Type
    , methods : List MethodImpl
    }


{-| Method implementation in an instance
-}
type alias MethodImpl =
    { name : Located String
    , args : List Pattern
    , body : Expr
    }


{-| Foreign function import: foreign foo : Int -> Int = "c_foo"
-}
type alias Foreign =
    { name : Located String
    , type_ : Type
    , cName : String
    , docs : Maybe DocComment
    }



-- EXPOSING


type Exposing
    = Open
    | Explicit (List Exposed)


type Exposed
    = Lower (Located String)
    | Upper (Located String) Privacy
    | Operator Region String


type Privacy
    = Public Region
    | Private
