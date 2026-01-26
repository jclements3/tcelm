module Parse.Variable exposing
    ( lower, upper
    , moduleName
    , foreignAlpha, foreignUpper
    , Upper(..)
    )

{-| Variable name parsers.
-}

import AST.Source exposing (Expr_(..), VarType(..))
import Parse.Primitives as P exposing (Parser)


-- UPPER / LOWER


lower : x -> Parser x String
lower expecting =
    P.Parser <|
        \s ->
            let
                firstChar =
                    getCharAt s.offset s.src
            in
            if Char.isLower firstChar then
                let
                    ( newOffset, name ) =
                        chompVarChars (s.offset + 1) s.src
                            |> (\end -> ( end, String.slice s.offset end s.src ))
                in
                if isReserved name then
                    P.Bad False (fromState s expecting)

                else
                    P.Good True
                        name
                        { src = s.src
                        , offset = newOffset
                        , indent = s.indent
                        , row = s.row
                        , col = s.col + (newOffset - s.offset)
                        }

            else
                P.Bad False (fromState s expecting)


upper : x -> Parser x String
upper expecting =
    P.Parser <|
        \s ->
            let
                firstChar =
                    getCharAt s.offset s.src
            in
            if Char.isUpper firstChar then
                let
                    newOffset =
                        chompVarChars (s.offset + 1) s.src
                in
                P.Good True
                    (String.slice s.offset newOffset s.src)
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = s.row
                    , col = s.col + (newOffset - s.offset)
                    }

            else
                P.Bad False (fromState s expecting)



-- MODULE NAME


moduleName : x -> Parser x String
moduleName expecting =
    P.Parser <|
        \s ->
            let
                firstChar =
                    getCharAt s.offset s.src
            in
            if Char.isUpper firstChar then
                let
                    ( newOffset, newCol ) =
                        chompModuleName (s.offset + 1) s.src (s.col + 1)
                in
                P.Good True
                    (String.slice s.offset newOffset s.src)
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = s.row
                    , col = newCol
                    }

            else
                P.Bad False (fromState s expecting)


chompModuleName : Int -> String -> Int -> ( Int, Int )
chompModuleName offset src col =
    let
        newOffset =
            chompVarChars offset src
    in
    if getCharAt newOffset src == '.' && Char.isUpper (getCharAt (newOffset + 1) src) then
        chompModuleName (newOffset + 2) src (col + (newOffset - offset) + 2)

    else
        ( newOffset, col + (newOffset - offset) )



-- FOREIGN VARIABLES


type Upper
    = Unqualified String
    | Qualified String String


foreignAlpha : x -> Parser x Expr_
foreignAlpha expecting =
    P.Parser <|
        \s ->
            let
                firstChar =
                    getCharAt s.offset s.src
            in
            if Char.isLower firstChar then
                -- lower case variable
                let
                    newOffset =
                        chompVarChars (s.offset + 1) s.src

                    name =
                        String.slice s.offset newOffset s.src
                in
                if isReserved name then
                    P.Bad False (fromState s expecting)

                else
                    P.Good True
                        (Var LowVar name)
                        { src = s.src
                        , offset = newOffset
                        , indent = s.indent
                        , row = s.row
                        , col = s.col + (newOffset - s.offset)
                        }

            else if Char.isUpper firstChar then
                -- upper case or qualified variable
                chompQualified s.offset (s.offset + 1) s.src s

            else
                P.Bad False (fromState s expecting)


chompQualified : Int -> Int -> String -> P.State -> P.PStep x Expr_
chompQualified startOffset offset src s =
    let
        newOffset =
            chompVarChars offset src
    in
    if getCharAt newOffset src == '.' then
        let
            nextChar =
                getCharAt (newOffset + 1) src
        in
        if Char.isUpper nextChar then
            chompQualified startOffset (newOffset + 2) src s

        else if Char.isLower nextChar then
            let
                varOffset =
                    chompVarChars (newOffset + 2) src

                modulePart =
                    String.slice startOffset newOffset src

                varName =
                    String.slice (newOffset + 1) varOffset src
            in
            if isReserved varName then
                P.Good True
                    (VarQual CapVar modulePart (String.slice (newOffset + 1) varOffset src))
                    { src = s.src
                    , offset = varOffset
                    , indent = s.indent
                    , row = s.row
                    , col = s.col + (varOffset - s.offset)
                    }

            else
                P.Good True
                    (VarQual LowVar modulePart varName)
                    { src = s.src
                    , offset = varOffset
                    , indent = s.indent
                    , row = s.row
                    , col = s.col + (varOffset - s.offset)
                    }

        else
            P.Good True
                (Var CapVar (String.slice startOffset newOffset src))
                { src = s.src
                , offset = newOffset
                , indent = s.indent
                , row = s.row
                , col = s.col + (newOffset - s.offset)
                }

    else
        -- No dot following - this could be a simple constructor or a qualified constructor
        -- Check if there's a dot in the name (indicating Module.Constructor)
        let
            fullName =
                String.slice startOffset newOffset src
        in
        case findLastDot fullName of
            Just dotIdx ->
                -- Split into module.Constructor
                let
                    modulePart =
                        String.left dotIdx fullName

                    ctorName =
                        String.dropLeft (dotIdx + 1) fullName
                in
                P.Good True
                    (VarQual CapVar modulePart ctorName)
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = s.row
                    , col = s.col + (newOffset - s.offset)
                    }

            Nothing ->
                -- Simple constructor without module prefix
                P.Good True
                    (Var CapVar fullName)
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = s.row
                    , col = s.col + (newOffset - s.offset)
                    }


foreignUpper : x -> Parser x Upper
foreignUpper expecting =
    P.Parser <|
        \s ->
            let
                firstChar =
                    getCharAt s.offset s.src
            in
            if Char.isUpper firstChar then
                chompQualifiedUpper s.offset (s.offset + 1) s.src s

            else
                P.Bad False (fromState s expecting)


chompQualifiedUpper : Int -> Int -> String -> P.State -> P.PStep x Upper
chompQualifiedUpper startOffset offset src s =
    let
        newOffset =
            chompVarChars offset src
    in
    if getCharAt newOffset src == '.' && Char.isUpper (getCharAt (newOffset + 1) src) then
        chompQualifiedUpper startOffset (newOffset + 2) src s

    else
        let
            fullName =
                String.slice startOffset newOffset src
        in
        case findLastDot fullName of
            Nothing ->
                P.Good True
                    (Unqualified fullName)
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = s.row
                    , col = s.col + (newOffset - s.offset)
                    }

            Just dotIndex ->
                P.Good True
                    (Qualified (String.left dotIndex fullName) (String.dropLeft (dotIndex + 1) fullName))
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = s.row
                    , col = s.col + (newOffset - s.offset)
                    }


findLastDot : String -> Maybe Int
findLastDot str =
    findLastDotHelp str (String.length str - 1)


findLastDotHelp : String -> Int -> Maybe Int
findLastDotHelp str index =
    if index < 0 then
        Nothing

    else if String.slice index (index + 1) str == "." then
        Just index

    else
        findLastDotHelp str (index - 1)



-- HELPERS


chompVarChars : Int -> String -> Int
chompVarChars offset src =
    let
        c =
            getCharAt offset src
    in
    if Char.isAlphaNum c || c == '_' then
        chompVarChars (offset + 1) src

    else
        offset


isReserved : String -> Bool
isReserved name =
    List.member name
        [ "if"
        , "then"
        , "else"
        , "case"
        , "of"
        , "let"
        , "in"
        , "type"
        , "module"
        , "where"
        , "import"
        , "exposing"
        , "as"
        , "port"
        ]


getCharAt : Int -> String -> Char
getCharAt offset src =
    String.uncons (String.dropLeft offset src)
        |> Maybe.map Tuple.first
        |> Maybe.withDefault '\u{0000}'


fromState : P.State -> x -> P.Bag (P.DeadEnd x)
fromState s x =
    P.AddRight P.Empty { row = s.row, col = s.col, problem = x, contextStack = [] }



-- Re-export types for internal use


type alias PStep x a =
    P.PStep x a


type alias Bag a =
    P.Bag a
