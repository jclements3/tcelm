module Parse.Symbol exposing (operator, leftArrow)

{-| Symbol/operator parsing for Elm.
-}

import Parse.Primitives as P exposing (Parser)


operator : x -> x -> Parser x String
operator expecting reservedError =
    P.Parser <|
        \s ->
            let
                newOffset =
                    chompOperatorChars s.offset s.src
            in
            if newOffset == s.offset then
                P.Bad False (fromState s expecting)

            else
                let
                    op =
                        String.slice s.offset newOffset s.src
                in
                if isReservedOperator op then
                    P.Bad False (fromState s reservedError)

                else
                    P.Good True
                        op
                        { src = s.src
                        , offset = newOffset
                        , indent = s.indent
                        , row = s.row
                        , col = s.col + (newOffset - s.offset)
                        }


chompOperatorChars : Int -> String -> Int
chompOperatorChars offset src =
    let
        c =
            getCharAt offset src
    in
    if isOperatorChar c then
        chompOperatorChars (offset + 1) src

    else
        offset


isOperatorChar : Char -> Bool
isOperatorChar c =
    List.member c
        [ '+'
        , '-'
        , '*'
        , '/'
        , '='
        , '<'
        , '>'
        , ':'
        , '&'
        , '|'
        , '^'
        , '?'
        , '%'
        , '!'
        , '.'
        , '#'
        , '$'
        , '~'
        , '@'
        ]


isReservedOperator : String -> Bool
isReservedOperator op =
    List.member op
        [ "="
        , "."
        , "->"
        , "--"
        , ":"
        , "|"
        , "\\"
        ]



-- HELPERS


getCharAt : Int -> String -> Char
getCharAt offset src =
    String.uncons (String.dropLeft offset src)
        |> Maybe.map Tuple.first
        |> Maybe.withDefault '\u{0000}'


fromState : P.State -> x -> P.Bag (P.DeadEnd x)
fromState s x =
    P.AddRight P.Empty { row = s.row, col = s.col, problem = x, contextStack = [] }


leftArrow : x -> Parser x ()
leftArrow expecting =
    P.symbol "<-" expecting
