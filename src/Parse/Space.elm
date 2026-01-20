module Parse.Space exposing
    ( chomp, chompAndCheckIndent
    , checkIndent, checkAligned, checkFreshLine
    , Parser
    )

{-| Whitespace and indentation handling for Elm parsing.
-}

import AST.Source exposing (Position)
import Parse.Primitives as P


{-| A Space.Parser returns a value and the end position.
-}
type alias Parser x a =
    P.Parser x ( a, Position )



-- CHOMP SPACES


chomp : x -> P.Parser x ()
chomp _ =
    P.Parser <|
        \s ->
            let
                ( newOffset, newRow, newCol ) =
                    chompSpaces s.offset s.row s.col s.src
            in
            P.Good (newOffset /= s.offset)
                ()
                { src = s.src
                , offset = newOffset
                , indent = s.indent
                , row = newRow
                , col = newCol
                }


chompAndCheckIndent : x -> x -> P.Parser x ()
chompAndCheckIndent spaceError indentError =
    P.Parser <|
        \s ->
            let
                ( newOffset, newRow, newCol ) =
                    chompSpaces s.offset s.row s.col s.src
            in
            if newCol > s.indent then
                P.Good (newOffset /= s.offset)
                    ()
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = newRow
                    , col = newCol
                    }

            else
                P.Bad (newOffset /= s.offset)
                    (P.AddRight P.Empty
                        { row = newRow
                        , col = newCol
                        , problem = indentError
                        , contextStack = []
                        }
                    )



-- CHOMP SPACES HELPER


chompSpaces : Int -> Int -> Int -> String -> ( Int, Int, Int )
chompSpaces offset row col src =
    let
        c =
            getCharAt offset src
    in
    if c == ' ' then
        chompSpaces (offset + 1) row (col + 1) src

    else if c == '\n' then
        chompSpaces (offset + 1) (row + 1) 1 src

    else if c == '\u{000D}' then
        chompSpaces (offset + 1) row col src

    else if c == '-' && getCharAt (offset + 1) src == '-' then
        chompLineComment (offset + 2) row src

    else if c == '{' && getCharAt (offset + 1) src == '-' then
        case chompMultiComment (offset + 2) row col src of
            Ok ( newOffset, newRow, newCol ) ->
                chompSpaces newOffset newRow newCol src

            Err ( newOffset, newRow, newCol ) ->
                ( newOffset, newRow, newCol )

    else
        ( offset, row, col )


chompLineComment : Int -> Int -> String -> ( Int, Int, Int )
chompLineComment offset row src =
    let
        c =
            getCharAt offset src
    in
    if c == '\n' then
        chompSpaces (offset + 1) (row + 1) 1 src

    else if c == '\u{0000}' then
        ( offset, row, offset )

    else
        chompLineComment (offset + 1) row src


chompMultiComment : Int -> Int -> Int -> String -> Result ( Int, Int, Int ) ( Int, Int, Int )
chompMultiComment offset row col src =
    let
        c =
            getCharAt offset src
    in
    if c == '\n' then
        chompMultiComment (offset + 1) (row + 1) 1 src

    else if c == '-' && getCharAt (offset + 1) src == '}' then
        Ok ( offset + 2, row, col + 2 )

    else if c == '{' && getCharAt (offset + 1) src == '-' then
        case chompMultiComment (offset + 2) row (col + 2) src of
            Ok ( newOffset, newRow, newCol ) ->
                chompMultiComment newOffset newRow newCol src

            Err err ->
                Err err

    else if c == '\u{0000}' then
        Err ( offset, row, col )

    else
        chompMultiComment (offset + 1) row (col + 1) src



-- INDENTATION CHECKS


checkIndent : Position -> x -> P.Parser x ()
checkIndent _ expecting =
    -- Check that the CURRENT position is indented (col > indent)
    -- The Position parameter is ignored; what matters is where we are NOW
    -- after whitespace has been consumed
    P.Parser <|
        \s ->
            if s.col > s.indent then
                P.Good False () s

            else
                P.Bad False
                    (P.AddRight P.Empty
                        { row = s.row
                        , col = s.col
                        , problem = expecting
                        , contextStack = []
                        }
                    )


checkAligned : x -> P.Parser x ()
checkAligned expecting =
    P.Parser <|
        \s ->
            if s.col == s.indent then
                P.Good False () s

            else
                P.Bad False
                    (P.AddRight P.Empty
                        { row = s.row
                        , col = s.col
                        , problem = expecting
                        , contextStack = []
                        }
                    )


checkFreshLine : x -> P.Parser x ()
checkFreshLine expecting =
    P.Parser <|
        \s ->
            if s.col == 1 then
                P.Good False () s

            else
                P.Bad False
                    (P.AddRight P.Empty
                        { row = s.row
                        , col = s.col
                        , problem = expecting
                        , contextStack = []
                        }
                    )



-- HELPERS


getCharAt : Int -> String -> Char
getCharAt offset src =
    String.uncons (String.dropLeft offset src)
        |> Maybe.map Tuple.first
        |> Maybe.withDefault '\u{0000}'
