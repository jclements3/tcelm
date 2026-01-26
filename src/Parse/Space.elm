module Parse.Space exposing
    ( chomp, chompAndCheckIndent
    , checkIndent, checkAligned, checkFreshLine
    , chompDocComment
    , Parser
    )

{-| Whitespace and indentation handling for Elm parsing.
-}

import AST.Source as Src exposing (Position)
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
        -- Check if this is a doc comment - if so, DON'T consume it
        if getCharAt (offset + 2) src == '|' then
            -- Stop here - let chompDocComment handle it
            ( offset, row, col )

        else
            -- Regular multi-line comment - consume it
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



-- DOC COMMENTS


{-| Result of chomping spaces with doc comment extraction.
-}
type alias ChompDocResult =
    { offset : Int
    , row : Int
    , col : Int
    , doc : Maybe String
    }


{-| Result of extracting doc comment content.
-}
type alias ExtractResult =
    { content : String
    , offset : Int
    , row : Int
    , col : Int
    }


{-| Parse whitespace and extract any doc comment found.
Returns Nothing if no doc comment, or Just the comment content.
-}
chompDocComment : x -> P.Parser x (Maybe Src.DocComment)
chompDocComment _ =
    P.Parser <|
        \s ->
            let
                result =
                    chompSpacesWithDoc s.offset s.row s.col s.src Nothing
            in
            P.Good (result.offset /= s.offset)
                result.doc
                { src = s.src
                , offset = result.offset
                , indent = s.indent
                , row = result.row
                , col = result.col
                }


{-| Like chompSpaces but also captures the last doc comment found.
-}
chompSpacesWithDoc : Int -> Int -> Int -> String -> Maybe String -> ChompDocResult
chompSpacesWithDoc offset row col src lastDoc =
    let
        c =
            getCharAt offset src
    in
    if c == ' ' then
        chompSpacesWithDoc (offset + 1) row (col + 1) src lastDoc

    else if c == '\n' then
        chompSpacesWithDoc (offset + 1) (row + 1) 1 src lastDoc

    else if c == '\u{000D}' then
        chompSpacesWithDoc (offset + 1) row col src lastDoc

    else if c == '-' && getCharAt (offset + 1) src == '-' then
        let
            ( newOffset, newRow, _ ) =
                chompLineCommentRaw (offset + 2) row src
        in
        chompSpacesWithDoc newOffset newRow 1 src lastDoc

    else if c == '{' && getCharAt (offset + 1) src == '-' then
        -- Check if this is a doc comment
        if getCharAt (offset + 2) src == '|' then
            -- Doc comment! Extract its content
            case extractDocCommentContent (offset + 3) row (col + 3) src of
                Ok result ->
                    -- Continue chomping, but remember this doc comment
                    chompSpacesWithDoc result.offset result.row result.col src (Just result.content)

                Err ( newOffset, newRow, newCol ) ->
                    -- Unclosed comment, stop here
                    { offset = newOffset, row = newRow, col = newCol, doc = lastDoc }

        else
            -- Regular multi-line comment
            case chompMultiComment (offset + 2) row col src of
                Ok ( newOffset, newRow, newCol ) ->
                    chompSpacesWithDoc newOffset newRow newCol src lastDoc

                Err ( newOffset, newRow, newCol ) ->
                    { offset = newOffset, row = newRow, col = newCol, doc = lastDoc }

    else
        { offset = offset, row = row, col = col, doc = lastDoc }


{- Extract doc comment content (everything between the opening
and closing delimiters). Returns the content string and position. -}
extractDocCommentContent : Int -> Int -> Int -> String -> Result ( Int, Int, Int ) ExtractResult
extractDocCommentContent startOffset row col src =
    extractDocHelper startOffset row col src 1 startOffset


extractDocHelper : Int -> Int -> Int -> String -> Int -> Int -> Result ( Int, Int, Int ) ExtractResult
extractDocHelper offset row col src depth contentStart =
    let
        c =
            getCharAt offset src
    in
    if c == '\n' then
        extractDocHelper (offset + 1) (row + 1) 1 src depth contentStart

    else if c == '-' && getCharAt (offset + 1) src == '}' then
        if depth == 1 then
            -- End of doc comment - extract content
            let
                content =
                    String.slice contentStart offset src
                        |> String.trim
            in
            Ok { content = content, offset = offset + 2, row = row, col = col + 2 }

        else
            -- Closing a nested comment
            extractDocHelper (offset + 2) row (col + 2) src (depth - 1) contentStart

    else if c == '{' && getCharAt (offset + 1) src == '-' then
        -- Nested comment
        extractDocHelper (offset + 2) row (col + 2) src (depth + 1) contentStart

    else if c == '\u{0000}' then
        -- EOF - unclosed comment
        Err ( offset, row, col )

    else
        extractDocHelper (offset + 1) row (col + 1) src depth contentStart


{-| Chomp a line comment without continuing to next whitespace.
-}
chompLineCommentRaw : Int -> Int -> String -> ( Int, Int, Int )
chompLineCommentRaw offset row src =
    let
        c =
            getCharAt offset src
    in
    if c == '\n' then
        ( offset + 1, row + 1, 1 )

    else if c == '\u{0000}' then
        ( offset, row, offset )

    else
        chompLineCommentRaw (offset + 1) row src



-- HELPERS


getCharAt : Int -> String -> Char
getCharAt offset src =
    String.uncons (String.dropLeft offset src)
        |> Maybe.map Tuple.first
        |> Maybe.withDefault '\u{0000}'
