module Parse.String exposing (string, character)

{-| String and character literal parsing.
-}

import Parse.Primitives as P exposing (Parser)


-- STRING


string : x -> x -> Parser x String
string startExpecting problemExpecting =
    P.Parser <|
        \s ->
            let
                c =
                    getCharAt s.offset s.src
            in
            if c == '"' then
                if getCharAt (s.offset + 1) s.src == '"' && getCharAt (s.offset + 2) s.src == '"' then
                    -- Multi-line string
                    chompMultiString (s.offset + 3) s.row (s.col + 3) s.src s problemExpecting

                else
                    -- Single-line string
                    chompSingleString (s.offset + 1) s.row (s.col + 1) s.src s problemExpecting

            else
                P.Bad False (fromState s startExpecting)


chompSingleString : Int -> Int -> Int -> String -> P.State -> x -> P.PStep x String
chompSingleString offset row col src s0 problemExpecting =
    let
        c =
            getCharAt offset src
    in
    if c == '"' then
        let
            content =
                String.slice (s0.offset + 1) offset src
        in
        case parseStringContent content of
            Ok parsed ->
                P.Good True
                    parsed
                    { src = s0.src
                    , offset = offset + 1
                    , indent = s0.indent
                    , row = row
                    , col = col + 1
                    }

            Err _ ->
                P.Bad True (fromState s0 problemExpecting)

    else if c == '\n' || c == '\u{0000}' then
        P.Bad True (fromStateAt row col problemExpecting)

    else if c == '\\' then
        chompSingleString (offset + 2) row (col + 2) src s0 problemExpecting

    else
        chompSingleString (offset + 1) row (col + 1) src s0 problemExpecting


chompMultiString : Int -> Int -> Int -> String -> P.State -> x -> P.PStep x String
chompMultiString offset row col src s0 problemExpecting =
    let
        c =
            getCharAt offset src
    in
    if c == '"' && getCharAt (offset + 1) src == '"' && getCharAt (offset + 2) src == '"' then
        let
            content =
                String.slice (s0.offset + 3) offset src
        in
        case parseStringContent content of
            Ok parsed ->
                P.Good True
                    parsed
                    { src = s0.src
                    , offset = offset + 3
                    , indent = s0.indent
                    , row = row
                    , col = col + 3
                    }

            Err _ ->
                P.Bad True (fromState s0 problemExpecting)

    else if c == '\u{0000}' then
        P.Bad True (fromStateAt row col problemExpecting)

    else if c == '\n' then
        chompMultiString (offset + 1) (row + 1) 1 src s0 problemExpecting

    else if c == '\\' then
        chompMultiString (offset + 2) row (col + 2) src s0 problemExpecting

    else
        chompMultiString (offset + 1) row (col + 1) src s0 problemExpecting



-- CHARACTER


character : x -> x -> Parser x String
character startExpecting problemExpecting =
    P.Parser <|
        \s ->
            if getCharAt s.offset s.src == '\'' then
                chompChar (s.offset + 1) s.row (s.col + 1) s.src s problemExpecting

            else
                P.Bad False (fromState s startExpecting)


chompChar : Int -> Int -> Int -> String -> P.State -> x -> P.PStep x String
chompChar offset row col src s0 problemExpecting =
    let
        c =
            getCharAt offset src
    in
    if c == '\\' then
        -- Escape sequence
        let
            escaped =
                getCharAt (offset + 1) src

            -- For unicode escapes \u{XXXX}, find the closing brace
            endOffset =
                if escaped == 'u' && getCharAt (offset + 2) src == '{' then
                    findClosingBrace (offset + 3) src + 1
                else
                    offset + 2
        in
        if getCharAt endOffset src == '\'' then
            let
                charStr =
                    String.slice (s0.offset + 1) endOffset src

                charLen =
                    endOffset - s0.offset
            in
            case parseCharContent charStr of
                Ok parsed ->
                    P.Good True
                        parsed
                        { src = s0.src
                        , offset = endOffset + 1
                        , indent = s0.indent
                        , row = row
                        , col = col + charLen
                        }

                Err _ ->
                    P.Bad True (fromState s0 problemExpecting)

        else
            P.Bad True (fromStateAt row col problemExpecting)

    else if c == '\'' || c == '\n' || c == '\u{0000}' then
        P.Bad True (fromStateAt row col problemExpecting)

    else if getCharAt (offset + 1) src == '\'' then
        P.Good True
            (String.fromChar c)
            { src = s0.src
            , offset = offset + 2
            , indent = s0.indent
            , row = row
            , col = col + 2
            }

    else
        P.Bad True (fromStateAt row col problemExpecting)



-- CONTENT PARSING


parseStringContent : String -> Result String String
parseStringContent content =
    parseEscapes content ""


parseCharContent : String -> Result String String
parseCharContent content =
    parseEscapes content ""


parseEscapes : String -> String -> Result String String
parseEscapes remaining acc =
    case String.uncons remaining of
        Nothing ->
            Ok acc

        Just ( '\\', rest ) ->
            case String.uncons rest of
                Nothing ->
                    Err "Unexpected end of escape"

                Just ( 'n', rest2 ) ->
                    parseEscapes rest2 (acc ++ "\n")

                Just ( 't', rest2 ) ->
                    parseEscapes rest2 (acc ++ "\t")

                Just ( 'r', rest2 ) ->
                    parseEscapes rest2 (acc ++ "\u{000D}")

                Just ( '\\', rest2 ) ->
                    parseEscapes rest2 (acc ++ "\\")

                Just ( '"', rest2 ) ->
                    parseEscapes rest2 (acc ++ "\"")

                Just ( '\'', rest2 ) ->
                    parseEscapes rest2 (acc ++ "'")

                Just ( 'u', rest2 ) ->
                    parseUnicodeEscape rest2 acc

                Just ( c, _ ) ->
                    Err ("Unknown escape: \\" ++ String.fromChar c)

        Just ( c, rest ) ->
            parseEscapes rest (acc ++ String.fromChar c)


parseUnicodeEscape : String -> String -> Result String String
parseUnicodeEscape remaining acc =
    if String.startsWith "{" remaining then
        case String.indexes "}" remaining of
            [] ->
                Err "Missing closing brace in unicode escape"

            closeIndex :: _ ->
                let
                    hexStr =
                        String.slice 1 closeIndex remaining
                in
                case hexToInt hexStr of
                    Just codePoint ->
                        let
                            rest =
                                String.dropLeft (closeIndex + 1) remaining
                        in
                        parseEscapes rest (acc ++ String.fromChar (Char.fromCode codePoint))

                    Nothing ->
                        Err ("Invalid unicode escape: " ++ hexStr)

    else
        Err "Expected { after \\u"


hexToInt : String -> Maybe Int
hexToInt hex =
    String.foldl
        (\c acc ->
            case acc of
                Nothing ->
                    Nothing

                Just n ->
                    case hexDigitValue c of
                        Just v ->
                            Just (n * 16 + v)

                        Nothing ->
                            Nothing
        )
        (Just 0)
        (String.toLower hex)


hexDigitValue : Char -> Maybe Int
hexDigitValue c =
    if c >= '0' && c <= '9' then
        Just (Char.toCode c - Char.toCode '0')

    else if c >= 'a' && c <= 'f' then
        Just (Char.toCode c - Char.toCode 'a' + 10)

    else
        Nothing



-- HELPERS


findClosingBrace : Int -> String -> Int
findClosingBrace offset src =
    let
        c = getCharAt offset src
    in
    if c == '}' then
        offset
    else if c == '\u{0000}' then
        offset  -- End of string, return current position
    else
        findClosingBrace (offset + 1) src


getCharAt : Int -> String -> Char
getCharAt offset src =
    String.uncons (String.dropLeft offset src)
        |> Maybe.map Tuple.first
        |> Maybe.withDefault '\u{0000}'


fromState : P.State -> x -> P.Bag (P.DeadEnd x)
fromState s x =
    P.AddRight P.Empty { row = s.row, col = s.col, problem = x, contextStack = [] }


fromStateAt : Int -> Int -> x -> P.Bag (P.DeadEnd x)
fromStateAt row col x =
    P.AddRight P.Empty { row = row, col = col, problem = x, contextStack = [] }
