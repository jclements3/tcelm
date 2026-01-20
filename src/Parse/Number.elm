module Parse.Number exposing (Number(..), number)

{-| Number literal parsing for Elm.
-}

import Parse.Primitives as P exposing (Parser)


type Number
    = Int Int
    | Float Float


number : x -> x -> Parser x Number
number startExpecting invalidExpecting =
    P.Parser <|
        \s ->
            let
                c =
                    getCharAt s.offset s.src
            in
            if Char.isDigit c then
                if c == '0' then
                    chompZero (s.offset + 1) s invalidExpecting

                else
                    chompDigits (s.offset + 1) s invalidExpecting

            else
                P.Bad False (fromState s startExpecting)


chompZero : Int -> P.State -> x -> P.PStep x Number
chompZero offset s0 invalidExpecting =
    let
        c =
            getCharAt offset s0.src
    in
    if c == 'x' || c == 'X' then
        -- Hexadecimal
        chompHex (offset + 1) s0 invalidExpecting

    else if c == '.' then
        -- Float starting with 0
        chompFraction offset s0 invalidExpecting

    else if Char.isDigit c then
        -- Invalid: numbers can't start with 0 followed by digits (octal not supported)
        P.Bad True (fromState s0 invalidExpecting)

    else
        -- Just 0
        P.Good True
            (Int 0)
            { src = s0.src
            , offset = offset
            , indent = s0.indent
            , row = s0.row
            , col = s0.col + (offset - s0.offset)
            }


chompDigits : Int -> P.State -> x -> P.PStep x Number
chompDigits offset s0 invalidExpecting =
    let
        c =
            getCharAt offset s0.src
    in
    if Char.isDigit c then
        chompDigits (offset + 1) s0 invalidExpecting

    else if c == '.' then
        let
            nextChar =
                getCharAt (offset + 1) s0.src
        in
        if Char.isDigit nextChar then
            chompFraction offset s0 invalidExpecting

        else
            -- It's an int followed by a dot accessor or something
            finalizeInt offset s0

    else if c == 'e' || c == 'E' then
        chompExponent offset s0 invalidExpecting

    else
        finalizeInt offset s0


chompFraction : Int -> P.State -> x -> P.PStep x Number
chompFraction dotOffset s0 invalidExpecting =
    -- dotOffset points to the '.'
    let
        newOffset =
            chompDigitsHelper (dotOffset + 1) s0.src
    in
    if newOffset == dotOffset + 1 then
        -- No digits after the dot
        P.Bad True (fromStateAt s0.row (s0.col + (dotOffset - s0.offset)) invalidExpecting)

    else
        let
            c =
                getCharAt newOffset s0.src
        in
        if c == 'e' || c == 'E' then
            chompExponent newOffset s0 invalidExpecting

        else
            finalizeFloat newOffset s0


chompExponent : Int -> P.State -> x -> P.PStep x Number
chompExponent eOffset s0 invalidExpecting =
    let
        signOffset =
            eOffset + 1

        c =
            getCharAt signOffset s0.src

        digitStart =
            if c == '+' || c == '-' then
                signOffset + 1

            else
                signOffset

        digitEnd =
            chompDigitsHelper digitStart s0.src
    in
    if digitEnd == digitStart then
        -- No digits after exponent
        P.Bad True (fromStateAt s0.row (s0.col + (eOffset - s0.offset)) invalidExpecting)

    else
        finalizeFloat digitEnd s0


chompHex : Int -> P.State -> x -> P.PStep x Number
chompHex offset s0 invalidExpecting =
    let
        newOffset =
            chompHexDigits offset s0.src
    in
    if newOffset == offset then
        -- No hex digits after 0x
        P.Bad True (fromState s0 invalidExpecting)

    else
        let
            hexStr =
                String.slice offset newOffset s0.src
        in
        case hexToInt hexStr of
            Just value ->
                P.Good True
                    (Int value)
                    { src = s0.src
                    , offset = newOffset
                    , indent = s0.indent
                    , row = s0.row
                    , col = s0.col + (newOffset - s0.offset)
                    }

            Nothing ->
                P.Bad True (fromState s0 invalidExpecting)


chompDigitsHelper : Int -> String -> Int
chompDigitsHelper offset src =
    let
        c =
            getCharAt offset src
    in
    if Char.isDigit c then
        chompDigitsHelper (offset + 1) src

    else
        offset


chompHexDigits : Int -> String -> Int
chompHexDigits offset src =
    let
        c =
            getCharAt offset src
    in
    if Char.isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') then
        chompHexDigits (offset + 1) src

    else
        offset


finalizeInt : Int -> P.State -> P.PStep x Number
finalizeInt endOffset s0 =
    let
        numStr =
            String.slice s0.offset endOffset s0.src
    in
    case String.toInt numStr of
        Just value ->
            P.Good True
                (Int value)
                { src = s0.src
                , offset = endOffset
                , indent = s0.indent
                , row = s0.row
                , col = s0.col + (endOffset - s0.offset)
                }

        Nothing ->
            -- This shouldn't happen if we chomped correctly
            P.Good True
                (Int 0)
                { src = s0.src
                , offset = endOffset
                , indent = s0.indent
                , row = s0.row
                , col = s0.col + (endOffset - s0.offset)
                }


finalizeFloat : Int -> P.State -> P.PStep x Number
finalizeFloat endOffset s0 =
    let
        numStr =
            String.slice s0.offset endOffset s0.src
    in
    case String.toFloat numStr of
        Just value ->
            P.Good True
                (Float value)
                { src = s0.src
                , offset = endOffset
                , indent = s0.indent
                , row = s0.row
                , col = s0.col + (endOffset - s0.offset)
                }

        Nothing ->
            -- This shouldn't happen if we chomped correctly
            P.Good True
                (Float 0.0)
                { src = s0.src
                , offset = endOffset
                , indent = s0.indent
                , row = s0.row
                , col = s0.col + (endOffset - s0.offset)
                }


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
