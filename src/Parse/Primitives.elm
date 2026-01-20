module Parse.Primitives exposing
    ( Parser(..), PStep(..), run
    , succeed, fail, problem
    , map, map2, andThen, lazy
    , oneOf, oneOfWithFallback
    , chompIf, chompWhile, getChompedString
    , getPosition, getRow, getCol, getSource, getOffset
    , State, getState
    , Located, addLocation, addEnd
    , withIndent, getIndent, checkIndent
    , token, keyword, symbol
    , int, float, number
    , spaces, lineComment, multiComment
    , Context, inContext, DeadEnd, Bag(..)
    , specialize
    , backtrackable, commit
    )

{-| Parser primitives for Elm source code.
This is a parser combinator library inspired by elm/parser but
designed to match the Haskell Elm compiler's parser structure.
-}

import AST.Source exposing (Position, Region)


-- PARSER TYPE


{-| A parser that consumes a String and produces a value of type `a`.
The error type is `x`.
-}
type Parser x a
    = Parser (State -> PStep x a)


type PStep x a
    = Good Bool a State
    | Bad Bool (Bag (DeadEnd x))


type alias State =
    { src : String
    , offset : Int
    , indent : Int
    , row : Int
    , col : Int
    }


type alias DeadEnd x =
    { row : Int
    , col : Int
    , problem : x
    , contextStack : List (Context x)
    }


type alias Context x =
    { row : Int
    , col : Int
    , context : x
    }


type Bag a
    = Empty
    | AddRight (Bag a) a
    | Append (Bag a) (Bag a)



-- RUNNING


run : Parser x a -> String -> Result (List (DeadEnd x)) a
run (Parser parse) src =
    case parse { src = src, offset = 0, indent = 1, row = 1, col = 1 } of
        Good _ value _ ->
            Ok value

        Bad _ bag ->
            Err (bagToList bag [])


bagToList : Bag a -> List a -> List a
bagToList bag list =
    case bag of
        Empty ->
            list

        AddRight b a ->
            bagToList b (a :: list)

        Append b1 b2 ->
            bagToList b1 (bagToList b2 list)



-- PRIMITIVES


succeed : a -> Parser x a
succeed value =
    Parser (\s -> Good False value s)


fail : x -> Parser x a
fail x =
    Parser (\s -> Bad False (fromState s x))


problem : x -> Parser x a
problem x =
    Parser (\s -> Bad False (fromState s x))


fromState : State -> x -> Bag (DeadEnd x)
fromState s x =
    AddRight Empty { row = s.row, col = s.col, problem = x, contextStack = [] }



-- MAPPING


map : (a -> b) -> Parser x a -> Parser x b
map f (Parser parse) =
    Parser <|
        \s0 ->
            case parse s0 of
                Good p a s1 ->
                    Good p (f a) s1

                Bad p x ->
                    Bad p x


map2 : (a -> b -> c) -> Parser x a -> Parser x b -> Parser x c
map2 f (Parser parseA) (Parser parseB) =
    Parser <|
        \s0 ->
            case parseA s0 of
                Bad p x ->
                    Bad p x

                Good p1 a s1 ->
                    case parseB s1 of
                        Bad p2 x ->
                            Bad (p1 || p2) x

                        Good p2 b s2 ->
                            Good (p1 || p2) (f a b) s2


andThen : (a -> Parser x b) -> Parser x a -> Parser x b
andThen callback (Parser parseA) =
    Parser <|
        \s0 ->
            case parseA s0 of
                Bad p x ->
                    Bad p x

                Good p1 a s1 ->
                    let
                        (Parser parseB) =
                            callback a
                    in
                    case parseB s1 of
                        Bad p2 x ->
                            Bad (p1 || p2) x

                        Good p2 b s2 ->
                            Good (p1 || p2) b s2


lazy : (() -> Parser x a) -> Parser x a
lazy thunk =
    Parser <|
        \s ->
            let
                (Parser parse) =
                    thunk ()
            in
            parse s



-- ONE OF


oneOf : x -> List (Parser x a) -> Parser x a
oneOf toError parsers =
    Parser (\s -> oneOfHelp s toError parsers)


oneOfHelp : State -> x -> List (Parser x a) -> PStep x a
oneOfHelp s0 toError parsers =
    case parsers of
        [] ->
            Bad False (fromState s0 toError)

        (Parser parse) :: rest ->
            case parse s0 of
                (Good _ _ _) as good ->
                    good

                Bad False _ ->
                    oneOfHelp s0 toError rest

                (Bad True _) as bad ->
                    bad


oneOfWithFallback : List (Parser x a) -> a -> Parser x a
oneOfWithFallback parsers fallback =
    Parser (\s -> oneOfWithFallbackHelp s parsers fallback)


oneOfWithFallbackHelp : State -> List (Parser x a) -> a -> PStep x a
oneOfWithFallbackHelp s0 parsers fallback =
    case parsers of
        [] ->
            Good False fallback s0

        (Parser parse) :: rest ->
            case parse s0 of
                (Good _ _ _) as good ->
                    good

                Bad False _ ->
                    oneOfWithFallbackHelp s0 rest fallback

                (Bad True _) as bad ->
                    bad



-- CHOMPING


chompIf : (Char -> Bool) -> x -> Parser x ()
chompIf isGood expecting =
    Parser <|
        \s ->
            let
                newOffset =
                    isSubChar isGood s.offset s.src
            in
            if newOffset == -1 then
                Bad False (fromState s expecting)

            else if newOffset == -2 then
                Good True
                    ()
                    { src = s.src
                    , offset = s.offset + 1
                    , indent = s.indent
                    , row = s.row + 1
                    , col = 1
                    }

            else
                Good True
                    ()
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = s.row
                    , col = s.col + 1
                    }


chompWhile : (Char -> Bool) -> Parser x ()
chompWhile isGood =
    Parser <|
        \s ->
            chompWhileHelp isGood s.offset s.row s.col s


chompWhileHelp : (Char -> Bool) -> Int -> Int -> Int -> State -> PStep x ()
chompWhileHelp isGood offset row col s0 =
    let
        newOffset =
            isSubChar isGood offset s0.src
    in
    if newOffset == -1 then
        Good (s0.offset < offset)
            ()
            { src = s0.src
            , offset = offset
            , indent = s0.indent
            , row = row
            , col = col
            }

    else if newOffset == -2 then
        chompWhileHelp isGood (offset + 1) (row + 1) 1 s0

    else
        chompWhileHelp isGood newOffset row (col + 1) s0


getChompedString : Parser x a -> Parser x String
getChompedString (Parser parse) =
    Parser <|
        \s0 ->
            case parse s0 of
                Bad p x ->
                    Bad p x

                Good p _ s1 ->
                    Good p (String.slice s0.offset s1.offset s0.src) s1



-- POSITION


getPosition : Parser x Position
getPosition =
    Parser (\s -> Good False { row = s.row, col = s.col } s)


getRow : Parser x Int
getRow =
    Parser (\s -> Good False s.row s)


getCol : Parser x Int
getCol =
    Parser (\s -> Good False s.col s)


getOffset : Parser x Int
getOffset =
    Parser (\s -> Good False s.offset s)


getSource : Parser x String
getSource =
    Parser (\s -> Good False s.src s)


getState : Parser x State
getState =
    Parser (\s -> Good False s s)



-- LOCATION HELPERS


type alias Located a =
    AST.Source.Located a


addLocation : Parser x a -> Parser x (Located a)
addLocation (Parser parse) =
    Parser <|
        \s0 ->
            case parse s0 of
                Bad p x ->
                    Bad p x

                Good p a s1 ->
                    let
                        region =
                            { start = { row = s0.row, col = s0.col }
                            , end = { row = s1.row, col = s1.col }
                            }
                    in
                    Good p (AST.Source.At region a) s1


addEnd : Position -> a -> Parser x (Located a)
addEnd start value =
    Parser <|
        \s ->
            Good False
                (AST.Source.at start { row = s.row, col = s.col } value)
                s



-- INDENTATION


withIndent : Parser x a -> Parser x a
withIndent (Parser parse) =
    Parser <|
        \s0 ->
            case parse { s0 | indent = s0.col } of
                Good p a s1 ->
                    Good p a { s1 | indent = s0.indent }

                bad ->
                    bad


getIndent : Parser x Int
getIndent =
    Parser (\s -> Good False s.indent s)


checkIndent : x -> Parser x ()
checkIndent expecting =
    Parser <|
        \s ->
            if s.col > s.indent then
                Good False () s

            else
                Bad False (fromState s expecting)



-- TOKENS


token : String -> x -> Parser x ()
token str expecting =
    Parser <|
        \s ->
            let
                ( newOffset, newRow, newCol ) =
                    isSubString str s.offset s.row s.col s.src
            in
            if newOffset == -1 then
                Bad False (fromState s expecting)

            else
                Good True
                    ()
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = newRow
                    , col = newCol
                    }


keyword : String -> x -> Parser x ()
keyword kwd expecting =
    Parser <|
        \s ->
            let
                ( newOffset, newRow, newCol ) =
                    isSubString kwd s.offset s.row s.col s.src
            in
            if newOffset == -1 then
                Bad False (fromState s expecting)

            else if isVarChar (getCharAt newOffset s.src) then
                Bad False (fromState s expecting)

            else
                Good True
                    ()
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = newRow
                    , col = newCol
                    }


symbol : String -> x -> Parser x ()
symbol str expecting =
    token str expecting


isVarChar : Char -> Bool
isVarChar c =
    Char.isAlphaNum c || c == '_'



-- NUMBERS


int : x -> x -> Parser x Int
int expecting invalid =
    number
        { int = Ok identity
        , hex = Err invalid
        , octal = Err invalid
        , binary = Err invalid
        , float = Err invalid
        , invalid = invalid
        , expecting = expecting
        }


float : x -> x -> Parser x Float
float expecting invalid =
    number
        { int = Ok toFloat
        , hex = Err invalid
        , octal = Err invalid
        , binary = Err invalid
        , float = Ok identity
        , invalid = invalid
        , expecting = expecting
        }


type alias NumberConfig x a =
    { int : Result x (Int -> a)
    , hex : Result x (Int -> a)
    , octal : Result x (Int -> a)
    , binary : Result x (Int -> a)
    , float : Result x (Float -> a)
    , invalid : x
    , expecting : x
    }


number : NumberConfig x a -> Parser x a
number c =
    Parser <|
        \s ->
            if isAsciiCode 0x30 s.offset s.src then
                let
                    zeroOffset =
                        s.offset + 1

                    baseOffset =
                        zeroOffset
                in
                if isAsciiCode 0x78 baseOffset s.src then
                    -- 0x
                    case c.hex of
                        Err x ->
                            Bad True (fromState s x)

                        Ok toValue ->
                            finalizeInt c.invalid toValue (baseOffset + 1) (consumeBase16 (baseOffset + 1) s.src) s

                else if isAsciiCode 0x6F baseOffset s.src then
                    -- 0o
                    case c.octal of
                        Err x ->
                            Bad True (fromState s x)

                        Ok toValue ->
                            finalizeInt c.invalid toValue (baseOffset + 1) (consumeBase 8 (baseOffset + 1) s.src) s

                else if isAsciiCode 0x62 baseOffset s.src then
                    -- 0b
                    case c.binary of
                        Err x ->
                            Bad True (fromState s x)

                        Ok toValue ->
                            finalizeInt c.invalid toValue (baseOffset + 1) (consumeBase 2 (baseOffset + 1) s.src) s

                else
                    finalizeFloat c c.invalid zeroOffset (consumeBase 10 zeroOffset s.src) s

            else
                finalizeFloat c c.invalid s.offset (consumeBase 10 s.offset s.src) s


finalizeInt : x -> (Int -> a) -> Int -> Int -> State -> PStep x a
finalizeInt invalid toValue startOffset endOffset s =
    if startOffset == endOffset then
        Bad (s.offset < startOffset) (fromState s invalid)

    else
        case String.toInt (String.slice startOffset endOffset s.src) of
            Nothing ->
                Bad True (fromState s invalid)

            Just n ->
                Good True
                    (toValue n)
                    { src = s.src
                    , offset = endOffset
                    , indent = s.indent
                    , row = s.row
                    , col = s.col + (endOffset - s.offset)
                    }


finalizeFloat : NumberConfig x a -> x -> Int -> Int -> State -> PStep x a
finalizeFloat c invalid intStartOffset intEndOffset s =
    if intStartOffset == intEndOffset then
        Bad (s.offset < intStartOffset) (fromState s c.expecting)

    else if isAsciiCode 0x2E intEndOffset s.src then
        -- .
        let
            floatEndOffset =
                consumeBase 10 (intEndOffset + 1) s.src
        in
        if intEndOffset + 1 == floatEndOffset then
            Bad True (fromState { s | row = s.row, col = s.col + (intEndOffset - s.offset) } invalid)

        else
            case c.float of
                Err x ->
                    Bad True (fromState s x)

                Ok toValue ->
                    let
                        expEndOffset =
                            consumeExp floatEndOffset s.src
                    in
                    case String.toFloat (String.slice s.offset expEndOffset s.src) of
                        Nothing ->
                            Bad True (fromState s invalid)

                        Just n ->
                            Good True
                                (toValue n)
                                { src = s.src
                                , offset = expEndOffset
                                , indent = s.indent
                                , row = s.row
                                , col = s.col + (expEndOffset - s.offset)
                                }

    else if isAsciiCode 0x65 intEndOffset s.src || isAsciiCode 0x45 intEndOffset s.src then
        -- e E
        case c.float of
            Err x ->
                Bad True (fromState s x)

            Ok toValue ->
                let
                    expEndOffset =
                        consumeExp (intEndOffset + 1) s.src
                in
                case String.toFloat (String.slice s.offset expEndOffset s.src) of
                    Nothing ->
                        Bad True (fromState s invalid)

                    Just n ->
                        Good True
                            (toValue n)
                            { src = s.src
                            , offset = expEndOffset
                            , indent = s.indent
                            , row = s.row
                            , col = s.col + (expEndOffset - s.offset)
                            }

    else
        case c.int of
            Err x ->
                Bad True (fromState s x)

            Ok toValue ->
                case String.toInt (String.slice s.offset intEndOffset s.src) of
                    Nothing ->
                        Bad True (fromState s invalid)

                    Just n ->
                        Good True
                            (toValue n)
                            { src = s.src
                            , offset = intEndOffset
                            , indent = s.indent
                            , row = s.row
                            , col = s.col + (intEndOffset - s.offset)
                            }


consumeBase : Int -> Int -> String -> Int
consumeBase base offset src =
    let
        digit =
            getCharCodeAt offset src
    in
    if digit >= 0x30 && digit < 0x30 + min base 10 then
        consumeBase base (offset + 1) src

    else if base > 10 && digit >= 0x61 && digit < 0x61 + base - 10 then
        consumeBase base (offset + 1) src

    else if base > 10 && digit >= 0x41 && digit < 0x41 + base - 10 then
        consumeBase base (offset + 1) src

    else
        offset


consumeBase16 : Int -> String -> Int
consumeBase16 offset src =
    let
        digit =
            getCharCodeAt offset src
    in
    if (digit >= 0x30 && digit <= 0x39) || (digit >= 0x61 && digit <= 0x66) || (digit >= 0x41 && digit <= 0x46) then
        consumeBase16 (offset + 1) src

    else
        offset


consumeExp : Int -> String -> Int
consumeExp offset src =
    if isAsciiCode 0x65 offset src || isAsciiCode 0x45 offset src then
        let
            signOffset =
                if isAsciiCode 0x2B (offset + 1) src || isAsciiCode 0x2D (offset + 1) src then
                    offset + 2

                else
                    offset + 1
        in
        consumeBase 10 signOffset src

    else
        offset



-- WHITESPACE


spaces : Parser x ()
spaces =
    chompWhile (\c -> c == ' ' || c == '\n' || c == '\u{000D}')


lineComment : x -> String -> Parser x ()
lineComment prob str =
    token str prob
        |> andThen (\_ -> chompWhile (\c -> c /= '\n'))


multiComment : x -> String -> String -> Parser x ()
multiComment prob open close =
    token open prob
        |> andThen (\_ -> chompUntilString close)


chompUntilString : String -> Parser x ()
chompUntilString str =
    Parser <|
        \s ->
            let
                ( newOffset, newRow, newCol ) =
                    findSubString str s.offset s.row s.col s.src
            in
            if newOffset == -1 then
                Bad False Empty

            else
                Good True
                    ()
                    { src = s.src
                    , offset = newOffset + String.length str
                    , indent = s.indent
                    , row = newRow
                    , col = newCol + String.length str
                    }



-- CONTEXT


inContext : x -> Parser x () -> Parser x a -> Parser x a
inContext ctx (Parser parseStart) (Parser parseA) =
    Parser <|
        \s0 ->
            case parseStart s0 of
                Bad p x ->
                    Bad p x

                Good p1 _ s1 ->
                    case parseA s1 of
                        Good p2 a s2 ->
                            Good (p1 || p2) a s2

                        Bad p2 bag ->
                            Bad (p1 || p2) (addContext s0.row s0.col ctx bag)


addContext : Int -> Int -> x -> Bag (DeadEnd x) -> Bag (DeadEnd x)
addContext row col ctx bag =
    let
        context =
            { row = row, col = col, context = ctx }
    in
    mapBag (\d -> { d | contextStack = context :: d.contextStack }) bag


mapBag : (a -> b) -> Bag a -> Bag b
mapBag f bag =
    case bag of
        Empty ->
            Empty

        AddRight b a ->
            AddRight (mapBag f b) (f a)

        Append b1 b2 ->
            Append (mapBag f b1) (mapBag f b2)



-- SPECIALIZE


specialize : (x -> y) -> Parser x a -> Parser y a
specialize func (Parser parse) =
    Parser <|
        \s0 ->
            case parse s0 of
                Good p a s1 ->
                    Good p a s1

                Bad p bag ->
                    Bad p (mapBag (mapDeadEnd func) bag)


mapDeadEnd : (x -> y) -> DeadEnd x -> DeadEnd y
mapDeadEnd func deadEnd =
    { row = deadEnd.row
    , col = deadEnd.col
    , problem = func deadEnd.problem
    , contextStack = List.map (mapContext func) deadEnd.contextStack
    }


mapContext : (x -> y) -> Context x -> Context y
mapContext func ctx =
    { row = ctx.row
    , col = ctx.col
    , context = func ctx.context
    }



-- BACKTRACKING


backtrackable : Parser x a -> Parser x a
backtrackable (Parser parse) =
    Parser <|
        \s0 ->
            case parse s0 of
                Bad _ x ->
                    Bad False x

                Good _ a s1 ->
                    Good False a s1


commit : a -> Parser x a
commit a =
    Parser (\s -> Good True a s)



-- LOW LEVEL HELPERS


isSubChar : (Char -> Bool) -> Int -> String -> Int
isSubChar isGood offset src =
    case String.uncons (String.dropLeft offset src) of
        Nothing ->
            -1

        Just ( c, _ ) ->
            if isGood c then
                if c == '\n' then
                    -2

                else
                    offset + charWidth c

            else
                -1


charWidth : Char -> Int
charWidth c =
    let
        code =
            Char.toCode c
    in
    if code < 0x00010000 then
        1

    else
        2


isSubString : String -> Int -> Int -> Int -> String -> ( Int, Int, Int )
isSubString target offset row col src =
    isSubStringHelp target 0 (String.length target) offset row col src


isSubStringHelp : String -> Int -> Int -> Int -> Int -> Int -> String -> ( Int, Int, Int )
isSubStringHelp target targetOffset targetLength offset row col src =
    if targetOffset >= targetLength then
        ( offset, row, col )

    else
        let
            targetChar =
                getCharAt targetOffset target

            srcChar =
                getCharAt offset src
        in
        if targetChar == srcChar then
            if targetChar == '\n' then
                isSubStringHelp target (targetOffset + 1) targetLength (offset + 1) (row + 1) 1 src

            else
                isSubStringHelp target (targetOffset + 1) targetLength (offset + 1) row (col + 1) src

        else
            ( -1, row, col )


findSubString : String -> Int -> Int -> Int -> String -> ( Int, Int, Int )
findSubString target offset row col src =
    let
        srcLen =
            String.length src

        targetLen =
            String.length target
    in
    findSubStringHelp target targetLen offset row col src srcLen


findSubStringHelp : String -> Int -> Int -> Int -> Int -> String -> Int -> ( Int, Int, Int )
findSubStringHelp target targetLen offset row col src srcLen =
    if offset + targetLen > srcLen then
        ( -1, row, col )

    else if String.slice offset (offset + targetLen) src == target then
        ( offset, row, col )

    else
        let
            c =
                getCharAt offset src
        in
        if c == '\n' then
            findSubStringHelp target targetLen (offset + 1) (row + 1) 1 src srcLen

        else
            findSubStringHelp target targetLen (offset + 1) row (col + 1) src srcLen


getCharAt : Int -> String -> Char
getCharAt offset src =
    String.uncons (String.dropLeft offset src)
        |> Maybe.map Tuple.first
        |> Maybe.withDefault '\u{0000}'


getCharCodeAt : Int -> String -> Int
getCharCodeAt offset src =
    Char.toCode (getCharAt offset src)


isAsciiCode : Int -> Int -> String -> Bool
isAsciiCode code offset src =
    getCharCodeAt offset src == code
