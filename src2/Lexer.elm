module Lexer exposing
    ( Token
    , TokenType(..)
    , lex
    , lexer
    )

{-| Lexer for tcelm.

Tokenizes source code into a stream of tokens for the parser.
Handles:
- Keywords and identifiers
- Operators and punctuation
- Literals (int, float, string, char)
- Comments
- Indentation-sensitive layout
-}

import AST exposing (Position, Region)


-- TOKENS


type alias Token =
    { type_ : TokenType
    , value : String
    , region : Region
    }


type TokenType
    -- Keywords
    = KwModule
    | KwExposing
    | KwImport
    | KwAs
    | KwType
    | KwAlias
    | KwPort
    | KwIf
    | KwThen
    | KwElse
    | KwCase
    | KwOf
    | KwLet
    | KwIn
    | KwWhere
    | KwDo
    | KwClass
    | KwInstance
    | KwDeriving
    | KwForeign
    | KwInfix
    | KwInfixl
    | KwInfixr
    -- Identifiers
    | LowerIdent      -- foo, bar
    | UpperIdent      -- Foo, Bar
    | Operator        -- +, -, ++, |>
    -- Literals
    | IntLit
    | FloatLit
    | StringLit
    | CharLit
    -- Punctuation
    | LParen          -- (
    | RParen          -- )
    | LBrace          -- {
    | RBrace          -- }
    | LBracket        -- [
    | RBracket        -- ]
    | Comma           -- ,
    | Dot             -- .
    | DotDot          -- ..
    | Colon           -- :
    | DoubleColon     -- ::
    | Equals          -- =
    | Pipe            -- |
    | Arrow           -- ->
    | BackArrow       -- <-
    | FatArrow        -- =>
    | Backslash       -- \
    | Underscore      -- _
    | At              -- @
    | Semicolon       -- ;
    -- Layout
    | Newline
    | Indent Int
    -- Special
    | EOF
    | Error String



-- LEXER STATE


type alias LexState =
    { input : String
    , pos : Int
    , line : Int
    , col : Int
    , tokens : List Token
    }


initState : String -> LexState
initState input =
    { input = input
    , pos = 0
    , line = 1
    , col = 1
    , tokens = []
    }


currentPos : LexState -> Position
currentPos state =
    { line = state.line, column = state.col }


peek : LexState -> Maybe Char
peek state =
    String.uncons (String.dropLeft state.pos state.input)
        |> Maybe.map Tuple.first


peekN : Int -> LexState -> Maybe Char
peekN n state =
    String.uncons (String.dropLeft (state.pos + n) state.input)
        |> Maybe.map Tuple.first


advance : LexState -> LexState
advance state =
    case peek state of
        Nothing ->
            state

        Just '\n' ->
            { state
                | pos = state.pos + 1
                , line = state.line + 1
                , col = 1
            }

        Just _ ->
            { state
                | pos = state.pos + 1
                , col = state.col + 1
            }


advanceN : Int -> LexState -> LexState
advanceN n state =
    if n <= 0 then
        state
    else
        advanceN (n - 1) (advance state)


emit : TokenType -> String -> Position -> Position -> LexState -> LexState
emit type_ value start end state =
    let
        token =
            { type_ = type_
            , value = value
            , region = { start = start, end = end }
            }
    in
    { state | tokens = token :: state.tokens }



-- MAIN LEXER


lex : String -> Result String (List Token)
lex input =
    let
        finalState = lexer (initState input)
    in
    Ok (List.reverse finalState.tokens ++ [ { type_ = EOF, value = "", region = { start = currentPos finalState, end = currentPos finalState } } ])


lexer : LexState -> LexState
lexer state =
    case peek state of
        Nothing ->
            state

        Just c ->
            lexer (lexOne state c)


lexOne : LexState -> Char -> LexState
lexOne state c =
    let
        start = currentPos state
    in
    if c == ' ' then
        advance state

    else if c == '\n' then
        let
            state1 = advance state
            indentLevel = countIndent state1
        in
        -- Emit newline and indent
        emit Newline "\n" start (currentPos state1) state1
            |> (\s -> emit (Indent indentLevel) (String.repeat indentLevel " ") (currentPos s) (currentPos (advanceN indentLevel s)) s)
            |> advanceN indentLevel

    else if c == '-' && peekN 1 state == Just '-' then
        lexLineComment state

    else if c == '{' && peekN 1 state == Just '-' then
        lexBlockComment state

    else if Char.isDigit c then
        lexNumber state

    else if c == '"' then
        lexString state

    else if c == '\'' then
        lexChar state

    else if isIdentStart c then
        lexIdent state

    else if isOperatorChar c then
        lexOperator state

    else
        lexPunctuation state c


countIndent : LexState -> Int
countIndent state =
    case peek state of
        Just ' ' ->
            1 + countIndent (advance state)

        _ ->
            0



-- COMMENTS


lexLineComment : LexState -> LexState
lexLineComment state =
    case peek state of
        Nothing ->
            state

        Just '\n' ->
            state

        Just _ ->
            lexLineComment (advance state)


lexBlockComment : LexState -> LexState
lexBlockComment state =
    let
        state1 = advanceN 2 state  -- skip {-
    in
    lexBlockCommentBody state1 1


lexBlockCommentBody : LexState -> Int -> LexState
lexBlockCommentBody state depth =
    if depth == 0 then
        state
    else
        case peek state of
            Nothing ->
                state

            Just '{' ->
                if peekN 1 state == Just '-' then
                    lexBlockCommentBody (advanceN 2 state) (depth + 1)
                else
                    lexBlockCommentBody (advance state) depth

            Just '-' ->
                if peekN 1 state == Just '}' then
                    lexBlockCommentBody (advanceN 2 state) (depth - 1)
                else
                    lexBlockCommentBody (advance state) depth

            Just _ ->
                lexBlockCommentBody (advance state) depth



-- NUMBERS


lexNumber : LexState -> LexState
lexNumber state =
    let
        start = currentPos state
        ( digits, state1 ) = collectWhile Char.isDigit state
    in
    case peek state1 of
        Just '.' ->
            case peekN 1 state1 of
                Just c ->
                    if Char.isDigit c then
                        -- Float
                        let
                            state2 = advance state1
                            ( decimals, state3 ) = collectWhile Char.isDigit state2
                            value = digits ++ "." ++ decimals
                        in
                        emit FloatLit value start (currentPos state3) state3
                    else
                        -- Int followed by dot
                        emit IntLit digits start (currentPos state1) state1

                Nothing ->
                    emit IntLit digits start (currentPos state1) state1

        _ ->
            emit IntLit digits start (currentPos state1) state1


collectWhile : (Char -> Bool) -> LexState -> ( String, LexState )
collectWhile pred state =
    case peek state of
        Just c ->
            if pred c then
                let
                    ( rest, state1 ) = collectWhile pred (advance state)
                in
                ( String.cons c rest, state1 )
            else
                ( "", state )

        Nothing ->
            ( "", state )



-- STRINGS


lexString : LexState -> LexState
lexString state =
    let
        start = currentPos state
        state1 = advance state  -- skip opening "
        ( content, state2 ) = lexStringContent state1
        state3 = advance state2  -- skip closing "
    in
    emit StringLit content start (currentPos state3) state3


lexStringContent : LexState -> ( String, LexState )
lexStringContent state =
    case peek state of
        Nothing ->
            ( "", state )

        Just '"' ->
            ( "", state )

        Just '\\' ->
            let
                ( escaped, state1 ) = lexEscape (advance state)
                ( rest, state2 ) = lexStringContent state1
            in
            ( escaped ++ rest, state2 )

        Just c ->
            let
                ( rest, state1 ) = lexStringContent (advance state)
            in
            ( String.cons c rest, state1 )


lexEscape : LexState -> ( String, LexState )
lexEscape state =
    case peek state of
        Just 'n' -> ( "\n", advance state )
        Just 't' -> ( "\t", advance state )
        Just 'r' -> ( "\u{000D}", advance state )
        Just '"' -> ( "\"", advance state )
        Just '\\' -> ( "\\", advance state )
        Just c -> ( String.fromChar c, advance state )
        Nothing -> ( "", state )



-- CHARS


lexChar : LexState -> LexState
lexChar state =
    let
        start = currentPos state
        state1 = advance state  -- skip opening '

        ( content, state2 ) =
            case peek state1 of
                Just '\\' ->
                    lexEscape (advance state1)

                Just c ->
                    ( String.fromChar c, advance state1 )

                Nothing ->
                    ( "", state1 )

        state3 = advance state2  -- skip closing '
    in
    emit CharLit content start (currentPos state3) state3



-- IDENTIFIERS AND KEYWORDS


isIdentStart : Char -> Bool
isIdentStart c =
    Char.isAlpha c || c == '_'


isIdentChar : Char -> Bool
isIdentChar c =
    Char.isAlphaNum c || c == '_' || c == '\''


lexIdent : LexState -> LexState
lexIdent state =
    let
        start = currentPos state
        ( ident, state1 ) = collectWhile isIdentChar state
        tokenType = identType ident
    in
    emit tokenType ident start (currentPos state1) state1


identType : String -> TokenType
identType ident =
    case ident of
        "module" -> KwModule
        "exposing" -> KwExposing
        "import" -> KwImport
        "as" -> KwAs
        "type" -> KwType
        "alias" -> KwAlias
        "port" -> KwPort
        "if" -> KwIf
        "then" -> KwThen
        "else" -> KwElse
        "case" -> KwCase
        "of" -> KwOf
        "let" -> KwLet
        "in" -> KwIn
        "where" -> KwWhere
        "do" -> KwDo
        "class" -> KwClass
        "instance" -> KwInstance
        "deriving" -> KwDeriving
        "foreign" -> KwForeign
        "infix" -> KwInfix
        "infixl" -> KwInfixl
        "infixr" -> KwInfixr
        _ ->
            case String.uncons ident of
                Just ( c, _ ) ->
                    if Char.isUpper c then
                        UpperIdent
                    else
                        LowerIdent

                Nothing ->
                    LowerIdent



-- OPERATORS


isOperatorChar : Char -> Bool
isOperatorChar c =
    List.member c [ '+', '-', '*', '/', '=', '<', '>', '!', '&', '|', '^', '~', ':', '.', '?', '@', '#', '$', '%' ]


lexOperator : LexState -> LexState
lexOperator state =
    let
        start = currentPos state
        ( op, state1 ) = collectWhile isOperatorChar state
    in
    -- Check for special operators that are tokens
    case op of
        "->" -> emit Arrow op start (currentPos state1) state1
        "<-" -> emit BackArrow op start (currentPos state1) state1
        "=>" -> emit FatArrow op start (currentPos state1) state1
        "=" -> emit Equals op start (currentPos state1) state1
        "|" -> emit Pipe op start (currentPos state1) state1
        ":" -> emit Colon op start (currentPos state1) state1
        "::" -> emit DoubleColon op start (currentPos state1) state1
        "." -> emit Dot op start (currentPos state1) state1
        ".." -> emit DotDot op start (currentPos state1) state1
        "@" -> emit At op start (currentPos state1) state1
        _ -> emit Operator op start (currentPos state1) state1



-- PUNCTUATION


lexPunctuation : LexState -> Char -> LexState
lexPunctuation state c =
    let
        start = currentPos state
        state1 = advance state
        end = currentPos state1
    in
    case c of
        '(' -> emit LParen "(" start end state1
        ')' -> emit RParen ")" start end state1
        '{' -> emit LBrace "{" start end state1
        '}' -> emit RBrace "}" start end state1
        '[' -> emit LBracket "[" start end state1
        ']' -> emit RBracket "]" start end state1
        ',' -> emit Comma "," start end state1
        '\\' -> emit Backslash "\\" start end state1
        '_' -> emit Underscore "_" start end state1
        ';' -> emit Semicolon ";" start end state1
        '\t' -> state1  -- skip tabs
        _ -> emit (Error ("Unexpected character: " ++ String.fromChar c)) (String.fromChar c) start end state1
