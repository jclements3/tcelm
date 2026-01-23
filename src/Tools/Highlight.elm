module Tools.Highlight exposing
    ( highlight, highlightHtml, highlightAnsi
    , Theme, defaultTheme, monokai, solarizedDark
    , Token, TokenType(..), tokenize
    )

{-| Syntax Highlighter - Output colored/highlighted Elm code.

Supports ANSI terminal colors and HTML output.

@docs highlight, highlightHtml, highlightAnsi
@docs Theme, defaultTheme, monokai, solarizedDark
@docs Token, TokenType, tokenize

-}


{-| A syntax token with its type and text.
-}
type alias Token =
    { tokenType : TokenType
    , text : String
    }


{-| Types of syntax tokens.
-}
type TokenType
    = Keyword
    | TypeName
    | FunctionName
    | VariableName
    | Operator
    | Punctuation
    | StringLit
    | NumberLit
    | CharLit
    | Comment
    | ModuleName
    | Whitespace


{-| Color theme for highlighting.
-}
type alias Theme =
    { keyword : String
    , typeName : String
    , functionName : String
    , variableName : String
    , operator : String
    , punctuation : String
    , stringLit : String
    , numberLit : String
    , charLit : String
    , comment : String
    , moduleName : String
    , default : String
    }


{-| Default color theme.
-}
defaultTheme : Theme
defaultTheme =
    { keyword = "#c678dd"
    , typeName = "#e5c07b"
    , functionName = "#61afef"
    , variableName = "#abb2bf"
    , operator = "#56b6c2"
    , punctuation = "#abb2bf"
    , stringLit = "#98c379"
    , numberLit = "#d19a66"
    , charLit = "#98c379"
    , comment = "#5c6370"
    , moduleName = "#e5c07b"
    , default = "#abb2bf"
    }


{-| Monokai color theme.
-}
monokai : Theme
monokai =
    { keyword = "#f92672"
    , typeName = "#66d9ef"
    , functionName = "#a6e22e"
    , variableName = "#f8f8f2"
    , operator = "#f92672"
    , punctuation = "#f8f8f2"
    , stringLit = "#e6db74"
    , numberLit = "#ae81ff"
    , charLit = "#e6db74"
    , comment = "#75715e"
    , moduleName = "#66d9ef"
    , default = "#f8f8f2"
    }


{-| Solarized Dark color theme.
-}
solarizedDark : Theme
solarizedDark =
    { keyword = "#859900"
    , typeName = "#b58900"
    , functionName = "#268bd2"
    , variableName = "#839496"
    , operator = "#859900"
    , punctuation = "#839496"
    , stringLit = "#2aa198"
    , numberLit = "#d33682"
    , charLit = "#2aa198"
    , comment = "#586e75"
    , moduleName = "#b58900"
    , default = "#839496"
    }


{-| Elm keywords.
-}
keywords : List String
keywords =
    [ "module", "exposing", "import", "as", "port"
    , "type", "alias"
    , "if", "then", "else"
    , "case", "of"
    , "let", "in"
    , "where"
    ]


{-| Check if a string is a keyword.
-}
isKeyword : String -> Bool
isKeyword s =
    List.member s keywords


{-| Check if a character starts an uppercase identifier.
-}
isUpper : Char -> Bool
isUpper c =
    Char.isUpper c


{-| Check if a character starts a lowercase identifier.
-}
isLower : Char -> Bool
isLower c =
    Char.isLower c || c == '_'


{-| Check if a character is an operator character.
-}
isOperator : Char -> Bool
isOperator c =
    List.member c [ '+', '-', '*', '/', '=', '<', '>', '&', '|', '^', '!', '?', '@', '#', '$', '%', '~', ':', '\\', '.', '`' ]


{-| Check if a character is punctuation.
-}
isPunctuation : Char -> Bool
isPunctuation c =
    List.member c [ '(', ')', '[', ']', '{', '}', ',', ';' ]


{-| Tokenize Elm source code.
-}
tokenize : String -> List Token
tokenize source =
    tokenizeHelper (String.toList source) []


tokenizeHelper : List Char -> List Token -> List Token
tokenizeHelper chars acc =
    case chars of
        [] ->
            List.reverse acc

        c :: rest ->
            if c == '-' && List.head rest == Just '-' then
                -- Line comment
                let
                    ( commentChars, remaining ) =
                        spanUntilNewline (c :: rest)
                in
                tokenizeHelper remaining ({ tokenType = Comment, text = String.fromList commentChars } :: acc)

            else if c == '{' && List.head rest == Just '-' then
                -- Block comment
                let
                    ( commentChars, remaining ) =
                        spanBlockComment (c :: rest)
                in
                tokenizeHelper remaining ({ tokenType = Comment, text = String.fromList commentChars } :: acc)

            else if c == '"' then
                -- String literal
                let
                    ( stringChars, remaining ) =
                        spanString rest
                in
                tokenizeHelper remaining ({ tokenType = StringLit, text = String.fromList (c :: stringChars) } :: acc)

            else if c == '\'' then
                -- Char literal
                let
                    ( charChars, remaining ) =
                        spanCharLit rest
                in
                tokenizeHelper remaining ({ tokenType = CharLit, text = String.fromList (c :: charChars) } :: acc)

            else if Char.isDigit c then
                -- Number
                let
                    ( numChars, remaining ) =
                        spanNumber (c :: rest)
                in
                tokenizeHelper remaining ({ tokenType = NumberLit, text = String.fromList numChars } :: acc)

            else if isUpper c then
                -- Type or module name
                let
                    ( identChars, remaining ) =
                        spanIdent (c :: rest)

                    text =
                        String.fromList identChars
                in
                tokenizeHelper remaining ({ tokenType = TypeName, text = text } :: acc)

            else if isLower c then
                -- Variable or function name or keyword
                let
                    ( identChars, remaining ) =
                        spanIdent (c :: rest)

                    text =
                        String.fromList identChars

                    tokenType =
                        if isKeyword text then
                            Keyword

                        else
                            VariableName
                in
                tokenizeHelper remaining ({ tokenType = tokenType, text = text } :: acc)

            else if isOperator c then
                -- Operator
                let
                    ( opChars, remaining ) =
                        spanOperator (c :: rest)
                in
                tokenizeHelper remaining ({ tokenType = Operator, text = String.fromList opChars } :: acc)

            else if isPunctuation c then
                tokenizeHelper rest ({ tokenType = Punctuation, text = String.fromChar c } :: acc)

            else if c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}' then
                -- Whitespace
                let
                    ( wsChars, remaining ) =
                        spanWhitespace (c :: rest)
                in
                tokenizeHelper remaining ({ tokenType = Whitespace, text = String.fromList wsChars } :: acc)

            else
                -- Unknown, treat as default
                tokenizeHelper rest ({ tokenType = Punctuation, text = String.fromChar c } :: acc)


spanUntilNewline : List Char -> ( List Char, List Char )
spanUntilNewline chars =
    spanWhile (\c -> c /= '\n') chars


spanBlockComment : List Char -> ( List Char, List Char )
spanBlockComment chars =
    spanBlockCommentHelper chars [] 0


spanBlockCommentHelper : List Char -> List Char -> Int -> ( List Char, List Char )
spanBlockCommentHelper chars acc depth =
    case chars of
        [] ->
            ( List.reverse acc, [] )

        '{' :: '-' :: rest ->
            spanBlockCommentHelper rest ('-' :: '{' :: acc) (depth + 1)

        '-' :: '}' :: rest ->
            if depth <= 1 then
                ( List.reverse ('}' :: '-' :: acc), rest )

            else
                spanBlockCommentHelper rest ('}' :: '-' :: acc) (depth - 1)

        c :: rest ->
            spanBlockCommentHelper rest (c :: acc) depth


spanString : List Char -> ( List Char, List Char )
spanString chars =
    spanStringHelper chars []


spanStringHelper : List Char -> List Char -> ( List Char, List Char )
spanStringHelper chars acc =
    case chars of
        [] ->
            ( List.reverse acc, [] )

        '\\' :: c :: rest ->
            spanStringHelper rest (c :: '\\' :: acc)

        '"' :: rest ->
            ( List.reverse ('"' :: acc), rest )

        c :: rest ->
            spanStringHelper rest (c :: acc)


spanCharLit : List Char -> ( List Char, List Char )
spanCharLit chars =
    case chars of
        '\\' :: c :: '\'' :: rest ->
            ( [ '\\', c, '\'' ], rest )

        c :: '\'' :: rest ->
            ( [ c, '\'' ], rest )

        _ ->
            ( [], chars )


spanNumber : List Char -> ( List Char, List Char )
spanNumber chars =
    spanWhile (\c -> Char.isDigit c || c == '.' || c == 'e' || c == 'E' || c == 'x' || c == 'X' || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) chars


spanIdent : List Char -> ( List Char, List Char )
spanIdent chars =
    spanWhile (\c -> Char.isAlphaNum c || c == '_') chars


spanOperator : List Char -> ( List Char, List Char )
spanOperator chars =
    spanWhile isOperator chars


spanWhitespace : List Char -> ( List Char, List Char )
spanWhitespace chars =
    spanWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}') chars


spanWhile : (Char -> Bool) -> List Char -> ( List Char, List Char )
spanWhile pred chars =
    spanWhileHelper pred chars []


spanWhileHelper : (Char -> Bool) -> List Char -> List Char -> ( List Char, List Char )
spanWhileHelper pred chars acc =
    case chars of
        [] ->
            ( List.reverse acc, [] )

        c :: rest ->
            if pred c then
                spanWhileHelper pred rest (c :: acc)

            else
                ( List.reverse acc, chars )



-- HIGHLIGHTING


{-| Highlight source code using the default theme and ANSI colors.
-}
highlight : String -> String
highlight =
    highlightAnsi defaultTheme


{-| Highlight source code to HTML with the given theme.
-}
highlightHtml : Theme -> String -> String
highlightHtml theme source =
    let
        tokens =
            tokenize source
    in
    "<pre><code>" ++ String.concat (List.map (tokenToHtml theme) tokens) ++ "</code></pre>"


tokenToHtml : Theme -> Token -> String
tokenToHtml theme token =
    let
        color =
            tokenColor theme token.tokenType

        escapedText =
            token.text
                |> String.replace "&" "&amp;"
                |> String.replace "<" "&lt;"
                |> String.replace ">" "&gt;"
    in
    if token.tokenType == Whitespace then
        escapedText

    else
        "<span style=\"color:" ++ color ++ "\">" ++ escapedText ++ "</span>"


{-| Highlight source code to ANSI terminal colors.
-}
highlightAnsi : Theme -> String -> String
highlightAnsi theme source =
    let
        tokens =
            tokenize source
    in
    String.concat (List.map (tokenToAnsi theme) tokens)


tokenToAnsi : Theme -> Token -> String
tokenToAnsi theme token =
    let
        ansiCode =
            tokenAnsiCode token.tokenType
    in
    if token.tokenType == Whitespace then
        token.text

    else
        ansiCode ++ token.text ++ "\u{001B}[0m"


tokenColor : Theme -> TokenType -> String
tokenColor theme tokenType =
    case tokenType of
        Keyword ->
            theme.keyword

        TypeName ->
            theme.typeName

        FunctionName ->
            theme.functionName

        VariableName ->
            theme.variableName

        Operator ->
            theme.operator

        Punctuation ->
            theme.punctuation

        StringLit ->
            theme.stringLit

        NumberLit ->
            theme.numberLit

        CharLit ->
            theme.charLit

        Comment ->
            theme.comment

        ModuleName ->
            theme.moduleName

        Whitespace ->
            theme.default


tokenAnsiCode : TokenType -> String
tokenAnsiCode tokenType =
    case tokenType of
        Keyword ->
            "\u{001B}[35m"

        -- Magenta
        TypeName ->
            "\u{001B}[33m"

        -- Yellow
        FunctionName ->
            "\u{001B}[34m"

        -- Blue
        VariableName ->
            "\u{001B}[0m"

        -- Default
        Operator ->
            "\u{001B}[36m"

        -- Cyan
        Punctuation ->
            "\u{001B}[0m"

        -- Default
        StringLit ->
            "\u{001B}[32m"

        -- Green
        NumberLit ->
            "\u{001B}[31m"

        -- Red
        CharLit ->
            "\u{001B}[32m"

        -- Green
        Comment ->
            "\u{001B}[90m"

        -- Gray
        ModuleName ->
            "\u{001B}[33m"

        -- Yellow
        Whitespace ->
            "\u{001B}[0m"
