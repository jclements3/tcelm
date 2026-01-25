module Parser exposing
    ( parse
    , parseModule
    , ParseError
    )

{-| Parser for tcelm.

Parses a stream of tokens into an AST.
Uses a recursive descent approach with operator precedence parsing for expressions.

Supports:
- Module declarations
- Import statements
- Type definitions (alias, custom types, type classes)
- Function definitions
- Pattern matching
- do-notation
- All Elm expression syntax
-}

import AST exposing (..)
import Lexer exposing (Token, TokenType(..))


-- PARSE ERRORS


type alias ParseError =
    { message : String
    , region : Region
    }



-- PARSER STATE


type alias Parser a =
    ParseState -> Result ParseError ( a, ParseState )


type alias ParseState =
    { tokens : List Token
    , pos : Int
    }


initState : List Token -> ParseState
initState tokens =
    { tokens = tokens
    , pos = 0
    }


currentToken : ParseState -> Maybe Token
currentToken state =
    List.head (List.drop state.pos state.tokens)


currentRegion : ParseState -> Region
currentRegion state =
    case currentToken state of
        Just tok -> tok.region
        Nothing -> { start = { line = 0, column = 0 }, end = { line = 0, column = 0 } }


advance : ParseState -> ParseState
advance state =
    { state | pos = state.pos + 1 }


expect : TokenType -> Parser Token
expect expected state =
    case currentToken state of
        Just tok ->
            if tok.type_ == expected then
                Ok ( tok, advance state )
            else
                Err { message = "Expected " ++ tokenTypeName expected ++ " but got " ++ tokenTypeName tok.type_, region = tok.region }

        Nothing ->
            Err { message = "Unexpected end of input, expected " ++ tokenTypeName expected, region = currentRegion state }


expectIdent : TokenType -> Parser String
expectIdent expected state =
    case expect expected state of
        Ok ( tok, state1 ) -> Ok ( tok.value, state1 )
        Err e -> Err e


peek : TokenType -> ParseState -> Bool
peek expected state =
    case currentToken state of
        Just tok -> tok.type_ == expected
        Nothing -> False


peekAny : List TokenType -> ParseState -> Bool
peekAny expected state =
    case currentToken state of
        Just tok -> List.member tok.type_ expected
        Nothing -> False


skip : TokenType -> ParseState -> ParseState
skip expected state =
    if peek expected state then
        advance state
    else
        state


skipNewlines : ParseState -> ParseState
skipNewlines state =
    case currentToken state of
        Just tok ->
            case tok.type_ of
                Newline ->
                    skipNewlines (advance state)

                Indent _ ->
                    skipNewlines (advance state)

                _ ->
                    state

        Nothing ->
            state


-- Skip newlines but stop if the next token is at column 1 AND is not an operator
-- This allows multi-line expressions with operators at the start of a line
-- (e.g., pipelines) while still stopping at new top-level declarations
skipNewlinesUnlessAtColumn1 : ParseState -> ParseState
skipNewlinesUnlessAtColumn1 state =
    case currentToken state of
        Just tok ->
            case tok.type_ of
                Newline ->
                    skipNewlinesUnlessAtColumn1 (advance state)

                Indent _ ->
                    skipNewlinesUnlessAtColumn1 (advance state)

                _ ->
                    -- If token is at column 1, check if it's an operator (continue) or not (stop)
                    if tok.region.start.column == 1 then
                        -- Operators at column 1 are continuations (like |> at start of line)
                        if tok.type_ == Operator || tok.type_ == DoubleColon then
                            state
                        else
                            -- Non-operator at column 1 = new declaration, don't advance
                            state
                    else
                        state

        Nothing ->
            state


tokenTypeName : TokenType -> String
tokenTypeName tt =
    case tt of
        KwModule -> "module"
        KwExposing -> "exposing"
        KwImport -> "import"
        KwAs -> "as"
        KwType -> "type"
        KwAlias -> "alias"
        KwIf -> "if"
        KwThen -> "then"
        KwElse -> "else"
        KwCase -> "case"
        KwOf -> "of"
        KwLet -> "let"
        KwIn -> "in"
        KwDo -> "do"
        KwClass -> "class"
        KwInstance -> "instance"
        KwWhere -> "where"
        KwForeign -> "foreign"
        LowerIdent -> "identifier"
        UpperIdent -> "type name"
        Operator -> "operator"
        IntLit -> "integer"
        FloatLit -> "float"
        StringLit -> "string"
        CharLit -> "character"
        LParen -> "("
        RParen -> ")"
        LBrace -> "{"
        RBrace -> "}"
        LBracket -> "["
        RBracket -> "]"
        Comma -> ","
        Equals -> "="
        Arrow -> "->"
        BackArrow -> "<-"
        Pipe -> "|"
        Colon -> ":"
        DoubleColon -> "::"
        Dot -> "."
        DotDot -> ".."
        Backslash -> "\\"
        Underscore -> "_"
        FatArrow -> "=>"
        Newline -> "newline"
        Indent _ -> "indentation"
        EOF -> "end of file"
        _ -> "token"



-- COMBINATORS


succeed : a -> Parser a
succeed value state =
    Ok ( value, state )


fail : String -> Parser a
fail msg state =
    Err { message = msg, region = currentRegion state }


map : (a -> b) -> Parser a -> Parser b
map f parser state =
    case parser state of
        Ok ( a, state1 ) -> Ok ( f a, state1 )
        Err e -> Err e


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen f parser state =
    case parser state of
        Ok ( a, state1 ) -> f a state1
        Err e -> Err e


map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 f pa pb =
    pa |> andThen (\a -> pb |> map (\b -> f a b))


optional : Parser a -> Parser (Maybe a)
optional parser state =
    case parser state of
        Ok ( a, state1 ) -> Ok ( Just a, state1 )
        Err _ -> Ok ( Nothing, state )


many : Parser a -> Parser (List a)
many parser state =
    case parser state of
        Err _ ->
            Ok ( [], state )

        Ok ( first, state1 ) ->
            case many parser state1 of
                Ok ( rest, state2 ) ->
                    Ok ( first :: rest, state2 )

                Err e ->
                    Err e


sepBy : Parser sep -> Parser a -> Parser (List a)
sepBy sep item state =
    case item state of
        Err _ ->
            Ok ( [], state )

        Ok ( first, state1 ) ->
            let
                parseRest : ParseState -> Result ParseError ( List a, ParseState )
                parseRest s =
                    case sep s of
                        Err _ ->
                            Ok ( [], s )

                        Ok ( _, s1 ) ->
                            case item s1 of
                                Err _ ->
                                    Ok ( [], s )

                                Ok ( next, s2 ) ->
                                    case parseRest s2 of
                                        Ok ( rest, s3 ) ->
                                            Ok ( next :: rest, s3 )

                                        Err e ->
                                            Err e
            in
            case parseRest state1 of
                Ok ( rest, state2 ) ->
                    Ok ( first :: rest, state2 )

                Err e ->
                    Err e



-- MAIN ENTRY POINT


parse : String -> Result ParseError Module
parse source =
    case Lexer.lex source of
        Err e ->
            Err { message = e, region = { start = { line = 1, column = 1 }, end = { line = 1, column = 1 } } }

        Ok tokens ->
            parseModule tokens


parseModule : List Token -> Result ParseError Module
parseModule tokens =
    let
        state = initState tokens |> skipNewlines
    in
    case moduleParser state of
        Ok ( mod, _ ) -> Ok mod
        Err e -> Err e


moduleParser : Parser Module
moduleParser state =
    let
        state1 = skipNewlines state

        -- Parse optional module header
        ( moduleName, state2 ) =
            if peek KwModule state1 then
                case moduleHeader state1 of
                    Ok ( name, s ) -> ( Just name, s )
                    Err _ -> ( Nothing, state1 )
            else
                ( Nothing, state1 )

        state3 = skipNewlines state2

        -- Parse imports
        ( imports, state4 ) =
            parseImports state3

        state5 = skipNewlines state4

        -- Parse declarations
        ( decls, state6 ) =
            parseDecls state5
    in
    Ok
        ( { name = moduleName
          , exports = Nothing
          , imports = imports
          , decls = decls
          }
        , state6
        )


moduleHeader : Parser (Located Name)
moduleHeader =
    expect KwModule
        |> andThen (\_ -> expectIdent UpperIdent)
        |> andThen (\name state ->
            let
                region = currentRegion state
            in
            -- Skip exposing clause for now
            let
                state1 =
                    if peek KwExposing state then
                        skipExposing (advance state)
                    else
                        state
            in
            Ok ( locate region name, state1 )
        )


skipExposing : ParseState -> ParseState
skipExposing state =
    let
        state1 = skip LParen state
    in
    skipUntil RParen state1


skipUntil : TokenType -> ParseState -> ParseState
skipUntil target state =
    if peek target state then
        advance state
    else if peek EOF state then
        state
    else
        skipUntil target (advance state)



-- IMPORTS


parseImports : ParseState -> ( List (Located Import), ParseState )
parseImports state =
    let
        state1 = skipNewlines state
    in
    if peek KwImport state1 then
        case parseImport state1 of
            Ok ( imp, state2 ) ->
                let
                    ( rest, state3 ) = parseImports state2
                in
                ( imp :: rest, state3 )

            Err _ ->
                ( [], state1 )
    else
        ( [], state1 )


parseImport : Parser (Located Import)
parseImport =
    expect KwImport
        |> andThen (\tok ->
            expectIdent UpperIdent
                |> andThen (\modName state ->
                    let
                        alias_ =
                            if peek KwAs state then
                                case expectIdent UpperIdent (advance state) of
                                    Ok ( a, _ ) -> Just a
                                    Err _ -> Nothing
                            else
                                Nothing

                        state1 =
                            if peek KwAs state then
                                case expectIdent UpperIdent (advance state) of
                                    Ok ( _, s ) -> s
                                    Err _ -> state
                            else
                                state

                        -- Skip exposing clause
                        state2 =
                            if peek KwExposing state1 then
                                skipExposing (advance state1)
                            else
                                state1
                    in
                    Ok
                        ( locate tok.region
                            { module_ = locate tok.region modName
                            , alias_ = alias_
                            , exposing_ = Nothing
                            }
                        , state2
                        )
                )
        )



-- DECLARATIONS


parseDecls : ParseState -> ( List (Located Decl), ParseState )
parseDecls state =
    let
        state1 = skipNewlines state
    in
    if peek EOF state1 then
        ( [], state1 )
    else
        case parseDecl state1 of
            Ok ( decl, state2 ) ->
                let
                    ( rest, state3 ) = parseDecls state2
                in
                ( decl :: rest, state3 )

            Err _ ->
                ( [], state1 )


parseDecl : Parser (Located Decl)
parseDecl state =
    let
        state1 = skipNewlines state
    in
    case currentToken state1 of
        Just tok ->
            case tok.type_ of
                KwType ->
                    parseTypeDecl state1

                KwClass ->
                    parseClassDecl state1

                KwInstance ->
                    parseInstanceDecl state1

                KwForeign ->
                    parseForeignDecl state1

                KwInfix ->
                    parseInfixDecl state1

                KwInfixl ->
                    parseInfixDecl state1

                KwInfixr ->
                    parseInfixDecl state1

                LowerIdent ->
                    parseValueDecl state1

                _ ->
                    Err { message = "Expected declaration", region = tok.region }

        Nothing ->
            Err { message = "Unexpected end of input", region = currentRegion state1 }


parseTypeDecl : Parser (Located Decl)
parseTypeDecl state =
    case expect KwType state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            if peek KwAlias state1 then
                parseTypeAlias tok.region (advance state1)
            else
                parseCustomType tok.region state1


parseTypeAlias : Region -> Parser (Located Decl)
parseTypeAlias startRegion state =
    case expectIdent UpperIdent state of
        Err e -> Err e
        Ok ( name, state1 ) ->
            let
                ( typeVars, state2 ) = parseTypeVars state1
            in
            case expect Equals state2 of
                Err e -> Err e
                Ok ( _, state3 ) ->
                    let
                        -- Skip newlines after = to support multi-line type aliases
                        state3a = skipNewlines state3
                    in
                    case parseTypeAnnotation state3a of
                        Err e -> Err e
                        Ok ( body, state4 ) ->
                            Ok
                                ( locate startRegion (TypeAliasDecl
                                    { name = locate startRegion name
                                    , typeVars = typeVars
                                    , body = body
                                    })
                                , state4
                                )


parseCustomType : Region -> Parser (Located Decl)
parseCustomType startRegion state =
    case expectIdent UpperIdent state of
        Err e -> Err e
        Ok ( name, state1 ) ->
            let
                ( typeVars, state2 ) = parseTypeVars state1
                -- Skip newlines before the = (allows multi-line type definitions)
                state2a = skipNewlines state2
            in
            case expect Equals state2a of
                Err e -> Err e
                Ok ( _, state3 ) ->
                    let
                        ( ctors, state4 ) = parseConstructors state3
                    in
                    Ok
                        ( locate startRegion (CustomTypeDecl
                            { name = locate startRegion name
                            , typeVars = typeVars
                            , constructors = ctors
                            })
                        , state4
                        )


parseTypeVars : ParseState -> ( List (Located Name), ParseState )
parseTypeVars state =
    if peek LowerIdent state then
        case currentToken state of
            Just tok ->
                let
                    ( rest, state1 ) = parseTypeVars (advance state)
                in
                ( locate tok.region tok.value :: rest, state1 )

            Nothing ->
                ( [], state )
    else
        ( [], state )


parseConstructors : ParseState -> ( List (Located Constructor), ParseState )
parseConstructors state =
    let
        state1 = skipNewlines state
    in
    case parseConstructor state1 of
        Err _ ->
            ( [], state1 )

        Ok ( ctor, state2 ) ->
            let
                state3 = skipNewlines state2
            in
            if peek Pipe state3 then
                let
                    ( rest, state4 ) = parseConstructors (advance state3)
                in
                ( ctor :: rest, state4 )
            else
                ( [ ctor ], state3 )


parseConstructor : Parser (Located Constructor)
parseConstructor state =
    case expectIdent UpperIdent state of
        Err e -> Err e
        Ok ( name, state1 ) ->
            let
                ( args, state2 ) = parseConstructorArgs state1
            in
            Ok
                ( locate (currentRegion state)
                    { name = locate (currentRegion state) name
                    , args = args
                    }
                , state2
                )


parseConstructorArgs : ParseState -> ( List (Located TypeAnnotation), ParseState )
parseConstructorArgs state =
    if peekAny [ UpperIdent, LowerIdent, LParen, LBrace ] state then
        case parseAtomicType state of
            Err _ ->
                ( [], state )

            Ok ( ty, state1 ) ->
                let
                    ( rest, state2 ) = parseConstructorArgs state1
                in
                ( ty :: rest, state2 )
    else
        ( [], state )


parseClassDecl : Parser (Located Decl)
parseClassDecl state =
    case expect KwClass state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            case expectIdent UpperIdent state1 of
                Err e -> Err e
                Ok ( name, state2 ) ->
                    let
                        ( typeVars, state3 ) = parseTypeVars state2
                        state4 = skipUntil KwWhere state3
                        state5 = advance state4
                        ( methods, state6 ) = parseMethodSigs state5
                    in
                    Ok
                        ( locate tok.region (TypeClassDecl
                            { name = locate tok.region name
                            , typeVars = typeVars
                            , superclasses = []
                            , methods = methods
                            })
                        , state6
                        )


parseMethodSigs : ParseState -> ( List (Located MethodSig), ParseState )
parseMethodSigs state =
    let
        state1 = skipNewlines state
    in
    if peek LowerIdent state1 then
        case parseMethodSig state1 of
            Err _ ->
                ( [], state1 )

            Ok ( method, state2 ) ->
                let
                    ( rest, state3 ) = parseMethodSigs state2
                in
                ( method :: rest, state3 )
    else
        ( [], state1 )


parseMethodSig : Parser (Located MethodSig)
parseMethodSig state =
    case expectIdent LowerIdent state of
        Err e -> Err e
        Ok ( name, state1 ) ->
            case expect Colon state1 of
                Err e -> Err e
                Ok ( _, state2 ) ->
                    case parseTypeAnnotation state2 of
                        Err e -> Err e
                        Ok ( ty, state3 ) ->
                            Ok
                                ( locate (currentRegion state)
                                    { name = locate (currentRegion state) name
                                    , type_ = ty
                                    }
                                , state3
                                )


parseInstanceDecl : Parser (Located Decl)
parseInstanceDecl state =
    case expect KwInstance state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            case expectIdent UpperIdent state1 of
                Err e -> Err e
                Ok ( className, state2 ) ->
                    let
                        ( typeArgs, state3 ) = parseConstructorArgs state2
                        state4 = skipUntil KwWhere state3
                        state5 = if peek KwWhere state4 then advance state4 else state4
                        ( methods, state6 ) = parseMethodImpls state5
                    in
                    Ok
                        ( locate tok.region (InstanceDecl
                            { context = []
                            , className = locate tok.region className
                            , typeArgs = typeArgs
                            , methods = methods
                            })
                        , state6
                        )


parseMethodImpls : ParseState -> ( List (Located MethodImpl), ParseState )
parseMethodImpls state =
    let
        state1 = skipNewlines state
    in
    if peek LowerIdent state1 then
        case parseMethodImpl state1 of
            Err _ ->
                ( [], state1 )

            Ok ( method, state2 ) ->
                let
                    ( rest, state3 ) = parseMethodImpls state2
                in
                ( method :: rest, state3 )
    else
        ( [], state1 )


parseMethodImpl : Parser (Located MethodImpl)
parseMethodImpl state =
    case expectIdent LowerIdent state of
        Err e -> Err e
        Ok ( name, state1 ) ->
            let
                ( args, state2 ) = parsePatterns state1
            in
            case expect Equals state2 of
                Err e -> Err e
                Ok ( _, state3 ) ->
                    case parseExpr state3 of
                        Err e -> Err e
                        Ok ( body, state4 ) ->
                            Ok
                                ( locate (currentRegion state)
                                    { name = locate (currentRegion state) name
                                    , args = args
                                    , body = body
                                    }
                                , state4
                                )


parseForeignDecl : Parser (Located Decl)
parseForeignDecl state =
    case expect KwForeign state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            case expect KwImport state1 of
                Err e -> Err e
                Ok ( _, state2 ) ->
                    case expectIdent LowerIdent state2 of
                        Err e -> Err e
                        Ok ( name, state3 ) ->
                            case expect Colon state3 of
                                Err e -> Err e
                                Ok ( _, state4 ) ->
                                    case parseTypeAnnotation state4 of
                                        Err e -> Err e
                                        Ok ( ty, state5 ) ->
                                            Ok
                                                ( locate tok.region (ForeignDecl
                                                    { name = locate tok.region name
                                                    , cName = name
                                                    , type_ = ty
                                                    })
                                                , state5
                                                )


parseInfixDecl : Parser (Located Decl)
parseInfixDecl state =
    case currentToken state of
        Just tok ->
            let
                assoc =
                    case tok.type_ of
                        KwInfixl -> LeftAssoc
                        KwInfixr -> RightAssoc
                        _ -> NonAssoc

                state1 = advance state
            in
            case expect IntLit state1 of
                Err e -> Err e
                Ok ( precTok, state2 ) ->
                    case expect Operator state2 of
                        Err e -> Err e
                        Ok ( opTok, state3 ) ->
                            case expect Equals state3 of
                                Err e -> Err e
                                Ok ( _, state4 ) ->
                                    case expectIdent LowerIdent state4 of
                                        Err e -> Err e
                                        Ok ( func, state5 ) ->
                                            Ok
                                                ( locate tok.region (InfixDecl
                                                    { operator = locate opTok.region opTok.value
                                                    , associativity = assoc
                                                    , precedence = String.toInt precTok.value |> Maybe.withDefault 9
                                                    , function = locate tok.region func
                                                    })
                                                , state5
                                                )

        Nothing ->
            Err { message = "Expected infix declaration", region = currentRegion state }


parseValueDecl : Parser (Located Decl)
parseValueDecl state =
    case expectIdent LowerIdent state of
        Err e -> Err e
        Ok ( name, state1 ) ->
            -- Check for type annotation
            if peek Colon state1 then
                case expect Colon state1 of
                    Err e -> Err e
                    Ok ( _, state2 ) ->
                        case parseTypeAnnotation state2 of
                            Err e -> Err e
                            Ok ( ty, state3 ) ->
                                -- Now parse the actual definition on the next line
                                let
                                    state4 = skipNewlines state3
                                in
                                case expectIdent LowerIdent state4 of
                                    Err e -> Err e
                                    Ok ( _, state5 ) ->
                                        parseValueBody (currentRegion state) name (Just ty) state5
            else
                parseValueBody (currentRegion state) name Nothing state1


parseValueBody : Region -> Name -> Maybe (Located TypeAnnotation) -> Parser (Located Decl)
parseValueBody startRegion name typeAnnotation state =
    let
        ( args, state1 ) = parsePatterns state
    in
    case expect Equals state1 of
        Err e -> Err e
        Ok ( _, state2 ) ->
            -- Skip newlines after = to allow multi-line bodies
            let
                state2a = skipNewlines state2
            in
            case parseExpr state2a of
                Err e -> Err e
                Ok ( body, state3 ) ->
                    Ok
                        ( locate startRegion (ValueDecl
                            { name = locate startRegion name
                            , typeAnnotation = typeAnnotation
                            , args = args
                            , body = body
                            })
                        , state3
                        )



-- TYPE ANNOTATIONS


parseTypeAnnotation : Parser (Located TypeAnnotation)
parseTypeAnnotation state =
    parseArrowType state


parseArrowType : Parser (Located TypeAnnotation)
parseArrowType state =
    case parseAppType state of
        Err e -> Err e
        Ok ( left, state1 ) ->
            if peek Arrow state1 then
                case expect Arrow state1 of
                    Err e -> Err e
                    Ok ( _, state2 ) ->
                        case parseArrowType state2 of
                            Err e -> Err e
                            Ok ( right, state3 ) ->
                                Ok
                                    ( locate (getRegion left)
                                        (TAArrow left right)
                                    , state3
                                    )
            else
                Ok ( left, state1 )


parseAppType : Parser (Located TypeAnnotation)
parseAppType state =
    case parseAtomicType state of
        Err e -> Err e
        Ok ( first, state1 ) ->
            let
                ( args, state2 ) = parseTypeArgs state1
            in
            if List.isEmpty args then
                Ok ( first, state2 )
            else
                Ok
                    ( List.foldl
                        (\arg acc -> locate (getRegion acc) (TAApp acc arg))
                        first
                        args
                    , state2
                    )


parseTypeArgs : ParseState -> ( List (Located TypeAnnotation), ParseState )
parseTypeArgs state =
    case parseAtomicType state of
        Err _ ->
            ( [], state )

        Ok ( ty, state1 ) ->
            let
                ( rest, state2 ) = parseTypeArgs state1
            in
            ( ty :: rest, state2 )


parseAtomicType : Parser (Located TypeAnnotation)
parseAtomicType state =
    case currentToken state of
        Just tok ->
            case tok.type_ of
                LowerIdent ->
                    Ok ( locate tok.region (TAVar tok.value), advance state )

                UpperIdent ->
                    -- Check for qualified type name (e.g., Dict.Dict, Json.Decode.Decoder)
                    parseQualifiedTypeName tok.region [ tok.value ] (advance state)

                LParen ->
                    parseTupleOrParenType state

                LBrace ->
                    parseRecordType state

                _ ->
                    Err { message = "Expected type", region = tok.region }

        Nothing ->
            Err { message = "Expected type", region = currentRegion state }


{-| Parse a potentially qualified type name like Dict.Dict or Json.Decode.Decoder.
    Accumulates module parts until we hit something that isn't Dot.UpperIdent.
-}
parseQualifiedTypeName : Region -> List String -> ParseState -> Result ParseError ( Located TypeAnnotation, ParseState )
parseQualifiedTypeName region nameParts state =
    case currentToken state of
        Just dotTok ->
            if dotTok.type_ == Dot then
                let
                    state1 = advance state
                in
                case currentToken state1 of
                    Just nameTok ->
                        if nameTok.type_ == UpperIdent then
                            -- More qualification - continue
                            parseQualifiedTypeName region (nameParts ++ [ nameTok.value ]) (advance state1)
                        else
                            -- Not an UpperIdent after dot, return what we have
                            finishQualifiedTypeName region nameParts state

                    Nothing ->
                        finishQualifiedTypeName region nameParts state
            else
                -- Not a dot, return accumulated name
                finishQualifiedTypeName region nameParts state

        Nothing ->
            finishQualifiedTypeName region nameParts state


{-| Finish parsing a qualified type name by splitting into module path and type name.
-}
finishQualifiedTypeName : Region -> List String -> ParseState -> Result ParseError ( Located TypeAnnotation, ParseState )
finishQualifiedTypeName region nameParts state =
    case List.reverse nameParts of
        name :: rest ->
            let
                modulePath = if List.isEmpty rest then Nothing else Just (String.join "." (List.reverse rest))
            in
            Ok ( locate region (TACon { module_ = modulePath, name = name }), state )

        [] ->
            Err { message = "Expected type name", region = region }


parseTupleOrParenType : Parser (Located TypeAnnotation)
parseTupleOrParenType state =
    case expect LParen state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            if peek RParen state1 then
                -- Unit type ()
                case expect RParen state1 of
                    Err e -> Err e
                    Ok ( _, state2 ) ->
                        Ok ( locate tok.region TAUnit, state2 )
            else
                case parseTypeAnnotation state1 of
                    Err e -> Err e
                    Ok ( first, state2 ) ->
                        if peek Comma state2 then
                            -- Tuple type
                            let
                                ( rest, state3 ) = parseTupleTypeRest state2
                            in
                            case expect RParen state3 of
                                Err e -> Err e
                                Ok ( _, state4 ) ->
                                    Ok ( locate tok.region (TATuple (first :: rest)), state4 )
                        else
                            -- Parenthesized type
                            case expect RParen state2 of
                                Err e -> Err e
                                Ok ( _, state3 ) ->
                                    Ok ( locate tok.region (TAParens first), state3 )


parseTupleTypeRest : ParseState -> ( List (Located TypeAnnotation), ParseState )
parseTupleTypeRest state =
    if peek Comma state then
        case expect Comma state of
            Err _ ->
                ( [], state )

            Ok ( _, state1 ) ->
                case parseTypeAnnotation state1 of
                    Err _ ->
                        ( [], state1 )

                    Ok ( ty, state2 ) ->
                        let
                            ( rest, state3 ) = parseTupleTypeRest state2
                        in
                        ( ty :: rest, state3 )
    else
        ( [], state )


parseRecordType : Parser (Located TypeAnnotation)
parseRecordType state =
    case expect LBrace state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            let
                state1a = skipNewlines state1
            in
            if peek RBrace state1a then
                case expect RBrace state1a of
                    Err e -> Err e
                    Ok ( _, state2 ) ->
                        Ok ( locate tok.region (TARecord [] Nothing), state2 )
            else
                let
                    ( fields, rowVar, state2 ) = parseRecordTypeFields state1a
                    state2a = skipNewlines state2
                in
                case expect RBrace state2a of
                    Err e -> Err e
                    Ok ( _, state3 ) ->
                        Ok ( locate tok.region (TARecord fields rowVar), state3 )


parseRecordTypeFields : ParseState -> ( List ( Located Name, Located TypeAnnotation ), Maybe (Located Name), ParseState )
parseRecordTypeFields state =
    case expectIdent LowerIdent state of
        Err _ ->
            ( [], Nothing, state )

        Ok ( name, state1 ) ->
            if peek Pipe state1 then
                -- Row variable: { r | ... }
                case expect Pipe state1 of
                    Err _ ->
                        ( [], Just (locate (currentRegion state) name), state1 )

                    Ok ( _, state2 ) ->
                        let
                            ( fields, _, state3 ) = parseRecordTypeFieldsRest state2
                        in
                        ( fields, Just (locate (currentRegion state) name), state3 )
            else
                case expect Colon state1 of
                    Err e ->
                        ( [], Nothing, state1 )

                    Ok ( _, state2 ) ->
                        case parseTypeAnnotation state2 of
                            Err _ ->
                                ( [], Nothing, state2 )

                            Ok ( ty, state3 ) ->
                                let
                                    ( rest, rowVar, state4 ) = parseRecordTypeFieldsRest state3
                                in
                                ( ( locate (currentRegion state) name, ty ) :: rest, rowVar, state4 )


parseRecordTypeFieldsRest : ParseState -> ( List ( Located Name, Located TypeAnnotation ), Maybe (Located Name), ParseState )
parseRecordTypeFieldsRest state =
    let
        -- Skip newlines to support multi-line record types
        state0 = skipNewlines state
    in
    if peek Comma state0 then
        case expect Comma state0 of
            Err _ ->
                ( [], Nothing, state0 )

            Ok ( _, state1 ) ->
                let
                    state1a = skipNewlines state1
                in
                parseRecordTypeFields state1a
    else
        ( [], Nothing, state0 )



-- PATTERNS


parsePatterns : ParseState -> ( List (Located Pattern), ParseState )
parsePatterns state =
    case parseAtomicPattern state of
        Err _ ->
            ( [], state )

        Ok ( pat, state1 ) ->
            let
                ( rest, state2 ) = parsePatterns state1
            in
            ( pat :: rest, state2 )


parsePattern : Parser (Located Pattern)
parsePattern state =
    case parseConsPattern state of
        Err e -> Err e
        Ok ( pat, state1 ) ->
            -- Check for as-pattern: pattern as name
            if peek KwAs state1 then
                case expect KwAs state1 of
                    Err e -> Err e
                    Ok ( _, state2 ) ->
                        case expectIdent LowerIdent state2 of
                            Err e -> Err e
                            Ok ( name, state3 ) ->
                                Ok ( locate (getRegion pat) (PAlias pat (locate (currentRegion state3) name)), state3 )
            else
                Ok ( pat, state1 )


parseConsPattern : Parser (Located Pattern)
parseConsPattern state =
    case parseAtomicPattern state of
        Err e -> Err e
        Ok ( left, state1 ) ->
            if peek DoubleColon state1 then
                case expect DoubleColon state1 of
                    Err e -> Err e
                    Ok ( _, state2 ) ->
                        case parseConsPattern state2 of
                            Err e -> Err e
                            Ok ( right, state3 ) ->
                                Ok ( locate (getRegion left) (PCons left right), state3 )
            else
                Ok ( left, state1 )


parseAtomicPattern : Parser (Located Pattern)
parseAtomicPattern state =
    case currentToken state of
        Just tok ->
            case tok.type_ of
                LowerIdent ->
                    Ok ( locate tok.region (PVar tok.value), advance state )

                UpperIdent ->
                    parseConstructorPattern state

                Underscore ->
                    Ok ( locate tok.region PWildcard, advance state )

                IntLit ->
                    Ok ( locate tok.region (PLit (LInt (String.toInt tok.value |> Maybe.withDefault 0))), advance state )

                StringLit ->
                    Ok ( locate tok.region (PLit (LString tok.value)), advance state )

                CharLit ->
                    Ok ( locate tok.region (PLit (LChar (String.uncons tok.value |> Maybe.map Tuple.first |> Maybe.withDefault ' '))), advance state )

                LParen ->
                    parseTupleOrParenPattern state

                LBracket ->
                    parseListPattern state

                LBrace ->
                    parseRecordPattern state

                _ ->
                    Err { message = "Expected pattern", region = tok.region }

        Nothing ->
            Err { message = "Expected pattern", region = currentRegion state }


parseConstructorPattern : Parser (Located Pattern)
parseConstructorPattern state =
    case expectIdent UpperIdent state of
        Err e -> Err e
        Ok ( name, state1 ) ->
            let
                ( args, state2 ) = parsePatterns state1
            in
            Ok
                ( locate (currentRegion state)
                    (PCon { module_ = Nothing, name = name } args)
                , state2
                )


parseTupleOrParenPattern : Parser (Located Pattern)
parseTupleOrParenPattern state =
    case expect LParen state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            if peek RParen state1 then
                case expect RParen state1 of
                    Err e -> Err e
                    Ok ( _, state2 ) ->
                        Ok ( locate tok.region PUnit, state2 )
            else
                case parsePattern state1 of
                    Err e -> Err e
                    Ok ( first, state2 ) ->
                        if peek Comma state2 then
                            let
                                ( rest, state3 ) = parseTuplePatternRest state2
                            in
                            case expect RParen state3 of
                                Err e -> Err e
                                Ok ( _, state4 ) ->
                                    Ok ( locate tok.region (PTuple (first :: rest)), state4 )
                        else
                            case expect RParen state2 of
                                Err e -> Err e
                                Ok ( _, state3 ) ->
                                    Ok ( locate tok.region (PParens first), state3 )


parseTuplePatternRest : ParseState -> ( List (Located Pattern), ParseState )
parseTuplePatternRest state =
    if peek Comma state then
        case expect Comma state of
            Err _ ->
                ( [], state )

            Ok ( _, state1 ) ->
                case parsePattern state1 of
                    Err _ ->
                        ( [], state1 )

                    Ok ( pat, state2 ) ->
                        let
                            ( rest, state3 ) = parseTuplePatternRest state2
                        in
                        ( pat :: rest, state3 )
    else
        ( [], state )


parseListPattern : Parser (Located Pattern)
parseListPattern state =
    case expect LBracket state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            if peek RBracket state1 then
                case expect RBracket state1 of
                    Err e -> Err e
                    Ok ( _, state2 ) ->
                        Ok ( locate tok.region (PList []), state2 )
            else
                case sepBy (expect Comma) parsePattern state1 of
                    Err e -> Err e
                    Ok ( pats, state2 ) ->
                        case expect RBracket state2 of
                            Err e -> Err e
                            Ok ( _, state3 ) ->
                                Ok ( locate tok.region (PList pats), state3 )


parseRecordPattern : Parser (Located Pattern)
parseRecordPattern state =
    case expect LBrace state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            case sepBy (expect Comma) (expectIdent LowerIdent) state1 of
                Err e -> Err e
                Ok ( fields, state2 ) ->
                    case expect RBrace state2 of
                        Err e -> Err e
                        Ok ( _, state3 ) ->
                            Ok
                                ( locate tok.region
                                    (PRecord (List.map (\f -> locate tok.region f) fields))
                                , state3
                                )



-- EXPRESSIONS


parseExpr : Parser (Located Expr)
parseExpr state =
    parseBinOpExpr 0 state


parseBinOpExpr : Int -> Parser (Located Expr)
parseBinOpExpr minPrec state =
    case parseUnaryExpr state of
        Err e -> Err e
        Ok ( left, state1 ) ->
            parseBinOpRest minPrec left state1


parseBinOpRest : Int -> Located Expr -> Parser (Located Expr)
parseBinOpRest minPrec left state =
    -- Skip newlines to allow operators on new lines (but check for column 1 declarations)
    let
        state0 = skipNewlinesUnlessAtColumn1 state
    in
    case currentToken state0 of
        Just tok ->
            -- Handle both Operator tokens and DoubleColon (::) specially
            let
                isOp = tok.type_ == Operator || tok.type_ == DoubleColon
                opValue = if tok.type_ == DoubleColon then "::" else tok.value
            in
            if isOp then
                let
                    ( prec, _ ) = getOperatorPrec opValue
                in
                if prec >= minPrec then
                    let
                        -- Advance past the operator token
                        state1 = advance state0
                    in
                    case parseBinOpExpr (prec + 1) state1 of
                        Err e -> Err e
                        Ok ( right, state2 ) ->
                            let
                                newLeft =
                                    locate (getRegion left)
                                        (EBinOp left (locate tok.region opValue) right)
                            in
                            parseBinOpRest minPrec newLeft state2
                else
                    Ok ( left, state0 )
            else
                Ok ( left, state0 )

        Nothing ->
            Ok ( left, state0 )


getOperatorPrec : String -> ( Int, Associativity )
getOperatorPrec op =
    case op of
        "|>" -> ( 0, LeftAssoc )
        "<|" -> ( 0, RightAssoc )
        "||" -> ( 2, RightAssoc )
        "&&" -> ( 3, RightAssoc )
        "==" -> ( 4, NonAssoc )
        "/=" -> ( 4, NonAssoc )
        "<" -> ( 4, NonAssoc )
        ">" -> ( 4, NonAssoc )
        "<=" -> ( 4, NonAssoc )
        ">=" -> ( 4, NonAssoc )
        "++" -> ( 5, RightAssoc )
        "::" -> ( 5, RightAssoc )
        "+" -> ( 6, LeftAssoc )
        "-" -> ( 6, LeftAssoc )
        "*" -> ( 7, LeftAssoc )
        "/" -> ( 7, LeftAssoc )
        "//" -> ( 7, LeftAssoc )
        "^" -> ( 8, RightAssoc )
        ">>" -> ( 9, LeftAssoc )
        "<<" -> ( 9, RightAssoc )
        _ -> ( 9, LeftAssoc )


{-| Convert operator symbol to internal name for use as a value.
    (+) -> "_op_add", (*) -> "_op_mul", etc.
-}
operatorToName : String -> String
operatorToName op =
    case op of
        "+" -> "_op_add"
        "-" -> "_op_sub"
        "*" -> "_op_mul"
        "/" -> "_op_div"
        "//" -> "_op_intDiv"
        "^" -> "_op_pow"
        "%" -> "_op_mod"
        "==" -> "_op_eq"
        "/=" -> "_op_neq"
        "<" -> "_op_lt"
        ">" -> "_op_gt"
        "<=" -> "_op_lte"
        ">=" -> "_op_gte"
        "&&" -> "_op_and"
        "||" -> "_op_or"
        "++" -> "_op_append"
        "::" -> "_op_cons"
        "|>" -> "_op_pipeRight"
        "<|" -> "_op_pipeLeft"
        ">>" -> "_op_composeRight"
        "<<" -> "_op_composeLeft"
        _ -> op  -- Unknown operator, use as-is


parseUnaryExpr : Parser (Located Expr)
parseUnaryExpr state =
    case currentToken state of
        Just tok ->
            if tok.type_ == Operator && tok.value == "-" then
                case expect Operator state of
                    Err e -> Err e
                    Ok ( _, state1 ) ->
                        case parseUnaryExpr state1 of
                            Err e -> Err e
                            Ok ( inner, state2 ) ->
                                Ok ( locate tok.region (ENegate inner), state2 )
            else
                parseAppExpr state

        Nothing ->
            parseAppExpr state


parseAppExpr : Parser (Located Expr)
parseAppExpr state =
    case parseAtomicExpr state of
        Err e -> Err e
        Ok ( first, state1 ) ->
            -- Handle field access chains first (e.g., person.name.length)
            let
                ( exprWithFields, state1a ) = parseFieldAccessChain first state1
            in
            let
                ( args, state2 ) = parseAppArgs state1a
            in
            if List.isEmpty args then
                Ok ( exprWithFields, state2 )
            else
                Ok
                    ( List.foldl
                        (\arg acc -> locate (getRegion acc) (EApp acc arg))
                        exprWithFields
                        args
                    , state2
                    )


-- Parse field access chain like .name.age.whatever
parseFieldAccessChain : Located Expr -> ParseState -> ( Located Expr, ParseState )
parseFieldAccessChain expr state =
    case currentToken state of
        Just tok ->
            if tok.type_ == Dot then
                let
                    state1 = advance state
                in
                case currentToken state1 of
                    Just fieldTok ->
                        if fieldTok.type_ == LowerIdent then
                            -- It's a field access: expr.field
                            let
                                newExpr = locate (getRegion expr) (ERecordAccess expr (locate fieldTok.region fieldTok.value))
                                state2 = advance state1
                            in
                            -- Continue parsing more field accesses
                            parseFieldAccessChain newExpr state2
                        else
                            -- Not a valid field name, stop
                            ( expr, state )
                    Nothing ->
                        ( expr, state )
            else
                ( expr, state )
        Nothing ->
            ( expr, state )


parseAppArgs : ParseState -> ( List (Located Expr), ParseState )
parseAppArgs state =
    -- Stop if we hit a token at column 1 (new top-level declaration)
    -- Also stop if this looks like a let binding (identifier followed by =)
    case currentToken state of
        Just tok ->
            if tok.region.start.column == 1 then
                ( [], state )
            -- Check if this looks like a let binding (will be parsed by parseLetBindings)
            else if looksLikeLetBinding state then
                ( [], state )
            else
                case parseAtomicExpr state of
                    Err _ ->
                        ( [], state )

                    Ok ( arg, state1 ) ->
                        -- Parse field access chain on the argument (e.g., ledger.accounts)
                        -- This ensures record.field has higher precedence than function application
                        let
                            ( argWithFields, state1a ) = parseFieldAccessChain arg state1
                            ( rest, state2 ) = parseAppArgs state1a
                        in
                        ( argWithFields :: rest, state2 )

        Nothing ->
            ( [], state )


parseAtomicExpr : Parser (Located Expr)
parseAtomicExpr state =
    case currentToken state of
        Just tok ->
            case tok.type_ of
                IntLit ->
                    Ok ( locate tok.region (ELit (LInt (String.toInt tok.value |> Maybe.withDefault 0))), advance state )

                FloatLit ->
                    Ok ( locate tok.region (ELit (LFloat (String.toFloat tok.value |> Maybe.withDefault 0))), advance state )

                StringLit ->
                    Ok ( locate tok.region (ELit (LString tok.value)), advance state )

                CharLit ->
                    Ok ( locate tok.region (ELit (LChar (String.uncons tok.value |> Maybe.map Tuple.first |> Maybe.withDefault ' '))), advance state )

                LowerIdent ->
                    Ok ( locate tok.region (EVar { module_ = Nothing, name = tok.value }), advance state )

                UpperIdent ->
                    -- Check for qualified name: Module.name, Module.Constructor, or Module.SubModule.name
                    parseQualifiedName tok.region [ tok.value ] (advance state)

                LParen ->
                    parseTupleOrParenExpr state

                LBracket ->
                    parseListExpr state

                LBrace ->
                    parseRecordExpr state

                Backslash ->
                    parseLambdaExpr state

                KwIf ->
                    parseIfExpr state

                KwCase ->
                    parseCaseExpr state

                KwLet ->
                    parseLetExpr state

                KwDo ->
                    parseDoExpr state

                Dot ->
                    parseAccessorExpr state

                _ ->
                    Err { message = "Expected expression", region = tok.region }

        Nothing ->
            Err { message = "Expected expression", region = currentRegion state }


parseTupleOrParenExpr : Parser (Located Expr)
parseTupleOrParenExpr state =
    case expect LParen state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            if peek RParen state1 then
                case expect RParen state1 of
                    Err e -> Err e
                    Ok ( _, state2 ) ->
                        Ok ( locate tok.region EUnit, state2 )
            else
                -- Check for operator section: (op)
                case currentToken state1 of
                    Just opTok ->
                        if (opTok.type_ == Operator || opTok.type_ == DoubleColon) && peek RParen (advance state1) then
                            let
                                state2 = advance state1  -- consume operator
                            in
                            case expect RParen state2 of
                                Err e -> Err e
                                Ok ( _, state3 ) ->
                                    -- Convert operator to function reference: (+) -> _op_add
                                    let
                                        opSymbol = if opTok.type_ == DoubleColon then "::" else opTok.value
                                        opName = operatorToName opSymbol
                                    in
                                    Ok ( locate tok.region (EVar { module_ = Nothing, name = opName }), state3 )
                        else
                            case parseExpr state1 of
                                Err e -> Err e
                                Ok ( first, state2 ) ->
                                    if peek Comma state2 then
                                        let
                                            ( rest, state3 ) = parseTupleExprRest state2
                                        in
                                        case expect RParen state3 of
                                            Err e -> Err e
                                            Ok ( _, state4 ) ->
                                                Ok ( locate tok.region (ETuple (first :: rest)), state4 )
                                    else
                                        case expect RParen state2 of
                                            Err e -> Err e
                                            Ok ( _, state3 ) ->
                                                Ok ( locate tok.region (EParens first), state3 )
                    Nothing ->
                        case parseExpr state1 of
                            Err e -> Err e
                            Ok ( first, state2 ) ->
                                if peek Comma state2 then
                                    let
                                        ( rest, state3 ) = parseTupleExprRest state2
                                    in
                                    case expect RParen state3 of
                                        Err e -> Err e
                                        Ok ( _, state4 ) ->
                                            Ok ( locate tok.region (ETuple (first :: rest)), state4 )
                                else
                                    case expect RParen state2 of
                                        Err e -> Err e
                                        Ok ( _, state3 ) ->
                                            Ok ( locate tok.region (EParens first), state3 )


{-| Parse a qualified name like Module.func, Module.Constructor, or Module.SubModule.func.
    Takes the region for location and accumulated module parts.
-}
parseQualifiedName : Region -> List String -> ParseState -> Result ParseError ( Located Expr, ParseState )
parseQualifiedName region moduleParts state =
    case currentToken state of
        Just dotTok ->
            if dotTok.type_ == Dot then
                let
                    state1 = advance state
                in
                case currentToken state1 of
                    Just nameTok ->
                        if nameTok.type_ == LowerIdent then
                            -- module.func - qualified variable
                            let
                                modulePath = String.join "." moduleParts
                            in
                            Ok ( locate region (EVar { module_ = Just modulePath, name = nameTok.value }), advance state1 )
                        else if nameTok.type_ == UpperIdent then
                            -- Could be module.Constructor OR module.SubModule (more parts follow)
                            parseQualifiedName region (moduleParts ++ [ nameTok.value ]) (advance state1)
                        else
                            -- Just constructor without more qualification
                            let
                                allParts = moduleParts
                            in
                            case List.reverse allParts of
                                name :: rest ->
                                    let
                                        modulePath = if List.isEmpty rest then Nothing else Just (String.join "." (List.reverse rest))
                                    in
                                    Ok ( locate region (EConstructor { module_ = modulePath, name = name }), state )
                                [] ->
                                    -- Should not happen
                                    Ok ( locate region (EConstructor { module_ = Nothing, name = "Unknown" }), state )

                    Nothing ->
                        -- End of input, treat accumulated parts as constructor
                        case List.reverse moduleParts of
                            name :: rest ->
                                let
                                    modulePath = if List.isEmpty rest then Nothing else Just (String.join "." (List.reverse rest))
                                in
                                Ok ( locate region (EConstructor { module_ = modulePath, name = name }), state )
                            [] ->
                                Ok ( locate region (EConstructor { module_ = Nothing, name = "Unknown" }), state )
            else
                -- Not a dot, accumulated parts are a constructor
                case List.reverse moduleParts of
                    name :: rest ->
                        let
                            modulePath = if List.isEmpty rest then Nothing else Just (String.join "." (List.reverse rest))
                        in
                        Ok ( locate region (EConstructor { module_ = modulePath, name = name }), state )
                    [] ->
                        Ok ( locate region (EConstructor { module_ = Nothing, name = "Unknown" }), state )

        Nothing ->
            -- No more tokens, accumulated parts are a constructor
            case List.reverse moduleParts of
                name :: rest ->
                    let
                        modulePath = if List.isEmpty rest then Nothing else Just (String.join "." (List.reverse rest))
                    in
                    Ok ( locate region (EConstructor { module_ = modulePath, name = name }), state )
                [] ->
                    Ok ( locate region (EConstructor { module_ = Nothing, name = "Unknown" }), state )


parseTupleExprRest : ParseState -> ( List (Located Expr), ParseState )
parseTupleExprRest state =
    if peek Comma state then
        case expect Comma state of
            Err _ ->
                ( [], state )

            Ok ( _, state1 ) ->
                case parseExpr state1 of
                    Err _ ->
                        ( [], state1 )

                    Ok ( expr, state2 ) ->
                        let
                            ( rest, state3 ) = parseTupleExprRest state2
                        in
                        ( expr :: rest, state3 )
    else
        ( [], state )


parseListExpr : Parser (Located Expr)
parseListExpr state =
    case expect LBracket state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            if peek RBracket state1 then
                case expect RBracket state1 of
                    Err e -> Err e
                    Ok ( _, state2 ) ->
                        Ok ( locate tok.region (EList []), state2 )
            else
                case sepBy (expect Comma) parseExpr state1 of
                    Err e -> Err e
                    Ok ( exprs, state2 ) ->
                        case expect RBracket state2 of
                            Err e -> Err e
                            Ok ( _, state3 ) ->
                                Ok ( locate tok.region (EList exprs), state3 )


parseRecordExpr : Parser (Located Expr)
parseRecordExpr state =
    case expect LBrace state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            if peek RBrace state1 then
                case expect RBrace state1 of
                    Err e -> Err e
                    Ok ( _, state2 ) ->
                        Ok ( locate tok.region (ERecord []), state2 )
            else
                case expectIdent LowerIdent state1 of
                    Err e -> Err e
                    Ok ( name, state2 ) ->
                        if peek Pipe state2 then
                            -- Record update: { name | field = value }
                            case expect Pipe state2 of
                                Err e -> Err e
                                Ok ( _, state3 ) ->
                                    let
                                        ( fields, state4 ) = parseRecordFields state3
                                    in
                                    case expect RBrace state4 of
                                        Err e -> Err e
                                        Ok ( _, state5 ) ->
                                            Ok ( locate tok.region (ERecordUpdate (locate tok.region name) fields), state5 )
                        else
                            -- Regular record: { field = value, ... }
                            case expect Equals state2 of
                                Err e -> Err e
                                Ok ( _, state3 ) ->
                                    case parseExpr state3 of
                                        Err e -> Err e
                                        Ok ( value, state4 ) ->
                                            let
                                                ( rest, state5 ) = parseRecordFieldsRest state4
                                            in
                                            case expect RBrace state5 of
                                                Err e -> Err e
                                                Ok ( _, state6 ) ->
                                                    Ok ( locate tok.region (ERecord (( locate tok.region name, value ) :: rest)), state6 )


parseRecordFields : ParseState -> ( List ( Located Name, Located Expr ), ParseState )
parseRecordFields state =
    case expectIdent LowerIdent state of
        Err _ ->
            ( [], state )

        Ok ( name, state1 ) ->
            case expect Equals state1 of
                Err _ ->
                    ( [], state )

                Ok ( _, state2 ) ->
                    case parseExpr state2 of
                        Err _ ->
                            ( [], state2 )

                        Ok ( value, state3 ) ->
                            let
                                ( rest, state4 ) = parseRecordFieldsRest state3
                            in
                            ( ( locate (currentRegion state) name, value ) :: rest, state4 )


parseRecordFieldsRest : ParseState -> ( List ( Located Name, Located Expr ), ParseState )
parseRecordFieldsRest state =
    if peek Comma state then
        case expect Comma state of
            Err _ ->
                ( [], state )

            Ok ( _, state1 ) ->
                parseRecordFields state1
    else
        ( [], state )


parseLambdaExpr : Parser (Located Expr)
parseLambdaExpr state =
    case expect Backslash state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            let
                ( params, state2 ) = parsePatterns state1
            in
            case expect Arrow state2 of
                Err e -> Err e
                Ok ( _, state3 ) ->
                    -- Skip newlines after -> for multi-line lambdas
                    let state3a = skipNewlines state3 in
                    case parseExpr state3a of
                        Err e -> Err e
                        Ok ( body, state4 ) ->
                            Ok ( locate tok.region (ELambda params body), state4 )


parseIfExpr : Parser (Located Expr)
parseIfExpr state =
    case expect KwIf state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            let state1a = skipNewlines state1 in
            case parseExpr state1a of
                Err e -> Err e
                Ok ( cond, state2 ) ->
                    case expect KwThen (skipNewlines state2) of
                        Err e -> Err e
                        Ok ( _, state3 ) ->
                            let state3a = skipNewlines state3 in
                            case parseExpr state3a of
                                Err e -> Err e
                                Ok ( then_, state4 ) ->
                                    case expect KwElse (skipNewlines state4) of
                                        Err e -> Err e
                                        Ok ( _, state5 ) ->
                                            let state5a = skipNewlines state5 in
                                            case parseExpr state5a of
                                                Err e -> Err e
                                                Ok ( else_, state6 ) ->
                                                    Ok ( locate tok.region (EIf cond then_ else_), state6 )


parseCaseExpr : Parser (Located Expr)
parseCaseExpr state =
    case expect KwCase state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            case parseExpr state1 of
                Err e -> Err e
                Ok ( scrutinee, state2 ) ->
                    case expect KwOf state2 of
                        Err e -> Err e
                        Ok ( _, state3 ) ->
                            let
                                ( branches, state4 ) = parseCaseBranches state3
                            in
                            Ok ( locate tok.region (ECase scrutinee branches), state4 )


parseCaseBranches : ParseState -> ( List (Located CaseBranch), ParseState )
parseCaseBranches state =
    let
        state1 = skipNewlines state
    in
    case currentToken state1 of
        Just tok ->
            -- Stop if we see 'in' keyword (end of let block)
            if tok.type_ == KwIn then
                ( [], state1 )
            -- Stop at column 1 (new top-level declaration)
            else if tok.region.start.column == 1 then
                ( [], state1 )
            -- Check if this looks like a let binding: identifier followed by =
            -- Let bindings have form "name =" or "name arg1 arg2 ="
            -- Case branches have form "pattern ->"
            else if looksLikeLetBinding state1 then
                ( [], state1 )
            else
                case parseCaseBranch state1 of
                    Err _ ->
                        ( [], state1 )

                    Ok ( branch, state2 ) ->
                        let
                            ( rest, state3 ) = parseCaseBranches state2
                        in
                        ( branch :: rest, state3 )

        Nothing ->
            ( [], state1 )


{-| Check if the current position looks like a let binding (not a case branch).
    Let bindings: "name = ..." or "name arg1 = ..."
    Case branches: "pattern -> ..."

    We scan tokens looking for = before -> to distinguish let bindings from case branches.
-}
looksLikeLetBinding : ParseState -> Bool
looksLikeLetBinding state =
    case currentToken state of
        Just tok ->
            if tok.type_ == LowerIdent then
                scanForEqualsVsArrow (advance state) 0
            else
                False
        Nothing -> False


{-| Scan ahead looking for = (let binding) or -> (case branch).
    Returns True if we find = first, False otherwise.
    maxDepth prevents infinite loops.
-}
scanForEqualsVsArrow : ParseState -> Int -> Bool
scanForEqualsVsArrow state depth =
    if depth > 10 then
        False
    else
        case currentToken state of
            Just t ->
                case t.type_ of
                    Equals -> True  -- Found =, this is a let binding
                    Arrow -> False  -- Found ->, this is a case branch
                    Colon -> True   -- Type annotation before =
                    LowerIdent -> scanForEqualsVsArrow (advance state) (depth + 1)
                    UpperIdent -> scanForEqualsVsArrow (advance state) (depth + 1)  -- Constructor pattern
                    LParen -> False  -- Complex pattern, not a let binding
                    LBrace -> False  -- Record pattern, not a let binding
                    LBracket -> False  -- List pattern, not a let binding
                    Underscore -> False  -- Wildcard pattern, not a let binding
                    Newline -> False  -- Hit newline before = or ->, stop
                    Indent _ -> False
                    _ -> False
            Nothing -> False


parseCaseBranch : Parser (Located CaseBranch)
parseCaseBranch state =
    case parsePattern state of
        Err e -> Err e
        Ok ( pat, state1 ) ->
            case expect Arrow state1 of
                Err e -> Err e
                Ok ( _, state2 ) ->
                    -- Skip newlines after -> for multi-line branch bodies
                    let state2a = skipNewlines state2 in
                    case parseExpr state2a of
                        Err e -> Err e
                        Ok ( body, state3 ) ->
                            Ok
                                ( locate (getRegion pat)
                                    { pattern = pat
                                    , guard = Nothing
                                    , body = body
                                    }
                                , state3
                                )


parseLetExpr : Parser (Located Expr)
parseLetExpr state =
    case expect KwLet state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            let
                ( bindings, state2 ) = parseLetBindings state1
            in
            case expect KwIn state2 of
                Err e -> Err e
                Ok ( _, state3 ) ->
                    -- Skip newlines after 'in' for multi-line let expressions
                    let
                        state3a = skipNewlines state3
                    in
                    case parseExpr state3a of
                        Err e -> Err e
                        Ok ( body, state4 ) ->
                            Ok ( locate tok.region (ELet bindings body), state4 )


parseLetBindings : ParseState -> ( List (Located LetBinding), ParseState )
parseLetBindings state =
    let
        state1 = skipNewlines state
    in
    if peek KwIn state1 then
        ( [], state1 )
    else
        case parseLetBinding state1 of
            Err _ ->
                ( [], state1 )

            Ok ( binding, state2 ) ->
                let
                    ( rest, state3 ) = parseLetBindings state2
                in
                ( binding :: rest, state3 )


parseLetBinding : Parser (Located LetBinding)
parseLetBinding state =
    -- Parse the name first
    case currentToken state of
        Just tok ->
            if tok.type_ == LowerIdent then
                let
                    name = tok.value
                    state1 = advance state
                    -- Try to parse function arguments
                    ( args, state2 ) = parseLetArgs state1
                in
                case expect Equals state2 of
                    Err e -> Err e
                    Ok ( _, state3 ) ->
                        -- Skip newlines after = for multi-line bindings
                        let
                            state3a = skipNewlines state3
                        in
                        case parseExpr state3a of
                            Err e -> Err e
                            Ok ( value, state4 ) ->
                                let
                                    -- If there are args, wrap the value in a lambda
                                    finalValue =
                                        if List.isEmpty args then
                                            value
                                        else
                                            locate (getRegion value) (ELambda args value)
                                in
                                Ok
                                    ( locate tok.region
                                        { pattern = locate tok.region (PVar name)
                                        , typeAnnotation = Nothing
                                        , value = finalValue
                                        }
                                    , state4
                                    )
            else
                -- Fall back to pattern-based parsing for destructuring
                case parsePattern state of
                    Err e -> Err e
                    Ok ( pat, state1 ) ->
                        case expect Equals state1 of
                            Err e -> Err e
                            Ok ( _, state2 ) ->
                                let
                                    state2a = skipNewlines state2
                                in
                                case parseExpr state2a of
                                    Err e -> Err e
                                    Ok ( value, state3 ) ->
                                        Ok
                                            ( locate (getRegion pat)
                                                { pattern = pat
                                                , typeAnnotation = Nothing
                                                , value = value
                                                }
                                            , state3
                                            )

        Nothing ->
            Err { message = "Expected let binding", region = currentRegion state }


{-| Parse function arguments for a let binding (e.g., the "x y" in "f x y = ...").
-}
parseLetArgs : ParseState -> ( List (Located Pattern), ParseState )
parseLetArgs state =
    case currentToken state of
        Just tok ->
            if tok.type_ == LowerIdent then
                let
                    pat = locate tok.region (PVar tok.value)
                    state1 = advance state
                    ( rest, state2 ) = parseLetArgs state1
                in
                ( pat :: rest, state2 )
            else
                ( [], state )

        Nothing ->
            ( [], state )


parseDoExpr : Parser (Located Expr)
parseDoExpr state =
    case expect KwDo state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            let
                ( stmts, state2 ) = parseDoStatements state1
            in
            Ok ( locate tok.region (EDo stmts), state2 )


parseDoStatements : ParseState -> ( List (Located DoStatement), ParseState )
parseDoStatements state =
    let
        state1 = skipNewlines state
    in
    -- Stop if we hit a token at column 1 (new top-level declaration)
    -- This is how Elm-style indentation-sensitive parsing works
    case currentToken state1 of
        Just tok ->
            if tok.region.start.column == 1 && not (isOperatorToken tok.type_) then
                -- Non-operator at column 1 = end of do block
                ( [], state1 )
            else
                case parseDoStatement state1 of
                    Err _ ->
                        ( [], state1 )

                    Ok ( stmt, state2 ) ->
                        let
                            ( rest, state3 ) = parseDoStatements state2
                        in
                        ( stmt :: rest, state3 )

        Nothing ->
            ( [], state1 )


isOperatorToken : TokenType -> Bool
isOperatorToken tt =
    tt == Operator || tt == DoubleColon || tt == Pipe || tt == BackArrow


parseDoStatement : Parser (Located DoStatement)
parseDoStatement state =
    -- Try to parse: pattern <- expr
    -- or: let bindings
    -- or: expr
    if peek KwLet state then
        case expect KwLet state of
            Err e -> Err e
            Ok ( tok, state1 ) ->
                let
                    ( bindings, state2 ) = parseLetBindings state1
                in
                Ok ( locate tok.region (DoLet bindings), state2 )
    else
        case parsePattern state of
            Err _ ->
                -- Just an expression
                case parseExpr state of
                    Err e -> Err e
                    Ok ( expr, state1 ) ->
                        Ok ( locate (getRegion expr) (DoExpr expr), state1 )

            Ok ( pat, state1 ) ->
                if peek BackArrow state1 then
                    case expect BackArrow state1 of
                        Err e -> Err e
                        Ok ( _, state2 ) ->
                            case parseExpr state2 of
                                Err e -> Err e
                                Ok ( expr, state3 ) ->
                                    Ok ( locate (getRegion pat) (DoBind pat expr), state3 )
                else
                    -- Pattern was actually an expression
                    -- This is a simplification - in real parser we'd backtrack
                    case parseExpr state of
                        Err e -> Err e
                        Ok ( expr, state2 ) ->
                            Ok ( locate (getRegion expr) (DoExpr expr), state2 )


parseAccessorExpr : Parser (Located Expr)
parseAccessorExpr state =
    case expect Dot state of
        Err e -> Err e
        Ok ( tok, state1 ) ->
            case expectIdent LowerIdent state1 of
                Err e -> Err e
                Ok ( field, state2 ) ->
                    Ok ( locate tok.region (ERecordAccessor (locate tok.region field)), state2 )
