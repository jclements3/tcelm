module Parser exposing
    ( parse
    , parseModule
    , ParseError
    )

{-| Parser for tcelm - UPGRADED with do-notation.

This version uses tcelm's do-notation for cleaner monadic parsing.
Compare to src2/Parser.elm for the original nested-case version.

Reduction: ~2,757 lines -> ~1,900 lines (-31%)
-}

import AST exposing (..)
import Lexer exposing (Token, TokenType(..))


-- PARSE ERRORS


type alias ParseError =
    { message : String
    , region : Region
    }



-- PARSER MONAD


{-| Parser monad: state -> Result error (value, state)
    tcelm do-notation recognizes Ok/Err and uses our andThen.
-}
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



-- CORE COMBINATORS


succeed : a -> Parser a
succeed value state =
    Ok ( value, state )


fail : String -> Parser a
fail msg state =
    Err { message = msg, region = currentRegion state }


{-| The key combinator for do-notation.
    `do { x <- p; f x }` desugars to `p |> andThen (\x -> f x)`
-}
andThen : (a -> Parser b) -> Parser a -> Parser b
andThen f parser state =
    case parser state of
        Ok ( a, state1 ) -> f a state1
        Err e -> Err e


map : (a -> b) -> Parser a -> Parser b
map f parser state =
    case parser state of
        Ok ( a, state1 ) -> Ok ( f a, state1 )
        Err e -> Err e


map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 f pa pb = do
    a <- pa
    b <- pb
    succeed (f a b)


{-| Try parser, return Nothing on failure instead of error -}
optional : Parser a -> Parser (Maybe a)
optional parser state =
    case parser state of
        Ok ( a, state1 ) -> Ok ( Just a, state1 )
        Err _ -> Ok ( Nothing, state )


{-| Parse zero or more -}
many : Parser a -> Parser (List a)
many parser state =
    case parser state of
        Err _ -> Ok ( [], state )
        Ok ( a, state1 ) ->
            case many parser state1 of
                Ok ( rest, state2 ) -> Ok ( a :: rest, state2 )
                Err e -> Err e


{-| Parse one or more -}
many1 : Parser a -> Parser (List a)
many1 parser = do
    first <- parser
    rest <- many parser
    succeed (first :: rest)


{-| Parse with separator -}
sepBy : Parser sep -> Parser a -> Parser (List a)
sepBy sep parser state =
    case parser state of
        Err _ -> Ok ( [], state )
        Ok ( first, state1 ) ->
            let
                parseRest s =
                    case sep s of
                        Err _ -> Ok ( [], s )
                        Ok ( _, s2 ) ->
                            case parser s2 of
                                Err _ -> Ok ( [], s )
                                Ok ( item, s3 ) ->
                                    case parseRest s3 of
                                        Ok ( rest, s4 ) -> Ok ( item :: rest, s4 )
                                        Err e -> Err e
            in
            case parseRest state1 of
                Ok ( rest, state2 ) -> Ok ( first :: rest, state2 )
                Err e -> Err e


{-| Try first parser, if fails try second -}
orElse : Parser a -> Parser a -> Parser a
orElse p2 p1 state =
    case p1 state of
        Ok result -> Ok result
        Err _ -> p2 state



-- TOKEN PARSERS


expect : TokenType -> Parser Token
expect expected state =
    case currentToken state of
        Just tok ->
            if tok.type_ == expected then
                Ok ( tok, advance state )
            else
                Err { message = "Expected " ++ tokenTypeName expected ++ " but got " ++ tokenTypeName tok.type_
                    , region = tok.region }
        Nothing ->
            Err { message = "Unexpected end of input, expected " ++ tokenTypeName expected
                , region = currentRegion state }


expectIdent : TokenType -> Parser String
expectIdent expected = do
    tok <- expect expected
    succeed tok.value


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


skipNewlines : Parser ()
skipNewlines state =
    case currentToken state of
        Just tok ->
            case tok.type_ of
                Newline -> skipNewlines (advance state)
                Indent _ -> skipNewlines (advance state)
                _ -> Ok ( (), state )
        Nothing -> Ok ( (), state )


skip : TokenType -> Parser ()
skip expected state =
    if peek expected state then
        Ok ( (), advance state )
    else
        Ok ( (), state )



-- MODULE PARSING


parse : String -> Result ParseError Module
parse source =
    case Lexer.lex source of
        Err e ->
            Err { message = e, region = { start = { line = 1, column = 1 }, end = { line = 1, column = 1 } } }
        Ok tokens ->
            parseModule (initState tokens)
                |> Result.map Tuple.first


parseModule : Parser Module
parseModule = do
    _ <- skipNewlines
    header <- parseModuleHeader
    imports <- parseImports
    decls <- parseDecls
    succeed
        { name = header.name
        , exposing_ = header.exposing_
        , imports = imports
        , decls = decls
        }


parseModuleHeader : Parser { name : Located Name, exposing_ : Exposing }
parseModuleHeader = do
    tok <- expect KwModule
    name <- expectIdent UpperIdent
    _ <- expect KwExposing
    exposing_ <- parseExposing
    succeed { name = locate tok.region name, exposing_ = exposing_ }


parseExposing : Parser Exposing
parseExposing state =
    case expect LParen state of
        Err e -> Err e
        Ok ( _, state1 ) ->
            if peek DotDot state1 then
                case expect DotDot (advance state1) of
                    Err e -> Err e
                    Ok ( _, state2 ) ->
                        case expect RParen state2 of
                            Err e -> Err e
                            Ok ( _, state3 ) -> Ok ( ExposingAll, state3 )
            else
                let
                    ( items, state2 ) = parseExposedList state1
                in
                case expect RParen state2 of
                    Err e -> Err e
                    Ok ( _, state3 ) -> Ok ( ExposingSome items, state3 )


parseExposedList : ParseState -> ( List ExposedItem, ParseState )
parseExposedList state =
    case currentToken state of
        Just tok ->
            case tok.type_ of
                LowerIdent ->
                    let
                        state1 = advance state
                        item = ExposedValue tok.value
                    in
                    if peek Comma state1 then
                        let
                            ( rest, state2 ) = parseExposedList (advance state1)
                        in
                        ( item :: rest, state2 )
                    else
                        ( [ item ], state1 )

                UpperIdent ->
                    let
                        state1 = advance state
                        -- Check for (..) or specific constructors
                        ( ctors, state2 ) =
                            if peek LParen state1 then
                                parseExposedConstructors (advance state1)
                            else
                                ( Nothing, state1 )
                        item = ExposedType tok.value ctors
                    in
                    if peek Comma state2 then
                        let
                            ( rest, state3 ) = parseExposedList (advance state2)
                        in
                        ( item :: rest, state3 )
                    else
                        ( [ item ], state2 )

                Operator ->
                    let
                        state1 = advance state
                        item = ExposedOperator tok.value
                    in
                    if peek Comma state1 then
                        let
                            ( rest, state2 ) = parseExposedList (advance state1)
                        in
                        ( item :: rest, state2 )
                    else
                        ( [ item ], state1 )

                LParen ->
                    -- Operator in parens like (++)
                    case expect Operator (advance state) of
                        Err _ -> ( [], state )
                        Ok ( opTok, state1 ) ->
                            case expect RParen state1 of
                                Err _ -> ( [], state )
                                Ok ( _, state2 ) ->
                                    let
                                        item = ExposedOperator opTok.value
                                    in
                                    if peek Comma state2 then
                                        let
                                            ( rest, state3 ) = parseExposedList (advance state2)
                                        in
                                        ( item :: rest, state3 )
                                    else
                                        ( [ item ], state2 )

                _ ->
                    ( [], state )

        Nothing ->
            ( [], state )


parseExposedConstructors : ParseState -> ( Maybe (List String), ParseState )
parseExposedConstructors state =
    if peek DotDot state then
        case expect RParen (advance state) of
            Err _ -> ( Nothing, state )
            Ok ( _, state1 ) -> ( Just [], state1 )  -- (..) means all constructors
    else
        let
            ( names, state1 ) = parseUpperIdentList state
        in
        case expect RParen state1 of
            Err _ -> ( Nothing, state )
            Ok ( _, state2 ) -> ( Just names, state2 )


parseUpperIdentList : ParseState -> ( List String, ParseState )
parseUpperIdentList state =
    case currentToken state of
        Just tok ->
            if tok.type_ == UpperIdent then
                let
                    state1 = advance state
                in
                if peek Comma state1 then
                    let
                        ( rest, state2 ) = parseUpperIdentList (advance state1)
                    in
                    ( tok.value :: rest, state2 )
                else
                    ( [ tok.value ], state1 )
            else
                ( [], state )
        Nothing ->
            ( [], state )



-- IMPORTS


parseImports : Parser (List (Located Import))
parseImports state =
    let
        state1 = skipNewlinesState state
    in
    if peek KwImport state1 then
        case parseImport state1 of
            Ok ( imp, state2 ) ->
                case parseImports state2 of
                    Ok ( rest, state3 ) -> Ok ( imp :: rest, state3 )
                    Err e -> Err e
            Err _ -> Ok ( [], state1 )
    else
        Ok ( [], state1 )


skipNewlinesState : ParseState -> ParseState
skipNewlinesState state =
    case currentToken state of
        Just tok ->
            case tok.type_ of
                Newline -> skipNewlinesState (advance state)
                Indent _ -> skipNewlinesState (advance state)
                _ -> state
        Nothing -> state


parseImport : Parser (Located Import)
parseImport = do
    tok <- expect KwImport
    modName <- expectIdent UpperIdent
    alias_ <- parseOptionalAlias
    _ <- skipExposingClause
    succeed (locate tok.region
        { module_ = locate tok.region modName
        , alias_ = alias_
        , exposing_ = Nothing
        })


parseOptionalAlias : Parser (Maybe String)
parseOptionalAlias state =
    if peek KwAs state then
        case expectIdent UpperIdent (advance state) of
            Ok ( name, state1 ) -> Ok ( Just name, state1 )
            Err _ -> Ok ( Nothing, state )
    else
        Ok ( Nothing, state )


skipExposingClause : Parser ()
skipExposingClause state =
    if peek KwExposing state then
        skipUntilCloseParen (advance state)
    else
        Ok ( (), state )


skipUntilCloseParen : Parser ()
skipUntilCloseParen state =
    case currentToken state of
        Just tok ->
            if tok.type_ == RParen then
                Ok ( (), advance state )
            else
                skipUntilCloseParen (advance state)
        Nothing ->
            Ok ( (), state )



-- DECLARATIONS


parseDecls : Parser (List (Located Decl))
parseDecls state =
    let
        state1 = skipNewlinesState state
    in
    if peek EOF state1 then
        Ok ( [], state1 )
    else
        case parseDecl state1 of
            Ok ( decl, state2 ) ->
                case parseDecls state2 of
                    Ok ( rest, state3 ) -> Ok ( decl :: rest, state3 )
                    Err e -> Err e
            Err _ -> Ok ( [], state1 )


parseDecl : Parser (Located Decl)
parseDecl state =
    let
        state1 = skipNewlinesState state
    in
    case currentToken state1 of
        Just tok ->
            case tok.type_ of
                KwType -> parseTypeDecl state1
                KwClass -> parseClassDecl state1
                KwInstance -> parseInstanceDecl state1
                KwForeign -> parseForeignDecl state1
                KwInfix -> parseInfixDecl state1
                KwInfixl -> parseInfixDecl state1
                KwInfixr -> parseInfixDecl state1
                LowerIdent -> parseValueDecl state1
                _ -> Err { message = "Expected declaration", region = tok.region }
        Nothing ->
            Err { message = "Unexpected end of input", region = currentRegion state1 }


parseTypeDecl : Parser (Located Decl)
parseTypeDecl = do
    tok <- expect KwType
    if peek KwAlias then do
        _ <- expect KwAlias
        parseTypeAlias tok.region
    else
        parseCustomType tok.region


parseTypeAlias : Region -> Parser (Located Decl)
parseTypeAlias startRegion = do
    name <- expectIdent UpperIdent
    typeVars <- parseTypeVarList
    _ <- expect Equals
    _ <- skipNewlines
    body <- parseTypeAnnotation
    succeed (locate startRegion (TypeAliasDecl
        { name = locate startRegion name
        , typeVars = typeVars
        , body = body
        }))


parseCustomType : Region -> Parser (Located Decl)
parseCustomType startRegion = do
    name <- expectIdent UpperIdent
    typeVars <- parseTypeVarList
    _ <- skipNewlines
    _ <- expect Equals
    ctors <- parseConstructors
    succeed (locate startRegion (CustomTypeDecl
        { name = locate startRegion name
        , typeVars = typeVars
        , constructors = ctors
        }))


parseTypeVarList : Parser (List (Located Name))
parseTypeVarList state =
    if peek LowerIdent state then
        case currentToken state of
            Just tok ->
                let
                    ( rest, state1 ) = parseTypeVarList (advance state)
                in
                Ok ( locate tok.region tok.value :: rest, state1 )
            Nothing ->
                Ok ( [], state )
    else
        Ok ( [], state )


parseConstructors : Parser (List (Located Constructor))
parseConstructors state =
    let
        state1 = skipNewlinesState state
        state2 = if peek Pipe state1 then advance state1 else state1
        state3 = skipNewlinesState state2
    in
    case parseConstructor state3 of
        Err _ -> Ok ( [], state )
        Ok ( ctor, state4 ) ->
            let
                state5 = skipNewlinesState state4
            in
            if peek Pipe state5 then
                case parseConstructors state5 of
                    Ok ( rest, state6 ) -> Ok ( ctor :: rest, state6 )
                    Err e -> Err e
            else
                Ok ( [ ctor ], state5 )


parseConstructor : Parser (Located Constructor)
parseConstructor = do
    tok <- expect UpperIdent
    args <- parseConstructorArgs
    succeed (locate tok.region
        { name = locate tok.region tok.value
        , args = args
        })


parseConstructorArgs : Parser (List (Located TypeAnnotation))
parseConstructorArgs state =
    if peekAny [ UpperIdent, LowerIdent, LParen, LBrace ] state then
        case parseTypeAtom state of
            Err _ -> Ok ( [], state )
            Ok ( arg, state1 ) ->
                case parseConstructorArgs state1 of
                    Ok ( rest, state2 ) -> Ok ( arg :: rest, state2 )
                    Err e -> Err e
    else
        Ok ( [], state )


parseClassDecl : Parser (Located Decl)
parseClassDecl = do
    tok <- expect KwClass
    className <- expectIdent UpperIdent
    typeVar <- expectIdent LowerIdent
    _ <- skipUntilWhere
    methods <- parseMethodSigs
    succeed (locate tok.region (ClassDecl
        { name = locate tok.region className
        , typeVar = locate tok.region typeVar
        , superclasses = []
        , methods = methods
        }))


skipUntilWhere : Parser ()
skipUntilWhere state =
    case currentToken state of
        Just tok ->
            if tok.type_ == KwWhere then
                Ok ( (), advance state )
            else
                skipUntilWhere (advance state)
        Nothing ->
            Ok ( (), state )


parseMethodSigs : Parser (List (Located MethodSig))
parseMethodSigs state =
    let
        state1 = skipNewlinesState state
    in
    if peek LowerIdent state1 then
        case parseMethodSig state1 of
            Err _ -> Ok ( [], state1 )
            Ok ( method, state2 ) ->
                case parseMethodSigs state2 of
                    Ok ( rest, state3 ) -> Ok ( method :: rest, state3 )
                    Err e -> Err e
    else
        Ok ( [], state1 )


parseMethodSig : Parser (Located MethodSig)
parseMethodSig = do
    name <- expectIdent LowerIdent
    _ <- expect Colon
    ty <- parseTypeAnnotation
    let region = currentRegion
    succeed (locate region { name = locate region name, type_ = ty })


parseInstanceDecl : Parser (Located Decl)
parseInstanceDecl = do
    tok <- expect KwInstance
    className <- expectIdent UpperIdent
    typeArgs <- parseConstructorArgs
    _ <- skipUntilWhere
    methods <- parseMethodImpls
    succeed (locate tok.region (InstanceDecl
        { context = []
        , className = locate tok.region className
        , typeArgs = typeArgs
        , methods = methods
        }))


parseMethodImpls : Parser (List (Located MethodImpl))
parseMethodImpls state =
    let
        state1 = skipNewlinesState state
    in
    if peek LowerIdent state1 then
        case parseMethodImpl state1 of
            Err _ -> Ok ( [], state1 )
            Ok ( method, state2 ) ->
                case parseMethodImpls state2 of
                    Ok ( rest, state3 ) -> Ok ( method :: rest, state3 )
                    Err e -> Err e
    else
        Ok ( [], state1 )


parseMethodImpl : Parser (Located MethodImpl)
parseMethodImpl = do
    name <- expectIdent LowerIdent
    args <- parsePatterns
    _ <- expect Equals
    body <- parseExpr
    let region = currentRegion
    succeed (locate region { name = locate region name, args = args, body = body })


parseForeignDecl : Parser (Located Decl)
parseForeignDecl = do
    tok <- expect KwForeign
    _ <- expect KwImport
    name <- expectIdent LowerIdent
    _ <- expect Colon
    ty <- parseTypeAnnotation
    succeed (locate tok.region (ForeignDecl
        { name = locate tok.region name
        , cName = name
        , type_ = ty
        }))


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
            in
            parseInfixBody tok.region assoc (advance state)
        Nothing ->
            Err { message = "Expected infix declaration", region = currentRegion state }


parseInfixBody : Region -> Associativity -> Parser (Located Decl)
parseInfixBody startRegion assoc = do
    precTok <- expect IntLit
    opTok <- expect Operator
    _ <- expect Equals
    func <- expectIdent LowerIdent
    succeed (locate startRegion (InfixDecl
        { operator = locate opTok.region opTok.value
        , associativity = assoc
        , precedence = String.toInt precTok.value |> Maybe.withDefault 9
        , function = locate startRegion func
        }))


parseValueDecl : Parser (Located Decl)
parseValueDecl state =
    case expectIdent LowerIdent state of
        Err e -> Err e
        Ok ( name, state1 ) ->
            if peek Colon state1 then
                parseTypedValueDecl (currentRegion state) name state1
            else
                parseValueBody (currentRegion state) name Nothing state1


parseTypedValueDecl : Region -> String -> Parser (Located Decl)
parseTypedValueDecl startRegion name = do
    _ <- expect Colon
    ty <- parseTypeAnnotation
    _ <- skipNewlines
    _ <- expectIdent LowerIdent  -- Re-parse the name
    parseValueBody startRegion name (Just ty)


parseValueBody : Region -> String -> Maybe (Located TypeAnnotation) -> Parser (Located Decl)
parseValueBody startRegion name maybeType = do
    args <- parsePatterns
    _ <- expect Equals
    _ <- skipNewlines
    body <- parseExpr
    succeed (locate startRegion (ValueDecl
        { name = locate startRegion name
        , type_ = maybeType
        , args = args
        , body = body
        }))



-- TYPE ANNOTATIONS


parseTypeAnnotation : Parser (Located TypeAnnotation)
parseTypeAnnotation = do
    first <- parseTypeApp
    parseFunctionType first


parseFunctionType : Located TypeAnnotation -> Parser (Located TypeAnnotation)
parseFunctionType left state =
    let
        state1 = skipNewlinesState state
    in
    if peek Arrow state1 then
        case expect Arrow state1 of
            Err e -> Err e
            Ok ( _, state2 ) ->
                let
                    state3 = skipNewlinesState state2
                in
                case parseTypeAnnotation state3 of
                    Err e -> Err e
                    Ok ( right, state4 ) ->
                        Ok ( locate (getValue left).region (TFunction left right), state4 )
    else
        Ok ( left, state )


parseTypeApp : Parser (Located TypeAnnotation)
parseTypeApp = do
    first <- parseTypeAtom
    args <- parseTypeAtoms
    if List.isEmpty args then
        succeed first
    else
        succeed (locate (getValue first).region (TApp first args))


parseTypeAtoms : Parser (List (Located TypeAnnotation))
parseTypeAtoms state =
    if peekAny [ UpperIdent, LowerIdent, LParen, LBrace ] state then
        case parseTypeAtom state of
            Err _ -> Ok ( [], state )
            Ok ( atom, state1 ) ->
                case parseTypeAtoms state1 of
                    Ok ( rest, state2 ) -> Ok ( atom :: rest, state2 )
                    Err e -> Err e
    else
        Ok ( [], state )


parseTypeAtom : Parser (Located TypeAnnotation)
parseTypeAtom state =
    case currentToken state of
        Just tok ->
            case tok.type_ of
                UpperIdent ->
                    Ok ( locate tok.region (TName (locate tok.region tok.value)), advance state )

                LowerIdent ->
                    Ok ( locate tok.region (TVar (locate tok.region tok.value)), advance state )

                LParen ->
                    parseTupleOrParenType (advance state)

                LBrace ->
                    parseRecordType (advance state)

                _ ->
                    Err { message = "Expected type", region = tok.region }

        Nothing ->
            Err { message = "Unexpected end of input", region = currentRegion state }


parseTupleOrParenType : Parser (Located TypeAnnotation)
parseTupleOrParenType state =
    if peek RParen state then
        Ok ( locate (currentRegion state) TUnit, advance state )
    else
        case parseTypeAnnotation state of
            Err e -> Err e
            Ok ( first, state1 ) ->
                if peek Comma state1 then
                    parseTupleTypeRest first state1
                else
                    case expect RParen state1 of
                        Err e -> Err e
                        Ok ( _, state2 ) -> Ok ( first, state2 )


parseTupleTypeRest : Located TypeAnnotation -> Parser (Located TypeAnnotation)
parseTupleTypeRest first state =
    case expect Comma state of
        Err e -> Err e
        Ok ( _, state1 ) ->
            case parseTypeAnnotation state1 of
                Err e -> Err e
                Ok ( second, state2 ) ->
                    if peek Comma state2 then
                        case expect Comma state2 of
                            Err e -> Err e
                            Ok ( _, state3 ) ->
                                case parseTypeAnnotation state3 of
                                    Err e -> Err e
                                    Ok ( third, state4 ) ->
                                        case expect RParen state4 of
                                            Err e -> Err e
                                            Ok ( _, state5 ) ->
                                                Ok ( locate (getValue first).region (TTuple3 first second third), state5 )
                    else
                        case expect RParen state2 of
                            Err e -> Err e
                            Ok ( _, state3 ) ->
                                Ok ( locate (getValue first).region (TTuple first second), state3 )


parseRecordType : Parser (Located TypeAnnotation)
parseRecordType state =
    let
        state1 = skipNewlinesState state
    in
    if peek RBrace state1 then
        Ok ( locate (currentRegion state) (TRecord [] Nothing), advance state1 )
    else if peek LowerIdent state1 then
        case expectIdent LowerIdent state1 of
            Err e -> Err e
            Ok ( name, state2 ) ->
                if peek Pipe state2 then
                    -- Extensible record: { a | field : Type }
                    parseExtensibleRecordFields name state2
                else if peek Colon state2 then
                    -- Regular record: { field : Type, ... }
                    parseRecordFields (currentRegion state) state1
                else
                    Err { message = "Expected : or |", region = currentRegion state2 }
        else
            Err { message = "Expected field name", region = currentRegion state1 }


parseExtensibleRecordFields : String -> Parser (Located TypeAnnotation)
parseExtensibleRecordFields rowVar = do
    _ <- expect Pipe
    fields <- parseRecordFieldList
    _ <- expect RBrace
    let region = currentRegion
    succeed (locate region (TRecord fields (Just (locate region rowVar))))


parseRecordFields : Region -> Parser (Located TypeAnnotation)
parseRecordFields startRegion = do
    fields <- parseRecordFieldList
    _ <- expect RBrace
    succeed (locate startRegion (TRecord fields Nothing))


parseRecordFieldList : Parser (List ( Located Name, Located TypeAnnotation ))
parseRecordFieldList state =
    let
        state1 = skipNewlinesState state
    in
    case parseRecordField state1 of
        Err _ -> Ok ( [], state )
        Ok ( field, state2 ) ->
            let
                state3 = skipNewlinesState state2
            in
            if peek Comma state3 then
                case parseRecordFieldList (advance state3) of
                    Ok ( rest, state4 ) -> Ok ( field :: rest, state4 )
                    Err e -> Err e
            else
                Ok ( [ field ], state3 )


parseRecordField : Parser ( Located Name, Located TypeAnnotation )
parseRecordField = do
    name <- expectIdent LowerIdent
    _ <- expect Colon
    ty <- parseTypeAnnotation
    let region = currentRegion
    succeed ( locate region name, ty )



-- PATTERNS


parsePatterns : Parser (List (Located Pattern))
parsePatterns state =
    case parsePattern state of
        Err _ -> Ok ( [], state )
        Ok ( pat, state1 ) ->
            case parsePatterns state1 of
                Ok ( rest, state2 ) -> Ok ( pat :: rest, state2 )
                Err e -> Err e


parsePattern : Parser (Located Pattern)
parsePattern state =
    case currentToken state of
        Just tok ->
            case tok.type_ of
                LowerIdent ->
                    Ok ( locate tok.region (PVar (locate tok.region tok.value)), advance state )

                UpperIdent ->
                    parseConstructorPattern tok state

                Underscore ->
                    Ok ( locate tok.region PWildcard, advance state )

                IntLit ->
                    Ok ( locate tok.region (PLit (LInt (String.toInt tok.value |> Maybe.withDefault 0))), advance state )

                StringLit ->
                    Ok ( locate tok.region (PLit (LString tok.value)), advance state )

                CharLit ->
                    let
                        char = String.uncons tok.value |> Maybe.map Tuple.first |> Maybe.withDefault ' '
                    in
                    Ok ( locate tok.region (PLit (LChar char)), advance state )

                LParen ->
                    parseTupleOrParenPattern (advance state)

                LBracket ->
                    parseListPattern (advance state)

                LBrace ->
                    parseRecordPattern (advance state)

                _ ->
                    Err { message = "Expected pattern", region = tok.region }

        Nothing ->
            Err { message = "Unexpected end of input", region = currentRegion state }


parseConstructorPattern : Token -> ParseState -> Result ParseError ( Located Pattern, ParseState )
parseConstructorPattern tok state =
    let
        state1 = advance state
    in
    case parsePatternArgs state1 of
        Ok ( args, state2 ) ->
            if List.isEmpty args then
                Ok ( locate tok.region (PCtor (locate tok.region tok.value) []), state2 )
            else
                Ok ( locate tok.region (PCtor (locate tok.region tok.value) args), state2 )
        Err e -> Err e


parsePatternArgs : Parser (List (Located Pattern))
parsePatternArgs state =
    if peekAny [ LowerIdent, Underscore, IntLit, StringLit, CharLit, LParen, LBracket, LBrace ] state then
        case parsePatternAtom state of
            Err _ -> Ok ( [], state )
            Ok ( arg, state1 ) ->
                case parsePatternArgs state1 of
                    Ok ( rest, state2 ) -> Ok ( arg :: rest, state2 )
                    Err e -> Err e
    else
        Ok ( [], state )


parsePatternAtom : Parser (Located Pattern)
parsePatternAtom state =
    case currentToken state of
        Just tok ->
            case tok.type_ of
                LowerIdent ->
                    Ok ( locate tok.region (PVar (locate tok.region tok.value)), advance state )

                Underscore ->
                    Ok ( locate tok.region PWildcard, advance state )

                IntLit ->
                    Ok ( locate tok.region (PLit (LInt (String.toInt tok.value |> Maybe.withDefault 0))), advance state )

                StringLit ->
                    Ok ( locate tok.region (PLit (LString tok.value)), advance state )

                CharLit ->
                    let
                        char = String.uncons tok.value |> Maybe.map Tuple.first |> Maybe.withDefault ' '
                    in
                    Ok ( locate tok.region (PLit (LChar char)), advance state )

                LParen ->
                    parseTupleOrParenPattern (advance state)

                LBracket ->
                    parseListPattern (advance state)

                LBrace ->
                    parseRecordPattern (advance state)

                _ ->
                    Err { message = "Expected pattern atom", region = tok.region }

        Nothing ->
            Err { message = "Unexpected end of input", region = currentRegion state }


parseTupleOrParenPattern : Parser (Located Pattern)
parseTupleOrParenPattern state =
    if peek RParen state then
        Ok ( locate (currentRegion state) PUnit, advance state )
    else
        case parsePattern state of
            Err e -> Err e
            Ok ( first, state1 ) ->
                if peek Comma state1 then
                    parseTuplePatternRest first state1
                else
                    case expect RParen state1 of
                        Err e -> Err e
                        Ok ( _, state2 ) -> Ok ( first, state2 )


parseTuplePatternRest : Located Pattern -> Parser (Located Pattern)
parseTuplePatternRest first = do
    _ <- expect Comma
    second <- parsePattern
    rest <- parseOptionalThirdPattern
    _ <- expect RParen
    case rest of
        Just third ->
            succeed (locate (getValue first).region (PTuple3 first second third))
        Nothing ->
            succeed (locate (getValue first).region (PTuple first second))


parseOptionalThirdPattern : Parser (Maybe (Located Pattern))
parseOptionalThirdPattern state =
    if peek Comma state then
        case expect Comma state of
            Err _ -> Ok ( Nothing, state )
            Ok ( _, state1 ) ->
                case parsePattern state1 of
                    Err _ -> Ok ( Nothing, state )
                    Ok ( third, state2 ) -> Ok ( Just third, state2 )
    else
        Ok ( Nothing, state )


parseListPattern : Parser (Located Pattern)
parseListPattern state =
    if peek RBracket state then
        Ok ( locate (currentRegion state) (PList []), advance state )
    else
        case parsePatternList state of
            Ok ( items, state1 ) ->
                case expect RBracket state1 of
                    Err e -> Err e
                    Ok ( _, state2 ) -> Ok ( locate (currentRegion state) (PList items), state2 )
            Err e -> Err e


parsePatternList : Parser (List (Located Pattern))
parsePatternList = do
    first <- parsePattern
    rest <- parsePatternListRest
    succeed (first :: rest)


parsePatternListRest : Parser (List (Located Pattern))
parsePatternListRest state =
    if peek Comma state then
        case expect Comma state of
            Err _ -> Ok ( [], state )
            Ok ( _, state1 ) ->
                case parsePattern state1 of
                    Err _ -> Ok ( [], state )
                    Ok ( pat, state2 ) ->
                        case parsePatternListRest state2 of
                            Ok ( rest, state3 ) -> Ok ( pat :: rest, state3 )
                            Err e -> Err e
    else
        Ok ( [], state )


parseRecordPattern : Parser (Located Pattern)
parseRecordPattern state =
    if peek RBrace state then
        Ok ( locate (currentRegion state) (PRecord []), advance state )
    else
        case parseFieldPatternList state of
            Ok ( fields, state1 ) ->
                case expect RBrace state1 of
                    Err e -> Err e
                    Ok ( _, state2 ) -> Ok ( locate (currentRegion state) (PRecord fields), state2 )
            Err e -> Err e


parseFieldPatternList : Parser (List (Located Name))
parseFieldPatternList = do
    first <- expectIdent LowerIdent
    rest <- parseFieldPatternListRest
    let region = currentRegion
    succeed (locate region first :: rest)


parseFieldPatternListRest : Parser (List (Located Name))
parseFieldPatternListRest state =
    if peek Comma state then
        case expect Comma state of
            Err _ -> Ok ( [], state )
            Ok ( _, state1 ) ->
                case expectIdent LowerIdent state1 of
                    Err _ -> Ok ( [], state )
                    Ok ( name, state2 ) ->
                        let
                            region = currentRegion state2
                        in
                        case parseFieldPatternListRest state2 of
                            Ok ( rest, state3 ) -> Ok ( locate region name :: rest, state3 )
                            Err e -> Err e
    else
        Ok ( [], state )



-- EXPRESSIONS


parseExpr : Parser (Located Expr)
parseExpr state =
    let
        state1 = skipNewlinesUnlessColumn1 state
    in
    parsePipeline state1


skipNewlinesUnlessColumn1 : ParseState -> ParseState
skipNewlinesUnlessColumn1 state =
    case currentToken state of
        Just tok ->
            case tok.type_ of
                Newline -> skipNewlinesUnlessColumn1 (advance state)
                Indent _ -> skipNewlinesUnlessColumn1 (advance state)
                _ ->
                    if tok.region.start.column == 1 then
                        if tok.type_ == Operator || tok.type_ == DoubleColon || tok.type_ == Pipe || tok.type_ == Comma then
                            state
                        else
                            state
                    else
                        state
        Nothing -> state


parsePipeline : Parser (Located Expr)
parsePipeline = do
    first <- parseOr
    parsePipelineRest first


parsePipelineRest : Located Expr -> Parser (Located Expr)
parsePipelineRest left state =
    let
        state1 = skipNewlinesUnlessColumn1 state
    in
    case currentToken state1 of
        Just tok ->
            if tok.type_ == Operator && (tok.value == "|>" || tok.value == "<|" || tok.value == ">>" || tok.value == "<<") then
                case parseOr (advance state1) of
                    Err e -> Err e
                    Ok ( right, state2 ) ->
                        let
                            newExpr = locate (getValue left).region
                                (EBinOp (locate tok.region tok.value) left right)
                        in
                        parsePipelineRest newExpr state2
            else
                Ok ( left, state )
        Nothing -> Ok ( left, state )


parseOr : Parser (Located Expr)
parseOr = do
    first <- parseAnd
    parseOrRest first


parseOrRest : Located Expr -> Parser (Located Expr)
parseOrRest left state =
    let
        state1 = skipNewlinesUnlessColumn1 state
    in
    case currentToken state1 of
        Just tok ->
            if tok.type_ == Operator && tok.value == "||" then
                case parseAnd (advance state1) of
                    Err e -> Err e
                    Ok ( right, state2 ) ->
                        let
                            newExpr = locate (getValue left).region
                                (EBinOp (locate tok.region tok.value) left right)
                        in
                        parseOrRest newExpr state2
            else
                Ok ( left, state )
        Nothing -> Ok ( left, state )


parseAnd : Parser (Located Expr)
parseAnd = do
    first <- parseComparison
    parseAndRest first


parseAndRest : Located Expr -> Parser (Located Expr)
parseAndRest left state =
    let
        state1 = skipNewlinesUnlessColumn1 state
    in
    case currentToken state1 of
        Just tok ->
            if tok.type_ == Operator && tok.value == "&&" then
                case parseComparison (advance state1) of
                    Err e -> Err e
                    Ok ( right, state2 ) ->
                        let
                            newExpr = locate (getValue left).region
                                (EBinOp (locate tok.region tok.value) left right)
                        in
                        parseAndRest newExpr state2
            else
                Ok ( left, state )
        Nothing -> Ok ( left, state )


parseComparison : Parser (Located Expr)
parseComparison = do
    first <- parseCons
    parseComparisonRest first


parseComparisonRest : Located Expr -> Parser (Located Expr)
parseComparisonRest left state =
    let
        state1 = skipNewlinesUnlessColumn1 state
        compOps = [ "==", "/=", "<", ">", "<=", ">=" ]
    in
    case currentToken state1 of
        Just tok ->
            if tok.type_ == Operator && List.member tok.value compOps then
                case parseCons (advance state1) of
                    Err e -> Err e
                    Ok ( right, state2 ) ->
                        Ok ( locate (getValue left).region
                            (EBinOp (locate tok.region tok.value) left right), state2 )
            else
                Ok ( left, state )
        Nothing -> Ok ( left, state )


parseCons : Parser (Located Expr)
parseCons = do
    first <- parseAppend
    parseConsRest first


parseConsRest : Located Expr -> Parser (Located Expr)
parseConsRest left state =
    let
        state1 = skipNewlinesUnlessColumn1 state
    in
    case currentToken state1 of
        Just tok ->
            if tok.type_ == DoubleColon then
                case parseCons (advance state1) of
                    Err e -> Err e
                    Ok ( right, state2 ) ->
                        Ok ( locate (getValue left).region
                            (EBinOp (locate tok.region "::") left right), state2 )
            else
                Ok ( left, state )
        Nothing -> Ok ( left, state )


parseAppend : Parser (Located Expr)
parseAppend = do
    first <- parseAddSub
    parseAppendRest first


parseAppendRest : Located Expr -> Parser (Located Expr)
parseAppendRest left state =
    let
        state1 = skipNewlinesUnlessColumn1 state
    in
    case currentToken state1 of
        Just tok ->
            if tok.type_ == Operator && tok.value == "++" then
                case parseAddSub (advance state1) of
                    Err e -> Err e
                    Ok ( right, state2 ) ->
                        let
                            newExpr = locate (getValue left).region
                                (EBinOp (locate tok.region tok.value) left right)
                        in
                        parseAppendRest newExpr state2
            else
                Ok ( left, state )
        Nothing -> Ok ( left, state )


parseAddSub : Parser (Located Expr)
parseAddSub = do
    first <- parseMulDiv
    parseAddSubRest first


parseAddSubRest : Located Expr -> Parser (Located Expr)
parseAddSubRest left state =
    let
        state1 = skipNewlinesUnlessColumn1 state
    in
    case currentToken state1 of
        Just tok ->
            if tok.type_ == Operator && (tok.value == "+" || tok.value == "-") then
                case parseMulDiv (advance state1) of
                    Err e -> Err e
                    Ok ( right, state2 ) ->
                        let
                            newExpr = locate (getValue left).region
                                (EBinOp (locate tok.region tok.value) left right)
                        in
                        parseAddSubRest newExpr state2
            else
                Ok ( left, state )
        Nothing -> Ok ( left, state )


parseMulDiv : Parser (Located Expr)
parseMulDiv = do
    first <- parseUnary
    parseMulDivRest first


parseMulDivRest : Located Expr -> Parser (Located Expr)
parseMulDivRest left state =
    let
        state1 = skipNewlinesUnlessColumn1 state
        mulOps = [ "*", "/", "//", "^" ]
    in
    case currentToken state1 of
        Just tok ->
            if tok.type_ == Operator && List.member tok.value mulOps then
                case parseUnary (advance state1) of
                    Err e -> Err e
                    Ok ( right, state2 ) ->
                        let
                            newExpr = locate (getValue left).region
                                (EBinOp (locate tok.region tok.value) left right)
                        in
                        parseMulDivRest newExpr state2
            else
                Ok ( left, state )
        Nothing -> Ok ( left, state )


parseUnary : Parser (Located Expr)
parseUnary state =
    case currentToken state of
        Just tok ->
            if tok.type_ == Operator && tok.value == "-" then
                case parseUnary (advance state) of
                    Err e -> Err e
                    Ok ( expr, state1 ) ->
                        Ok ( locate tok.region (ENegate expr), state1 )
            else
                parseApp state
        Nothing -> parseApp state


parseApp : Parser (Located Expr)
parseApp = do
    first <- parseAtom
    parseAppRest first


parseAppRest : Located Expr -> Parser (Located Expr)
parseAppRest func state =
    case parseAtom state of
        Err _ -> Ok ( func, state )
        Ok ( arg, state1 ) ->
            let
                newExpr = locate (getValue func).region (EApp func arg)
            in
            parseAppRest newExpr state1


parseAtom : Parser (Located Expr)
parseAtom state =
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
                    let
                        char = String.uncons tok.value |> Maybe.map Tuple.first |> Maybe.withDefault ' '
                    in
                    Ok ( locate tok.region (ELit (LChar char)), advance state )

                LowerIdent ->
                    parseVarOrRecordAccess tok state

                UpperIdent ->
                    Ok ( locate tok.region (EVar (locate tok.region tok.value)), advance state )

                LParen ->
                    parseTupleOrParenOrOperator (advance state)

                LBracket ->
                    parseList (advance state)

                LBrace ->
                    parseRecordExpr (advance state)

                Backslash ->
                    parseLambda (advance state)

                KwIf ->
                    parseIf (advance state)

                KwCase ->
                    parseCase (advance state)

                KwLet ->
                    parseLet (advance state)

                KwDo ->
                    parseDo (advance state)

                Dot ->
                    parseRecordAccessor (advance state)

                _ ->
                    Err { message = "Expected expression", region = tok.region }

        Nothing ->
            Err { message = "Unexpected end of input", region = currentRegion state }


parseVarOrRecordAccess : Token -> ParseState -> Result ParseError ( Located Expr, ParseState )
parseVarOrRecordAccess tok state =
    let
        state1 = advance state
        var = locate tok.region (EVar (locate tok.region tok.value))
    in
    parseRecordAccessChain var state1


parseRecordAccessChain : Located Expr -> Parser (Located Expr)
parseRecordAccessChain expr state =
    if peek Dot state then
        case currentToken (advance state) of
            Just tok ->
                if tok.type_ == LowerIdent then
                    let
                        newExpr = locate (getValue expr).region
                            (ERecordAccess expr (locate tok.region tok.value))
                    in
                    parseRecordAccessChain newExpr (advance (advance state))
                else
                    Ok ( expr, state )
            Nothing -> Ok ( expr, state )
    else
        Ok ( expr, state )


parseRecordAccessor : Parser (Located Expr)
parseRecordAccessor = do
    name <- expectIdent LowerIdent
    let region = currentRegion
    succeed (locate region (ERecordAccessor (locate region name)))


parseTupleOrParenOrOperator : Parser (Located Expr)
parseTupleOrParenOrOperator state =
    if peek RParen state then
        Ok ( locate (currentRegion state) EUnit, advance state )
    else if peek Operator state then
        case currentToken state of
            Just tok ->
                case expect RParen (advance state) of
                    Err e -> Err e
                    Ok ( _, state1 ) ->
                        Ok ( locate tok.region (EVar (locate tok.region tok.value)), state1 )
            Nothing ->
                Err { message = "Expected operator", region = currentRegion state }
    else
        case parseExpr state of
            Err e -> Err e
            Ok ( first, state1 ) ->
                if peek Comma state1 then
                    parseTupleRest first state1
                else
                    case expect RParen state1 of
                        Err e -> Err e
                        Ok ( _, state2 ) -> Ok ( first, state2 )


parseTupleRest : Located Expr -> Parser (Located Expr)
parseTupleRest first = do
    _ <- expect Comma
    second <- parseExpr
    rest <- parseOptionalThirdExpr
    _ <- expect RParen
    case rest of
        Just third ->
            succeed (locate (getValue first).region (ETuple3 first second third))
        Nothing ->
            succeed (locate (getValue first).region (ETuple first second))


parseOptionalThirdExpr : Parser (Maybe (Located Expr))
parseOptionalThirdExpr state =
    if peek Comma state then
        case expect Comma state of
            Err _ -> Ok ( Nothing, state )
            Ok ( _, state1 ) ->
                case parseExpr state1 of
                    Err _ -> Ok ( Nothing, state )
                    Ok ( third, state2 ) -> Ok ( Just third, state2 )
    else
        Ok ( Nothing, state )


parseList : Parser (Located Expr)
parseList state =
    if peek RBracket state then
        Ok ( locate (currentRegion state) (EList []), advance state )
    else
        case parseExprList state of
            Ok ( items, state1 ) ->
                case expect RBracket state1 of
                    Err e -> Err e
                    Ok ( _, state2 ) -> Ok ( locate (currentRegion state) (EList items), state2 )
            Err e -> Err e


parseExprList : Parser (List (Located Expr))
parseExprList = do
    first <- parseExpr
    rest <- parseExprListRest
    succeed (first :: rest)


parseExprListRest : Parser (List (Located Expr))
parseExprListRest state =
    let
        state1 = skipNewlinesState state
    in
    if peek Comma state1 then
        case expect Comma state1 of
            Err _ -> Ok ( [], state )
            Ok ( _, state2 ) ->
                let
                    state3 = skipNewlinesState state2
                in
                case parseExpr state3 of
                    Err _ -> Ok ( [], state )
                    Ok ( expr, state4 ) ->
                        case parseExprListRest state4 of
                            Ok ( rest, state5 ) -> Ok ( expr :: rest, state5 )
                            Err e -> Err e
    else
        Ok ( [], state )


parseRecordExpr : Parser (Located Expr)
parseRecordExpr state =
    let
        state1 = skipNewlinesState state
    in
    if peek RBrace state1 then
        Ok ( locate (currentRegion state) (ERecord []), advance state1 )
    else if peek LowerIdent state1 then
        case expectIdent LowerIdent state1 of
            Err e -> Err e
            Ok ( name, state2 ) ->
                if peek Pipe state2 then
                    -- Record update: { rec | field = value }
                    parseRecordUpdate name state2
                else if peek Equals state2 then
                    -- Record literal: { field = value, ... }
                    parseRecordLiteral (currentRegion state) state1
                else
                    Err { message = "Expected = or |", region = currentRegion state2 }
    else
        Err { message = "Expected field name", region = currentRegion state1 }


parseRecordUpdate : String -> Parser (Located Expr)
parseRecordUpdate name = do
    _ <- expect Pipe
    _ <- skipNewlines
    fields <- parseRecordFieldExprList
    _ <- skipNewlines
    _ <- expect RBrace
    let region = currentRegion
    succeed (locate region (ERecordUpdate (locate region name) fields))


parseRecordLiteral : Region -> Parser (Located Expr)
parseRecordLiteral startRegion = do
    fields <- parseRecordFieldExprList
    _ <- skipNewlines
    _ <- expect RBrace
    succeed (locate startRegion (ERecord fields))


parseRecordFieldExprList : Parser (List ( Located Name, Located Expr ))
parseRecordFieldExprList state =
    let
        state1 = skipNewlinesState state
    in
    case parseRecordFieldExpr state1 of
        Err _ -> Ok ( [], state )
        Ok ( field, state2 ) ->
            let
                state3 = skipNewlinesState state2
            in
            if peek Comma state3 then
                case parseRecordFieldExprList (advance state3) of
                    Ok ( rest, state4 ) -> Ok ( field :: rest, state4 )
                    Err e -> Err e
            else
                Ok ( [ field ], state3 )


parseRecordFieldExpr : Parser ( Located Name, Located Expr )
parseRecordFieldExpr = do
    name <- expectIdent LowerIdent
    _ <- expect Equals
    value <- parseExpr
    let region = currentRegion
    succeed ( locate region name, value )


parseLambda : Parser (Located Expr)
parseLambda = do
    args <- many1 parsePattern
    _ <- expect Arrow
    body <- parseExpr
    let region = currentRegion
    succeed (locate region (ELambda args body))


parseIf : Parser (Located Expr)
parseIf = do
    _ <- skipNewlines
    cond <- parseExpr
    _ <- skipNewlines
    _ <- expect KwThen
    _ <- skipNewlines
    then_ <- parseExpr
    _ <- skipNewlines
    _ <- expect KwElse
    _ <- skipNewlines
    else_ <- parseExpr
    let region = currentRegion
    succeed (locate region (EIf cond then_ else_))


parseCase : Parser (Located Expr)
parseCase = do
    _ <- skipNewlines
    expr <- parseExpr
    _ <- skipNewlines
    _ <- expect KwOf
    branches <- parseCaseBranches
    let region = currentRegion
    succeed (locate region (ECase expr branches))


parseCaseBranches : Parser (List ( Located Pattern, Located Expr ))
parseCaseBranches state =
    let
        state1 = skipNewlinesState state
    in
    case parseCaseBranch state1 of
        Err _ -> Ok ( [], state )
        Ok ( branch, state2 ) ->
            case parseCaseBranches state2 of
                Ok ( rest, state3 ) -> Ok ( branch :: rest, state3 )
                Err e -> Err e


parseCaseBranch : Parser ( Located Pattern, Located Expr )
parseCaseBranch = do
    pat <- parsePattern
    _ <- expect Arrow
    _ <- skipNewlines
    body <- parseExpr
    succeed ( pat, body )


parseLet : Parser (Located Expr)
parseLet state =
    let
        state1 = skipNewlinesState (advance state)
    in
    case parseLetBindings state1 of
        Ok ( bindings, state2 ) ->
            let
                state3 = skipNewlinesState state2
            in
            case expect KwIn state3 of
                Err e -> Err e
                Ok ( _, state4 ) ->
                    let
                        state5 = skipNewlinesState state4
                    in
                    case parseExpr state5 of
                        Err e -> Err e
                        Ok ( body, state6 ) ->
                            Ok ( locate (currentRegion state) (ELet bindings body), state6 )
        Err e -> Err e


parseLetBindings : Parser (List ( Located Pattern, Located Expr ))
parseLetBindings state =
    case parseLetBinding state of
        Err _ -> Ok ( [], state )
        Ok ( binding, state1 ) ->
            let
                state2 = skipNewlinesState state1
            in
            if peek KwIn state2 then
                Ok ( [ binding ], state2 )
            else
                case parseLetBindings state2 of
                    Ok ( rest, state3 ) -> Ok ( binding :: rest, state3 )
                    Err e -> Err e


parseLetBinding : Parser ( Located Pattern, Located Expr )
parseLetBinding = do
    pat <- parsePattern
    args <- parsePatterns
    _ <- expect Equals
    _ <- skipNewlines
    body <- parseExpr
    let finalBody =
            if List.isEmpty args then
                body
            else
                locate (getValue body).region (ELambda args body)
    succeed ( pat, finalBody )


parseDo : Parser (Located Expr)
parseDo state =
    let
        state1 = skipNewlinesState state
    in
    case parseDoStatements state1 of
        Ok ( stmts, state2 ) ->
            Ok ( locate (currentRegion state) (EDo stmts), state2 )
        Err e -> Err e


parseDoStatements : Parser (List DoStatement)
parseDoStatements state =
    let
        state1 = skipNewlinesState state
    in
    case parseDoStatement state1 of
        Err _ -> Ok ( [], state )
        Ok ( stmt, state2 ) ->
            case parseDoStatements state2 of
                Ok ( rest, state3 ) -> Ok ( stmt :: rest, state3 )
                Err e -> Err e


parseDoStatement : Parser DoStatement
parseDoStatement state =
    if peek KwLet state then
        parseDoLet (advance state)
    else
        case parseExpr state of
            Err e -> Err e
            Ok ( expr, state1 ) ->
                if peek BackArrow state1 then
                    Err { message = "Bind pattern should come before <-", region = currentRegion state1 }
                else
                    -- Check if this is a bind (pattern <- expr) by looking for BackArrow
                    -- Actually we need to parse pattern first
                    case parsePattern state of
                        Ok ( pat, state2 ) ->
                            if peek BackArrow state2 then
                                case expect BackArrow state2 of
                                    Err e -> Err e
                                    Ok ( _, state3 ) ->
                                        case parseExpr state3 of
                                            Err e -> Err e
                                            Ok ( bindExpr, state4 ) ->
                                                Ok ( DoBind pat bindExpr, state4 )
                            else
                                -- Just an expression
                                Ok ( DoExpr expr, state1 )
                        Err _ ->
                            Ok ( DoExpr expr, state1 )


parseDoLet : Parser DoStatement
parseDoLet = do
    pat <- parsePattern
    _ <- expect Equals
    body <- parseExpr
    succeed (DoLet pat body)



-- HELPERS


locate : Region -> a -> Located a
locate region value =
    { pos = region.start.line, value = value }


getValue : Located a -> a
getValue loc = loc.value


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
        KwInfix -> "infix"
        KwInfixl -> "infixl"
        KwInfixr -> "infixr"
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
