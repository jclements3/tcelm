port module Cli exposing (main)

{-| CLI entry point for tcelm compiler.

Compiles Elm source code to C code for RTEMS.

-}

import AST.Source as Src
import Generate.C as C
import Json.Decode as Decode
import Json.Encode as Encode
import Parse.Module as Module
import Parse.Primitives
import Platform
import Reporting.Error.Syntax


-- PORTS


port receiveSource : (String -> msg) -> Sub msg


port sendOutput : Encode.Value -> Cmd msg



-- MAIN


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Flags =
    { target : String
    }


type alias Model =
    { target : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { target = flags.target }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotSource String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSource source ->
            let
                result =
                    compile model.target source
            in
            ( model, sendOutput result )


compile : String -> String -> Encode.Value
compile target source =
    case Module.parse source of
        Ok ast ->
            let
                cCode =
                    if target == "i386-rtems-nuc" then
                        generateRtemsCode ast

                    else if target == "native" then
                        generateNativeCode ast

                    else
                        C.generateModule ast
            in
            Encode.object
                [ ( "success", Encode.bool True )
                , ( "code", Encode.string cCode )
                ]

        Err errors ->
            Encode.object
                [ ( "success", Encode.bool False )
                , ( "error", Encode.string (formatErrors errors) )
                ]


{-| Generate C code for RTEMS target with Init wrapper
    For simple programs, we generate standalone code without the full runtime.
    Outputs to both serial console and SXGA framebuffer.
-}
generateRtemsCode : Src.Module -> String
generateRtemsCode ast =
    let
        moduleName =
            ast.name
                |> Maybe.map (\(Src.At _ n) -> n)
                |> Maybe.withDefault "Main"

        mainValue =
            extractMain ast

        -- Generate user-defined functions (non-main values with arguments)
        userFunctions =
            ast.values
                |> List.filterMap
                    (\(Src.At _ value) ->
                        let
                            (Src.At _ name) =
                                value.name
                        in
                        if name /= "main" && not (List.isEmpty value.args) then
                            Just (generateUserFunction name value.args value.body)

                        else
                            Nothing
                    )
                |> String.join "\n\n"

        -- Check if a union has any constructors with data
        unionHasData union =
            union.ctors
                |> List.any (\( _, args ) -> not (List.isEmpty args))

        -- Generate custom type definitions (structs, tags, and constructors)
        customTypeCode =
            ast.unions
                |> List.map
                    (\(Src.At _ union) ->
                        let
                            (Src.At _ typeName) =
                                union.name

                            hasData =
                                unionHasData union

                            -- Tag defines for all constructors
                            tagDefines =
                                union.ctors
                                    |> List.indexedMap
                                        (\i ( Src.At _ ctorName, _ ) ->
                                            "#define TAG_" ++ ctorName ++ " " ++ String.fromInt i
                                        )
                                    |> String.join "\n"

                            -- Type alias for all custom types (use common elm_union_t)
                            structDef =
                                "typedef elm_union_t elm_" ++ typeName ++ ";\n"

                            -- Constructor functions using elm_union_t
                            ctorFuncs =
                                union.ctors
                                    |> List.indexedMap
                                        (\i ( Src.At _ ctorName, ctorArgs ) ->
                                            if List.isEmpty ctorArgs then
                                                -- No-data constructor
                                                "static elm_union_t elm_"
                                                    ++ ctorName
                                                    ++ "(void) {\n    elm_union_t result = { .tag = TAG_"
                                                    ++ ctorName
                                                    ++ ", .data = 0 };\n    return result;\n}"

                                            else
                                                -- Constructor with data
                                                "static elm_union_t elm_"
                                                    ++ ctorName
                                                    ++ "(int value) {\n    elm_union_t result = { .tag = TAG_"
                                                    ++ ctorName
                                                    ++ ", .data = value };\n    return result;\n}"
                                        )
                                    |> String.join "\n\n"
                        in
                        tagDefines ++ "\n" ++ structDef ++ ctorFuncs
                    )
                |> String.join "\n\n"

        constructorDefinesCode =
            if String.isEmpty customTypeCode then
                ""

            else
                "/* Custom type definitions */\n" ++ customTypeCode ++ "\n\n"

        -- Collect and generate lifted local functions from all values
        liftedFunctions =
            ast.values
                |> List.concatMap
                    (\(Src.At _ value) ->
                        let
                            (Src.At _ name) =
                                value.name
                        in
                        collectLocalFunctions name value.body
                    )
                |> List.map
                    (\lf ->
                        generateLiftedFunction lf.prefix lf.name lf.args lf.body
                    )
                |> String.join "\n\n"

        liftedFunctionsCode =
            if String.isEmpty liftedFunctions then
                ""

            else
                "/* Lifted local functions */\n" ++ liftedFunctions ++ "\n\n"

        userFunctionsCode =
            if String.isEmpty userFunctions then
                ""

            else
                "/* User-defined functions */\n" ++ userFunctions ++ "\n\n"

        -- Generate elm_main function and result handling based on type
        codeGen =
            case mainValue of
                MainString s ->
                    { elmMainFunc = "static const char *elm_main(void) {\n    return \"" ++ escapeC s ++ "\";\n}"
                    , resultDecl = "const char *result = elm_main();"
                    , resultPrint = "serial_print(result);"
                    , fbPrint = "fb_print(result);"
                    }

                MainInt n ->
                    { elmMainFunc = "static int elm_main(void) {\n    return " ++ String.fromInt n ++ ";\n}"
                    , resultDecl = "int result = elm_main();\n    char result_str[32];\n    int_to_str(result, result_str);"
                    , resultPrint = "serial_print(result_str);"
                    , fbPrint = "fb_print(result_str);"
                    }

                MainExpr cType cExpr ->
                    if cType == "int" then
                        { elmMainFunc = "static int elm_main(void) {\n    return " ++ cExpr ++ ";\n}"
                        , resultDecl = "int result = elm_main();\n    char result_str[32];\n    int_to_str(result, result_str);"
                        , resultPrint = "serial_print(result_str);"
                        , fbPrint = "fb_print(result_str);"
                        }
                    else
                        { elmMainFunc = "static const char *elm_main(void) {\n    return " ++ cExpr ++ ";\n}"
                        , resultDecl = "const char *result = elm_main();"
                        , resultPrint = "serial_print(result);"
                        , fbPrint = "fb_print(result);"
                        }

        standaloneCode =
            String.join "\n"
                [ "/*"
                , " * Generated by tcelm from " ++ moduleName
                , " * DO NOT EDIT - This file is auto-generated"
                , " * Standalone version with SXGA framebuffer output"
                , " */"
                , ""
                , "/* RTEMS entry point */"
                , "typedef unsigned int rtems_task_argument;"
                , "typedef unsigned int rtems_id;"
                , "#define RTEMS_SELF 0"
                , "extern void rtems_task_delete(rtems_id id);"
                , ""
                , "/* String comparison */"
                , "static int strcmp(const char *a, const char *b) {"
                , "    while (*a && *a == *b) { a++; b++; }"
                , "    return (unsigned char)*a - (unsigned char)*b;"
                , "}"
                , ""
                , "/* Memory operations (needed for struct initialization) */"
                , "static void *memset(void *s, int c, unsigned int n) {"
                , "    unsigned char *p = s;"
                , "    while (n--) *p++ = (unsigned char)c;"
                , "    return s;"
                , "}"
                , ""
                , "static void *memmove(void *dest, const void *src, unsigned int n) {"
                , "    unsigned char *d = dest;"
                , "    const unsigned char *s = src;"
                , "    if (d < s) { while (n--) *d++ = *s++; }"
                , "    else { d += n; s += n; while (n--) *--d = *--s; }"
                , "    return dest;"
                , "}"
                , ""
                , "/* Generic tagged union type for custom types */"
                , "typedef struct { int tag; int data; } elm_union_t;"
                , ""
                , "/* Serial port output (COM1) */"
                , "static inline void outb(unsigned short port, unsigned char val) {"
                , "    __asm__ volatile (\"outb %0, %1\" : : \"a\"(val), \"Nd\"(port));"
                , "}"
                , "static inline unsigned char inb(unsigned short port) {"
                , "    unsigned char ret;"
                , "    __asm__ volatile (\"inb %1, %0\" : \"=a\"(ret) : \"Nd\"(port));"
                , "    return ret;"
                , "}"
                , "#define COM1 0x3F8"
                , ""
                , "static void serial_init(void) {"
                , "    outb(COM1 + 1, 0x00);"
                , "    outb(COM1 + 3, 0x80);"
                , "    outb(COM1 + 0, 0x03);"
                , "    outb(COM1 + 1, 0x00);"
                , "    outb(COM1 + 3, 0x03);"
                , "    outb(COM1 + 2, 0xC7);"
                , "    outb(COM1 + 4, 0x0B);"
                , "}"
                , ""
                , "static void serial_putc(char c) {"
                , "    while ((inb(COM1 + 5) & 0x20) == 0);"
                , "    outb(COM1, c);"
                , "}"
                , ""
                , "static void serial_print(const char *s) {"
                , "    while (*s) {"
                , "        if (*s == '\\n') serial_putc('\\r');"
                , "        serial_putc(*s++);"
                , "    }"
                , "}"
                , ""
                , "/* Integer to string conversion */"
                , "static void int_to_str(int n, char *buf) {"
                , "    char tmp[32];"
                , "    int i = 0, j = 0;"
                , "    int neg = 0;"
                , "    if (n < 0) { neg = 1; n = -n; }"
                , "    if (n == 0) { buf[0] = '0'; buf[1] = 0; return; }"
                , "    while (n > 0) { tmp[i++] = '0' + (n % 10); n /= 10; }"
                , "    if (neg) buf[j++] = '-';"
                , "    while (i > 0) buf[j++] = tmp[--i];"
                , "    buf[j] = 0;"
                , "}"
                , ""
                , "/* Include framebuffer support */"
                , "#include \"framebuffer.h\""
                , ""
                , constructorDefinesCode ++ liftedFunctionsCode ++ userFunctionsCode ++ "/* Elm main value */"
                , codeGen.elmMainFunc
                , ""
                , "/* RTEMS Init task */"
                , "void Init(rtems_task_argument arg) {"
                , "    (void)arg;"
                , ""
                , "    /* Initialize serial for debug output */"
                , "    serial_init();"
                , "    serial_print(\"tcelm: Initializing...\\r\\n\");"
                , ""
                , "    /* Initialize framebuffer */"
                , "    if (fb_init() == 0) {"
                , "        serial_print(\"tcelm: Framebuffer initialized\\r\\n\");"
                , "        fb_clear();"
                , ""
                , "        /* Draw border */"
                , "        fb_setfg(FB_COLOR_CYAN);"
                , "        fb_rect(10, 10, fb_width - 20, fb_height - 20, FB_COLOR_CYAN);"
                , "        fb_rect(12, 12, fb_width - 24, fb_height - 24, FB_COLOR_CYAN);"
                , ""
                , "        /* Title bar */"
                , "        fb_setfg(FB_COLOR_YELLOW);"
                , "        fb_gotoxy(3, 2);"
                , "        fb_print(\"tcelm - Elm to RTEMS Compiler\");"
                , ""
                , "        /* Module info */"
                , "        fb_setfg(FB_COLOR_GRAY);"
                , "        fb_gotoxy(3, 4);"
                , "        fb_print(\"Module: " ++ moduleName ++ "\");"
                , ""
                , "        /* Separator */"
                , "        fb_hline(20, 90, fb_width - 40, FB_COLOR_DARKGRAY);"
                , ""
                , "        /* Output header */"
                , "        fb_setfg(FB_COLOR_WHITE);"
                , "        fb_gotoxy(3, 7);"
                , "        fb_print(\"Output:\");"
                , ""
                , "        /* Run main and display result */"
                , "        " ++ codeGen.resultDecl
                , ""
                , "        fb_setfg(FB_COLOR_GREEN);"
                , "        fb_gotoxy(5, 9);"
                , "        " ++ codeGen.fbPrint
                , ""
                , "        /* Footer */"
                , "        fb_setfg(FB_COLOR_DARKGRAY);"
                , "        fb_gotoxy(3, 60);"
                , "        fb_print(\"SXGA 1280x1024 | Generated by tcelm\");"
                , ""
                , "        serial_print(\"tcelm: Output displayed on framebuffer\\r\\n\");"
                , "    } else {"
                , "        serial_print(\"tcelm: Framebuffer init failed, serial only\\r\\n\");"
                , "    }"
                , ""
                , "    /* Also output to serial */"
                , "    {"
                , "        " ++ codeGen.resultDecl
                , "        serial_print(\"Result: \");"
                , "        " ++ codeGen.resultPrint
                , "        serial_print(\"\\r\\n\");"
                , "    }"
                , ""
                , "    /* Halt instead of delete for testing */"
                , "    serial_print(\"tcelm: Done. Halting.\\r\\n\");"
                , "    while(1) { __asm__ volatile(\"hlt\"); }"
                , "}"
                ]
    in
    standaloneCode


{-| Generate standalone C code for native target (regular computer)
-}
generateNativeCode : Src.Module -> String
generateNativeCode ast =
    let
        moduleName =
            ast.name
                |> Maybe.map (\(Src.At _ n) -> n)
                |> Maybe.withDefault "Main"

        -- Separate helper functions from main
        ( helpers, mainFunc ) =
            List.partition
                (\(Src.At _ v) ->
                    let
                        (Src.At _ name) = v.name
                    in
                    name /= "main"
                )
                ast.values

        -- Generate forward declarations for helpers
        forwardDecls =
            List.map generateStandaloneForwardDecl helpers

        -- Generate helper function implementations
        helperImpls =
            List.map generateStandaloneFunction helpers

        -- Generate main
        mainValue =
            extractMain ast

        ( returnType, returnExpr, printFormat ) =
            case mainValue of
                MainString s ->
                    ( "const char *", "\"" ++ escapeC s ++ "\"", "%s" )

                MainInt n ->
                    ( "int", String.fromInt n, "%d" )

                MainExpr cType cExpr ->
                    ( cType, cExpr, if cType == "int" then "%d" else "%s" )

        header =
            [ "/*"
            , " * Generated by tcelm from " ++ moduleName
            , " * Standalone native version"
            , " */"
            , ""
            , "#include <stdio.h>"
            , ""
            ]

        mainImpl =
            [ "static " ++ returnType ++ " elm_main(void) {"
            , "    return " ++ returnExpr ++ ";"
            , "}"
            , ""
            , "int main(void) {"
            , "    printf(\"" ++ printFormat ++ "\\n\", elm_main());"
            , "    return 0;"
            , "}"
            ]
    in
    String.join "\n"
        (header
            ++ [ "/* Forward declarations */" ]
            ++ forwardDecls
            ++ [ "" ]
            ++ [ "/* Helper functions */" ]
            ++ helperImpls
            ++ [ "" ]
            ++ [ "/* Main */" ]
            ++ mainImpl
        )


{-| Generate forward declaration for a standalone function
-}
generateStandaloneForwardDecl : Src.Located Src.Value -> String
generateStandaloneForwardDecl (Src.At _ value) =
    let
        (Src.At _ name) = value.name
        arity = List.length value.args
        params =
            if arity == 0 then
                "void"
            else
                String.join ", " (List.repeat arity "int")
    in
    "static int elm_" ++ name ++ "(" ++ params ++ ");"


{-| Generate a standalone function implementation
-}
generateStandaloneFunction : Src.Located Src.Value -> String
generateStandaloneFunction (Src.At _ value) =
    let
        (Src.At _ name) = value.name
        args = value.args

        -- Generate parameter names from patterns
        paramNames =
            List.indexedMap
                (\i pat ->
                    case pat of
                        Src.At _ (Src.PVar pname) ->
                            pname
                        _ ->
                            "arg" ++ String.fromInt i
                )
                args

        params =
            if List.isEmpty paramNames then
                "void"
            else
                String.join ", " (List.map (\p -> "int elm_" ++ p) paramNames)

        bodyCode =
            generateStandaloneExpr value.body
    in
    String.join "\n"
        [ "static int elm_" ++ name ++ "(" ++ params ++ ") {"
        , "    return " ++ bodyCode ++ ";"
        , "}"
        , ""
        ]


{-| Type representing the main value's type and code
-}
type MainValue
    = MainString String
    | MainInt Int
    | MainExpr String String  -- (C type, C expression)


{-| Extract the main value from a module
-}
extractMain : Src.Module -> MainValue
extractMain ast =
    ast.values
        |> List.filterMap extractMainValue
        |> List.head
        |> Maybe.withDefault (MainString "Hello from tcelm!")


{-| Extract main value from a located value
-}
extractMainValue : Src.Located Src.Value -> Maybe MainValue
extractMainValue (Src.At _ value) =
    let
        (Src.At _ name) =
            value.name
    in
    if name == "main" then
        Just (exprToMainValue value.body)

    else
        Nothing


{-| Convert an expression to a MainValue
-}
exprToMainValue : Src.Expr -> MainValue
exprToMainValue (Src.At region expr) =
    case expr of
        Src.Str s ->
            MainString s

        Src.Int n ->
            MainInt n

        Src.Negate innerExpr ->
            case exprToMainValue innerExpr of
                MainInt n ->
                    MainInt -n

                other ->
                    other

        Src.Binops pairs finalExpr ->
            -- Generate C code for binary operations
            let
                cCode = generateStandaloneBinops pairs finalExpr

                -- Check if this is string concatenation
                isStringConcat =
                    List.all (\( _, Src.At _ op ) -> op == "++") pairs

                cType =
                    if isStringConcat then
                        "const char *"

                    else
                        "int"
            in
            MainExpr cType cCode

        Src.If branches elseExpr ->
            -- Infer type from first branch's then-expression
            let
                cCode = generateStandaloneIf branches elseExpr
                inferredType =
                    case branches of
                        ( _, thenExpr ) :: _ ->
                            case exprToMainValue thenExpr of
                                MainString _ -> "const char *"
                                MainInt _ -> "int"
                                MainExpr t _ -> t
                        [] ->
                            case exprToMainValue elseExpr of
                                MainString _ -> "const char *"
                                MainInt _ -> "int"
                                MainExpr t _ -> t
            in
            MainExpr inferredType cCode

        Src.Let defs body ->
            -- Infer type from body expression
            let
                cCode = generateStandaloneLet defs body
                inferredType =
                    case exprToMainValue body of
                        MainString _ -> "const char *"
                        MainInt _ -> "int"
                        MainExpr t _ -> t
            in
            MainExpr inferredType cCode

        _ ->
            -- Fallback: try to generate as expression
            MainExpr "int" (generateStandaloneExpr (Src.At region expr))


{-| Generate standalone C code for binary operations (no runtime needed)
    Outputs as flat expression relying on C operator precedence (matches Elm for arithmetic)
-}
generateStandaloneBinops : List ( Src.Expr, Src.Located String ) -> Src.Expr -> String
generateStandaloneBinops pairs finalExpr =
    let
        -- Check if this is a forward pipe chain (all ops are |>)
        isForwardPipe =
            List.all (\( _, Src.At _ op ) -> op == "|>") pairs

        -- Check if this is a backward pipe chain (all ops are <|)
        isBackwardPipe =
            List.all (\( _, Src.At _ op ) -> op == "<|") pairs

        -- Check if this is string concatenation (all ops are ++)
        isStringConcat =
            List.all (\( _, Src.At _ op ) -> op == "++") pairs

        -- Try to extract string literal
        extractStringLiteral (Src.At _ e) =
            case e of
                Src.Str s ->
                    Just s

                _ ->
                    Nothing

        -- For string concatenation, try compile-time concat
        allExprs =
            List.map Tuple.first pairs ++ [ finalExpr ]

        allStrings =
            List.filterMap extractStringLiteral allExprs
    in
    if isForwardPipe then
        -- Pipe operator: a |> f |> g becomes g(f(a))
        -- pairs is [(a, |>), (f, |>)], finalExpr is g
        -- Result: finalExpr(pairs[n-1](pairs[n-2](...pairs[0])))
        let
            firstArg =
                case pairs of
                    ( expr, _ ) :: _ ->
                        generateStandaloneExpr expr

                    [] ->
                        generateStandaloneExpr finalExpr

            -- Get expressions for the "functions" (can be functions or accessors)
            functionExprs =
                (List.drop 1 pairs |> List.map Tuple.first)
                    ++ [ finalExpr ]

            -- Build nested calls, handling accessors and lambdas specially
            buildPipe : String -> List Src.Expr -> String
            buildPipe arg exprs =
                case exprs of
                    [] ->
                        arg

                    (Src.At _ (Src.Accessor fieldName)) :: rest ->
                        -- Accessor: arg.field
                        buildPipe (arg ++ "." ++ fieldName) rest

                    (Src.At pos (Src.Lambda patterns lambdaBody)) :: rest ->
                        -- Lambda: create a Call expression and generate it
                        let
                            -- Create a temporary expression representing the piped argument
                            -- We need to wrap the current arg string in a way that generateInlinedLambda can use
                            -- The simplest approach: bind to a variable and use that variable
                            varName =
                                "__pipe_" ++ String.fromInt (List.length exprs)

                            binding =
                                "int elm_" ++ varName ++ " = " ++ arg ++ ";"

                            -- Create a Var expression for the bound variable
                            argExpr =
                                Src.At pos (Src.Var Src.LowVar varName)

                            -- Generate the inlined lambda call
                            inlined =
                                generateInlinedLambda patterns [ argExpr ] lambdaBody

                            result =
                                "({ " ++ binding ++ " " ++ inlined ++ "; })"
                        in
                        buildPipe result rest

                    fnExpr :: rest ->
                        -- Regular function call
                        buildPipe (generateStandaloneExpr fnExpr ++ "(" ++ arg ++ ")") rest
        in
        buildPipe firstArg functionExprs

    else if isBackwardPipe then
        -- Backward pipe operator: f <| g <| a becomes f(g(a))
        -- pairs is [(f, <|), (g, <|)], finalExpr is a
        -- Result: pairs[0](pairs[1](...finalExpr))
        let
            arg =
                generateStandaloneExpr finalExpr

            functionExprs =
                pairs
                    |> List.map Tuple.first
                    |> List.reverse

            -- Build nested calls, handling accessors specially
            buildBackPipe : String -> List Src.Expr -> String
            buildBackPipe innerArg exprs =
                case exprs of
                    [] ->
                        innerArg

                    (Src.At _ (Src.Accessor fieldName)) :: rest ->
                        -- Accessor: innerArg.field
                        buildBackPipe (innerArg ++ "." ++ fieldName) rest

                    fnExpr :: rest ->
                        -- Regular function call
                        buildBackPipe (generateStandaloneExpr fnExpr ++ "(" ++ innerArg ++ ")") rest
        in
        buildBackPipe arg functionExprs

    else if isStringConcat && List.length allStrings == List.length allExprs then
        -- All operands are string literals - concatenate at compile time
        "\"" ++ escapeC (String.concat allStrings) ++ "\""

    else
        -- Normal binary operation
        let
            -- Build list of all terms and operators
            buildTerms ps =
                case ps of
                    [] ->
                        []

                    ( expr, Src.At _ op ) :: rest ->
                        ( generateStandaloneExpr expr, op ) :: buildTerms rest

            terms = buildTerms pairs
            finalTerm = generateStandaloneExpr finalExpr

            -- Convert Elm operator to C operator
            elmOpToC op =
                case op of
                    "//" -> "/"
                    "/=" -> "!="
                    "++" -> "/* string concat not supported at runtime */ +"
                    _ -> op

            -- Build the expression string
            buildExpr ts =
                case ts of
                    [] ->
                        finalTerm

                    ( term, op ) :: rest ->
                        term ++ " " ++ elmOpToC op ++ " " ++ buildExpr rest
        in
        "(" ++ buildExpr terms ++ ")"


{-| Generate standalone C code for a single expression (no runtime)
-}
generateStandaloneExpr : Src.Expr -> String
generateStandaloneExpr (Src.At _ expr) =
    case expr of
        Src.Int n ->
            String.fromInt n

        Src.Float f ->
            String.fromFloat f

        Src.Str s ->
            "\"" ++ escapeC s ++ "\""

        Src.Chr c ->
            -- Char as integer (ASCII value)
            "'" ++ escapeC c ++ "'"

        Src.Negate inner ->
            "(-" ++ generateStandaloneExpr inner ++ ")"

        Src.Binops pairs final ->
            generateStandaloneBinops pairs final

        Src.If branches elseExpr ->
            generateStandaloneIf branches elseExpr

        Src.Let defs body ->
            generateStandaloneLet defs body

        Src.Call fn args ->
            case fn of
                Src.At _ (Src.Lambda patterns lambdaBody) ->
                    -- Inline immediately-called lambda
                    generateInlinedLambda patterns args lambdaBody

                Src.At _ (Src.Accessor fieldName) ->
                    -- Accessor function applied to record: .field record -> record.field
                    case args of
                        [ recordArg ] ->
                            generateStandaloneExpr recordArg ++ "." ++ fieldName

                        _ ->
                            "/* accessor with wrong arity */ 0"

                Src.At _ (Src.Binops pairs finalExpr) ->
                    -- Check if this is function composition applied to an argument
                    -- (f >> g) x becomes g(f(x))
                    -- (f << g) x becomes f(g(x))
                    let
                        isForwardCompose =
                            List.all (\( _, Src.At _ op ) -> op == ">>") pairs

                        isBackwardCompose =
                            List.all (\( _, Src.At _ op ) -> op == "<<") pairs
                    in
                    if isForwardCompose && List.length args == 1 then
                        -- Forward composition: (f >> g >> h) x = h(g(f(x)))
                        let
                            argStr =
                                generateStandaloneExpr (List.head args |> Maybe.withDefault finalExpr)

                            functions =
                                (pairs |> List.map (\( e, _ ) -> generateStandaloneExpr e))
                                    ++ [ generateStandaloneExpr finalExpr ]

                            buildCompose innerArg fns =
                                case fns of
                                    [] ->
                                        innerArg

                                    f :: rest ->
                                        buildCompose (f ++ "(" ++ innerArg ++ ")") rest
                        in
                        buildCompose argStr functions

                    else if isBackwardCompose && List.length args == 1 then
                        -- Backward composition: (f << g << h) x = f(g(h(x)))
                        let
                            argStr =
                                generateStandaloneExpr (List.head args |> Maybe.withDefault finalExpr)

                            functions =
                                (pairs |> List.map (\( e, _ ) -> generateStandaloneExpr e))
                                    ++ [ generateStandaloneExpr finalExpr ]
                                    |> List.reverse

                            buildCompose innerArg fns =
                                case fns of
                                    [] ->
                                        innerArg

                                    f :: rest ->
                                        buildCompose (f ++ "(" ++ innerArg ++ ")") rest
                        in
                        buildCompose argStr functions

                    else
                        -- Not a composition or multiple args - fall back
                        generateStandaloneCall fn args

                _ ->
                    generateStandaloneCall fn args

        Src.Var varType name ->
            case ( varType, name ) of
                ( Src.CapVar, "True" ) ->
                    "1"

                ( Src.CapVar, "False" ) ->
                    "0"

                ( Src.CapVar, _ ) ->
                    -- Constructor - call as function (nullary constructor)
                    "elm_" ++ name ++ "()"

                _ ->
                    "elm_" ++ name

        Src.Case scrutinee branches ->
            generateStandaloneCase scrutinee branches

        Src.Tuple first second rest ->
            -- Generate tuple as compound struct literal
            let
                elements =
                    first :: second :: rest

                numElements =
                    List.length elements

                structFields =
                    List.indexedMap (\i _ -> "int _" ++ String.fromInt i) elements
                        |> String.join "; "

                values =
                    List.map generateStandaloneExpr elements
                        |> String.join ", "
            in
            "((struct { " ++ structFields ++ "; }){" ++ values ++ "})"

        Src.Record fields ->
            -- Generate record as compound struct literal with named fields
            let
                fieldDefs =
                    fields
                        |> List.map
                            (\( Src.At _ fieldName, _ ) ->
                                "int " ++ fieldName
                            )
                        |> String.join "; "

                fieldValues =
                    fields
                        |> List.map
                            (\( Src.At _ fieldName, fieldValue ) ->
                                "." ++ fieldName ++ " = " ++ generateStandaloneExpr fieldValue
                            )
                        |> String.join ", "
            in
            "((struct { " ++ fieldDefs ++ "; }){" ++ fieldValues ++ "})"

        Src.Access recordExpr (Src.At _ fieldName) ->
            -- Generate field access
            generateStandaloneExpr recordExpr ++ "." ++ fieldName

        Src.Accessor fieldName ->
            -- Accessor function .field - this shouldn't appear standalone
            -- but if it does, generate a placeholder
            "/* accessor ." ++ fieldName ++ " */"

        Src.Update (Src.At _ recordName) updates ->
            -- Record update: { record | field = value, ... }
            -- In C, we need to copy the original and update specific fields
            -- Generate: ({ struct { ... } __tmp = record; __tmp.field = value; __tmp; })
            let
                -- Generate the update assignments
                updateAssignments =
                    updates
                        |> List.map
                            (\( Src.At _ fieldName, valueExpr ) ->
                                "__update_tmp." ++ fieldName ++ " = " ++ generateStandaloneExpr valueExpr ++ ";"
                            )
                        |> String.join " "
            in
            "({ typeof(elm_" ++ recordName ++ ") __update_tmp = elm_" ++ recordName ++ "; " ++ updateAssignments ++ " __update_tmp; })"

        _ ->
            "/* unsupported expr */ 0"


{-| Generate standalone C code for function calls
-}
generateStandaloneCall : Src.Expr -> List Src.Expr -> String
generateStandaloneCall fn args =
    -- Handle built-in functions
    case fn of
        Src.At _ (Src.Var _ "modBy") ->
            -- modBy divisor dividend = dividend % divisor (positive result)
            case args of
                [ divisor, dividend ] ->
                    "((" ++ generateStandaloneExpr dividend ++ " % " ++ generateStandaloneExpr divisor ++ " + " ++ generateStandaloneExpr divisor ++ ") % " ++ generateStandaloneExpr divisor ++ ")"

                [ divisor ] ->
                    -- Partial application - generate a comment placeholder
                    "/* partial modBy */ 0"

                _ ->
                    "/* modBy wrong arity */ 0"

        Src.At _ (Src.Var _ "remainderBy") ->
            -- remainderBy divisor dividend = dividend % divisor (can be negative)
            case args of
                [ divisor, dividend ] ->
                    "(" ++ generateStandaloneExpr dividend ++ " % " ++ generateStandaloneExpr divisor ++ ")"

                _ ->
                    "/* remainderBy wrong arity */ 0"

        Src.At _ (Src.Var _ "abs") ->
            -- abs value = absolute value
            case args of
                [ value ] ->
                    "((" ++ generateStandaloneExpr value ++ " < 0) ? -(" ++ generateStandaloneExpr value ++ ") : (" ++ generateStandaloneExpr value ++ "))"

                _ ->
                    "/* abs wrong arity */ 0"

        Src.At _ (Src.Var _ "negate") ->
            -- negate value = -value
            case args of
                [ value ] ->
                    "(-" ++ generateStandaloneExpr value ++ ")"

                _ ->
                    "/* negate wrong arity */ 0"

        Src.At _ (Src.Var _ "min") ->
            -- min a b = smaller of a and b
            case args of
                [ a, b ] ->
                    "((" ++ generateStandaloneExpr a ++ " < " ++ generateStandaloneExpr b ++ ") ? (" ++ generateStandaloneExpr a ++ ") : (" ++ generateStandaloneExpr b ++ "))"

                _ ->
                    "/* min wrong arity */ 0"

        Src.At _ (Src.Var _ "max") ->
            -- max a b = larger of a and b
            case args of
                [ a, b ] ->
                    "((" ++ generateStandaloneExpr a ++ " > " ++ generateStandaloneExpr b ++ ") ? (" ++ generateStandaloneExpr a ++ ") : (" ++ generateStandaloneExpr b ++ "))"

                _ ->
                    "/* max wrong arity */ 0"

        _ ->
            -- Regular function call
            let
                fnName =
                    case fn of
                        Src.At _ (Src.Var _ name) ->
                            "elm_" ++ name

                        _ ->
                            "/* complex fn */" ++ generateStandaloneExpr fn

                argStrs =
                    List.map generateStandaloneExpr args
            in
            fnName ++ "(" ++ String.join ", " argStrs ++ ")"


{-| Generate inlined lambda - substitute args for parameters and evaluate body
-}
generateInlinedLambda : List Src.Pattern -> List Src.Expr -> Src.Expr -> String
generateInlinedLambda patterns args body =
    let
        -- Create bindings for each pattern/arg pair
        bindings =
            List.map2
                (\(Src.At _ pat) arg ->
                    case pat of
                        Src.PVar varName ->
                            "int elm_" ++ varName ++ " = " ++ generateStandaloneExpr arg ++ ";"

                        _ ->
                            "/* unsupported pattern in lambda */"
                )
                patterns
                args

        bodyExpr =
            generateStandaloneExpr body
    in
    if List.isEmpty bindings then
        bodyExpr

    else
        "({\n        " ++ String.join "\n        " bindings ++ "\n        " ++ bodyExpr ++ ";\n    })"


{-| Generate a user-defined function in C
-}
generateUserFunction : String -> List Src.Pattern -> Src.Expr -> String
generateUserFunction name args body =
    let
        -- Generate function body first so we can check parameter usage
        bodyExpr =
            generateStandaloneExpr body

        -- Generate parameter list with type inference based on usage
        params =
            args
                |> List.map
                    (\(Src.At _ pat) ->
                        case pat of
                            Src.PVar varName ->
                                -- Check if this parameter is used as union type:
                                -- 1. Direct .tag access
                                -- 2. Assigned to elm_union_t variable
                                -- 3. Used with constructor call pattern
                                let
                                    isUnionType =
                                        String.contains ("elm_" ++ varName ++ ".tag") bodyExpr
                                            || String.contains ("elm_union_t elm_case_scrutinee = elm_" ++ varName) bodyExpr
                                in
                                if isUnionType then
                                    "elm_union_t elm_" ++ varName

                                else
                                    "int elm_" ++ varName

                            _ ->
                                "int /* unsupported pattern */"
                    )
                |> String.join ", "
    in
    "static int elm_" ++ name ++ "(" ++ params ++ ") {\n    return " ++ bodyExpr ++ ";\n}"


{-| A local function to be lifted to module level
-}
type alias LiftedFunc =
    { prefix : String
    , name : String
    , args : List Src.Pattern
    , body : Src.Expr
    }


{-| Collect local function definitions from an expression for lifting to module level
-}
collectLocalFunctions : String -> Src.Expr -> List LiftedFunc
collectLocalFunctions prefix (Src.At _ expr) =
    case expr of
        Src.Let defs body ->
            let
                -- Collect functions defined in this let
                localFuncs =
                    defs
                        |> List.filterMap
                            (\(Src.At _ def) ->
                                case def of
                                    Src.Define (Src.At _ name) args defBody _ ->
                                        if not (List.isEmpty args) then
                                            Just { prefix = prefix, name = name, args = args, body = defBody }

                                        else
                                            Nothing

                                    Src.Destruct _ _ ->
                                        Nothing
                            )

                -- Recursively collect from def bodies and let body
                nestedFuncs =
                    defs
                        |> List.concatMap
                            (\(Src.At _ def) ->
                                case def of
                                    Src.Define (Src.At _ name) _ defBody _ ->
                                        collectLocalFunctions (prefix ++ "_" ++ name) defBody

                                    Src.Destruct _ _ ->
                                        []
                            )

                bodyFuncs =
                    collectLocalFunctions prefix body
            in
            localFuncs ++ nestedFuncs ++ bodyFuncs

        Src.If branches elseExpr ->
            let
                branchFuncs =
                    branches
                        |> List.concatMap
                            (\( cond, thenExpr ) ->
                                collectLocalFunctions prefix cond
                                    ++ collectLocalFunctions prefix thenExpr
                            )
            in
            branchFuncs ++ collectLocalFunctions prefix elseExpr

        Src.Case scrutinee branches ->
            let
                scrutineeFuncs =
                    collectLocalFunctions prefix scrutinee

                branchFuncs =
                    branches
                        |> List.concatMap (\( _, branchExpr ) -> collectLocalFunctions prefix branchExpr)
            in
            scrutineeFuncs ++ branchFuncs

        Src.Call fn args ->
            collectLocalFunctions prefix fn
                ++ List.concatMap (collectLocalFunctions prefix) args

        Src.Binops pairs final ->
            let
                pairFuncs =
                    pairs
                        |> List.concatMap (\( e, _ ) -> collectLocalFunctions prefix e)
            in
            pairFuncs ++ collectLocalFunctions prefix final

        Src.Negate inner ->
            collectLocalFunctions prefix inner

        _ ->
            []


{-| Generate a lifted local function as a module-level function
-}
generateLiftedFunction : String -> String -> List Src.Pattern -> Src.Expr -> String
generateLiftedFunction prefix funcName args body =
    let
        liftedName =
            prefix ++ "_" ++ funcName

        params =
            args
                |> List.map
                    (\(Src.At _ pat) ->
                        case pat of
                            Src.PVar varName ->
                                "int elm_" ++ varName

                            _ ->
                                "int /* unsupported pattern */"
                    )
                |> String.join ", "

        bodyExpr =
            generateStandaloneExpr body
    in
    "static int elm_" ++ liftedName ++ "(" ++ params ++ ") {\n    return " ++ bodyExpr ++ ";\n}"


{-| Infer C type and initializer for an expression
    Returns (type, initializer) pair
-}
inferCTypeAndInit : Src.Expr -> ( String, String )
inferCTypeAndInit (Src.At _ expr) =
    case expr of
        Src.Record fields ->
            let
                fieldDefs =
                    fields
                        |> List.map (\( Src.At _ fieldName, _ ) -> "int " ++ fieldName)
                        |> String.join "; "

                fieldValues =
                    fields
                        |> List.map
                            (\( Src.At _ fieldName, fieldValue ) ->
                                "." ++ fieldName ++ " = " ++ generateStandaloneExpr fieldValue
                            )
                        |> String.join ", "
            in
            ( "struct { " ++ fieldDefs ++ "; }", "{" ++ fieldValues ++ "}" )

        Src.Tuple first second rest ->
            let
                elements =
                    first :: second :: rest

                structFields =
                    List.indexedMap (\i _ -> "int _" ++ String.fromInt i) elements
                        |> String.join "; "

                values =
                    List.map generateStandaloneExpr elements
                        |> String.join ", "
            in
            ( "struct { " ++ structFields ++ "; }", "{" ++ values ++ "}" )

        Src.Update (Src.At _ recordName) updates ->
            -- Record update: type is same as original, use typeof
            let
                updateAssignments =
                    updates
                        |> List.map
                            (\( Src.At _ fieldName, valueExpr ) ->
                                "__update_tmp." ++ fieldName ++ " = " ++ generateStandaloneExpr valueExpr ++ ";"
                            )
                        |> String.join " "
            in
            ( "typeof(elm_" ++ recordName ++ ")"
            , "({ typeof(elm_" ++ recordName ++ ") __update_tmp = elm_" ++ recordName ++ "; " ++ updateAssignments ++ " __update_tmp; })"
            )

        _ ->
            ( "int", generateStandaloneExpr (Src.At { start = { row = 0, col = 0 }, end = { row = 0, col = 0 } } expr) )


{-| Generate standalone C code for let bindings using GCC compound statements
-}
generateStandaloneLet : List (Src.Located Src.Def) -> Src.Expr -> String
generateStandaloneLet defs body =
    let
        generateDef (Src.At _ def) =
            case def of
                Src.Define (Src.At _ name) args defBody _ ->
                    if List.isEmpty args then
                        -- Simple binding: let x = expr
                        -- Detect type from expression for records/tuples
                        let
                            ( varType, varInit ) =
                                inferCTypeAndInit defBody
                        in
                        varType ++ " elm_" ++ name ++ " = " ++ varInit ++ ";"

                    else
                        -- Local function - create alias to lifted function
                        -- The actual function is lifted to module level as elm_main_<name>
                        -- We create a function pointer alias so local calls work
                        let
                            numArgs =
                                List.length args

                            argTypes =
                                List.repeat numArgs "int" |> String.join ", "
                        in
                        "int (*elm_" ++ name ++ ")(" ++ argTypes ++ ") = elm_main_" ++ name ++ ";"

                Src.Destruct _ _ ->
                    "/* pattern destructuring not supported */"

        defStrs =
            List.map generateDef defs

        bodyStr =
            generateStandaloneExpr body
    in
    "({\n        " ++ String.join "\n        " defStrs ++ "\n        " ++ bodyStr ++ ";\n    })"


{-| Generate standalone C code for if/else expressions using ternary operator
-}
generateStandaloneIf : List ( Src.Expr, Src.Expr ) -> Src.Expr -> String
generateStandaloneIf branches elseExpr =
    case branches of
        [] ->
            generateStandaloneExpr elseExpr

        ( condition, thenExpr ) :: rest ->
            "("
                ++ generateStandaloneExpr condition
                ++ " ? "
                ++ generateStandaloneExpr thenExpr
                ++ " : "
                ++ generateStandaloneIf rest elseExpr
                ++ ")"


{-| Generate standalone C code for case expressions
-}
generateStandaloneCase : Src.Expr -> List ( Src.Pattern, Src.Expr ) -> String
generateStandaloneCase scrutinee branches =
    let
        scrutineeStr =
            generateStandaloneExpr scrutinee

        -- Check if any branch uses a variable binding pattern
        hasVarBinding =
            List.any
                (\( Src.At _ pat, _ ) ->
                    case pat of
                        Src.PVar _ ->
                            True

                        _ ->
                            False
                )
                branches

        -- Check if any branch has a constructor pattern with data (needs struct type)
        hasCtorWithData =
            List.any
                (\( Src.At _ pat, _ ) ->
                    case pat of
                        Src.PCtor _ _ ctorPatterns ->
                            not (List.isEmpty ctorPatterns)

                        _ ->
                            False
                )
                branches

        -- Check if this is a custom type case (has constructor patterns, not just True/False)
        isCustomTypeCase =
            List.any
                (\( Src.At _ pat, _ ) ->
                    case pat of
                        Src.PCtor _ name _ ->
                            name /= "True" && name /= "False"

                        _ ->
                            False
                )
                branches

        generateBranches : List ( Src.Pattern, Src.Expr ) -> String
        generateBranches bs =
            case bs of
                [] ->
                    "0 /* no match */"

                ( Src.At _ pattern, resultExpr ) :: rest ->
                    case pattern of
                        Src.PAnything ->
                            -- Wildcard always matches
                            generateStandaloneExpr resultExpr

                        Src.PVar varName ->
                            -- Variable binding - bind scrutinee to variable name
                            "({\n            int elm_"
                                ++ varName
                                ++ " = elm_case_scrutinee;\n            "
                                ++ generateStandaloneExpr resultExpr
                                ++ ";\n        })"

                        Src.PInt n ->
                            -- Integer pattern
                            "(elm_case_scrutinee == "
                                ++ String.fromInt n
                                ++ " ? "
                                ++ generateStandaloneExpr resultExpr
                                ++ " : "
                                ++ generateBranches rest
                                ++ ")"

                        Src.PStr s ->
                            -- String pattern
                            "(strcmp(elm_case_scrutinee, \""
                                ++ escapeC s
                                ++ "\") == 0 ? "
                                ++ generateStandaloneExpr resultExpr
                                ++ " : "
                                ++ generateBranches rest
                                ++ ")"

                        Src.PChr c ->
                            -- Char pattern
                            "(elm_case_scrutinee == '"
                                ++ escapeC c
                                ++ "' ? "
                                ++ generateStandaloneExpr resultExpr
                                ++ " : "
                                ++ generateBranches rest
                                ++ ")"

                        Src.PTuple first second restPats ->
                            -- Tuple pattern - destructure and bind variables
                            let
                                allPats =
                                    first :: second :: restPats

                                bindings =
                                    allPats
                                        |> List.indexedMap
                                            (\i (Src.At _ pat) ->
                                                case pat of
                                                    Src.PVar varName ->
                                                        "int elm_" ++ varName ++ " = elm_case_scrutinee._" ++ String.fromInt i ++ ";"

                                                    Src.PAnything ->
                                                        ""

                                                    _ ->
                                                        "/* unsupported tuple element pattern */"
                                            )
                                        |> List.filter (not << String.isEmpty)
                                        |> String.join "\n            "

                                bodyStr =
                                    generateStandaloneExpr resultExpr
                            in
                            "({\n            "
                                ++ bindings
                                ++ "\n            "
                                ++ bodyStr
                                ++ ";\n        })"

                        Src.PCtor _ ctorName ctorPatterns ->
                            -- Constructor pattern
                            case ctorName of
                                "True" ->
                                    "(elm_case_scrutinee ? "
                                        ++ generateStandaloneExpr resultExpr
                                        ++ " : "
                                        ++ generateBranches rest
                                        ++ ")"

                                "False" ->
                                    "(!elm_case_scrutinee ? "
                                        ++ generateStandaloneExpr resultExpr
                                        ++ " : "
                                        ++ generateBranches rest
                                        ++ ")"

                                _ ->
                                    if List.isEmpty ctorPatterns then
                                        -- Simple enum constructor - compare tag
                                        "(elm_case_scrutinee.tag == TAG_"
                                            ++ ctorName
                                            ++ " ? "
                                            ++ generateStandaloneExpr resultExpr
                                            ++ " : "
                                            ++ generateBranches rest
                                            ++ ")"

                                    else
                                        -- Constructor with data - bind variable and compare tag
                                        let
                                            bindings =
                                                ctorPatterns
                                                    |> List.indexedMap
                                                        (\_ (Src.At _ pat) ->
                                                            case pat of
                                                                Src.PVar varName ->
                                                                    "int elm_" ++ varName ++ " = elm_case_scrutinee.data;"

                                                                _ ->
                                                                    ""
                                                        )
                                                    |> List.filter (not << String.isEmpty)
                                                    |> String.join " "
                                        in
                                        "(elm_case_scrutinee.tag == TAG_"
                                            ++ ctorName
                                            ++ " ? ({ "
                                            ++ bindings
                                            ++ " "
                                            ++ generateStandaloneExpr resultExpr
                                            ++ "; }) : "
                                            ++ generateBranches rest
                                            ++ ")"

                        _ ->
                            "/* unsupported pattern */ " ++ generateBranches rest
    in
    if hasVarBinding || hasCtorWithData then
        -- Use compound statement to bind scrutinee to a variable
        let
            scrutineeType =
                if isCustomTypeCase then
                    "elm_union_t"

                else
                    "int"
        in
        "({\n        "
            ++ scrutineeType
            ++ " elm_case_scrutinee = "
            ++ scrutineeStr
            ++ ";\n        "
            ++ generateBranches branches
            ++ ";\n    })"

    else
        -- Simple case - use direct comparison
        let
            -- For simple cases, we can inline the scrutinee
            inlineScrutinee =
                scrutineeStr

            generateSimpleBranches : List ( Src.Pattern, Src.Expr ) -> String
            generateSimpleBranches bs =
                case bs of
                    [] ->
                        "0 /* no match */"

                    ( Src.At _ pattern, resultExpr ) :: rest ->
                        case pattern of
                            Src.PAnything ->
                                generateStandaloneExpr resultExpr

                            Src.PInt n ->
                                "("
                                    ++ inlineScrutinee
                                    ++ " == "
                                    ++ String.fromInt n
                                    ++ " ? "
                                    ++ generateStandaloneExpr resultExpr
                                    ++ " : "
                                    ++ generateSimpleBranches rest
                                    ++ ")"

                            Src.PStr s ->
                                "(strcmp("
                                    ++ inlineScrutinee
                                    ++ ", \""
                                    ++ escapeC s
                                    ++ "\") == 0 ? "
                                    ++ generateStandaloneExpr resultExpr
                                    ++ " : "
                                    ++ generateSimpleBranches rest
                                    ++ ")"

                            Src.PChr c ->
                                "("
                                    ++ inlineScrutinee
                                    ++ " == '"
                                    ++ escapeC c
                                    ++ "' ? "
                                    ++ generateStandaloneExpr resultExpr
                                    ++ " : "
                                    ++ generateSimpleBranches rest
                                    ++ ")"

                            Src.PCtor _ ctorName ctorPatterns ->
                                case ctorName of
                                    "True" ->
                                        "("
                                            ++ inlineScrutinee
                                            ++ " ? "
                                            ++ generateStandaloneExpr resultExpr
                                            ++ " : "
                                            ++ generateSimpleBranches rest
                                            ++ ")"

                                    "False" ->
                                        "(!"
                                            ++ inlineScrutinee
                                            ++ " ? "
                                            ++ generateStandaloneExpr resultExpr
                                            ++ " : "
                                            ++ generateSimpleBranches rest
                                            ++ ")"

                                    _ ->
                                        if List.isEmpty ctorPatterns then
                                            -- Simple enum constructor - compare tag
                                            "("
                                                ++ inlineScrutinee
                                                ++ ".tag == TAG_"
                                                ++ ctorName
                                                ++ " ? "
                                                ++ generateStandaloneExpr resultExpr
                                                ++ " : "
                                                ++ generateSimpleBranches rest
                                                ++ ")"

                                        else
                                            -- Constructor with data - bind variable and compare tag
                                            let
                                                bindings =
                                                    ctorPatterns
                                                        |> List.indexedMap
                                                            (\_ (Src.At _ pat) ->
                                                                case pat of
                                                                    Src.PVar varName ->
                                                                        "int elm_" ++ varName ++ " = " ++ inlineScrutinee ++ ".data;"

                                                                    _ ->
                                                                        ""
                                                            )
                                                        |> List.filter (not << String.isEmpty)
                                                        |> String.join " "
                                            in
                                            "("
                                                ++ inlineScrutinee
                                                ++ ".tag == TAG_"
                                                ++ ctorName
                                                ++ " ? ({ "
                                                ++ bindings
                                                ++ " "
                                                ++ generateStandaloneExpr resultExpr
                                                ++ "; }) : "
                                                ++ generateSimpleBranches rest
                                                ++ ")"

                            Src.PTuple first second restPats ->
                                -- Tuple pattern - destructure and bind variables
                                let
                                    allPats =
                                        first :: second :: restPats

                                    bindings =
                                        allPats
                                            |> List.indexedMap
                                                (\i (Src.At _ pat) ->
                                                    case pat of
                                                        Src.PVar varName ->
                                                            "int elm_" ++ varName ++ " = " ++ inlineScrutinee ++ "._" ++ String.fromInt i ++ ";"

                                                        Src.PAnything ->
                                                            ""

                                                        _ ->
                                                            "/* unsupported tuple element pattern */"
                                                )
                                            |> List.filter (not << String.isEmpty)
                                            |> String.join "\n            "

                                    bodyStr =
                                        generateStandaloneExpr resultExpr
                                in
                                "({\n            "
                                    ++ bindings
                                    ++ "\n            "
                                    ++ bodyStr
                                    ++ ";\n        })"

                            _ ->
                                "/* unsupported pattern */ " ++ generateSimpleBranches rest
        in
        generateSimpleBranches branches


{-| Extract the main string literal from a value (legacy, for compatibility)
-}
extractMainString : Src.Located Src.Value -> Maybe String
extractMainString (Src.At _ value) =
    let
        (Src.At _ name) =
            value.name
    in
    if name == "main" then
        case exprToMainValue value.body of
            MainString s -> Just s
            _ -> Nothing

    else
        Nothing


{-| Escape a string for C
-}
escapeC : String -> String
escapeC s =
    String.toList s
        |> List.map
            (\c ->
                case c of
                    '"' ->
                        "\\\""

                    '\\' ->
                        "\\\\"

                    '\n' ->
                        "\\n"

                    '\t' ->
                        "\\t"

                    '\u{000D}' ->
                        "\\r"

                    _ ->
                        String.fromChar c
            )
        |> String.concat


{- OLD CODE - keeping for reference when runtime is available
        baseCode =
            C.generateModule ast

        rtemsWrapper =
            String.join "\n"
                [ ""
                , "/* RTEMS entry point */"
                , "typedef unsigned int rtems_task_argument;"
                , "typedef unsigned int rtems_id;"
                , "#define RTEMS_SELF 0"
                , "extern void rtems_task_delete(rtems_id id);"
                , ""
                , "/* Serial port output (COM1) */"
                , "static inline void outb(unsigned short port, unsigned char val) {"
                , "    __asm__ volatile (\"outb %0, %1\" : : \"a\"(val), \"Nd\"(port));"
                , "}"
                , "static inline unsigned char inb(unsigned short port) {"
                , "    unsigned char ret;"
                , "    __asm__ volatile (\"inb %1, %0\" : \"=a\"(ret) : \"Nd\"(port));"
                , "    return ret;"
-}


formatErrors : List (Parse.Primitives.DeadEnd Reporting.Error.Syntax.Problem) -> String
formatErrors errors =
    "Parse error at: "
        ++ (errors
                |> List.head
                |> Maybe.map (\e -> "line " ++ String.fromInt e.row ++ ", col " ++ String.fromInt e.col)
                |> Maybe.withDefault "unknown location"
           )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveSource GotSource
