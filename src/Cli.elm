port module Cli exposing (main)

{-| CLI entry point for tcelm compiler.

Compiles Elm source code to C code for multiple targets:

  - RTEMS: Real-time embedded systems
  - TCC: Tiny C Compiler (portable, no GCC extensions)
  - Native: Standard desktop/server environments

## Module Organization

Code generation is split across several modules:

  - Codegen.Shared: Common types and utilities
  - Codegen.Builtins: Built-in function handlers (Basics, String, List, etc.)
  - Codegen.Pattern: Pattern matching generation
  - Codegen.Expr: Expression generation utilities
  - Codegen.Union: Custom type generation
  - Codegen.Lambda: Lambda lifting types and utilities

Target-specific code:

  - Target.TCC: TCC target (skeleton)
  - Target.RTEMS: RTEMS target (skeleton)
  - Target.Native: Native target (skeleton)
  - Target.NativeWorker: Native worker target (skeleton)

The main expression generation remains in this module for now,
using callback patterns to delegate to the Codegen modules.

-}

import AST.Source as Src
import Codegen.Builtins as Builtins exposing (isRecordValue, isStringValue, isUnionValue, makeUnionCtor, wrapUnionData)
import Codegen.Expr as Expr
import Codegen.Lambda as Lambda exposing (LiftedFunc, collectVarRefs)
import Codegen.Pattern as Pattern
import Codegen.Shared as Shared exposing (ExprCtx, MainValue(..), collectAllFunctionNames, collectUserFunctionNames, escapeC, getModuleName, getModulePrefix, isSimpleLiteral, patternVars)
import Codegen.Union as Union
import Generate.C as C
import Json.Decode as Decode
import Target.Native as Native
import Target.RTEMS as RTEMS exposing (CodegenConfig)
import Target.TCC as TCC
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
                    if target == "rtems" then
                        RTEMS.generateCode rtemsCodegenConfig ast

                    else if target == "native" then
                        Native.generateCode nativeCodegenConfig ast

                    else if target == "native-worker" then
                        C.generateNativeWorkerModule ast

                    else if target == "tcc" then
                        TCC.generateCode tccCodegenConfig ast

                    else if target == "tcc-lib" then
                        -- Generate library module with module-qualified names
                        TCC.generateLibCode tccCodegenConfig ast

                    else if target == "tcc-header" then
                        -- Generate header file with extern declarations
                        TCC.generateHeader tccCodegenConfig ast

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


{-| Configuration for RTEMS code generation.
Provides callbacks to shared code generation functions.
-}
rtemsCodegenConfig : CodegenConfig
rtemsCodegenConfig =
    { extractMain = extractMain
    , generateImportCode = generateImportCode
    , generateUserFunction = generateUserFunction
    , collectLocalFunctionsWithScope = collectLocalFunctionsWithScope
    , generateLiftedFunction = generateLiftedFunction
    }


{-| Configuration for native code generation.
Provides callbacks to shared code generation functions.
-}
nativeCodegenConfig : Native.CodegenConfig
nativeCodegenConfig =
    { extractMain = extractMain
    , generateImportCode = generateImportCode
    , generateStandaloneForwardDecl = generateStandaloneForwardDecl
    , generateStandaloneFunction = generateStandaloneFunction
    }


{-| Configuration for TCC code generation.
Provides callbacks to shared code generation functions.
-}
tccCodegenConfig : TCC.CodegenConfig
tccCodegenConfig =
    { extractMain = extractMain
    , extractMainWithCtx = extractMainWithCtx
    , generateImportCode = generateImportCode
    , generateUserFunction = generateUserFunction
    , collectLocalFunctionsWithScope = collectLocalFunctionsWithScope
    , generateLiftedFunction = generateLiftedFunction
    , generateStandaloneExpr = generateStandaloneExpr
    , generateStandaloneExprWithPrefix = \funcPrefix modulePrefix userFunctions expr ->
        generateStandaloneExprWithCtx { funcPrefix = funcPrefix, modulePrefix = modulePrefix, userFunctions = userFunctions } expr
    , filterReachableValues = filterReachableValues
    }


{-| Generate C code for RTEMS target (x86_64) with Init wrapper
    For simple programs, we generate standalone code without the full runtime.
    Outputs to both serial console and SXGA framebuffer.
-}
generateRtemsCode : Src.Module -> String
generateRtemsCode ast =
    let
        moduleName =
            getModuleName ast

        modulePrefix =
            getModulePrefix ast

        -- Process imports to generate includes
        importCode =
            generateImportCode ast.imports

        mainValue =
            extractMain ast

        -- Collect user function names for context
        userFunctionNames =
            collectUserFunctionNames ast.values

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
                            Just (generateUserFunction modulePrefix userFunctionNames name value.args value.body)

                        else
                            Nothing
                    )
                |> String.join "\n\n"

        -- Generate custom type definitions using Union module
        customTypeCode =
            ast.unions
                |> List.map Union.generateUnionDef
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

                            -- Include function parameters in scope for captured variable detection
                            funcParamNames =
                                List.concatMap patternVars value.args
                        in
                        collectLocalFunctionsWithScope name funcParamNames value.body
                    )
                |> List.map
                    (\lf ->
                        generateLiftedFunction lf.prefix lf.name lf.args lf.body lf.capturedVars
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
                    { elmMainFunc = "static double elm_main(void) {\n    return " ++ String.fromInt n ++ ";\n}"
                    , resultDecl = "int result = elm_main();\n    char result_str[32];\n    int_to_str(result, result_str);"
                    , resultPrint = "serial_print(result_str);"
                    , fbPrint = "fb_print(result_str);"
                    }

                MainExpr cType cExpr ->
                    if cType == "int" then
                        { elmMainFunc = "static double elm_main(void) {\n    return " ++ cExpr ++ ";\n}"
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
                , importCode
                , RTEMS.standalonePreamble
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
                , "/* Font 8x16 bitmap data */"
                , "#define FONT_WIDTH  8"
                , "#define FONT_HEIGHT 16"
                , "static const unsigned char font8x16_data[95][16] = {"
                , "    {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x18,0x3C,0x3C,0x3C,0x18,0x18,0x18,0x00,0x18,0x18,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x66,0x66,0x66,0x24,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x6C,0x6C,0xFE,0x6C,0x6C,0x6C,0xFE,0x6C,0x6C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x10,0x10,0x7C,0xD6,0xD0,0x7C,0x16,0xD6,0x7C,0x10,0x10,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0xC2,0xC6,0x0C,0x18,0x30,0x60,0xC6,0x86,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x38,0x6C,0x6C,0x38,0x76,0xDC,0xCC,0xCC,0xCC,0x76,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x30,0x30,0x30,0x60,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x0C,0x18,0x30,0x30,0x30,0x30,0x30,0x30,0x18,0x0C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x30,0x18,0x0C,0x0C,0x0C,0x0C,0x0C,0x0C,0x18,0x30,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0x66,0x3C,0xFF,0x3C,0x66,0x00,0x00,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0x18,0x18,0x7E,0x18,0x18,0x00,0x00,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x18,0x18,0x18,0x30,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xFE,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x18,0x18,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x02,0x06,0x0C,0x18,0x30,0x60,0xC0,0x80,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x7C,0xC6,0xC6,0xCE,0xDE,0xF6,0xE6,0xC6,0xC6,0x7C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x18,0x38,0x78,0x18,0x18,0x18,0x18,0x18,0x18,0x7E,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x7C,0xC6,0x06,0x0C,0x18,0x30,0x60,0xC0,0xC6,0xFE,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x7C,0xC6,0x06,0x06,0x3C,0x06,0x06,0x06,0xC6,0x7C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x0C,0x1C,0x3C,0x6C,0xCC,0xFE,0x0C,0x0C,0x0C,0x1E,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xFE,0xC0,0xC0,0xC0,0xFC,0x06,0x06,0x06,0xC6,0x7C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x38,0x60,0xC0,0xC0,0xFC,0xC6,0xC6,0xC6,0xC6,0x7C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xFE,0xC6,0x06,0x06,0x0C,0x18,0x30,0x30,0x30,0x30,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x7C,0xC6,0xC6,0xC6,0x7C,0xC6,0xC6,0xC6,0xC6,0x7C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x7C,0xC6,0xC6,0xC6,0x7E,0x06,0x06,0x06,0x0C,0x78,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x18,0x18,0x00,0x00,0x00,0x18,0x18,0x00,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x18,0x18,0x00,0x00,0x00,0x18,0x18,0x30,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x06,0x0C,0x18,0x30,0x60,0x30,0x18,0x0C,0x06,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0x7E,0x00,0x00,0x7E,0x00,0x00,0x00,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x60,0x30,0x18,0x0C,0x06,0x0C,0x18,0x30,0x60,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x7C,0xC6,0xC6,0x0C,0x18,0x18,0x18,0x00,0x18,0x18,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x7C,0xC6,0xC6,0xC6,0xDE,0xDE,0xDE,0xDC,0xC0,0x7C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x10,0x38,0x6C,0xC6,0xC6,0xFE,0xC6,0xC6,0xC6,0xC6,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xFC,0x66,0x66,0x66,0x7C,0x66,0x66,0x66,0x66,0xFC,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x3C,0x66,0xC2,0xC0,0xC0,0xC0,0xC0,0xC2,0x66,0x3C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xF8,0x6C,0x66,0x66,0x66,0x66,0x66,0x66,0x6C,0xF8,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xFE,0x66,0x62,0x68,0x78,0x68,0x60,0x62,0x66,0xFE,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xFE,0x66,0x62,0x68,0x78,0x68,0x60,0x60,0x60,0xF0,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x3C,0x66,0xC2,0xC0,0xC0,0xDE,0xC6,0xC6,0x66,0x3A,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xC6,0xC6,0xC6,0xC6,0xFE,0xC6,0xC6,0xC6,0xC6,0xC6,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x3C,0x18,0x18,0x18,0x18,0x18,0x18,0x18,0x18,0x3C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x1E,0x0C,0x0C,0x0C,0x0C,0x0C,0xCC,0xCC,0xCC,0x78,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xE6,0x66,0x66,0x6C,0x78,0x78,0x6C,0x66,0x66,0xE6,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xF0,0x60,0x60,0x60,0x60,0x60,0x60,0x62,0x66,0xFE,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xC6,0xEE,0xFE,0xFE,0xD6,0xC6,0xC6,0xC6,0xC6,0xC6,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xC6,0xE6,0xF6,0xFE,0xDE,0xCE,0xC6,0xC6,0xC6,0xC6,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x7C,0xC6,0xC6,0xC6,0xC6,0xC6,0xC6,0xC6,0xC6,0x7C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xFC,0x66,0x66,0x66,0x7C,0x60,0x60,0x60,0x60,0xF0,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x7C,0xC6,0xC6,0xC6,0xC6,0xC6,0xC6,0xD6,0xDE,0x7C,0x0C,0x0E,0x00,0x00},"
                , "    {0x00,0x00,0xFC,0x66,0x66,0x66,0x7C,0x6C,0x66,0x66,0x66,0xE6,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x7C,0xC6,0xC6,0x60,0x38,0x0C,0x06,0xC6,0xC6,0x7C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xFF,0xDB,0x99,0x18,0x18,0x18,0x18,0x18,0x18,0x3C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xC6,0xC6,0xC6,0xC6,0xC6,0xC6,0xC6,0xC6,0xC6,0x7C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xC6,0xC6,0xC6,0xC6,0xC6,0xC6,0xC6,0x6C,0x38,0x10,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xC6,0xC6,0xC6,0xC6,0xD6,0xD6,0xD6,0xFE,0xEE,0x6C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xC6,0xC6,0x6C,0x7C,0x38,0x38,0x7C,0x6C,0xC6,0xC6,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xC3,0xC3,0xC3,0x66,0x3C,0x18,0x18,0x18,0x18,0x3C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xFE,0xC6,0x86,0x0C,0x18,0x30,0x60,0xC2,0xC6,0xFE,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x3C,0x30,0x30,0x30,0x30,0x30,0x30,0x30,0x30,0x3C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x80,0xC0,0xE0,0x70,0x38,0x1C,0x0E,0x06,0x02,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x3C,0x0C,0x0C,0x0C,0x0C,0x0C,0x0C,0x0C,0x0C,0x3C,0x00,0x00,0x00,0x00},"
                , "    {0x10,0x38,0x6C,0xC6,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xFF,0x00,0x00},"
                , "    {0x00,0x30,0x18,0x0C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0x78,0x0C,0x7C,0xCC,0xCC,0xCC,0x76,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0xE0,0x60,0x60,0x78,0x6C,0x66,0x66,0x66,0x66,0x7C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0x7C,0xC6,0xC0,0xC0,0xC0,0xC6,0x7C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x1C,0x0C,0x0C,0x3C,0x6C,0xCC,0xCC,0xCC,0xCC,0x76,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0x7C,0xC6,0xFE,0xC0,0xC0,0xC6,0x7C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x38,0x6C,0x64,0x60,0xF0,0x60,0x60,0x60,0x60,0xF0,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0x76,0xCC,0xCC,0xCC,0xCC,0xCC,0x7C,0x0C,0xCC,0x78,0x00},"
                , "    {0x00,0x00,0xE0,0x60,0x60,0x6C,0x76,0x66,0x66,0x66,0x66,0xE6,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x18,0x18,0x00,0x38,0x18,0x18,0x18,0x18,0x18,0x3C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x06,0x06,0x00,0x0E,0x06,0x06,0x06,0x06,0x06,0x06,0x66,0x66,0x3C,0x00},"
                , "    {0x00,0x00,0xE0,0x60,0x60,0x66,0x6C,0x78,0x78,0x6C,0x66,0xE6,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x38,0x18,0x18,0x18,0x18,0x18,0x18,0x18,0x18,0x3C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0xE6,0xFF,0xDB,0xDB,0xDB,0xDB,0xDB,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0xDC,0x66,0x66,0x66,0x66,0x66,0x66,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0x7C,0xC6,0xC6,0xC6,0xC6,0xC6,0x7C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0xDC,0x66,0x66,0x66,0x66,0x66,0x7C,0x60,0x60,0xF0,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0x76,0xCC,0xCC,0xCC,0xCC,0xCC,0x7C,0x0C,0x0C,0x1E,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0xDC,0x76,0x66,0x60,0x60,0x60,0xF0,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0x7C,0xC6,0x60,0x38,0x0C,0xC6,0x7C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x10,0x30,0x30,0xFC,0x30,0x30,0x30,0x30,0x36,0x1C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0xCC,0xCC,0xCC,0xCC,0xCC,0xCC,0x76,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0xC3,0xC3,0xC3,0xC3,0x66,0x3C,0x18,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0xC6,0xC6,0xC6,0xD6,0xD6,0xFE,0x6C,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0xC6,0x6C,0x38,0x38,0x38,0x6C,0xC6,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0xC6,0xC6,0xC6,0xC6,0xC6,0xC6,0x7E,0x06,0x0C,0xF8,0x00},"
                , "    {0x00,0x00,0x00,0x00,0x00,0xFE,0xCC,0x18,0x30,0x60,0xC6,0xFE,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x0E,0x18,0x18,0x18,0x70,0x18,0x18,0x18,0x18,0x0E,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x18,0x18,0x18,0x18,0x00,0x18,0x18,0x18,0x18,0x18,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x70,0x18,0x18,0x18,0x0E,0x18,0x18,0x18,0x18,0x70,0x00,0x00,0x00,0x00},"
                , "    {0x00,0x00,0x76,0xDC,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00}"
                , "};"
                , ""
                , "/* Framebuffer graphics */"
                , "typedef struct {"
                , "    unsigned int flags;"
                , "    unsigned int mem_lower;"
                , "    unsigned int mem_upper;"
                , "    unsigned int boot_device;"
                , "    unsigned int cmdline;"
                , "    unsigned int mods_count;"
                , "    unsigned int mods_addr;"
                , "    unsigned int syms[4];"
                , "    unsigned int mmap_length;"
                , "    unsigned int mmap_addr;"
                , "    unsigned int drives_length;"
                , "    unsigned int drives_addr;"
                , "    unsigned int config_table;"
                , "    unsigned int boot_loader_name;"
                , "    unsigned int apm_table;"
                , "    unsigned int vbe_control_info;"
                , "    unsigned int vbe_mode_info;"
                , "    unsigned short vbe_mode;"
                , "    unsigned short vbe_interface_seg;"
                , "    unsigned short vbe_interface_off;"
                , "    unsigned short vbe_interface_len;"
                , "    unsigned long long framebuffer_addr;"
                , "    unsigned int framebuffer_pitch;"
                , "    unsigned int framebuffer_width;"
                , "    unsigned int framebuffer_height;"
                , "    unsigned char framebuffer_bpp;"
                , "    unsigned char framebuffer_type;"
                , "} __attribute__((packed)) multiboot_info_t;"
                , ""
                , "static unsigned int *fb_addr = (void*)0;"
                , "static unsigned int fb_width = 0;"
                , "static unsigned int fb_height = 0;"
                , "static unsigned int fb_pitch = 0;"
                , "static unsigned int fb_bpp = 0;"
                , "static unsigned int cursor_x = 0;"
                , "static unsigned int cursor_y = 0;"
                , ""
                , "#define FB_COLOR_BLACK      0x00000000"
                , "#define FB_COLOR_WHITE      0x00FFFFFF"
                , "#define FB_COLOR_GREEN      0x0000FF00"
                , "#define FB_COLOR_CYAN       0x0000FFFF"
                , "#define FB_COLOR_YELLOW     0x00FFFF00"
                , "#define FB_COLOR_ORANGE     0x00FF8800"
                , "#define FB_COLOR_GRAY       0x00808080"
                , "#define FB_COLOR_DARKGRAY   0x00404040"
                , ""
                , "static unsigned int fg_color = FB_COLOR_GREEN;"
                , "static unsigned int bg_color = FB_COLOR_BLACK;"
                , ""
                , "extern unsigned int multiboot_info_ptr;"
                , ""
                , "static int fb_init(void) {"
                , "    multiboot_info_t *mbi = (multiboot_info_t *)(unsigned long)multiboot_info_ptr;"
                , "    if (!mbi) return -1;"
                , "    if (!(mbi->flags & (1 << 12))) {"
                , "        fb_addr = (unsigned int *)0xFD000000;"
                , "        fb_width = 1280; fb_height = 1024;"
                , "        fb_pitch = 1280 * 4; fb_bpp = 32;"
                , "        return 0;"
                , "    }"
                , "    fb_addr = (unsigned int *)(unsigned long)mbi->framebuffer_addr;"
                , "    fb_width = mbi->framebuffer_width;"
                , "    fb_height = mbi->framebuffer_height;"
                , "    fb_pitch = mbi->framebuffer_pitch;"
                , "    fb_bpp = mbi->framebuffer_bpp;"
                , "    return 0;"
                , "}"
                , ""
                , "static inline void fb_putpixel(unsigned int x, unsigned int y, unsigned int color) {"
                , "    if (x < fb_width && y < fb_height) {"
                , "        unsigned int *pixel = (unsigned int *)((unsigned char *)fb_addr + y * fb_pitch + x * 4);"
                , "        *pixel = color;"
                , "    }"
                , "}"
                , ""
                , "static void fb_fillrect(unsigned int x, unsigned int y, unsigned int w, unsigned int h, unsigned int color) {"
                , "    for (unsigned int j = 0; j < h; j++)"
                , "        for (unsigned int i = 0; i < w; i++)"
                , "            fb_putpixel(x + i, y + j, color);"
                , "}"
                , ""
                , "static void fb_clear(void) {"
                , "    fb_fillrect(0, 0, fb_width, fb_height, bg_color);"
                , "    cursor_x = 0; cursor_y = 0;"
                , "}"
                , ""
                , "static void fb_drawchar(unsigned int x, unsigned int y, char c) {"
                , "    if (c < 32 || c > 126) c = '?';"
                , "    const unsigned char *glyph = font8x16_data[c - 32];"
                , "    for (int row = 0; row < FONT_HEIGHT; row++) {"
                , "        unsigned char bits = glyph[row];"
                , "        for (int col = 0; col < FONT_WIDTH; col++) {"
                , "            unsigned int color = (bits & (0x80 >> col)) ? fg_color : bg_color;"
                , "            fb_putpixel(x + col, y + row, color);"
                , "        }"
                , "    }"
                , "}"
                , ""
                , "static void fb_putc(char c) {"
                , "    unsigned int max_cols = fb_width / FONT_WIDTH;"
                , "    unsigned int max_rows = fb_height / FONT_HEIGHT;"
                , "    if (c == '\\n') { cursor_x = 0; cursor_y++; }"
                , "    else if (c == '\\r') { cursor_x = 0; }"
                , "    else if (c == '\\t') { cursor_x = (cursor_x + 8) & ~7; }"
                , "    else { fb_drawchar(cursor_x * FONT_WIDTH, cursor_y * FONT_HEIGHT, c); cursor_x++; }"
                , "    if (cursor_x >= max_cols) { cursor_x = 0; cursor_y++; }"
                , "    if (cursor_y >= max_rows) { cursor_y = 0; }"
                , "}"
                , ""
                , "static void fb_print(const char *s) { while (*s) fb_putc(*s++); }"
                , "static void fb_println(const char *s) { fb_print(s); fb_putc('\\n'); }"
                , "static void fb_setfg(unsigned int color) { fg_color = color; }"
                , "static void fb_setbg(unsigned int color) { bg_color = color; }"
                , ""
                , "static void fb_hline(unsigned int x, unsigned int y, unsigned int len, unsigned int color) {"
                , "    for (unsigned int i = 0; i < len; i++) fb_putpixel(x + i, y, color);"
                , "}"
                , ""
                , "static void fb_vline(unsigned int x, unsigned int y, unsigned int len, unsigned int color) {"
                , "    for (unsigned int i = 0; i < len; i++) fb_putpixel(x, y + i, color);"
                , "}"
                , ""
                , "static void fb_rect(unsigned int x, unsigned int y, unsigned int w, unsigned int h, unsigned int color) {"
                , "    fb_hline(x, y, w, color); fb_hline(x, y + h - 1, w, color);"
                , "    fb_vline(x, y, h, color); fb_vline(x + w - 1, y, h, color);"
                , "}"
                , ""
                , "static void fb_gotoxy(unsigned int x, unsigned int y) { cursor_x = x; cursor_y = y; }"
                , ""
                , "/* Rate Monotonic Scheduler (RMS) support */"
                , "typedef enum { RMS_ON_TIME = 0, RMS_MISSED = 1, RMS_NOT_STARTED = 2 } rms_deadline_status_t;"
                , ""
                , "typedef struct {"
                , "    unsigned int count;"
                , "    unsigned int missed_count;"
                , "    unsigned int min_cpu_time_us;"
                , "    unsigned int max_cpu_time_us;"
                , "    unsigned int avg_cpu_time_us;"
                , "    unsigned int period_ms;"
                , "    rms_deadline_status_t last_status;"
                , "} rms_stats_t;"
                , ""
                , "typedef struct {"
                , "    unsigned int period_id;"
                , "    unsigned int period_ms;"
                , "    unsigned int period_ticks;"
                , "    int started;"
                , "    rms_deadline_status_t last_status;"
                , "    unsigned int local_missed_count;"
                , "    unsigned long long last_period_start_us;"
                , "} rms_period_t;"
                , ""
                , "static unsigned int rms_period_counter = 0;"
                , "static unsigned int rms_global_missed_count = 0;"
                , ""
                , "#ifdef __rtems__"
                , "/* Real RTEMS implementation */"
                , "#include <rtems/rtems/ratemon.h>"
                , ""
                , "static unsigned long long rms_now_us(void) {"
                , "    struct timespec ts;"
                , "    rtems_clock_get_uptime(&ts);"
                , "    return (unsigned long long)ts.tv_sec * 1000000ULL + (unsigned long long)ts.tv_nsec / 1000ULL;"
                , "}"
                , ""
                , "static unsigned int rms_ms_to_ticks(unsigned int ms) {"
                , "    unsigned int tps = rtems_clock_get_ticks_per_second();"
                , "    unsigned int ticks = (ms * tps) / 1000;"
                , "    return ticks > 0 ? ticks : 1;"
                , "}"
                , ""
                , "static rms_period_t *rms_create(unsigned int period_ms) {"
                , "    rms_period_t *p = (rms_period_t *)malloc(sizeof(rms_period_t));"
                , "    if (!p) return 0;"
                , "    rtems_id period_id;"
                , "    char name[5];"
                , "    unsigned int num = rms_period_counter++;"
                , "    name[0] = 'R'; name[1] = '0' + (num/100)%10; name[2] = '0' + (num/10)%10; name[3] = '0' + num%10; name[4] = 0;"
                , "    rtems_name rname = rtems_build_name(name[0], name[1], name[2], name[3]);"
                , "    if (rtems_rate_monotonic_create(rname, &period_id) != RTEMS_SUCCESSFUL) { free(p); return 0; }"
                , "    p->period_id = period_id;"
                , "    p->period_ms = period_ms;"
                , "    p->period_ticks = rms_ms_to_ticks(period_ms);"
                , "    p->started = 0; p->last_status = RMS_NOT_STARTED;"
                , "    p->local_missed_count = 0; p->last_period_start_us = 0;"
                , "    return p;"
                , "}"
                , ""
                , "static rms_deadline_status_t rms_wait_period(rms_period_t *p) {"
                , "    if (!p) return RMS_NOT_STARTED;"
                , "    if (!p->started) {"
                , "        rtems_rate_monotonic_period(p->period_id, p->period_ticks);"
                , "        p->started = 1; p->last_period_start_us = rms_now_us();"
                , "        return RMS_NOT_STARTED;"
                , "    }"
                , "    rtems_status_code status = rtems_rate_monotonic_period(p->period_id, p->period_ticks);"
                , "    p->last_period_start_us = rms_now_us();"
                , "    if (status == RTEMS_TIMEOUT) {"
                , "        p->last_status = RMS_MISSED; p->local_missed_count++; rms_global_missed_count++;"
                , "        return RMS_MISSED;"
                , "    }"
                , "    p->last_status = RMS_ON_TIME;"
                , "    return RMS_ON_TIME;"
                , "}"
                , ""
                , "static int rms_get_stats(rms_period_t *p, rms_stats_t *s) {"
                , "    if (!p || !s) return -1;"
                , "    rtems_rate_monotonic_period_statistics rs;"
                , "    if (rtems_rate_monotonic_get_statistics(p->period_id, &rs) != RTEMS_SUCCESSFUL) return -1;"
                , "    s->count = rs.count; s->missed_count = rs.missed_count;"
                , "    s->min_cpu_time_us = rs.min_cpu_time.tv_sec * 1000000 + rs.min_cpu_time.tv_nsec / 1000;"
                , "    s->max_cpu_time_us = rs.max_cpu_time.tv_sec * 1000000 + rs.max_cpu_time.tv_nsec / 1000;"
                , "    s->avg_cpu_time_us = rs.count > 0 ? (rs.total_cpu_time.tv_sec * 1000000 + rs.total_cpu_time.tv_nsec / 1000) / rs.count : 0;"
                , "    s->period_ms = p->period_ms; s->last_status = p->last_status;"
                , "    return 0;"
                , "}"
                , ""
                , "static unsigned int rms_get_missed_count(rms_period_t *p) {"
                , "    if (!p) return 0;"
                , "    rtems_rate_monotonic_period_statistics rs;"
                , "    return (rtems_rate_monotonic_get_statistics(p->period_id, &rs) == RTEMS_SUCCESSFUL) ? rs.missed_count : p->local_missed_count;"
                , "}"
                , ""
                , "static void rms_delete(rms_period_t *p) {"
                , "    if (p && p->period_id) { rtems_rate_monotonic_delete(p->period_id); free(p); }"
                , "}"
                , ""
                , "static int rms_assign_priority(unsigned int period_ms) {"
                , "    rtems_task_priority prio = period_ms < 1 ? 1 : (period_ms > 255 ? 255 : period_ms);"
                , "    rtems_task_priority old;"
                , "    return rtems_task_set_priority(RTEMS_SELF, prio, &old) == RTEMS_SUCCESSFUL ? 0 : -1;"
                , "}"
                , ""
                , "#else"
                , "/* Stub implementation for non-RTEMS (testing with TCC) */"
                , "static unsigned long long rms_now_us(void) { return 0; }"
                , "static unsigned int rms_ms_to_ticks(unsigned int ms) { return ms; }"
                , "static rms_period_t *rms_create(unsigned int period_ms) {"
                , "    rms_period_t *p = (rms_period_t *)malloc(sizeof(rms_period_t));"
                , "    if (!p) return 0;"
                , "    p->period_id = ++rms_period_counter; p->period_ms = period_ms;"
                , "    p->period_ticks = period_ms; p->started = 0;"
                , "    p->last_status = RMS_NOT_STARTED; p->local_missed_count = 0;"
                , "    return p;"
                , "}"
                , "static rms_deadline_status_t rms_wait_period(rms_period_t *p) {"
                , "    if (!p) return RMS_NOT_STARTED;"
                , "    if (!p->started) { p->started = 1; return RMS_NOT_STARTED; }"
                , "    p->last_status = RMS_ON_TIME; return RMS_ON_TIME; /* Stub: always on time */"
                , "}"
                , "static int rms_get_stats(rms_period_t *p, rms_stats_t *s) {"
                , "    if (!p || !s) return -1;"
                , "    s->count = 0; s->missed_count = p ? p->local_missed_count : 0;"
                , "    s->min_cpu_time_us = 0; s->max_cpu_time_us = 0; s->avg_cpu_time_us = 0;"
                , "    s->period_ms = p ? p->period_ms : 0; s->last_status = p ? p->last_status : RMS_NOT_STARTED;"
                , "    return 0;"
                , "}"
                , "static unsigned int rms_get_missed_count(rms_period_t *p) { return p ? p->local_missed_count : 0; }"
                , "static void rms_delete(rms_period_t *p) { if (p) free(p); }"
                , "static int rms_assign_priority(unsigned int period_ms) { (void)period_ms; return 0; }"
                , "#endif"
                , ""
                , "static unsigned int rms_global_get_missed(void) { return rms_global_missed_count; }"
                , "static void rms_global_reset_missed(void) { rms_global_missed_count = 0; }"
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
            getModuleName ast

        -- Process imports to generate includes
        importCode =
            generateImportCode ast.imports

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
            ++ [ importCode ]
            ++ [ "/* Forward declarations */" ]
            ++ forwardDecls
            ++ [ "" ]
            ++ [ "/* Helper functions */" ]
            ++ helperImpls
            ++ [ "" ]
            ++ [ "/* Main */" ]
            ++ mainImpl
        )


{-| Generate C code for TCC (Tiny C Compiler)
    Uses lambda lifting to avoid GCC-specific nested functions.
    Similar to RTEMS code but without RTEMS-specific runtime.
-}
generateTccCode : Src.Module -> String
generateTccCode ast =
    let
        moduleName =
            getModuleName ast

        modulePrefix =
            getModulePrefix ast

        -- Collect user function names for context (used for module-prefixed function calls)
        userFunctionNames =
            collectUserFunctionNames ast.values

        -- Check if module has a main function
        hasMain =
            ast.values
                |> List.any (\(Src.At _ v) ->
                    case v.name of
                        Src.At _ "main" -> True
                        _ -> False
                )

        -- Dead Code Elimination: only keep functions reachable from main
        -- Only apply DCE if module has main (library modules keep all functions)
        values =
            if hasMain then
                filterReachableValues ast.values
            else
                ast.values  -- Keep all for library modules

        -- Process imports to generate includes and extern declarations
        importCode =
            generateImportCode ast.imports

        mainValue =
            extractMain ast

        -- Extract lambdas from record fields and generate static functions
        extractRecordLambdas : String -> Src.Expr -> List ( String, String )
        extractRecordLambdas recordName (Src.At _ expr) =
            case expr of
                Src.Record fields ->
                    fields
                        |> List.filterMap
                            (\( Src.At _ fieldName, fieldValue ) ->
                                case fieldValue of
                                    Src.At _ (Src.Lambda patterns body) ->
                                        let
                                            funcName = "elm_" ++ recordName ++ "_" ++ fieldName
                                            params = patterns
                                                |> List.map
                                                    (\(Src.At _ p) ->
                                                        case p of
                                                            Src.PVar vn -> "double elm_" ++ vn
                                                            Src.PRecord fns ->
                                                                fns |> List.map (\(Src.At _ fn) -> "double elm_" ++ fn) |> String.join ", "
                                                            _ -> "double __arg"
                                                    )
                                                |> String.join ", "
                                            bodyStr = generateStandaloneExpr body
                                            -- Infer return type from body
                                            lambdaReturnType =
                                                if String.contains "elm_str_" bodyStr || String.startsWith "\"" bodyStr then
                                                    "const char *"
                                                else if String.contains "elm_union_t" bodyStr || String.contains "TAG_" bodyStr then
                                                    "elm_union_t"
                                                else
                                                    "double"
                                            funcDef = "static " ++ lambdaReturnType ++ " " ++ funcName ++ "(" ++ params ++ ") {\n    return " ++ bodyStr ++ ";\n}"
                                        in
                                        Just ( fieldName, funcDef )
                                    _ ->
                                        Nothing
                            )
                _ ->
                    []

        -- Collect all lambdas from complex constants that are records
        recordLambdaFunctions =
            values
                |> List.filterMap
                    (\(Src.At _ value) ->
                        let
                            (Src.At _ name) = value.name
                        in
                        if name /= "main" && List.isEmpty value.args then
                            let
                                lambdas = extractRecordLambdas name value.body
                            in
                            if List.isEmpty lambdas then
                                Nothing
                            else
                                Just ( name, lambdas )
                        else
                            Nothing
                    )

        -- Generate all lambda functions as static functions
        recordLambdaFunctionsCode =
            recordLambdaFunctions
                |> List.concatMap (\( _, lambdas ) -> List.map (\( _, funcDef ) -> funcDef) lambdas)
                |> String.join "\n\n"

        -- Build lookup for which record fields are lambdas
        recordLambdaFieldsLookup : List ( String, List String )
        recordLambdaFieldsLookup =
            recordLambdaFunctions
                |> List.map (\( recName, lambdas ) -> ( recName, List.map (\( fieldName, _ ) -> fieldName) lambdas ))

        -- Generate module-level constants (non-main values without arguments)
        -- Simple expressions become static const, complex ones become getter functions
        ( simpleConstants, complexConstants ) =
            values
                |> List.filterMap
                    (\(Src.At _ value) ->
                        let
                            (Src.At _ name) =
                                value.name
                        in
                        if name /= "main" && List.isEmpty value.args then
                            Just ( name, value.body, isSimpleLiteral value.body )
                        else
                            Nothing
                    )
                |> List.partition (\( _, _, isSimple ) -> isSimple)

        simpleConstantsCode =
            simpleConstants
                |> List.map
                    (\( name, body, _ ) ->
                        let
                            cExpr = generateStandaloneExpr body
                            isString = String.startsWith "\"" cExpr
                            cType = if isString then "const char *" else "double"
                        in
                        "static " ++ cType ++ " elm_" ++ name ++ " = " ++ cExpr ++ ";"
                    )
                |> String.join "\n"

        -- Get complex constant names for reference transformation
        complexConstantNames =
            complexConstants |> List.map (\( name, _, _ ) -> name)

        -- Replace complex constant references with function calls in an expression
        -- For each complex constant name, replace `elm_name` with `elm_name()` when
        -- not already followed by `(` and not followed by `_` (which would indicate
        -- a different identifier like `elm_name_helper`)
        fixComplexConstantRefs code =
            let
                replaceOne name codeStr =
                    let
                        pattern = "elm_" ++ name
                        patternLen = String.length pattern

                        isAlphaNum s =
                            case String.uncons s of
                                Just (c, _) ->
                                    Char.isAlphaNum c
                                Nothing ->
                                    False

                        -- Check if we should replace at a given position in the given string
                        shouldReplace str pos =
                            let
                                -- Check character after the pattern
                                charAfter = String.slice (pos + patternLen) (pos + patternLen + 1) str
                            in
                            charAfter /= "_"  -- Not part of longer identifier
                                && charAfter /= "("  -- Not already a function call
                                && not (isAlphaNum charAfter)  -- Not followed by alphanumeric

                        -- Find all occurrences and replace
                        doReplace str offset =
                            let
                                remaining = String.dropLeft offset str
                            in
                            case findNext pattern remaining 0 of
                                Nothing ->
                                    str
                                Just relativePos ->
                                    let
                                        absolutePos = offset + relativePos
                                    in
                                    if shouldReplace str absolutePos then
                                        -- Replace: insert () after the pattern
                                        let
                                            before = String.left absolutePos str
                                            after = String.dropLeft (absolutePos + patternLen) str
                                        in
                                        doReplace (before ++ pattern ++ "()" ++ after) (absolutePos + patternLen + 2)
                                    else
                                        -- Skip this occurrence
                                        doReplace str (absolutePos + patternLen)

                        -- Simple substring finder
                        findNext needle haystack start =
                            let
                                needleLen = String.length needle
                                haystackLen = String.length haystack
                                checkAt i =
                                    if i + needleLen > haystackLen then
                                        Nothing
                                    else if String.slice i (i + needleLen) haystack == needle then
                                        Just i
                                    else
                                        checkAt (i + 1)
                            in
                            checkAt start
                    in
                    doReplace codeStr 0
            in
            List.foldl replaceOne code complexConstantNames

        -- Replace __LAMBDA_fieldName__ markers with actual function names
        replaceLambdaMarkers : String -> String -> String
        replaceLambdaMarkers recName code =
            let
                lambdaFields =
                    recordLambdaFieldsLookup
                        |> List.filter (\( rn, _ ) -> rn == recName)
                        |> List.concatMap (\( _, fields ) -> fields)
                replaceOne fieldName codeStr =
                    String.replace ("__LAMBDA_" ++ fieldName ++ "__") ("elm_" ++ recName ++ "_" ++ fieldName) codeStr
            in
            List.foldl replaceOne code lambdaFields

        complexConstantsCode =
            complexConstants
                |> List.map
                    (\( name, body, _ ) ->
                        let
                            -- Use correct function prefix for closure macro generation
                            ctx = { funcPrefix = name, modulePrefix = modulePrefix, userFunctions = userFunctionNames }
                            cExpr = generateStandaloneExprWithCtx ctx body
                            -- Check for record list first (special marker format)
                            isRecordList = String.contains "/*RECORD_LIST:" cExpr
                            -- Check for list type (before record, since list of records contains struct)
                            isList = not isRecordList && (String.contains "elm_list_t" cExpr || String.startsWith "((elm_list_t)" cExpr)
                            -- Check for union type (Maybe, Result, etc.) - contains TAG_ usage or elm_union_t
                            isUnion = not isList && not isRecordList && (String.contains "TAG_" cExpr || String.contains "elm_union_t" cExpr || String.contains "elm_Just" cExpr || String.contains "elm_Nothing" cExpr)
                            -- String detection - only if it's a string expression, not just contains a string literal argument
                            isString = not isUnion && not isList && not isRecordList && (String.contains "elm_str_" cExpr || (String.startsWith "\"" cExpr && String.endsWith "\"" cExpr))
                            isRecord = not isList && not isRecordList && String.contains "((struct {" cExpr
                            -- Extract record type from the expression
                            recordType =
                                if isRecord then
                                    let
                                        startMarker = "((struct {"
                                        startIdx = String.indexes startMarker cExpr |> List.head |> Maybe.withDefault 0
                                        searchStart = startIdx + String.length startMarker
                                        afterStart = String.dropLeft searchStart cExpr
                                        endIdx = String.indexes "})" afterStart |> List.head |> Maybe.withDefault 0
                                        fieldDefs = String.left endIdx afterStart
                                    in
                                    "struct {" ++ fieldDefs ++ "}"
                                else
                                    ""
                            cType =
                                if isList then "elm_list_t"
                                else if isUnion then "elm_union_t"
                                else if isString then "const char *"
                                else if isRecord then recordType
                                else "double"
                        in
                        -- For record lists, generate typedef and static array
                        if isRecordList then
                            let
                                -- Extract record type from /*RECORD_LIST:struct {...}*/
                                typeStartMarker = "/*RECORD_LIST:"
                                typeStartIdx = String.indexes typeStartMarker cExpr |> List.head |> Maybe.withDefault 0
                                afterTypeStart = String.dropLeft (typeStartIdx + String.length typeStartMarker) cExpr
                                typeEndMarker = "*/ "
                                typeEndIdx = String.indexes typeEndMarker afterTypeStart |> List.head |> Maybe.withDefault 0
                                recType = String.left typeEndIdx afterTypeStart
                                -- Extract count from /*END_RECORD_LIST:N*/
                                countStartMarker = "/*END_RECORD_LIST:"
                                countStartIdx = String.indexes countStartMarker cExpr |> List.head |> Maybe.withDefault 0
                                afterCountStart = String.dropLeft (countStartIdx + String.length countStartMarker) cExpr
                                countEndIdx = String.indexes "*/" afterCountStart |> List.head |> Maybe.withDefault 0
                                countStr = String.left countEndIdx afterCountStart
                                -- Extract the data after */ { ... } /*END
                                -- Find the position after the closing */ of RECORD_LIST comment
                                afterTypeComment = String.dropLeft (typeEndIdx + String.length typeEndMarker) afterTypeStart
                                -- Now find the opening { and extract content until } /*END
                                dataStartIdx = String.indexes "{ " afterTypeComment |> List.head |> Maybe.withDefault 0
                                afterDataStart = String.dropLeft (dataStartIdx + 2) afterTypeComment
                                dataEndIdx = String.indexes " } /*END" afterDataStart |> List.head |> Maybe.withDefault (String.length afterDataStart)
                                dataContent = String.left dataEndIdx afterDataStart
                                -- Generate typedef name from constant name
                                -- Use modulePrefix from outer scope (module name with . replaced by _)
                                typedefName = "elm_" ++ modulePrefix ++ "_" ++ name ++ "_elem_t"
                                listTypeName = "elm_" ++ modulePrefix ++ "_" ++ name ++ "_list_t"
                                dataArrayName = "elm_" ++ modulePrefix ++ "_" ++ name ++ "_data"
                                varName = "elm_" ++ modulePrefix ++ "_" ++ name
                            in
                            -- Generate typedef for element
                            "typedef " ++ recType ++ " " ++ typedefName ++ ";\n" ++
                            -- Generate static array
                            "static " ++ typedefName ++ " " ++ dataArrayName ++ "[] = { " ++ dataContent ++ " };\n" ++
                            -- Generate list wrapper type with typed data pointer
                            "typedef struct { int length; " ++ typedefName ++ " *data; } " ++ listTypeName ++ ";\n" ++
                            -- Generate wrapper variable that points to the array
                            "static " ++ listTypeName ++ " " ++ varName ++ " = { " ++ countStr ++ ", " ++ dataArrayName ++ " };\n" ++
                            -- Generate alias for backward compatibility (elm_name = module-prefixed name)
                            "#define elm_" ++ name ++ " " ++ varName
                        -- For regular lists, generate static constants
                        else if isList then
                            let
                                -- Extract initializer from ((elm_list_t){...})
                                initStartMarker = "((elm_list_t){"
                                initStartIdx = String.indexes initStartMarker cExpr |> List.head |> Maybe.withDefault 0
                                afterInitStart = String.dropLeft (initStartIdx + String.length initStartMarker) cExpr
                                initEndIdx = String.length afterInitStart - 2
                                initializer = String.left initEndIdx afterInitStart
                            in
                            "static elm_list_t elm_" ++ name ++ " = {" ++ initializer ++ "};"
                        -- For records, generate static constants instead of functions
                        -- This allows `model = init` to work correctly
                        else if isRecord then
                            let
                                -- Extract initializer from ((struct {...}){...})
                                initStartMarker = "){"
                                initStartIdx = String.indexes initStartMarker cExpr |> List.head |> Maybe.withDefault 0
                                afterInitStart = String.dropLeft (initStartIdx + String.length initStartMarker) cExpr
                                initEndIdx = String.length afterInitStart - 2
                                initializer = String.left initEndIdx afterInitStart
                                -- Replace lambda markers with function names
                                fixedInitializer = replaceLambdaMarkers name initializer
                            in
                            "static " ++ recordType ++ " elm_" ++ name ++ " = {" ++ fixedInitializer ++ "};"
                        else
                            "static " ++ cType ++ " elm_" ++ name ++ "(void) {\n    return " ++ fixComplexConstantRefs cExpr ++ ";\n}"
                    )
                |> String.join "\n\n"

        -- Early constants code (doesn't depend on lifted functions)
        earlyConstantsCode =
            let
                lambdas = if String.isEmpty recordLambdaFunctionsCode then "" else "/* Lambda functions lifted from records */\n" ++ recordLambdaFunctionsCode ++ "\n\n"
                simple = if String.isEmpty simpleConstantsCode then "" else "/* Simple constants */\n" ++ simpleConstantsCode ++ "\n\n"
            in
            lambdas ++ simple

        -- Complex constants code (depends on lifted functions, output after them)
        complexConstantsSection =
            if String.isEmpty complexConstantsCode then "" else "/* Computed constants (as functions) */\n" ++ complexConstantsCode ++ "\n\n"

        -- Generate user-defined functions (non-main values with arguments)
        userFunctions =
            values
                |> List.filterMap
                    (\(Src.At _ value) ->
                        let
                            (Src.At _ name) =
                                value.name
                        in
                        if name /= "main" && not (List.isEmpty value.args) then
                            Just (fixComplexConstantRefs (generateUserFunction modulePrefix userFunctionNames name value.args value.body))

                        else
                            Nothing
                    )
                |> String.join "\n\n"

        -- Collect and generate lifted local functions from all values
        liftedFunctions =
            values
                |> List.concatMap
                    (\(Src.At _ value) ->
                        let
                            (Src.At _ name) =
                                value.name

                            -- Include function parameters in scope for captured variable detection
                            funcParamNames =
                                List.concatMap patternVars value.args
                        in
                        collectLocalFunctionsWithScope name funcParamNames value.body
                    )
                |> List.map
                    (\lf ->
                        generateLiftedFunction lf.prefix lf.name lf.args lf.body lf.capturedVars
                    )
                |> String.join "\n\n"

        liftedFunctionsCode =
            if String.isEmpty liftedFunctions then
                ""

            else
                "/* Lifted local functions */\n" ++ fixComplexConstantRefs liftedFunctions ++ "\n\n"

        userFunctionsCode =
            if String.isEmpty userFunctions then
                ""

            else
                "/* User-defined functions */\n" ++ userFunctions ++ "\n\n"

        -- Generate custom type definitions (tags and constructors)
        -- Skip built-in types (Maybe, Result, Order) as they're already defined in the header
        builtinTypes = [ "Maybe", "Result", "Order" ]

        customTypeCode =
            ast.unions
                |> List.filter
                    (\(Src.At _ union) ->
                        let
                            (Src.At _ typeName) = union.name
                        in
                        not (List.member typeName builtinTypes)
                    )
                |> List.map
                    (\(Src.At _ union) ->
                        let
                            (Src.At _ typeName) =
                                union.name

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
                                            let
                                                argCount =
                                                    List.length ctorArgs

                                            in
                                            if argCount == 0 then
                                                -- No-data constructor
                                                "static elm_union_t elm_"
                                                    ++ ctorName
                                                    ++ "(void) {\n    elm_union_t result = { .tag = TAG_"
                                                    ++ ctorName
                                                    ++ ", .data = {.num = 0}, .data2 = 0 };\n    return result;\n}"

                                            else if argCount == 1 then
                                                -- Single argument constructor
                                                "static elm_union_t elm_"
                                                    ++ ctorName
                                                    ++ "(elm_union_t v1) {\n    elm_union_t result = { .tag = TAG_"
                                                    ++ ctorName
                                                    ++ ", .data = {.child = elm_alloc_union(v1)}, .data2 = 0 };\n    return result;\n}"

                                            else
                                                -- Two argument constructor (Add Expr Expr, etc)
                                                "static elm_union_t elm_"
                                                    ++ ctorName
                                                    ++ "(elm_union_t v1, elm_union_t v2) {\n    elm_union_t result = { .tag = TAG_"
                                                    ++ ctorName
                                                    ++ ", .data = {.child = elm_alloc_union(v1)}, .data2 = elm_alloc_union(v2) };\n    return result;\n}"
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

        -- Generate forward declarations for user-defined functions
        -- Skip functions that return anonymous structs (they cause redefinition errors)
        forwardDecls =
            values
                |> List.filterMap
                    (\(Src.At _ value) ->
                        let
                            (Src.At _ name) =
                                value.name
                        in
                        if name /= "main" && not (List.isEmpty value.args) then
                            let
                                -- Build prefixed name for this function
                                prefixedName =
                                    if String.isEmpty modulePrefix then
                                        name
                                    else
                                        modulePrefix ++ "_" ++ name
                                -- Generate implementation to infer types
                                implCode = generateUserFunction modulePrefix userFunctionNames name value.args value.body
                                -- Extract signature parts from implementation
                                signatureEnd = String.indexes "(" implCode |> List.head |> Maybe.withDefault 0
                                paramsStart = signatureEnd + 1
                                paramsEnd = String.indexes ")" implCode |> List.head |> Maybe.withDefault 0
                                returnTypeStart = 7 -- Length of "static "
                                -- Find " elm_<name>(" to locate the function name position
                                funcNameMarker = " elm_" ++ prefixedName ++ "("
                                returnTypeEnd = String.indexes funcNameMarker implCode |> List.head |> Maybe.withDefault 0
                                funcReturnType = String.slice returnTypeStart returnTypeEnd implCode
                                params = String.slice paramsStart paramsEnd implCode
                                -- Skip forward declarations for functions with anonymous structs
                                -- In C, anonymous structs are always different types
                                isStructReturn = String.contains "struct {" funcReturnType
                                isStructParam = String.contains "struct {" params
                                -- Skip typeof return types (needs the type to be declared first)
                                isTypeofReturn = String.contains "typeof(" funcReturnType
                            in
                            if isStructReturn || isStructParam || isTypeofReturn then
                                Nothing
                            else
                                Just ("static " ++ funcReturnType ++ " elm_" ++ prefixedName ++ "(" ++ params ++ ");")
                        else
                            Nothing
                    )
                |> String.join "\n"

        forwardDeclsCode =
            if String.isEmpty forwardDecls then
                ""
            else
                "/* Forward declarations */\n" ++ forwardDecls ++ "\n\n"

        -- Generate elm_main function and result handling based on type
        ( returnType, returnExpr, printFormat ) =
            case mainValue of
                MainString s ->
                    ( "const char *", "\"" ++ escapeC s ++ "\"", "%s" )

                MainInt n ->
                    ( "int", String.fromInt n, "%d" )

                MainExpr cType cExpr ->
                    ( cType, fixComplexConstantRefs cExpr, if cType == "int" then "%d" else "%s" )

        header =
            [ "/*"
            , " * Generated by tcelm from " ++ moduleName
            , " * TCC-compatible version (no GCC extensions)"
            , " */"
            , ""
            , importCode
            , TCC.runtimePreamble
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
            ++ [ constructorDefinesCode ++ forwardDeclsCode ++ earlyConstantsCode ++ liftedFunctionsCode ++ complexConstantsSection ++ userFunctionsCode ++ "/* Elm main value */" ]
            ++ mainImpl
        )


{-| Generate C library code for a module with module-qualified names.
    This is used for multi-file compilation where modules need unique names.
-}
generateTccLibCode : Src.Module -> String
generateTccLibCode ast =
    let
        moduleName =
            getModuleName ast

        -- Convert module name to C identifier (replace dots with underscores)
        cModuleName =
            getModulePrefix ast

        -- Process imports
        importCode =
            generateImportCode ast.imports

        -- Generate module-qualified function names
        qualifyName name =
            "elm_" ++ cModuleName ++ "_" ++ name

        -- Generate module-level constants with qualified names
        ( simpleConstants, complexConstants ) =
            ast.values
                |> List.filterMap
                    (\(Src.At _ value) ->
                        let
                            (Src.At _ name) = value.name
                        in
                        if List.isEmpty value.args then
                            Just ( name, value.body, isSimpleLiteral value.body )
                        else
                            Nothing
                    )
                |> List.partition (\( _, _, isSimple ) -> isSimple)

        -- Collect user function names for context (moved before complexConstantsCode)
        userFunctionNames =
            collectAllFunctionNames ast.values

        simpleConstantsCode =
            simpleConstants
                |> List.map
                    (\( name, body, _ ) ->
                        let
                            cExpr = generateStandaloneExpr body
                            isString = String.startsWith "\"" cExpr
                            cType = if isString then "const char *" else "double"
                        in
                        cType ++ " " ++ qualifyName name ++ " = " ++ cExpr ++ ";"
                    )
                |> String.join "\n"

        complexConstantsCode =
            complexConstants
                |> List.map
                    (\( name, body, _ ) ->
                        let
                            -- Use correct function prefix for closure macro generation
                            ctx = { funcPrefix = name, modulePrefix = cModuleName, userFunctions = userFunctionNames }
                            cExpr = generateStandaloneExprWithCtx ctx body
                            isString = String.contains "elm_str_" cExpr || String.contains "\"" cExpr
                            cType = if isString then "const char *" else "double"
                        in
                        cType ++ " " ++ qualifyName name ++ "(void) {\n    return " ++ cExpr ++ ";\n}"
                    )
                |> String.join "\n\n"

        -- Generate user-defined functions with qualified names
        userFunctions =
            ast.values
                |> List.filterMap
                    (\(Src.At _ value) ->
                        let
                            (Src.At _ name) = value.name
                        in
                        if not (List.isEmpty value.args) then
                            -- generateUserFunction now handles module prefixing internally
                            Just (generateUserFunction cModuleName userFunctionNames name value.args value.body)
                        else
                            Nothing
                    )
                |> String.join "\n\n"

        -- Collect and generate lifted local functions
        liftedFunctions =
            ast.values
                |> List.concatMap
                    (\(Src.At _ value) ->
                        let
                            (Src.At _ name) = value.name
                            funcParamNames = List.concatMap patternVars value.args
                        in
                        collectLocalFunctionsWithScope name funcParamNames value.body
                    )
                |> List.map
                    (\lf ->
                        let
                            funcCode = generateLiftedFunction lf.prefix lf.name lf.args lf.body lf.capturedVars
                            -- Qualify lifted function names too
                            qualifiedCode = String.replace ("elm_" ++ lf.prefix ++ "_") (qualifyName (lf.prefix ++ "_")) funcCode
                        in
                        qualifiedCode
                    )
                |> String.join "\n\n"

        -- Generate custom type constructors
        constructorDefines =
            ast.unions
                |> List.concatMap
                    (\(Src.At _ union) ->
                        let
                            (Src.At _ typeName) = union.name
                        in
                        union.ctors
                            |> List.indexedMap
                                (\idx ( Src.At _ ctorName, _ ) ->
                                    "#define TAG_" ++ cModuleName ++ "_" ++ ctorName ++ " " ++ String.fromInt idx
                                )
                    )
                |> String.join "\n"

        header =
            [ "/*"
            , " * Generated by tcelm from " ++ moduleName
            , " * Library module with qualified names"
            , " */"
            , ""
            , "#ifndef ELM_" ++ String.toUpper cModuleName ++ "_H"
            , "#define ELM_" ++ String.toUpper cModuleName ++ "_H"
            , ""
            , "#include <stdio.h>"
            , "#include <stdlib.h>"
            , "#include <string.h>"
            , ""
            , importCode
            ]

        footer =
            [ ""
            , "#endif /* ELM_" ++ String.toUpper cModuleName ++ "_H */"
            ]
    in
    String.join "\n"
        (header
            ++ [ if String.isEmpty constructorDefines then "" else "/* Type tags */\n" ++ constructorDefines ++ "\n" ]
            ++ [ if String.isEmpty simpleConstantsCode then "" else "/* Constants */\n" ++ simpleConstantsCode ++ "\n" ]
            ++ [ if String.isEmpty complexConstantsCode then "" else "/* Computed values */\n" ++ complexConstantsCode ++ "\n" ]
            ++ [ if String.isEmpty liftedFunctions then "" else "/* Local functions */\n" ++ liftedFunctions ++ "\n" ]
            ++ [ if String.isEmpty userFunctions then "" else "/* Functions */\n" ++ userFunctions ++ "\n" ]
            ++ footer
        )


{-| Generate C header file with extern declarations for a module's exports.
-}
generateTccHeader : Src.Module -> String
generateTccHeader ast =
    let
        moduleName =
            getModuleName ast

        cModuleName =
            getModulePrefix ast

        -- Collect user function names for context
        userFunctionNames =
            collectAllFunctionNames ast.values

        -- Get exported names
        (Src.At _ exports) = ast.exports
        exportedNames =
            case exports of
                Src.Open ->
                    -- Export all values
                    ast.values
                        |> List.map (\(Src.At _ v) -> let (Src.At _ name) = v.name in name)

                Src.Explicit exposed ->
                    exposed
                        |> List.filterMap
                            (\e ->
                                case e of
                                    Src.Lower (Src.At _ name) -> Just name
                                    _ -> Nothing
                            )

        -- Qualify function name
        qualifyName name =
            "elm_" ++ cModuleName ++ "_" ++ name

        -- Generate extern declaration for a value
        generateExtern : Src.Located Src.Value -> Maybe String
        generateExtern (Src.At _ value) =
            let
                (Src.At _ name) = value.name
            in
            if List.member name exportedNames then
                let
                    -- Generate the function to infer return type
                    funcCode = generateUserFunction cModuleName userFunctionNames name value.args value.body
                    -- Extract return type (format: static TYPE elm_NAME...)
                    returnTypeStart = 7 -- "static "
                    returnTypeEnd = String.indexes " elm_" funcCode |> List.head |> Maybe.withDefault 0
                    returnType = String.slice returnTypeStart returnTypeEnd funcCode

                    -- Generate parameter types
                    hasArgs = not (List.isEmpty value.args)
                    params =
                        if hasArgs then
                            let
                                paramStart = String.indexes "(" funcCode |> List.head |> Maybe.map ((+) 1) |> Maybe.withDefault 0
                                paramEnd = String.indexes ")" funcCode |> List.head |> Maybe.withDefault 0
                            in
                            String.slice paramStart paramEnd funcCode
                        else
                            "void"
                in
                Just ("extern " ++ returnType ++ " " ++ qualifyName name ++ "(" ++ params ++ ");")
            else
                Nothing

        externDecls =
            ast.values
                |> List.filterMap generateExtern
                |> String.join "\n"

        -- Generate type tag externs
        typeTagExterns =
            ast.unions
                |> List.concatMap
                    (\(Src.At _ union) ->
                        let
                            (Src.At _ typeName) = union.name
                        in
                        union.ctors
                            |> List.indexedMap
                                (\idx ( Src.At _ ctorName, _ ) ->
                                    "#define TAG_" ++ cModuleName ++ "_" ++ ctorName ++ " " ++ String.fromInt idx
                                )
                    )
                |> String.join "\n"

        header =
            [ "/*"
            , " * Header for " ++ moduleName
            , " * Generated by tcelm"
            , " */"
            , ""
            , "#ifndef ELM_" ++ String.toUpper cModuleName ++ "_H"
            , "#define ELM_" ++ String.toUpper cModuleName ++ "_H"
            , ""
            ]

        footer =
            [ ""
            , "#endif /* ELM_" ++ String.toUpper cModuleName ++ "_H */"
            ]
    in
    String.join "\n"
        (header
            ++ [ if String.isEmpty typeTagExterns then "" else "/* Type tags */\n" ++ typeTagExterns ++ "\n" ]
            ++ [ if String.isEmpty externDecls then "" else "/* Function declarations */\n" ++ externDecls ]
            ++ footer
        )


{-| Generate C code for imports (includes and extern declarations)
-}
generateImportCode : List Src.Import -> String
generateImportCode imports =
    let
        -- Built-in modules that don't need includes (handled by runtime)
        builtinModules =
            [ "Basics", "String", "List", "Maybe", "Result", "Tuple", "Debug", "Char", "Platform", "Bitwise", "Array", "Dict", "Set", "Json.Decode", "Json.Encode" ]

        -- RTEMS modules that map to specific runtime includes
        rtemsModuleMap =
            [ ( "Rtems.Task", "tcelm_task.h" )
            , ( "Rtems.Interrupt", "tcelm_isr.h" )
            , ( "Rtems.Uart", "tcelm_uart.h" )
            , ( "Rtems.Gpio", "tcelm_gpio.h" )
            , ( "Rtems.Sync", "tcelm_semaphore.h" )
            , ( "Rtems.Semaphore", "tcelm_semaphore.h" )
            , ( "Rtems.Mutex", "tcelm_semaphore.h" )
            , ( "Rtems.Events", "tcelm_events.h" )
            , ( "Rtems.Clock", "tcelm_clock.h" )
            , ( "Rtems.Barrier", "tcelm_barrier.h" )
            , ( "Rtems.SMP", "tcelm_smp.h" )
            , ( "Rtems.SPI", "tcelm_spi.h" )
            , ( "Rtems.I2C", "tcelm_i2c.h" )
            , ( "Rtems.ADC", "tcelm_adc.h" )
            , ( "Rtems.DAC", "tcelm_dac.h" )
            , ( "Rtems.Socket", "tcelm_socket.h" )
            , ( "Rtems.Network", "tcelm_socket.h" )
            , ( "Rtems.Timer", "tcelm_timer.h" )
            , ( "Rtems.Channel", "tcelm_channel.h" )
            , ( "Rtems.MVar", "tcelm_mvar.h" )
            , ( "Rtems.Budget", "tcelm_budget.h" )
            , ( "Rtems.Protected", "tcelm_protected.h" )
            , ( "RMS", "tcelm_rms.h" )
            , ( "Rtems.RMS", "tcelm_rms.h" )
            , ( "Platform", "" )
            , ( "Platform.Cmd", "" )
            , ( "Platform.Sub", "" )
            ]

        -- Check if a module needs an include
        getInclude : String -> Maybe String
        getInclude modName =
            rtemsModuleMap
                |> List.filterMap (\( m, h ) -> if m == modName && h /= "" then Just h else Nothing)
                |> List.head

        -- Process each import
        processImport : Src.Import -> Maybe String
        processImport imp =
            let
                (Src.At _ name) = imp.name
            in
            if List.member name builtinModules then
                Nothing
            else
                getInclude name

        -- Generate unique includes
        includes =
            imports
                |> List.filterMap processImport
                |> List.foldr (\h acc -> if List.member h acc then acc else h :: acc) []
                |> List.map (\h -> "#include \"" ++ h ++ "\"")
                |> String.join "\n"

        -- Generate extern declarations for local modules (non-RTEMS, non-builtin)
        isLocalModule : String -> Bool
        isLocalModule name =
            not (List.member name builtinModules)
                && not (String.startsWith "Rtems." name)
                && name /= "RMS"
                && name /= "Platform"

        localModules =
            imports
                |> List.filterMap
                    (\imp ->
                        let
                            (Src.At _ name) = imp.name
                        in
                        if isLocalModule name then
                            Just name
                        else
                            Nothing
                    )

        -- Generate includes for local modules
        -- Convention: MyLib module -> MyLib.h header file
        localIncludes =
            localModules
                |> List.map (\name -> "#include \"" ++ String.replace "." "_" name ++ ".h\"")
                |> String.join "\n"

        -- Combine all includes
        allIncludes =
            [ includes, localIncludes ]
                |> List.filter (not << String.isEmpty)
                |> String.join "\n"
    in
    if String.isEmpty allIncludes then
        ""
    else
        "/* Imports */\n" ++ allIncludes ++ "\n\n"


{-| Generate forward declaration for a standalone function
-}
generateStandaloneForwardDecl : Src.Located Src.Value -> String
generateStandaloneForwardDecl (Src.At _ value) =
    let
        (Src.At _ name) = value.name
        -- Generate the implementation first to infer types
        -- Use empty module prefix for native code (single module)
        implCode = generateUserFunction "" [] name value.args value.body
        -- Extract the signature from the implementation
        -- Format is: static TYPE elm_NAME(PARAMS) {
        signatureEnd = String.indexes "(" implCode |> List.head |> Maybe.withDefault 0
        paramsStart = signatureEnd + 1
        paramsEnd = String.indexes ")" implCode |> List.head |> Maybe.withDefault 0
        returnTypeStart = 7 -- Length of "static "
        returnTypeEnd = String.indexes " elm_" implCode |> List.head |> Maybe.withDefault 0
        returnType = String.slice returnTypeStart returnTypeEnd implCode
        params = String.slice paramsStart paramsEnd implCode
    in
    "static " ++ returnType ++ " elm_" ++ name ++ "(" ++ params ++ ");"


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
                String.join ", " (List.map (\p -> "double elm_" ++ p) paramNames)

        bodyCode =
            generateStandaloneExpr value.body
    in
    String.join "\n"
        [ "static double elm_" ++ name ++ "(" ++ params ++ ") {"
        , "    return " ++ bodyCode ++ ";"
        , "}"
        , ""
        ]


extractMain : Src.Module -> MainValue
extractMain ast =
    ast.values
        |> List.filterMap extractMainValue
        |> List.head
        |> Maybe.withDefault (MainString "Hello from tcelm!")


{-| Extract main value with module context for proper function name prefixing
-}
extractMainWithCtx : String -> List String -> Src.Module -> MainValue
extractMainWithCtx modulePrefix userFunctions ast =
    let
        ctx = { funcPrefix = "main", modulePrefix = modulePrefix, userFunctions = userFunctions }
    in
    ast.values
        |> List.filterMap (extractMainValueWithCtx ctx)
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
        Just (exprToMainValueWithType value.type_ value.body)

    else
        Nothing


{-| Extract main value with context for proper function prefixing
-}
extractMainValueWithCtx : ExprCtx -> Src.Located Src.Value -> Maybe MainValue
extractMainValueWithCtx ctx (Src.At _ value) =
    let
        (Src.At _ name) =
            value.name
    in
    if name == "main" then
        Just (exprToMainValueWithTypeCtx ctx value.type_ value.body)

    else
        Nothing


{-| Convert an expression to a MainValue, using type annotation if available
-}
exprToMainValueWithType : Maybe Src.Type -> Src.Expr -> MainValue
exprToMainValueWithType maybeType expr =
    let
        baseValue =
            exprToMainValue expr
    in
    -- If type annotation says String, override int inference
    if Shared.mainTypeIsString maybeType then
        case baseValue of
            MainExpr "int" cCode ->
                MainExpr "const char *" cCode

            _ ->
                baseValue
    else
        baseValue


{-| Convert an expression to a MainValue with context for proper function prefixing
-}
exprToMainValueWithTypeCtx : ExprCtx -> Maybe Src.Type -> Src.Expr -> MainValue
exprToMainValueWithTypeCtx ctx maybeType expr =
    let
        baseValue =
            exprToMainValueWithCtx ctx expr
    in
    -- If type annotation says String, override int inference
    if Shared.mainTypeIsString maybeType then
        case baseValue of
            MainExpr "int" cCode ->
                MainExpr "const char *" cCode

            _ ->
                baseValue
    else
        baseValue


{-| Convert an expression to a MainValue
-}
exprToMainValue : Src.Expr -> MainValue
exprToMainValue locatedExpr =
    let
        (Src.At region expr) =
            locatedExpr
    in
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
            -- Fallback: try to generate as expression and infer type
            -- Pass the original located expression directly to avoid reconstructing AST nodes
            let
                cCode = generateStandaloneExpr locatedExpr
                -- Infer type from generated code patterns
                -- Check for string literals inside ternary/case expressions (e.g., ? "string" :)
                hasStringLiteralInBranch =
                    String.contains "? \"" cCode
                        || String.contains ": \"" cCode
                        || String.contains "const char *elm_" cCode

                inferredType =
                    if String.contains "elm_str_" cCode
                        || String.contains "elm_from_int" cCode
                        || String.contains "elm_from_float" cCode
                        || String.startsWith "\"" cCode
                        || hasStringLiteralInBranch then
                        "const char *"
                    else
                        "int"
            in
            MainExpr inferredType cCode


{-| Convert an expression to a MainValue with context for proper function prefixing
-}
exprToMainValueWithCtx : ExprCtx -> Src.Expr -> MainValue
exprToMainValueWithCtx ctx locatedExpr =
    let
        (Src.At region expr) =
            locatedExpr
    in
    case expr of
        Src.Str s ->
            MainString s

        Src.Int n ->
            MainInt n

        Src.Negate innerExpr ->
            case exprToMainValueWithCtx ctx innerExpr of
                MainInt n ->
                    MainInt -n

                other ->
                    other

        Src.Binops pairs finalExpr ->
            -- Generate C code for binary operations with context
            let
                cCode = generateStandaloneBinopsWithCtx ctx pairs finalExpr

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
                cCode = generateStandaloneIfWithCtx ctx branches elseExpr
                inferredType =
                    case branches of
                        ( _, thenExpr ) :: _ ->
                            case exprToMainValueWithCtx ctx thenExpr of
                                MainString _ -> "const char *"
                                MainInt _ -> "int"
                                MainExpr t _ -> t
                        [] ->
                            case exprToMainValueWithCtx ctx elseExpr of
                                MainString _ -> "const char *"
                                MainInt _ -> "int"
                                MainExpr t _ -> t
            in
            MainExpr inferredType cCode

        Src.Let defs body ->
            -- Infer type from body expression
            let
                cCode = generateStandaloneLetWithCtx ctx defs body
                inferredType =
                    case exprToMainValueWithCtx ctx body of
                        MainString _ -> "const char *"
                        MainInt _ -> "int"
                        MainExpr t _ -> t
            in
            MainExpr inferredType cCode

        _ ->
            -- Fallback: try to generate as expression and infer type with context
            let
                cCode = generateStandaloneExprWithCtx ctx locatedExpr
                -- Infer type from generated code patterns
                hasStringLiteralInBranch =
                    String.contains "? \"" cCode
                        || String.contains ": \"" cCode
                        || String.contains "const char *elm_" cCode

                inferredType =
                    if String.contains "elm_str_" cCode
                        || String.contains "elm_from_int" cCode
                        || String.contains "elm_from_float" cCode
                        || String.startsWith "\"" cCode
                        || hasStringLiteralInBranch then
                        "const char *"
                    else
                        "int"
            in
            MainExpr inferredType cCode


{-| Extract accessor field name from an expression, if it's an accessor.
    Used by buildPipe to handle accessor patterns without complex nested matching.
    Uses isAccessor helper to avoid nested pattern matching issues during self-hosting.
-}
extractAccessor : Src.Expr -> Maybe String
extractAccessor locatedExpr =
    let
        (Src.At _ innerExpr) =
            locatedExpr
    in
    extractAccessorInner innerExpr


{-| Helper to extract accessor from inner expression.
-}
extractAccessorInner : Src.Expr_ -> Maybe String
extractAccessorInner innerExpr =
    case innerExpr of
        Src.Accessor fieldName ->
            Just fieldName

        _ ->
            Nothing


{-| Generate C bindings from a lambda pattern.
    Returns (bindings, element accessor) where:
    - bindings: C code to bind pattern variables (e.g., "double elm_x = __elem._0;")
    - element accessor: how to access the element (e.g., "__lst.data[__i]")

    elemExpr is the C expression for the current element (e.g., "__lst.data[__i]")
-}
generateLambdaPatternBindings : String -> Src.Pattern -> ( String, String )
generateLambdaPatternBindings elemExpr locatedPattern =
    let
        (Src.At _ pattern) = locatedPattern
    in
    case pattern of
        -- Simple variable: \x -> ...
        Src.PVar varName ->
            ( "typeof(" ++ elemExpr ++ ") elm_" ++ varName ++ " = " ++ elemExpr ++ ";", elemExpr )

        -- Wildcard: \_ -> ...
        Src.PAnything ->
            ( "", elemExpr )

        -- Tuple pattern: \(a, b) -> ...
        Src.PTuple (Src.At _ first) (Src.At _ second) rest ->
            let
                elemType = if List.isEmpty rest then "elm_tuple2_t" else "elm_tuple3_t"
                tupleDecl = elemType ++ " __telem = *(" ++ elemType ++ "*)&(" ++ elemExpr ++ ");"

                bindOne idx pat =
                    case pat of
                        Src.PVar vn ->
                            "double elm_" ++ vn ++ " = __telem._" ++ String.fromInt idx ++ ".d;"
                        Src.PAnything ->
                            ""
                        -- Nested constructor: \(_, Src.At _ x) -> ...
                        Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ] ->
                            "double elm_" ++ innerName ++ " = ((elm_union_t*)&__telem._" ++ String.fromInt idx ++ ")->data.num;"
                        Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ] ->
                            "double elm_" ++ innerName ++ " = ((elm_union_t*)&__telem._" ++ String.fromInt idx ++ ")->data.num;"
                        _ ->
                            "/* unsupported tuple element pattern */"

                firstBinding = bindOne 0 first
                secondBinding = bindOne 1 second
                restBindings = rest
                    |> List.indexedMap (\i (Src.At _ p) -> bindOne (i + 2) p)

                allBindings = [ tupleDecl, firstBinding, secondBinding ] ++ restBindings
                    |> List.filter (\s -> s /= "")
                    |> String.join " "
            in
            ( allBindings, elemExpr )

        -- Constructor pattern: \(Src.At _ x) -> ...
        Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ] ->
            ( "typeof(((elm_union_t)" ++ elemExpr ++ ").data) elm_" ++ innerName ++ " = ((elm_union_t)" ++ elemExpr ++ ").data;", elemExpr )

        -- Qualified constructor pattern: \(Src.At _ x) -> ...
        Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ] ->
            ( "typeof(((elm_union_t)" ++ elemExpr ++ ").data) elm_" ++ innerName ++ " = ((elm_union_t)" ++ elemExpr ++ ").data;", elemExpr )

        -- Record pattern: \{ x, y } -> ...
        Src.PRecord fieldNames ->
            let
                bindings = fieldNames
                    |> List.map (\(Src.At _ fn) -> "typeof(" ++ elemExpr ++ "." ++ fn ++ ") elm_" ++ fn ++ " = " ++ elemExpr ++ "." ++ fn ++ ";")
                    |> String.join " "
            in
            ( bindings, elemExpr )

        _ ->
            ( "/* unsupported lambda pattern */", elemExpr )


{-| Generate inline lambda body with bindings.
    Used for List.map, List.filter, etc. when the lambda pattern is complex.

    patterns: the lambda's pattern list
    body: the lambda body expression
    elemExpr: C expression for the current element
-}
generateInlineLambdaBody : List Src.Pattern -> Src.Expr -> String -> String
generateInlineLambdaBody patterns body elemExpr =
    case patterns of
        [ pat ] ->
            let
                ( bindings, _ ) = generateLambdaPatternBindings elemExpr pat
                bodyCode = generateStandaloneExpr body
            in
            if bindings == "" then
                bodyCode
            else
                "({ " ++ bindings ++ " " ++ bodyCode ++ "; })"

        -- Two patterns (e.g., \i x -> ... for indexedMap)
        [ pat1, pat2 ] ->
            let
                ( bindings1, _ ) = generateLambdaPatternBindings "__idx" pat1
                ( bindings2, _ ) = generateLambdaPatternBindings elemExpr pat2
                bodyCode = generateStandaloneExpr body
                allBindings = [ bindings1, bindings2 ]
                    |> List.filter (\s -> s /= "" && not (String.contains "unsupported" s))
                    |> String.join " "
            in
            if allBindings == "" then
                bodyCode
            else
                "({ " ++ allBindings ++ " " ++ bodyCode ++ "; })"

        _ ->
            "/* unsupported multi-pattern lambda */ 0"


{-| Generate binary operations with function context (for nested Let handling)
-}
generateStandaloneBinopsWithCtx : ExprCtx -> List ( Src.Expr, Src.Located String ) -> Src.Expr -> String
generateStandaloneBinopsWithCtx ctx pairs finalExpr =
    -- Thread the context through to generateStandaloneBinopsImpl
    generateStandaloneBinopsImpl ctx pairs finalExpr


{-| Generate standalone C code for binary operations (no runtime needed)
    Outputs as flat expression relying on C operator precedence (matches Elm for arithmetic)
-}
generateStandaloneBinops : List ( Src.Expr, Src.Located String ) -> Src.Expr -> String
generateStandaloneBinops pairs finalExpr =
    -- Use default context for backward compatibility
    generateStandaloneBinopsImpl Shared.defaultExprCtx pairs finalExpr


{-| Internal implementation with context threading
-}
generateStandaloneBinopsImpl : ExprCtx -> List ( Src.Expr, Src.Located String ) -> Src.Expr -> String
generateStandaloneBinopsImpl ctx pairs finalExpr =
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

        -- Helper to infer C type from an expression string
        inferCTypeFromExpr exprStr =
            if String.endsWith ".ctors" exprStr then
                "elm_list_t"
            else if String.endsWith ".values" exprStr then
                "elm_list_t"
            else if String.endsWith ".unions" exprStr then
                "elm_list_t"
            else if String.endsWith ".args" exprStr then
                "elm_list_t"
            else if String.contains "elm_list_t" exprStr then
                "elm_list_t"
            else
                "double"
    in
    if isForwardPipe then
        -- Pipe operator: a |> f |> g becomes g(f(a))
        -- pairs is [(a, |>), (f, |>)], finalExpr is g
        -- Result: finalExpr(pairs[n-1](pairs[n-2](...pairs[0])))
        let
            firstArg =
                case pairs of
                    ( expr, _ ) :: _ ->
                        generateStandaloneExprWithCtx ctx expr

                    [] ->
                        generateStandaloneExprWithCtx ctx finalExpr

            -- Get expressions for the "functions" (can be functions or accessors)
            functionExprs =
                (List.drop 1 pairs |> List.map Tuple.first)
                    ++ [ finalExpr ]

            -- Build nested calls, handling accessors specially
            buildPipe : String -> List Src.Expr -> String
            buildPipe arg exprs =
                case exprs of
                    [] ->
                        arg

                    first :: rest ->
                        -- Check for accessor pattern using helper
                        case extractAccessor first of
                            Just fieldName ->
                                -- Accessor: arg.field
                                buildPipe (arg ++ "." ++ fieldName) rest

                            Nothing ->
                                -- Check for partial application (Call with args that needs pipe arg)
                                case first of
                                    Src.At pos (Src.Call fn partialArgs) ->
                                        -- Partial application: use specialized handler that knows about pipe arg
                                        let
                                            fullCall = generateStandaloneCallWithPipeArgCtx ctx fn partialArgs arg
                                        in
                                        buildPipe fullCall rest

                                    Src.At pos (Src.VarQual varType moduleName funcName) ->
                                        -- Qualified function in pipe: use generateStandaloneCall for proper inlining
                                        -- This handles List.head, List.filter, etc. correctly
                                        let
                                            -- Create a fake argument expression from the pipe arg string
                                            -- We wrap the arg in a Var so generateStandaloneCall can process it
                                            -- Actually, generateStandaloneCall expects Src.Expr, but we have a string
                                            -- Use context-aware version for proper module prefixing
                                            fullCall = generateStandaloneCallWithPipeArgCtx ctx first [] arg
                                        in
                                        buildPipe fullCall rest

                                    _ ->
                                        -- Regular function call
                                        buildPipe (generateStandaloneExprWithCtx ctx first ++ "(" ++ arg ++ ")") rest
        in
        buildPipe firstArg functionExprs

    else if isBackwardPipe then
        -- Backward pipe operator: f <| g <| a becomes f(g(a))
        -- pairs is [(f, <|), (g, <|)], finalExpr is a
        -- Result: pairs[0](pairs[1](...finalExpr))
        let
            arg =
                generateStandaloneExprWithCtx ctx finalExpr

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

                    first :: rest ->
                        -- Check for accessor pattern using helper
                        case extractAccessor first of
                            Just fieldName ->
                                -- Accessor: innerArg.field
                                buildBackPipe (innerArg ++ "." ++ fieldName) rest

                            Nothing ->
                                -- Regular function call
                                buildBackPipe (generateStandaloneExprWithCtx ctx first ++ "(" ++ innerArg ++ ")") rest
        in
        buildBackPipe arg functionExprs

    else if isStringConcat && List.length allStrings == List.length allExprs then
        -- All operands are string literals - concatenate at compile time
        "\"" ++ escapeC (String.concat allStrings) ++ "\""

    else
        -- Normal binary operation
        let
            -- Check for power operator (needs special handling)
            isPowerOp =
                List.all (\( _, Src.At _ op ) -> op == "^") pairs

            -- Check for list cons operator (::)
            isListCons =
                List.all (\( _, Src.At _ op ) -> op == "::") pairs

            -- Build list cons expression: h :: t creates list with h at front
            buildConsExpr exprs final =
                case exprs of
                    [] ->
                        final

                    ( headExpr, _ ) :: rest ->
                        let
                            tailCode =
                                buildConsExpr rest final

                            headCode =
                                generateStandaloneExprWithCtx ctx headExpr
                        in
                        "({ elm_list_t __cons_result = " ++ tailCode ++ "; for(int __i = __cons_result.length; __i > 0; __i--) __cons_result.data[__i] = __cons_result.data[__i-1]; __cons_result.data[0] = " ++ headCode ++ "; __cons_result.length++; __cons_result; })"

            -- Check if expression looks like a string
            isStringExpr (Src.At _ e) =
                case e of
                    Src.Str _ ->
                        True

                    Src.Call (Src.At _ (Src.VarQual _ "String" _)) _ ->
                        True

                    _ ->
                        False

            -- Check if this is a string equality comparison (a == b where both are strings)
            isStringEquality =
                case pairs of
                    [ ( leftExpr, Src.At _ "==" ) ] ->
                        isStringExpr leftExpr || isStringExpr finalExpr

                    [ ( leftExpr, Src.At _ "/=" ) ] ->
                        isStringExpr leftExpr || isStringExpr finalExpr

                    _ ->
                        False

            -- Build list of all terms and operators
            buildTerms ps =
                case ps of
                    [] ->
                        []

                    ( expr, Src.At _ op ) :: rest ->
                        ( generateStandaloneExprWithCtx ctx expr, op ) :: buildTerms rest

            terms = buildTerms pairs
            finalTerm = generateStandaloneExprWithCtx ctx finalExpr

            -- Convert Elm operator to C operator
            elmOpToC op =
                case op of
                    "//" -> "/"
                    "/=" -> "!="
                    "++" -> "/* use elm_str_append */"
                    _ -> op

            -- Check if this is runtime string concatenation
            isRuntimeStringConcat =
                List.all (\( _, Src.At _ op ) -> op == "++") pairs

            -- Build string concatenation chain using elm_str_append
            buildStringConcat ts final =
                case ts of
                    [] ->
                        final

                    [ ( term, _ ) ] ->
                        "elm_str_append(" ++ term ++ ", " ++ final ++ ")"

                    ( term, _ ) :: rest ->
                        "elm_str_append(" ++ term ++ ", " ++ buildStringConcat rest final ++ ")"

            -- Build the expression string for regular operators
            buildExpr ts =
                case ts of
                    [] ->
                        finalTerm

                    ( term, op ) :: rest ->
                        term ++ " " ++ elmOpToC op ++ " " ++ buildExpr rest

            -- Build power expression (right-associative: a ^ b ^ c = a ^ (b ^ c))
            buildPowerExpr exprList =
                case exprList of
                    [] ->
                        finalTerm

                    [ ( term, _ ) ] ->
                        "elm_pow(" ++ term ++ ", " ++ finalTerm ++ ")"

                    ( term, _ ) :: rest ->
                        "elm_pow(" ++ term ++ ", " ++ buildPowerExpr rest ++ ")"
        in
        if isPowerOp then
            buildPowerExpr terms

        else if isListCons then
            -- List cons: h :: t builds list with h at front of t
            buildConsExpr pairs finalTerm

        else if isRuntimeStringConcat then
            -- Runtime string concatenation using elm_str_append
            buildStringConcat terms finalTerm

        else if isStringEquality then
            -- String comparison using strcmp
            case pairs of
                [ ( leftExpr, Src.At _ "==" ) ] ->
                    "(strcmp(" ++ generateStandaloneExprWithCtx ctx leftExpr ++ ", " ++ finalTerm ++ ") == 0)"

                [ ( leftExpr, Src.At _ "/=" ) ] ->
                    "(strcmp(" ++ generateStandaloneExprWithCtx ctx leftExpr ++ ", " ++ finalTerm ++ ") != 0)"

                _ ->
                    "(" ++ buildExpr terms ++ ")"

        else
            -- Check if this might be a union type comparison
            -- If either side contains .tag or .country/.type/.class patterns, compare tags
            let
                leftStr = List.head terms |> Maybe.map Tuple.first |> Maybe.withDefault finalTerm
                isUnionComparison =
                    -- Check for field access patterns that are likely union types
                    (String.contains ".country" leftStr || String.contains ".country" finalTerm)
                    || (String.contains ".type" leftStr || String.contains ".type" finalTerm)
                    || (String.contains ".class" leftStr || String.contains ".class" finalTerm)
                    || (String.contains ".kind" leftStr || String.contains ".kind" finalTerm)
                    || (String.contains ".status" leftStr || String.contains ".status" finalTerm)
                    || (String.contains ".state" leftStr || String.contains ".state" finalTerm)
                    -- Also check for direct union comparisons
                    || (String.contains "elm_union_t" leftStr || String.contains "elm_union_t" finalTerm)
            in
            if isUnionComparison then
                case pairs of
                    [ ( leftExpr, Src.At _ "==" ) ] ->
                        "((" ++ generateStandaloneExprWithCtx ctx leftExpr ++ ").tag == (" ++ finalTerm ++ ").tag)"

                    [ ( leftExpr, Src.At _ "/=" ) ] ->
                        "((" ++ generateStandaloneExprWithCtx ctx leftExpr ++ ").tag != (" ++ finalTerm ++ ").tag)"

                    _ ->
                        "(" ++ buildExpr terms ++ ")"
            else
                "(" ++ buildExpr terms ++ ")"


{-| Generate standalone C code for a single expression (no runtime)
-}
generateStandaloneExpr : Src.Expr -> String
generateStandaloneExpr expr =
    generateStandaloneExprWithCtx Shared.defaultExprCtx expr


{-| Generate standalone C code with function context for lifted local functions
-}
generateStandaloneExprWithCtx : ExprCtx -> Src.Expr -> String
generateStandaloneExprWithCtx ctx (Src.At _ expr) =
    -- Try literal generation first (delegates to Codegen.Expr)
    case Expr.generateLiteral expr of
        Just litStr ->
            litStr

        Nothing ->
            generateStandaloneExprWithCtxImpl ctx expr


{-| Implementation of expression generation for non-literal cases
-}
generateStandaloneExprWithCtxImpl : ExprCtx -> Src.Expr_ -> String
generateStandaloneExprWithCtxImpl ctx expr =
    case expr of
        Src.Negate inner ->
            "(-" ++ generateStandaloneExprWithCtx ctx inner ++ ")"

        Src.Binops pairs final ->
            generateStandaloneBinopsWithCtx ctx pairs final

        Src.If branches elseExpr ->
            generateStandaloneIfWithCtx ctx branches elseExpr

        Src.Let defs body ->
            generateStandaloneLetWithPrefix ctx.funcPrefix defs body

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
                        generateStandaloneCallWithCtx ctx fn args

                _ ->
                    generateStandaloneCallWithCtx ctx fn args

        Src.Var varType name ->
            case ( varType, name ) of
                ( Src.CapVar, "True" ) ->
                    "1"

                ( Src.CapVar, "False" ) ->
                    "0"

                ( Src.CapVar, "Nothing" ) ->
                    -- Built-in Maybe Nothing constructor
                    makeUnionCtor "TAG_Nothing" "0"

                ( Src.CapVar, _ ) ->
                    -- Constructor - generate union tag for nullary constructor
                    makeUnionCtor ("TAG_" ++ name) "0"

                _ ->
                    -- Check if this is a user-defined function that needs module prefix
                    if List.member name ctx.userFunctions && not (String.isEmpty ctx.modulePrefix) then
                        "elm_" ++ ctx.modulePrefix ++ "_" ++ name
                    else
                        "elm_" ++ name

        Src.VarQual varType qualModuleName name ->
            case varType of
                Src.CapVar ->
                    -- Qualified constructor - generate tag value
                    -- For nullary constructors like Src.LowVar, Src.CapVar
                    let
                        prefixedModuleName = String.replace "." "_" qualModuleName
                    in
                    makeUnionCtor ("TAG_" ++ prefixedModuleName ++ "_" ++ name) "0"

                Src.LowVar ->
                    -- Qualified variable - reference as function/value
                    let
                        prefixedModuleName = String.replace "." "_" qualModuleName
                    in
                    "elm_" ++ prefixedModuleName ++ "_" ++ name

        Src.Case scrutinee branches ->
            generateStandaloneCaseWithCtx ctx scrutinee branches

        Src.Tuple first second rest ->
            -- Generate tuple as compound struct literal
            let
                elements =
                    first :: second :: rest

                numElements =
                    List.length elements

                tupleType =
                    if numElements == 2 then
                        "elm_tuple2_t"
                    else if numElements == 3 then
                        "elm_tuple3_t"
                    else
                        -- Fallback for larger tuples (rare)
                        "struct { " ++ (List.indexedMap (\i _ -> "int _" ++ String.fromInt i) elements |> String.join "; ") ++ "; }"

                values =
                    List.map generateStandaloneExpr elements
                        |> String.join ", "
            in
            "((" ++ tupleType ++ "){" ++ values ++ "})"

        Src.Record fields ->
            -- Generate record as compound struct literal with named fields
            let
                -- Check if field value is a lambda
                isLambdaField : Src.Expr -> Bool
                isLambdaField (Src.At _ fv) =
                    case fv of
                        Src.Lambda _ _ -> True
                        _ -> False

                -- Count parameters in lambda (including record field expansion)
                lambdaParamCount : Src.Expr -> Int
                lambdaParamCount (Src.At _ fv) =
                    case fv of
                        Src.Lambda patterns _ ->
                            patterns
                                |> List.map
                                    (\(Src.At _ p) ->
                                        case p of
                                            Src.PRecord recFields -> List.length recFields
                                            _ -> 1
                                    )
                                |> List.sum
                        _ -> 0

                -- Infer field type from the value expression
                inferFieldType : Src.Expr -> String
                inferFieldType fieldValue =
                    if isLambdaField fieldValue then
                        -- Generate function pointer type
                        let
                            numParams = lambdaParamCount fieldValue
                            paramTypes = List.repeat numParams "double" |> String.join ", "
                        in
                        "double (*" ++ ")(" ++ paramTypes ++ ")"
                    else
                        let
                            valueStr = generateStandaloneExpr fieldValue
                        in
                        if String.startsWith "\"" valueStr then
                            "const char *"
                        else if String.contains "elm_str_" valueStr || String.contains "elm_from_" valueStr then
                            "const char *"
                        else if String.contains "TAG_" valueStr || String.contains "elm_union_t" valueStr then
                            "elm_union_t"
                        else
                            "double"

                -- Generate field type (name goes in the middle for function pointers)
                generateFieldDef : ( Src.Located String, Src.Expr ) -> String
                generateFieldDef ( Src.At _ fieldName, fieldValue ) =
                    if isLambdaField fieldValue then
                        let
                            numParams = lambdaParamCount fieldValue
                            paramTypes = List.repeat numParams "double" |> String.join ", "
                        in
                        "double (*" ++ fieldName ++ ")(" ++ paramTypes ++ ")"
                    else
                        inferFieldType fieldValue ++ " " ++ fieldName

                fieldDefs =
                    fields
                        |> List.map generateFieldDef
                        |> String.join "; "

                -- Generate field value - for lambdas, generate __LAMBDA_recordName_fieldName__ marker
                generateFieldValue : ( Src.Located String, Src.Expr ) -> String
                generateFieldValue ( Src.At _ fieldName, fieldValue ) =
                    if isLambdaField fieldValue then
                        "." ++ fieldName ++ " = __LAMBDA_" ++ fieldName ++ "__"
                    else
                        "." ++ fieldName ++ " = " ++ generateStandaloneExpr fieldValue

                fieldValues =
                    fields
                        |> List.map generateFieldValue
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

        Src.Op opName ->
            -- Operator used as a value - generate a lambda-like placeholder
            -- e.g., (+) becomes a function that adds two numbers
            "/* op " ++ opName ++ " */"

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

        Src.List elements ->
            -- Generate list literal
            let
                numElements =
                    List.length elements

                isUnionExpr (Src.At _ e) =
                    case e of
                        Src.Var Src.CapVar _ -> True
                        Src.VarQual Src.CapVar _ _ -> True
                        Src.Call (Src.At _ (Src.Var Src.CapVar _)) _ -> True
                        Src.Call (Src.At _ (Src.VarQual Src.CapVar _ _)) _ -> True
                        -- Variables with AST-related names are likely unions
                        Src.Var Src.LowVar name ->
                            String.contains "Expr" name ||
                            String.contains "expr" name ||
                            String.contains "Pat" name ||
                            String.contains "pat" name ||
                            String.contains "Type" name
                        _ -> False

                -- Check if expression is a record
                isRecordExpr (Src.At _ e) =
                    case e of
                        Src.Record _ -> True
                        _ -> False

                -- Check if first element is a record (determines list type)
                firstIsRecord =
                    case elements of
                        first :: _ -> isRecordExpr first
                        [] -> False

                -- Extract record type from first element if it's a record
                extractRecordType elemExpr =
                    let
                        genExpr = generateStandaloneExpr elemExpr
                    in
                    if String.startsWith "((struct {" genExpr then
                        let
                            startMarker = "((struct {"
                            afterStart = String.dropLeft (String.length startMarker) genExpr
                            endIdx = String.indexes "})" afterStart |> List.head |> Maybe.withDefault 0
                            fieldDefs = String.left endIdx afterStart
                        in
                        "struct {" ++ fieldDefs ++ "}"
                    else
                        ""

                recordType =
                    case elements of
                        first :: _ -> extractRecordType first
                        [] -> ""

                -- For record lists, extract just the initializer part of each record
                extractRecordInitializer genExpr =
                    if String.startsWith "((struct {" genExpr then
                        let
                            initMarker = "){"
                            initIdx = String.indexes initMarker genExpr |> List.head |> Maybe.withDefault 0
                            afterInit = String.dropLeft (initIdx + String.length initMarker) genExpr
                            -- Remove trailing }))
                            initLen = String.length afterInit - 2
                        in
                        "{" ++ String.left initLen afterInit ++ "}"
                    else
                        genExpr

                wrapDataElement elemExpr =
                    let
                        genExpr = generateStandaloneExpr elemExpr
                        -- Check for record first (struct) - it may contain union fields but is not a union
                        isRecord = isRecordExpr elemExpr || String.startsWith "((struct {" genExpr
                        -- Only check for union if not a record
                        isUnion = not isRecord && (isUnionExpr elemExpr ||
                                  String.startsWith "((elm_union_t)" genExpr ||
                                  String.contains "elm_union_t" genExpr)
                    in
                    if isRecord then
                        -- For record lists, just output the initializer
                        extractRecordInitializer genExpr
                    else if isUnion then
                        "{.u = " ++ genExpr ++ "}"
                    else if String.startsWith "\"" genExpr then
                        "{.str = " ++ genExpr ++ "}"
                    else
                        -- Numeric values need to be wrapped in {.d = ...} for elm_data_t union
                        "{.d = " ++ genExpr ++ "}"

                values =
                    List.map wrapDataElement elements
                        |> String.join ", "
            in
            if numElements == 0 then
                "((elm_list_t){ .length = 0 })"
            else if firstIsRecord && recordType /= "" then
                -- For lists of records, use special marker format
                "/*RECORD_LIST:" ++ recordType ++ "*/ { " ++ values ++ " } /*END_RECORD_LIST:" ++ String.fromInt numElements ++ "*/"
            else
                "((elm_list_t){ .length = " ++ String.fromInt numElements ++ ", .data = { " ++ values ++ " } })"

        _ ->
            "/* unsupported expr */ 0"


{-| Generate standalone C code for function calls (context-aware)
-}
generateStandaloneCallWithCtx : ExprCtx -> Src.Expr -> List Src.Expr -> String
generateStandaloneCallWithCtx ctx fn args =
    -- Delegate to implementation
    generateStandaloneCallImpl ctx fn args


{-| Generate standalone C code for function calls (backward compatible)
-}
generateStandaloneCall : Src.Expr -> List Src.Expr -> String
generateStandaloneCall fn args =
    generateStandaloneCallImpl Shared.defaultExprCtx fn args


{-| Implementation of standalone call generation with context
-}
generateStandaloneCallImpl : ExprCtx -> Src.Expr -> List Src.Expr -> String
generateStandaloneCallImpl ctx fn args =
    -- First try the extracted builtin handlers
    case Builtins.generateBuiltinCall generateStandaloneExpr ctx fn args of
        Just result ->
            result

        Nothing ->
            -- Fall back to remaining handlers
            generateStandaloneCallImplFallback ctx fn args


{-| Fallback implementation for builtins not yet migrated to Builtins module
-}
generateStandaloneCallImplFallback : ExprCtx -> Src.Expr -> List Src.Expr -> String
generateStandaloneCallImplFallback ctx fn args =
    -- Handle built-in functions not yet migrated to Codegen.Builtins
    -- Note: Basic module functions, Tuple.*, Char.*, and simple String.* functions
    -- are now handled by Builtins.generateBuiltinCall (called first in generateStandaloneCallImpl).
    -- Complex String functions that need lambda handling (any, all, foldl, foldr, filter, map) remain here.
    case fn of
        -- Note: Most built-in functions are now handled by Builtins.generateBuiltinCall
        -- including: String.*, List.*, Maybe.*, Result.*, Debug.*, Bitwise.*, Basics.*, Char.*, Tuple.*
        -- and built-in constructors: Just, Ok, Err

        _ ->
            -- Check if this is a constructor call (CapVar) vs function call (LowVar)
            case fn of
                Src.At _ (Src.Var Src.CapVar ctorName) ->
                    -- Unqualified constructor call: Ctor arg1 arg2
                    let
                        -- Check if AST expression is a record literal or call to function returning record
                        isRecordLikeExpr (Src.At _ e) =
                            case e of
                                Src.Record _ -> True
                                -- Function call might return a record if name ends with known patterns
                                Src.Call (Src.At _ (Src.Var Src.LowVar name)) _ ->
                                    -- Check for common record-returning function name patterns
                                    String.contains "Region" name || String.contains "region" name
                                        || String.contains "Position" name || String.contains "position" name
                                Src.Call (Src.At _ (Src.VarQual Src.LowVar _ name)) _ ->
                                    String.contains "Region" name || String.contains "region" name
                                        || String.contains "Position" name || String.contains "position" name
                                _ -> False

                        -- Check if generated expression is a struct value
                        isStructValue exprStr =
                            String.startsWith "((struct {" exprStr

                        -- Wrap each argument as elm_union_t if it's a primitive or string
                        wrapAsUnion argExpr =
                            let
                                argStr = generateStandaloneExprWithCtx ctx argExpr
                            in
                            if isUnionValue argStr then
                                argStr
                            else if isStringValue argStr then
                                -- Wrap string as union with tag 0 (generic)
                                "((elm_union_t){0, {.str = " ++ argStr ++ "}, 0})"
                            else if isRecordLikeExpr argExpr || isStructValue argStr then
                                -- Wrap struct/record as union using .ptr field
                                -- Need to allocate on heap since struct is a temporary
                                "((elm_union_t){0, {.ptr = (void*)&((" ++ argStr ++ "))}, 0})"
                            else
                                -- Wrap primitive as union with tag 0 (generic)
                                "((elm_union_t){0, {.num = " ++ argStr ++ "}, 0})"

                        argStrs =
                            List.map wrapAsUnion args
                    in
                    case argStrs of
                        [] ->
                            -- Nullary constructor - use inline form
                            makeUnionCtor ("TAG_" ++ ctorName) "0"
                        [ single ] ->
                            -- Single arg constructor - call generated function
                            "elm_" ++ ctorName ++ "(" ++ single ++ ")"
                        [ first, second ] ->
                            -- Two arg constructor - call generated function with both args
                            "elm_" ++ ctorName ++ "(" ++ first ++ ", " ++ second ++ ")"
                        _ ->
                            -- More than two args - not currently supported
                            "/* constructor with >2 args not supported */ " ++ makeUnionCtor ("TAG_" ++ ctorName) "0"

                Src.At _ (Src.VarQual Src.CapVar moduleName ctorName) ->
                    -- Qualified constructor call: Module.Ctor arg1 arg2
                    let
                        -- Check if AST expression is a record literal or call to function returning record
                        isRecordLikeExpr (Src.At _ e) =
                            case e of
                                Src.Record _ -> True
                                -- Function call might return a record if name ends with known patterns
                                Src.Call (Src.At _ (Src.Var Src.LowVar name)) _ ->
                                    String.contains "Region" name || String.contains "region" name
                                        || String.contains "Position" name || String.contains "position" name
                                Src.Call (Src.At _ (Src.VarQual Src.LowVar _ name)) _ ->
                                    String.contains "Region" name || String.contains "region" name
                                        || String.contains "Position" name || String.contains "position" name
                                _ -> False

                        -- Check if generated expression is a struct value
                        isStructValue exprStr =
                            String.startsWith "((struct {" exprStr

                        -- Wrap each argument as elm_union_t if it's a primitive or string
                        wrapAsUnion argExpr =
                            let
                                argStr = generateStandaloneExprWithCtx ctx argExpr
                            in
                            if isUnionValue argStr then
                                argStr
                            else if isStringValue argStr then
                                -- Wrap string as union with tag 0 (generic)
                                "((elm_union_t){0, {.str = " ++ argStr ++ "}, 0})"
                            else if isRecordLikeExpr argExpr || isStructValue argStr then
                                -- Wrap struct/record as union using .ptr field
                                "((elm_union_t){0, {.ptr = (void*)&((" ++ argStr ++ "))}, 0})"
                            else
                                -- Wrap primitive as union with tag 0 (generic)
                                "((elm_union_t){0, {.num = " ++ argStr ++ "}, 0})"

                        argStrs =
                            List.map wrapAsUnion args
                    in
                    case argStrs of
                        [] ->
                            -- Nullary constructor - use inline form
                            makeUnionCtor ("TAG_" ++ moduleName ++ "_" ++ ctorName) "0"
                        [ single ] ->
                            -- Single arg constructor - call generated function
                            "elm_" ++ moduleName ++ "_" ++ ctorName ++ "(" ++ single ++ ")"
                        [ first, second ] ->
                            -- Two arg constructor - call generated function with both args
                            "elm_" ++ moduleName ++ "_" ++ ctorName ++ "(" ++ first ++ ", " ++ second ++ ")"
                        _ ->
                            -- More than two args - not currently supported
                            "/* constructor with >2 args not supported */ " ++ makeUnionCtor ("TAG_" ++ moduleName ++ "_" ++ ctorName) "0"

                _ ->
                    -- Regular function call
                    let
                        fnName =
                            case fn of
                                Src.At _ (Src.Var _ name) ->
                                    -- Check if this is a user-defined function that needs module prefix
                                    if List.member name ctx.userFunctions && not (String.isEmpty ctx.modulePrefix) then
                                        "elm_" ++ ctx.modulePrefix ++ "_" ++ name
                                    else
                                        "elm_" ++ name

                                Src.At _ (Src.VarQual _ qualModuleName funcName) ->
                                    -- Qualified function: Module.function -> elm_Module_function
                                    let
                                        prefixedModuleName = String.replace "." "_" qualModuleName
                                    in
                                    "elm_" ++ prefixedModuleName ++ "_" ++ funcName

                                _ ->
                                    "/* complex fn */" ++ generateStandaloneExprWithCtx ctx fn

                        -- Expand record literal arguments to individual field values
                        -- This matches functions with record patterns that take individual field params
                        expandCallArg : Src.Expr -> List String
                        expandCallArg arg =
                            case arg of
                                Src.At _ (Src.Record fields) ->
                                    -- Expand record to field values in alphabetical order by field name
                                    fields
                                        |> List.sortBy (\( Src.At _ fieldName, _ ) -> fieldName)
                                        |> List.map (\( _, fieldValue ) -> generateStandaloneExprWithCtx ctx fieldValue)

                                _ ->
                                    [ generateStandaloneExprWithCtx ctx arg ]

                        argStrs =
                            List.concatMap expandCallArg args
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
                            "double elm_" ++ varName ++ " = " ++ generateStandaloneExpr arg ++ ";"

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


{-| Generate a function call with an additional pipe argument (context-aware version).
    Used when handling partial applications in pipes like: x |> Maybe.map .field
    The function has partial args from the Call node, plus the pipe arg.
-}
generateStandaloneCallWithPipeArgCtx : ExprCtx -> Src.Expr -> List Src.Expr -> String -> String
generateStandaloneCallWithPipeArgCtx ctx fn partialArgs pipeArg =
    -- First check if Builtins can handle this
    case Builtins.generateBuiltinCallWithPipeArg (generateStandaloneExprWithCtx ctx) ctx fn partialArgs pipeArg of
        Just result ->
            result

        Nothing ->
            -- Fall back to the local implementation
            generateStandaloneCallWithPipeArgImpl ctx fn partialArgs pipeArg


{-| Generate a function call with an additional pipe argument (backward compatible).
-}
generateStandaloneCallWithPipeArg : Src.Expr -> List Src.Expr -> String -> String
generateStandaloneCallWithPipeArg fn partialArgs pipeArg =
    generateStandaloneCallWithPipeArgImpl Shared.defaultExprCtx fn partialArgs pipeArg


{-| Implementation of pipe argument call generation.
    Note: Most builtin pipe handlers have been moved to Codegen/Builtins.elm.
    This function now only handles the default case for user-defined functions.
-}
generateStandaloneCallWithPipeArgImpl : ExprCtx -> Src.Expr -> List Src.Expr -> String -> String
generateStandaloneCallWithPipeArgImpl ctx fn partialArgs pipeArg =
    -- Default: just generate a simple function call with all args
    let
        allArgsStr =
            (partialArgs |> List.map (generateStandaloneExprWithCtx ctx))
                ++ [ pipeArg ]
                |> String.join ", "
    in
    generateStandaloneExprWithCtx ctx fn ++ "(" ++ allArgsStr ++ ")"


{-| Generate a user-defined function in C
-}
generateUserFunction : String -> List String -> String -> List Src.Pattern -> Src.Expr -> String
generateUserFunction modulePrefix userFunctions name args body =
    let
        -- Build the full prefixed function name
        prefixedName =
            if String.isEmpty modulePrefix then
                name
            else
                modulePrefix ++ "_" ++ name

        -- Generate function body with correct context for lifted local functions
        bodyExpr =
            generateStandaloneExprWithCtx
                { funcPrefix = name
                , modulePrefix = modulePrefix
                , userFunctions = userFunctions
                }
                body

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
                                            || String.contains ("elm_union_t __maybe_val = elm_" ++ varName) bodyExpr
                                            -- Parameter compared with record field (like .country == elm_country)
                                            -- Detect patterns like: .fieldname == elm_varName where field is likely a union type
                                            || (String.contains (".country == elm_" ++ varName) bodyExpr)
                                            || (String.contains (".type == elm_" ++ varName) bodyExpr)
                                            || (String.contains (".kind == elm_" ++ varName) bodyExpr)
                                            || (String.contains (".class == elm_" ++ varName) bodyExpr)
                                            || (String.contains (".status == elm_" ++ varName) bodyExpr)
                                            || (String.contains (".state == elm_" ++ varName) bodyExpr)
                                            -- Also detect tag comparison patterns: (elm_varName).tag
                                            || String.contains ("(elm_" ++ varName ++ ").tag") bodyExpr
                                            -- Variable is used as case scrutinee in custom type case
                                            || (String.contains ("elm_case_scrutinee = elm_" ++ varName) bodyExpr
                                                && String.contains "elm_case_scrutinee.tag == TAG_" bodyExpr)

                                    -- Check if parameter is used as a list (has .length or .data access, or is bound as elm_list_t)
                                    isListType =
                                        String.contains ("elm_" ++ varName ++ ".length") bodyExpr
                                            || String.contains ("elm_" ++ varName ++ ".data") bodyExpr
                                            || String.contains ("(elm_" ++ varName ++ ").length") bodyExpr
                                            || String.contains ("elm_list_t elm_case_scrutinee = elm_" ++ varName) bodyExpr

                                    -- Check if parameter is used as a string (with string functions or string comparisons)
                                    -- Use more precise patterns with trailing delimiter to avoid matching function name prefixes
                                    isStringType =
                                        String.contains ("elm_str_append(elm_" ++ varName ++ ",") bodyExpr
                                            || String.contains ("elm_str_append(elm_" ++ varName ++ ")") bodyExpr
                                            || String.contains ("elm_str_replace(") bodyExpr && String.contains (", elm_" ++ varName ++ ")") bodyExpr
                                            || String.contains ("elm_str_ends_with(") bodyExpr && String.contains (", elm_" ++ varName ++ ")") bodyExpr
                                            || String.contains ("elm_str_contains(") bodyExpr && String.contains (", elm_" ++ varName ++ ")") bodyExpr
                                            || String.contains ("strlen(elm_" ++ varName ++ ")") bodyExpr
                                            || String.contains ("strcmp(elm_" ++ varName ++ ", ") bodyExpr
                                            || String.contains ("strcmp(elm_" ++ varName ++ ",") bodyExpr
                                            -- Detect string comparisons: .name == elm_varName or elm_varName == .name
                                            || String.contains (".name == elm_" ++ varName) bodyExpr
                                            || String.contains ("elm_" ++ varName ++ " == ") bodyExpr && String.contains ".name" bodyExpr
                                            -- Passed to a function that takes a string (e.g., elm_byName(elm_varName))
                                            || String.contains ("elm_byName(elm_" ++ varName ++ ")") bodyExpr
                                            -- Also check for module-prefixed version
                                            || (not (String.isEmpty modulePrefix) && String.contains ("elm_" ++ modulePrefix ++ "_byName(elm_" ++ varName ++ ")") bodyExpr)
                                            -- Generic pattern: passed to any user function that might take strings
                                            || (not (String.isEmpty modulePrefix) && List.any (\fn -> String.contains ("elm_" ++ modulePrefix ++ "_" ++ fn ++ "(elm_" ++ varName) bodyExpr) userFunctions)

                                    -- Check if parameter is used as a record (field access like .name, .value)
                                    -- Exclude .tag, .data, .length which are for unions/lists
                                    prefix = "elm_" ++ varName ++ "."

                                    -- Extract field names accessed on this variable
                                    extractFields : String -> List String
                                    extractFields str =
                                        let
                                            -- Helper to take characters while predicate is true
                                            takeWhileHelper : (Char -> Bool) -> List Char -> List Char
                                            takeWhileHelper pred chars =
                                                case chars of
                                                    [] -> []
                                                    c :: rest ->
                                                        if pred c then
                                                            c :: takeWhileHelper pred rest
                                                        else
                                                            []

                                            -- Find all occurrences of elm_varName.fieldName
                                            findField remaining =
                                                case String.indexes prefix remaining of
                                                    [] -> []
                                                    idx :: _ ->
                                                        let
                                                            afterPrefix = String.dropLeft (idx + String.length prefix) remaining
                                                            -- Extract field name (alphanumeric chars until non-alphanum)
                                                            fieldChars = String.toList afterPrefix
                                                                |> takeWhileHelper (\c -> Char.isAlphaNum c || c == '_')
                                                            fieldName = String.fromList fieldChars
                                                            rest = String.dropLeft (idx + String.length prefix + String.length fieldName) remaining
                                                        in
                                                        if String.isEmpty fieldName then
                                                            findField rest
                                                        else
                                                            fieldName :: findField rest
                                        in
                                        findField str
                                            |> List.filter (\f -> f /= "tag" && f /= "data" && f /= "length")
                                            |> List.foldr (\f acc -> if List.member f acc then acc else f :: acc) []

                                    recordFields = extractFields bodyExpr

                                    isRecordType = not (List.isEmpty recordFields)

                                    -- Build struct type from field accesses
                                    -- Infer field types: if used directly with elm_str_append as first arg, it's a string
                                    inferFieldType : String -> String
                                    inferFieldType fieldName =
                                        let
                                            fieldAccess = prefix ++ fieldName
                                            -- Check if this field is used directly as first argument to elm_str_append
                                            -- Pattern: elm_str_append(elm_cfg.fieldName,
                                            isStringField =
                                                String.contains ("elm_str_append(" ++ fieldAccess ++ ",") bodyExpr
                                                    || String.contains ("elm_str_append(" ++ fieldAccess ++ ")") bodyExpr
                                        in
                                        if isStringField then
                                            "const char *"
                                        else
                                            "double"

                                    paramRecordType =
                                        if isRecordType then
                                            let
                                                fieldDefs = recordFields
                                                    |> List.map (\f -> inferFieldType f ++ " " ++ f)
                                                    |> String.join "; "
                                            in
                                            "struct { " ++ fieldDefs ++ "; }"
                                        else
                                            ""
                                    -- Check if parameter is used as a tuple (has ._0, ._1 access or assigned to elm_tuple2_t/elm_tuple3_t)
                                    isTuple2Type =
                                        String.contains ("elm_" ++ varName ++ "._0") bodyExpr
                                            || String.contains ("elm_" ++ varName ++ "._1") bodyExpr
                                            || String.contains ("elm_tuple2_t elm_case_scrutinee = elm_" ++ varName) bodyExpr

                                    isTuple3Type =
                                        String.contains ("elm_" ++ varName ++ "._2") bodyExpr
                                            || String.contains ("elm_tuple3_t elm_case_scrutinee = elm_" ++ varName) bodyExpr
                                in
                                if isListType then
                                    "elm_list_t elm_" ++ varName
                                else if isStringType then
                                    "const char *elm_" ++ varName
                                else if isUnionType then
                                    "elm_union_t elm_" ++ varName
                                else if isTuple3Type then
                                    "elm_tuple3_t elm_" ++ varName
                                else if isTuple2Type then
                                    "elm_tuple2_t elm_" ++ varName
                                else if isRecordType then
                                    -- For struct parameters, generate the struct type directly
                                    -- We'll handle type compatibility at call sites
                                    paramRecordType ++ " elm_" ++ varName
                                else
                                    "double elm_" ++ varName

                            -- Handle At pattern (Located): extract inner variable
                            -- Pattern is: At _ varName  ->  PCtor "At" [PAnything, PVar varName]
                            Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerVar) ] ->
                                let
                                    isUnionType =
                                        String.contains ("elm_" ++ innerVar ++ ".tag") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".data") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".ctors") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".name") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".args") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".body") bodyExpr
                                            || String.contains ("elm_union_t elm_case_scrutinee = elm_" ++ innerVar) bodyExpr
                                in
                                if isUnionType then
                                    "elm_union_t elm_" ++ innerVar

                                else
                                    "double elm_" ++ innerVar

                            -- Handle qualified At pattern (e.g., Src.At _ e)
                            Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerVar) ] ->
                                let
                                    isUnionType =
                                        String.contains ("elm_" ++ innerVar ++ ".tag") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".data") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".ctors") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".name") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".args") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".body") bodyExpr
                                            || String.contains ("elm_union_t elm_case_scrutinee = elm_" ++ innerVar) bodyExpr
                                in
                                if isUnionType then
                                    "elm_union_t elm_" ++ innerVar

                                else
                                    "double elm_" ++ innerVar

                            -- Handle At pattern with nested At pattern
                            Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerVar) ]) ] ->
                                "double elm_" ++ innerVar

                            -- Handle qualified nested At pattern
                            Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerVar) ]) ] ->
                                "double elm_" ++ innerVar

                            -- Handle 2-tuple pattern
                            Src.PTuple (Src.At _ (Src.PVar v1)) (Src.At _ (Src.PVar v2)) [] ->
                                "double elm_" ++ v1 ++ ", double elm_" ++ v2

                            -- Handle 3-tuple pattern
                            Src.PTuple (Src.At _ (Src.PVar v1)) (Src.At _ (Src.PVar v2)) [ Src.At _ (Src.PVar v3) ] ->
                                "double elm_" ++ v1 ++ ", double elm_" ++ v2 ++ ", double elm_" ++ v3

                            -- Handle tuple with constructor pattern in first position: ( Src.At _ fieldName, fieldValue )
                            Src.PTuple (Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar v1) ])) (Src.At _ (Src.PVar v2)) [] ->
                                "double elm_" ++ v1 ++ ", double elm_" ++ v2

                            Src.PTuple (Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar v1) ])) (Src.At _ (Src.PVar v2)) [] ->
                                "double elm_" ++ v1 ++ ", double elm_" ++ v2

                            -- Handle record pattern { x, y, ... }
                            -- Generate a struct parameter - the field extractions happen in preamble
                            Src.PRecord fields ->
                                let
                                    fieldDefs =
                                        fields
                                            |> List.map
                                                (\(Src.At _ fieldName) ->
                                                    let
                                                        isStringField =
                                                            String.contains ("elm_str_append(elm_" ++ fieldName ++ ",") bodyExpr
                                                                || String.contains ("elm_str_append(elm_" ++ fieldName ++ ")") bodyExpr

                                                        fieldType =
                                                            if isStringField then
                                                                "const char *"
                                                            else
                                                                "double"
                                                    in
                                                    fieldType ++ " " ++ fieldName
                                                )
                                            |> String.join "; "
                                in
                                "struct { " ++ fieldDefs ++ "; } __rec"

                            -- Handle unit pattern ()
                            Src.PUnit ->
                                ""

                            -- Handle wildcard pattern _
                            Src.PAnything ->
                                "double __unused"

                            _ ->
                                "double /* unsupported pattern */"
                    )
                |> String.join ", "

        -- Generate preamble bindings for record patterns (extract fields from __rec)
        recordPreamble =
            args
                |> List.filterMap
                    (\(Src.At _ pat) ->
                        case pat of
                            Src.PRecord fields ->
                                Just (
                                    fields
                                        |> List.map
                                            (\(Src.At _ fieldName) ->
                                                let
                                                    isStringField =
                                                        String.contains ("elm_str_append(elm_" ++ fieldName ++ ",") bodyExpr
                                                            || String.contains ("elm_str_append(elm_" ++ fieldName ++ ")") bodyExpr

                                                    fieldType =
                                                        if isStringField then
                                                            "const char *"
                                                        else
                                                            "double"
                                                in
                                                fieldType ++ " elm_" ++ fieldName ++ " = __rec." ++ fieldName ++ ";"
                                            )
                                        |> String.join " "
                                )
                            _ ->
                                Nothing
                    )
                |> String.join " "

        -- Infer return type from body expression
        -- Check for string returns, including in case expressions (ternary with string literals)
        isStringReturn =
            String.contains "elm_str_" bodyExpr
                || String.contains "elm_from_" bodyExpr
                || String.startsWith "\"" bodyExpr
                -- For case expressions with string branches: ? "stringlit" : pattern
                || String.contains "? \"" bodyExpr

        isUnionReturn =
            String.contains "((elm_union_t){TAG_" bodyExpr
                || String.contains "elm_union_t result" bodyExpr
                || String.contains "elm_Just(" bodyExpr
                || String.contains "elm_Nothing(" bodyExpr
                || String.contains "elm_Ok(" bodyExpr
                || String.contains "elm_Err(" bodyExpr
                || String.contains "elm_At(" bodyExpr

        -- Check for typeof-based list returns (typed record lists)
        -- Pattern: ({ typeof(elm_varname) __lst = elm_varname; typeof(__lst) __result; ... __result; })
        isTypeofListReturn =
            String.contains "typeof(elm_" bodyExpr && String.contains "__result.length" bodyExpr

        -- Extract the variable name from typeof pattern for proper return type
        typeofListVarName =
            if isTypeofListReturn then
                -- Find typeof(elm_NAME) pattern and extract NAME
                let
                    marker = "typeof(elm_"
                    startIdx = String.indexes marker bodyExpr |> List.head |> Maybe.withDefault 0
                    afterMarker = String.dropLeft (startIdx + String.length marker) bodyExpr
                    -- Find the closing paren
                    endIdx = String.indexes ")" afterMarker |> List.head |> Maybe.withDefault 0
                    varName = String.left endIdx afterMarker
                in
                "elm_" ++ varName
            else
                ""

        isListReturn =
            String.contains "((elm_list_t){" bodyExpr
                || String.contains "elm_list_t __" bodyExpr

        -- Check for record returns - generates ((struct { ... }){...})
        -- Must START with the record pattern - not just contain it (e.g., inside a case expression)
        -- Must contain struct literal pattern }){ not struct cast pattern } *)
        isRecordReturn =
            String.startsWith "((struct {" bodyExpr
                && String.contains "}){" bodyExpr
                && not (String.contains "} *)" bodyExpr)

        -- Extract the record type definition for proper return type
        recordType =
            if isRecordReturn then
                -- Extract the struct type from the body: ((struct { ... }){...})
                -- Find the struct definition between "((struct {" and "})"
                let
                    -- Find first occurrence of struct definition
                    startMarker = "((struct {"
                    endMarker = "})"
                    startIdx = String.indexes startMarker bodyExpr |> List.head |> Maybe.withDefault 0
                    -- Find the matching close brace - the struct def ends at first "})"
                    searchStart = startIdx + String.length startMarker
                    afterStart = String.dropLeft searchStart bodyExpr
                    -- Find the end of the field definitions (first "})" after opening)
                    endIdx = String.indexes "})" afterStart |> List.head |> Maybe.withDefault 0
                    fieldDefs = String.left endIdx afterStart
                in
                "struct {" ++ fieldDefs ++ "}"
            else
                ""

        -- Check for struct return from case expression (extracted via data.ptr)
        -- Pattern: ({ struct { ... } elm_varName = *(struct { ... }*)...->data.ptr; elm_varName; })
        isStructCaseReturn =
            String.contains "->data.ptr; elm_" bodyExpr
                && String.contains "= *(struct {" bodyExpr

        -- Extract struct type from case expression extraction
        structCaseType =
            if isStructCaseReturn then
                let
                    -- Find the struct type: "= *(struct { ... }*)"
                    marker = "= *(struct {"
                    startIdx2 = String.indexes marker bodyExpr |> List.head |> Maybe.withDefault 0
                    afterMarker = String.dropLeft (startIdx2 + String.length marker) bodyExpr
                    -- Find the end at "*)"
                    endIdx2 = String.indexes "}*)" afterMarker |> List.head |> Maybe.withDefault 0
                    fieldDefs2 = String.left endIdx2 afterMarker
                in
                "struct {" ++ fieldDefs2 ++ "}"
            else
                ""

        returnType =
            if isStringReturn then
                "const char *"
            else if isUnionReturn then
                "elm_union_t"
            else if isTypeofListReturn then
                -- Use typeof for typed list return (e.g., typeof(elm_all))
                "typeof(" ++ typeofListVarName ++ ")"
            else if isListReturn then
                "elm_list_t"
            else if isRecordReturn then
                recordType
            else if isStructCaseReturn then
                structCaseType
            else
                "double"

        -- Check if this is a zero-arg value definition (not a function)
        -- For records, generate static constants instead of functions
        isZeroArgValue =
            List.isEmpty args

        -- For zero-arg record definitions, generate static constants
        -- This allows `model = init` to work correctly without needing ()
        isRecordConstant =
            isZeroArgValue && isRecordReturn
    in
    if isRecordConstant then
        -- Generate static constant instead of function
        -- Extract the initializer from ((struct {...}){...})
        let
            -- Find the initializer part: starts after "){"
            initStartMarker = "){"
            initStartIdx = String.indexes initStartMarker bodyExpr |> List.head |> Maybe.withDefault 0
            afterInitStart = String.dropLeft (initStartIdx + String.length initStartMarker) bodyExpr
            -- Remove the trailing }); - find the last })
            initEndIdx = String.length afterInitStart - 2
            initializer = String.left initEndIdx afterInitStart
        in
        "static " ++ recordType ++ " elm_" ++ prefixedName ++ " = {" ++ initializer ++ "};"
    else if isRecordReturn then
        -- For functions returning records, use a typedef to ensure type consistency
        -- TCC and standard C treat each anonymous struct as a distinct type
        let
            typedefName = "elm_" ++ prefixedName ++ "_return_t"
            initStartMarker = "){"
            initStartIdx = String.indexes initStartMarker bodyExpr |> List.head |> Maybe.withDefault 0
            afterInitStart = String.dropLeft (initStartIdx + String.length initStartMarker) bodyExpr
            initEndIdx = String.length afterInitStart - 2
            initializer = String.left initEndIdx afterInitStart
        in
        "typedef " ++ recordType ++ " " ++ typedefName ++ ";\n" ++
        "static " ++ typedefName ++ " elm_" ++ prefixedName ++ "(" ++ params ++ ") {\n    return (" ++ typedefName ++ "){" ++ initializer ++ "};\n}"
    else if not (String.isEmpty recordPreamble) then
        -- Has record pattern - extract fields directly from parameter
        -- Use individual parameters for each field to avoid anonymous struct incompatibility
        let
            -- Generate parameters for all args, expanding record patterns to individual fields
            allParams =
                args
                    |> List.concatMap
                        (\(Src.At _ pat) ->
                            case pat of
                                Src.PRecord fields ->
                                    -- Expand record to individual field parameters (sorted alphabetically)
                                    fields
                                        |> List.sortBy (\(Src.At _ fieldName) -> fieldName)
                                        |> List.map
                                            (\(Src.At _ fieldName) ->
                                                let
                                                    isStringField =
                                                        String.contains ("elm_str_append(elm_" ++ fieldName ++ ",") bodyExpr
                                                            || String.contains ("elm_str_append(elm_" ++ fieldName ++ ")") bodyExpr

                                                    fieldType =
                                                        if isStringField then
                                                            "const char *"
                                                        else
                                                            "double"
                                                in
                                                fieldType ++ " elm_" ++ fieldName
                                            )

                                Src.PVar varName ->
                                    -- Regular variable parameter
                                    let
                                        isStringVar =
                                            String.contains ("elm_str_append(elm_" ++ varName ++ ",") bodyExpr
                                                || String.contains ("elm_str_append(elm_" ++ varName ++ ")") bodyExpr

                                        varType =
                                            if isStringVar then
                                                "const char *"
                                            else
                                                "double"
                                    in
                                    [ varType ++ " elm_" ++ varName ]

                                Src.PAnything ->
                                    [ "double __unused" ]

                                Src.PUnit ->
                                    []

                                _ ->
                                    [ "double /* unsupported pattern */" ]
                        )
                    |> String.join ", "
        in
        -- No preamble needed since fields are passed directly as parameters
        "static " ++ returnType ++ " elm_" ++ prefixedName ++ "(" ++ allParams ++ ") {\n    return " ++ bodyExpr ++ ";\n}"
    else
        -- Check for tail recursion and apply TCO if possible
        let
            -- Extract parameter names for TCO
            paramNames =
                args
                    |> List.filterMap
                        (\(Src.At _ pat) ->
                            case pat of
                                Src.PVar varName -> Just varName
                                _ -> Nothing
                        )

            -- Check if the function is tail-recursive
            isTailRecursive =
                hasTailCall name body

            -- Generate TCO loop body if tail-recursive
            tcoBody =
                if isTailRecursive && not (List.isEmpty paramNames) then
                    generateTCOBody name paramNames body
                else
                    Nothing
        in
        case tcoBody of
            Just loopCode ->
                "static " ++ returnType ++ " elm_" ++ prefixedName ++ "(" ++ params ++ ") {\n    while (1) {\n" ++ loopCode ++ "\n    }\n}"

            Nothing ->
                "static " ++ returnType ++ " elm_" ++ prefixedName ++ "(" ++ params ++ ") {\n    return " ++ bodyExpr ++ ";\n}"


{-| Check if an expression contains a tail call to the given function name.
    A tail call is a call that is the last thing evaluated before returning.
-}
hasTailCall : String -> Src.Expr -> Bool
hasTailCall funcName (Src.At _ expr) =
    case expr of
        -- Direct call to the function in tail position
        Src.Call (Src.At _ (Src.Var _ callee)) _ ->
            callee == funcName

        -- If expression: check if either branch has tail call
        Src.If branches elseExpr ->
            List.any (\( _, thenExpr ) -> hasTailCall funcName thenExpr) branches
                || hasTailCall funcName elseExpr

        -- Case expression: check if any branch has tail call
        Src.Case _ branches ->
            List.any (\( _, _, branchExpr ) -> hasTailCall funcName branchExpr) branches

        -- Let expression: check the body (the "in" part)
        Src.Let _ inExpr ->
            hasTailCall funcName inExpr

        _ ->
            False


{-| Generate TCO loop body for a tail-recursive function.
    Returns Nothing if TCO transformation is not possible.
-}
generateTCOBody : String -> List String -> Src.Expr -> Maybe String
generateTCOBody funcName paramNames body =
    generateTCOExpr funcName paramNames body


{-| Generate TCO code for an expression.
    Non-recursive branches return; recursive branches reassign parameters and continue.
-}
generateTCOExpr : String -> List String -> Src.Expr -> Maybe String
generateTCOExpr funcName paramNames (Src.At _ expr) =
    case expr of
        -- If expression
        Src.If branches elseExpr ->
            let
                generateBranch ( condExpr, thenExpr ) =
                    let
                        condStr = generateStandaloneExpr condExpr
                        thenCode = generateTCOExpr funcName paramNames thenExpr
                    in
                    Maybe.map (\code -> "        if (" ++ condStr ++ ") {\n" ++ code ++ "\n        }") thenCode

                branchCodes = List.filterMap generateBranch branches
                elseCode = generateTCOExpr funcName paramNames elseExpr
            in
            case ( branchCodes, elseCode ) of
                ( [], _ ) ->
                    Nothing

                ( firstBranch :: restBranches, Just elseStr ) ->
                    Just (firstBranch ++ String.concat (List.map (\b -> " else " ++ String.dropLeft 8 b) restBranches) ++ " else {\n" ++ elseStr ++ "\n        }")

                _ ->
                    Nothing

        -- Case expression
        Src.Case scrutinee branches ->
            -- For now, only handle simple case expressions
            -- Complex case expressions fall back to normal code generation
            Nothing

        -- Let expression: generate the bindings then the TCO body
        Src.Let defs inExpr ->
            let
                defsCode = defs
                    |> List.map (\(Src.At _ def) ->
                        case def of
                            Src.Define (Src.At _ defName) defArgs defBody _ ->
                                if List.isEmpty defArgs then
                                    let
                                        ( varType, varInit ) = inferCTypeAndInit defBody
                                    in
                                    "            " ++ varType ++ " elm_" ++ defName ++ " = " ++ varInit ++ ";"
                                else
                                    "            /* local function not supported in TCO */"

                            Src.Destruct pattern defExpr ->
                                "            " ++ generateDestructuring pattern defExpr
                        )
                    |> String.join "\n"

                bodyCode = generateTCOExpr funcName paramNames inExpr
            in
            Maybe.map (\b -> defsCode ++ "\n" ++ b) bodyCode

        -- Direct tail call to the function
        Src.Call (Src.At _ (Src.Var _ callee)) callArgs ->
            if callee == funcName && List.length callArgs == List.length paramNames then
                let
                    -- Generate temp variables for new parameter values
                    tempAssignments =
                        List.map2
                            (\pName argExpr ->
                                "            double __tco_" ++ pName ++ " = " ++ generateStandaloneExpr argExpr ++ ";"
                            )
                            paramNames
                            callArgs
                        |> String.join "\n"

                    -- Reassign parameters from temps
                    paramReassignments =
                        paramNames
                            |> List.map (\pName -> "            elm_" ++ pName ++ " = __tco_" ++ pName ++ ";")
                            |> String.join "\n"
                in
                Just (tempAssignments ++ "\n" ++ paramReassignments ++ "\n            continue;")
            else
                -- Non-recursive call or wrong arity - return the result
                Just ("            return " ++ generateStandaloneExpr (Src.At { start = { row = 1, col = 1 }, end = { row = 1, col = 1 } } expr) ++ ";")

        -- Any other expression - not a tail call, just return it
        _ ->
            Just ("            return " ++ generateStandaloneExpr (Src.At { start = { row = 1, col = 1 }, end = { row = 1, col = 1 } } expr) ++ ";")


{-| Dead Code Elimination: compute the set of reachable function names from main.
    Returns a list of function names that are transitively called from main.
-}
computeReachableFunctions : List (Src.Located Src.Value) -> List String
computeReachableFunctions values =
    let
        -- Build a map from function name to its body expression
        funcBodies : List ( String, Src.Expr )
        funcBodies =
            values
                |> List.map
                    (\(Src.At _ value) ->
                        let
                            (Src.At _ name) = value.name
                        in
                        ( name, value.body )
                    )

        -- Get all function names defined in this module
        definedFuncs : List String
        definedFuncs =
            List.map (\( name, _ ) -> name) funcBodies

        -- Get references from a function body, filtered to only include defined functions
        getCallees : String -> List String
        getCallees funcName =
            funcBodies
                |> List.filter (\( name, _ ) -> name == funcName)
                |> List.head
                |> Maybe.map (\( _, body ) -> collectVarRefs body)
                |> Maybe.withDefault []
                |> List.filter (\ref -> List.member ref definedFuncs)
                |> Shared.uniqueStrings

        -- Compute transitive closure starting from "main"
        computeClosure : List String -> List String -> List String
        computeClosure frontier visited =
            case frontier of
                [] ->
                    visited

                current :: rest ->
                    if List.member current visited then
                        computeClosure rest visited
                    else
                        let
                            callees = getCallees current
                            newFrontier = rest ++ List.filter (\c -> not (List.member c visited)) callees
                        in
                        computeClosure newFrontier (current :: visited)
    in
    computeClosure [ "main" ] []


{-| Filter values to only include reachable functions (Dead Code Elimination).
    Always includes "main" and all functions transitively called from main.
-}
filterReachableValues : List (Src.Located Src.Value) -> List (Src.Located Src.Value)
filterReachableValues values =
    let
        reachable = computeReachableFunctions values
    in
    values
        |> List.filter
            (\(Src.At _ value) ->
                let
                    (Src.At _ name) = value.name
                in
                List.member name reachable
            )


{-| Collect local function definitions from an expression for lifting to module level
    The scope parameter tracks variables defined in outer scopes that might be captured.
-}
collectLocalFunctions : String -> Src.Expr -> List LiftedFunc
collectLocalFunctions prefix expr =
    collectLocalFunctionsWithScope prefix [] expr


{-| Internal helper that tracks scope for capture detection
-}
collectLocalFunctionsWithScope : String -> List String -> Src.Expr -> List LiftedFunc
collectLocalFunctionsWithScope prefix scope (Src.At _ expr) =
    case expr of
        Src.Let defs body ->
            let
                -- Names defined in this let block (both values and functions)
                letBindingNames =
                    defs
                        |> List.filterMap
                            (\(Src.At _ def) ->
                                case def of
                                    Src.Define (Src.At _ name) _ _ _ ->
                                        Just name

                                    Src.Destruct _ _ ->
                                        Nothing
                            )

                -- Extended scope includes current let bindings
                extendedScope =
                    scope ++ letBindingNames

                -- Collect functions defined in this let
                localFuncs =
                    defs
                        |> List.filterMap
                            (\(Src.At _ def) ->
                                case def of
                                    Src.Define (Src.At _ name) args defBody _ ->
                                        if not (List.isEmpty args) then
                                            let
                                                -- Get parameter names
                                                argNames =
                                                    List.concatMap patternVars args

                                                -- Get all var refs in body
                                                allRefs =
                                                    collectVarRefs defBody |> Shared.uniqueStrings

                                                -- Captured = refs that are in scope but not in args or stdlib
                                                stdlibFuncs =
                                                    [ "elm_str_append", "elm_from_int", "elm_from_float"
                                                    , "String", "List", "Maybe", "Result", "Debug"
                                                    ]

                                                capturedVars =
                                                    allRefs
                                                        |> List.filter
                                                            (\v ->
                                                                List.member v extendedScope
                                                                    && not (List.member v argNames)
                                                                    && not (List.member v stdlibFuncs)
                                                            )
                                            in
                                            Just
                                                { prefix = prefix
                                                , name = name
                                                , args = args
                                                , body = defBody
                                                , capturedVars = capturedVars
                                                }

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
                                        collectLocalFunctionsWithScope (prefix ++ "_" ++ name) extendedScope defBody

                                    Src.Destruct _ _ ->
                                        []
                            )

                bodyFuncs =
                    collectLocalFunctionsWithScope prefix extendedScope body
            in
            localFuncs ++ nestedFuncs ++ bodyFuncs

        Src.If branches elseExpr ->
            let
                branchFuncs =
                    branches
                        |> List.concatMap
                            (\( cond, thenExpr ) ->
                                collectLocalFunctionsWithScope prefix scope cond
                                    ++ collectLocalFunctionsWithScope prefix scope thenExpr
                            )
            in
            branchFuncs ++ collectLocalFunctionsWithScope prefix scope elseExpr

        Src.Case scrutinee branches ->
            let
                scrutineeFuncs =
                    collectLocalFunctionsWithScope prefix scope scrutinee

                branchFuncs =
                    branches
                        |> List.concatMap
                            (\( pat, maybeGuard, branchExpr ) ->
                                -- Add pattern vars to scope for this branch
                                let
                                    patVars = patternVars pat
                                    guardFuncs =
                                        case maybeGuard of
                                            Just guardExpr ->
                                                collectLocalFunctionsWithScope prefix (scope ++ patVars) guardExpr
                                            Nothing ->
                                                []
                                in
                                guardFuncs ++ collectLocalFunctionsWithScope prefix (scope ++ patVars) branchExpr
                            )
            in
            scrutineeFuncs ++ branchFuncs

        Src.Call fn args ->
            collectLocalFunctionsWithScope prefix scope fn
                ++ List.concatMap (collectLocalFunctionsWithScope prefix scope) args

        Src.Binops pairs final ->
            let
                pairFuncs =
                    pairs
                        |> List.concatMap (\( e, _ ) -> collectLocalFunctionsWithScope prefix scope e)
            in
            pairFuncs ++ collectLocalFunctionsWithScope prefix scope final

        Src.Negate inner ->
            collectLocalFunctionsWithScope prefix scope inner

        _ ->
            []


{-| Generate a lifted local function as a module-level function
    capturedVars are variables from outer scope that need to be passed as additional parameters
-}
generateLiftedFunction : String -> String -> List Src.Pattern -> Src.Expr -> List String -> String
generateLiftedFunction prefix funcName args body capturedVars =
    let
        liftedName =
            prefix ++ "_" ++ funcName

        bodyExpr =
            generateStandaloneExpr body

        -- Generate parameter declarations for captured variables
        -- All captured vars are assumed to be double unless we can infer otherwise from usage
        capturedParams =
            capturedVars
                |> List.map
                    (\varName ->
                        let
                            -- Check if captured var is used as a string in the body
                            isStringType =
                                String.contains ("elm_str_append(elm_" ++ varName) bodyExpr
                                    || String.contains ("elm_str_append(" ++ "\"") bodyExpr && String.contains (", elm_" ++ varName ++ ")") bodyExpr
                                    || String.contains ("strlen(elm_" ++ varName) bodyExpr

                            -- Check if captured var is used as a list
                            isListType =
                                String.contains ("elm_" ++ varName ++ ".length") bodyExpr
                                    || String.contains ("elm_" ++ varName ++ ".data") bodyExpr
                        in
                        if isStringType then
                            "const char *elm_" ++ varName
                        else if isListType then
                            "elm_list_t elm_" ++ varName
                        else
                            "double elm_" ++ varName
                    )

        params =
            args
                |> List.map
                    (\(Src.At _ pat) ->
                        case pat of
                            Src.PVar varName ->
                                let
                                    -- Check if variable is used as a tagged union (has .tag or .data access)
                                    isUnionType =
                                        String.contains ("elm_" ++ varName ++ ".tag") bodyExpr
                                            || String.contains ("elm_" ++ varName ++ ".data") bodyExpr
                                            || String.contains ("elm_union_t elm_case_scrutinee = elm_" ++ varName) bodyExpr

                                    -- Check for AST module record (has .values, .unions, .name fields)
                                    isModuleType =
                                        String.contains ("elm_" ++ varName ++ ".values") bodyExpr
                                            || String.contains ("elm_" ++ varName ++ ".unions") bodyExpr

                                    -- Check for AST value record (has .args, .body fields)
                                    isValueType =
                                        String.contains ("elm_" ++ varName ++ ".args") bodyExpr
                                            && String.contains ("elm_" ++ varName ++ ".body") bodyExpr

                                    -- Check for AST union record (has .ctors field)
                                    isSrcUnionType =
                                        String.contains ("elm_" ++ varName ++ ".ctors") bodyExpr

                                    -- Check for local function record (has .prefix field)
                                    isLocalFuncType =
                                        String.contains ("elm_" ++ varName ++ ".prefix") bodyExpr

                                    -- Check for flags/model record (has .target field)
                                    isFlagsType =
                                        String.contains ("elm_" ++ varName ++ ".target") bodyExpr

                                    -- Check if parameter is used as a string (with string functions)
                                    isStringType =
                                        String.contains ("elm_str_append(elm_" ++ varName) bodyExpr
                                            || String.contains ("elm_str_replace(" ++ "\"") bodyExpr && String.contains (", elm_" ++ varName ++ ")") bodyExpr
                                            || String.contains ("elm_str_ends_with(" ++ "\"") bodyExpr && String.contains (", elm_" ++ varName ++ ")") bodyExpr
                                            || String.contains ("elm_str_contains(" ++ "\"") bodyExpr && String.contains (", elm_" ++ varName ++ ")") bodyExpr
                                            || String.contains ("strlen(elm_" ++ varName) bodyExpr
                                            || String.contains ("strcmp(elm_" ++ varName ++ ", ") bodyExpr
                                            || String.contains ("strcmp(elm_" ++ varName ++ ",") bodyExpr

                                    -- Check if parameter is used as a list (has .length or .data access, or is bound as elm_list_t)
                                    isListType =
                                        String.contains ("elm_" ++ varName ++ ".length") bodyExpr
                                            || String.contains ("elm_" ++ varName ++ ".data") bodyExpr
                                            || String.contains ("(elm_" ++ varName ++ ").length") bodyExpr
                                            || String.contains ("elm_list_t elm_case_scrutinee = elm_" ++ varName) bodyExpr
                                in
                                if isListType then
                                    "elm_list_t elm_" ++ varName
                                else if isStringType then
                                    "const char *elm_" ++ varName
                                else if isUnionType then
                                    "elm_union_t elm_" ++ varName
                                else if isModuleType then
                                    "elm_module_t elm_" ++ varName
                                else if isValueType then
                                    "elm_value_t elm_" ++ varName
                                else if isSrcUnionType then
                                    "elm_src_union_t elm_" ++ varName
                                else if isLocalFuncType then
                                    "elm_local_func_t elm_" ++ varName
                                else if isFlagsType then
                                    "elm_flags_t elm_" ++ varName
                                else
                                    "double elm_" ++ varName

                            -- Handle At pattern (Located): extract inner variable
                            -- Pattern is: At _ varName  ->  PCtor "At" [PAnything, PVar varName]
                            Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerVar) ] ->
                                let
                                    isUnionType =
                                        String.contains ("elm_" ++ innerVar ++ ".tag") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".data") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".ctors") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".name") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".args") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".body") bodyExpr
                                            || String.contains ("elm_union_t elm_case_scrutinee = elm_" ++ innerVar) bodyExpr
                                in
                                if isUnionType then
                                    "elm_union_t elm_" ++ innerVar

                                else
                                    "double elm_" ++ innerVar

                            -- Handle qualified At pattern (e.g., Src.At _ e)
                            Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerVar) ] ->
                                let
                                    isUnionType =
                                        String.contains ("elm_" ++ innerVar ++ ".tag") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".data") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".ctors") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".name") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".args") bodyExpr
                                            || String.contains ("elm_" ++ innerVar ++ ".body") bodyExpr
                                            || String.contains ("elm_union_t elm_case_scrutinee = elm_" ++ innerVar) bodyExpr
                                in
                                if isUnionType then
                                    "elm_union_t elm_" ++ innerVar

                                else
                                    "double elm_" ++ innerVar

                            -- Handle At pattern with nested At pattern
                            Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerVar) ]) ] ->
                                "double elm_" ++ innerVar

                            -- Handle qualified nested At pattern
                            Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerVar) ]) ] ->
                                "double elm_" ++ innerVar

                            -- Handle 2-tuple pattern
                            Src.PTuple (Src.At _ (Src.PVar v1)) (Src.At _ (Src.PVar v2)) [] ->
                                "double elm_" ++ v1 ++ ", double elm_" ++ v2

                            -- Handle tuple with constructor pattern: ( Src.At _ fieldName, fieldValue )
                            Src.PTuple (Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar v1) ])) (Src.At _ (Src.PVar v2)) [] ->
                                "double elm_" ++ v1 ++ ", double elm_" ++ v2

                            Src.PTuple (Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar v1) ])) (Src.At _ (Src.PVar v2)) [] ->
                                "double elm_" ++ v1 ++ ", double elm_" ++ v2

                            _ ->
                                "double /* unsupported pattern */"
                    )
                |> String.join ", "

        -- Detect return type based on the final expression in the body
        -- Be careful to check what's actually being returned, not just what's used in the body
        -- Check for record returns - generates ((struct { ... }){...})
        -- Must START with the record pattern - not just contain it (e.g., inside a case expression)
        -- Must contain struct literal pattern }){ not struct cast pattern } *)
        isRecordReturn =
            String.startsWith "((struct {" bodyExpr
                && String.contains "}){" bodyExpr
                && not (String.contains "} *)" bodyExpr)

        -- Extract the record type definition for proper return type
        recordType =
            if isRecordReturn then
                let
                    startMarker = "((struct {"
                    startIdx = String.indexes startMarker bodyExpr |> List.head |> Maybe.withDefault 0
                    searchStart = startIdx + String.length startMarker
                    afterStart = String.dropLeft searchStart bodyExpr
                    endIdx = String.indexes "})" afterStart |> List.head |> Maybe.withDefault 0
                    fieldDefs = String.left endIdx afterStart
                in
                "struct {" ++ fieldDefs ++ "}"
            else
                ""

        returnType =
            -- Check if the expression directly returns a Maybe value
            if String.contains "((elm_union_t){TAG_Just" bodyExpr || String.contains "((elm_union_t){TAG_Nothing" bodyExpr then
                "elm_union_t"
            -- Check if it returns a string literal or string operation
            else if String.startsWith "\"" bodyExpr then
                "const char *"
            -- Check if it's a ternary expression that returns string literals
            else if String.contains "? \"" bodyExpr && String.contains ": \"" bodyExpr then
                "const char *"
            -- Check if the body starts with a case that returns a string parameter
            -- Pattern: (elm_case_scrutinee.length == 0 ? elm_arg : ...)
            else if String.contains "? elm_arg :" bodyExpr || String.contains "? elm_innerArg :" bodyExpr then
                "const char *"
            -- Check if the function uses elm_str_append and returns the result
            else if String.contains "elm_str_append(" bodyExpr && (String.startsWith "elm_str_append(" bodyExpr || String.startsWith "({" bodyExpr) then
                "const char *"
            -- Check if a string-typed parameter is being returned in a ternary
            else if String.contains "const char *elm_" params && String.contains "? elm_" bodyExpr then
                "const char *"
            -- Check if the final result is an elm_list_t (not just using one)
            else if String.startsWith "({ elm_list_t " bodyExpr && String.endsWith "__lst; })" bodyExpr then
                "elm_list_t"
            -- Check for record returns
            else if isRecordReturn then
                recordType
            else
                "double"

        -- Fix recursive calls: replace "elm_funcName(" with "elm_prefix_funcName("
        -- This handles local recursive functions that get lifted
        fixedBodyExpr =
            String.replace ("elm_" ++ funcName ++ "(") ("elm_" ++ liftedName ++ "(") bodyExpr

        -- Combine regular params with captured params
        allParams =
            let
                regularParams = params
                captured = String.join ", " capturedParams
            in
            if String.isEmpty regularParams then
                captured
            else if String.isEmpty captured then
                regularParams
            else
                regularParams ++ ", " ++ captured
    in
    "static " ++ returnType ++ " elm_" ++ liftedName ++ "(" ++ allParams ++ ") {\n    return " ++ fixedBodyExpr ++ ";\n}"


{-| Infer C type and initializer for an expression
    Returns (type, initializer) pair
-}
inferCTypeAndInit : Src.Expr -> ( String, String )
inferCTypeAndInit locatedExpr =
    let
        (Src.At _ expr) =
            locatedExpr
    in
    case expr of
        Src.Record fields ->
            let
                -- Infer field type from the value expression
                inferFieldType : Src.Expr -> String
                inferFieldType fieldValue =
                    let
                        valueStr = generateStandaloneExpr fieldValue
                    in
                    if String.startsWith "\"" valueStr then
                        "const char *"
                    else if String.contains "elm_str_" valueStr || String.contains "elm_from_" valueStr then
                        "const char *"
                    else
                        "double"

                fieldDefs =
                    fields
                        |> List.map (\( Src.At _ fieldName, fieldValue ) -> inferFieldType fieldValue ++ " " ++ fieldName)
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

                numElements =
                    List.length elements

                tupleType =
                    if numElements == 2 then
                        "elm_tuple2_t"
                    else if numElements == 3 then
                        "elm_tuple3_t"
                    else
                        "struct { " ++ (List.indexedMap (\i _ -> "int _" ++ String.fromInt i) elements |> String.join "; ") ++ "; }"

                values =
                    List.map generateStandaloneExpr elements
                        |> String.join ", "
            in
            ( tupleType, "{" ++ values ++ "}" )

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

        Src.Str s ->
            ( "const char *", "\"" ++ escapeC s ++ "\"" )

        Src.Int n ->
            ( "double", String.fromInt n )

        Src.Float f ->
            ( "double", String.fromFloat f )

        Src.Call (Src.At _ (Src.Var Src.CapVar _)) _ ->
            -- Constructor call returns elm_union_t
            ( "elm_union_t", generateStandaloneExpr locatedExpr )

        Src.Call (Src.At _ (Src.VarQual Src.CapVar _ _)) _ ->
            -- Qualified constructor call returns elm_union_t
            ( "elm_union_t", generateStandaloneExpr locatedExpr )

        Src.Call (Src.At _ (Src.VarQual _ "String" _)) _ ->
            -- String module function call returns string
            ( "const char *", generateStandaloneExpr locatedExpr )

        Src.Call (Src.At _ (Src.VarQual _ "List" "partition")) _ ->
            -- List.partition returns (List a, List a) tuple - use custom struct for list elements
            ( "struct { elm_list_t _0; elm_list_t _1; }", generateStandaloneExpr locatedExpr )

        Src.Call (Src.At _ (Src.VarQual _ "List" "unzip")) _ ->
            -- List.unzip returns (List a, List b) tuple - use custom struct for list elements
            ( "struct { elm_list_t _0; elm_list_t _1; }", generateStandaloneExpr locatedExpr )

        Src.Call (Src.At _ (Src.Var _ funcName)) _ ->
            -- Check if function call result is a string or record
            let
                cExpr = generateStandaloneExpr locatedExpr
                -- String functions, or user functions with string-like names
                isStringResult =
                    String.contains "elm_str_" cExpr
                        || String.contains "elm_from_" cExpr
                        || String.contains "Header" funcName
                        || String.contains "header" funcName
                        || String.contains "List" funcName
                        || String.contains "String" funcName
                        || String.contains "format" funcName
            in
            if isStringResult then
                ( "const char *", cExpr )
            else
                -- Use typeof to infer the return type (works for records, etc.)
                ( "typeof(" ++ cExpr ++ ")", cExpr )

        Src.Binops pairs finalExpr ->
            -- Check if this is string concatenation
            let
                isStringConcat = List.all (\( _, Src.At _ op ) -> op == "++") pairs
                cExpr = generateStandaloneExpr locatedExpr
            in
            if isStringConcat then
                ( "const char *", cExpr )
            else
                ( "double", cExpr )

        Src.Var Src.CapVar _ ->
            -- Standalone constructor (nullary) returns elm_union_t
            ( "elm_union_t", generateStandaloneExpr locatedExpr )

        Src.VarQual Src.CapVar _ _ ->
            -- Qualified constructor (nullary) returns elm_union_t
            ( "elm_union_t", generateStandaloneExpr locatedExpr )

        Src.Var Src.LowVar name ->
            -- Reference to a variable - could be a local binding or module-level constant
            -- Use typeof to infer the correct type (works for records, etc.)
            ( "typeof(elm_" ++ name ++ ")", "elm_" ++ name )

        Src.List _ ->
            -- List literal returns elm_list_t
            ( "elm_list_t", generateStandaloneExpr locatedExpr )

        _ ->
            ( "double", generateStandaloneExpr locatedExpr )


{-| Infer C type and initializer for an expression with context for module prefixing
-}
inferCTypeAndInitWithCtx : ExprCtx -> Src.Expr -> ( String, String )
inferCTypeAndInitWithCtx ctx locatedExpr =
    let
        (Src.At _ expr) =
            locatedExpr
    in
    case expr of
        Src.Record fields ->
            let
                inferFieldType : Src.Expr -> String
                inferFieldType fieldValue =
                    let
                        valueStr = generateStandaloneExprWithCtx ctx fieldValue
                    in
                    if String.startsWith "\"" valueStr then
                        "const char *"
                    else if String.contains "elm_str_" valueStr || String.contains "elm_from_" valueStr then
                        "const char *"
                    else
                        "double"

                fieldDefs =
                    fields
                        |> List.map (\( Src.At _ fieldName, fieldValue ) -> inferFieldType fieldValue ++ " " ++ fieldName)
                        |> String.join "; "

                fieldValues =
                    fields
                        |> List.map
                            (\( Src.At _ fieldName, fieldValue ) ->
                                "." ++ fieldName ++ " = " ++ generateStandaloneExprWithCtx ctx fieldValue
                            )
                        |> String.join ", "
            in
            ( "struct { " ++ fieldDefs ++ "; }", "{" ++ fieldValues ++ "}" )

        Src.Tuple first second rest ->
            let
                elements =
                    first :: second :: rest

                numElements =
                    List.length elements

                tupleType =
                    if numElements == 2 then
                        "elm_tuple2_t"
                    else if numElements == 3 then
                        "elm_tuple3_t"
                    else
                        "struct { " ++ (List.indexedMap (\i _ -> "int _" ++ String.fromInt i) elements |> String.join "; ") ++ "; }"

                values =
                    List.map (generateStandaloneExprWithCtx ctx) elements
                        |> String.join ", "
            in
            ( tupleType, "{" ++ values ++ "}" )

        Src.Update (Src.At _ recordName) updates ->
            let
                updateAssignments =
                    updates
                        |> List.map
                            (\( Src.At _ fieldName, valueExpr ) ->
                                "__update_tmp." ++ fieldName ++ " = " ++ generateStandaloneExprWithCtx ctx valueExpr ++ ";"
                            )
                        |> String.join " "
            in
            ( "typeof(elm_" ++ recordName ++ ")"
            , "({ typeof(elm_" ++ recordName ++ ") __update_tmp = elm_" ++ recordName ++ "; " ++ updateAssignments ++ " __update_tmp; })"
            )

        Src.Str s ->
            ( "const char *", "\"" ++ escapeC s ++ "\"" )

        Src.Int n ->
            ( "double", String.fromInt n )

        Src.Float f ->
            ( "double", String.fromFloat f )

        Src.Call (Src.At _ (Src.Var Src.CapVar _)) _ ->
            ( "elm_union_t", generateStandaloneExprWithCtx ctx locatedExpr )

        Src.Call (Src.At _ (Src.VarQual Src.CapVar _ _)) _ ->
            ( "elm_union_t", generateStandaloneExprWithCtx ctx locatedExpr )

        Src.Call (Src.At _ (Src.VarQual _ "String" _)) _ ->
            ( "const char *", generateStandaloneExprWithCtx ctx locatedExpr )

        Src.Call (Src.At _ (Src.VarQual _ "List" "partition")) _ ->
            ( "struct { elm_list_t _0; elm_list_t _1; }", generateStandaloneExprWithCtx ctx locatedExpr )

        Src.Call (Src.At _ (Src.VarQual _ "List" "unzip")) _ ->
            ( "struct { elm_list_t _0; elm_list_t _1; }", generateStandaloneExprWithCtx ctx locatedExpr )

        Src.Call (Src.At _ (Src.Var _ funcName)) _ ->
            let
                cExpr = generateStandaloneExprWithCtx ctx locatedExpr
                isStringResult =
                    String.contains "elm_str_" cExpr
                        || String.contains "elm_from_" cExpr
                        || String.contains "Header" funcName
                        || String.contains "header" funcName
                        || String.contains "List" funcName
                        || String.contains "String" funcName
                        || String.contains "format" funcName
            in
            if isStringResult then
                ( "const char *", cExpr )
            else
                ( "typeof(" ++ cExpr ++ ")", cExpr )

        Src.Binops pairs finalExpr ->
            let
                isStringConcat = List.all (\( _, Src.At _ op ) -> op == "++") pairs
                cExpr = generateStandaloneExprWithCtx ctx locatedExpr
            in
            if isStringConcat then
                ( "const char *", cExpr )
            else
                ( "double", cExpr )

        Src.Var Src.CapVar _ ->
            ( "elm_union_t", generateStandaloneExprWithCtx ctx locatedExpr )

        Src.VarQual Src.CapVar _ _ ->
            ( "elm_union_t", generateStandaloneExprWithCtx ctx locatedExpr )

        Src.Var Src.LowVar name ->
            -- Check if this is a user function that needs module prefix
            if List.member name ctx.userFunctions && not (String.isEmpty ctx.modulePrefix) then
                ( "typeof(elm_" ++ ctx.modulePrefix ++ "_" ++ name ++ ")", "elm_" ++ ctx.modulePrefix ++ "_" ++ name )
            else
                ( "typeof(elm_" ++ name ++ ")", "elm_" ++ name )

        Src.List _ ->
            ( "elm_list_t", generateStandaloneExprWithCtx ctx locatedExpr )

        _ ->
            ( "double", generateStandaloneExprWithCtx ctx locatedExpr )


{-| Generate standalone C code for let bindings using GCC compound statements
-}
generateStandaloneLet : List (Src.Located Src.Def) -> Src.Expr -> String
generateStandaloneLet defs body =
    generateStandaloneLetWithPrefix "main" defs body


{-| Generate standalone C code for let bindings with full context
-}
generateStandaloneLetWithCtx : ExprCtx -> List (Src.Located Src.Def) -> Src.Expr -> String
generateStandaloneLetWithCtx ctx defs body =
    generateStandaloneLetWithPrefixAndCtx ctx.funcPrefix ctx defs body


{-| Generate standalone C code for let bindings with a known prefix for lifted functions
-}
generateStandaloneLetWithPrefix : String -> List (Src.Located Src.Def) -> Src.Expr -> String
generateStandaloneLetWithPrefix prefix defs body =
    let
        -- Collect info about local functions for macro generation
        localFuncInfo =
            defs
                |> List.filterMap
                    (\(Src.At _ def) ->
                        case def of
                            Src.Define (Src.At _ name) args defBody _ ->
                                if not (List.isEmpty args) then
                                    let
                                        -- Get parameter names
                                        argNames =
                                            List.concatMap patternVars args

                                        -- Get all var refs in body
                                        allRefs =
                                            collectVarRefs defBody |> Shared.uniqueStrings

                                        -- Collect names of values defined in this let (excluding functions)
                                        letValueNames =
                                            defs
                                                |> List.filterMap
                                                    (\(Src.At _ d) ->
                                                        case d of
                                                            Src.Define (Src.At _ n) a _ _ ->
                                                                if List.isEmpty a then Just n else Nothing

                                                            _ ->
                                                                Nothing
                                                    )

                                        -- Collect names of other local functions in this let block
                                        localFuncNames =
                                            defs
                                                |> List.filterMap
                                                    (\(Src.At _ d) ->
                                                        case d of
                                                            Src.Define (Src.At _ n) a _ _ ->
                                                                if not (List.isEmpty a) then Just n else Nothing

                                                            _ ->
                                                                Nothing
                                                    )

                                        -- Standard library names that aren't captured
                                        stdlibNames =
                                            [ "String", "List", "Maybe", "Result", "Debug", "Basics" ]

                                        -- Captured = refs that are NOT:
                                        -- - the local function's own args
                                        -- - other local functions in this let block
                                        -- - stdlib module names
                                        -- This includes both let bindings AND outer function parameters
                                        capturedVars =
                                            allRefs
                                                |> List.filter
                                                    (\v ->
                                                        not (List.member v argNames)
                                                            && not (List.member v localFuncNames)
                                                            && not (List.member v stdlibNames)
                                                    )
                                    in
                                    Just { name = name, numArgs = List.length args, capturedVars = capturedVars }

                                else
                                    Nothing

                            Src.Destruct _ _ ->
                                Nothing
                    )

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
                        -- Local function - generate a macro that maps local calls to lifted function
                        -- with captured variables appended
                        let
                            funcInfo =
                                localFuncInfo
                                    |> List.filter (\f -> f.name == name)
                                    |> List.head

                            capturedVars =
                                funcInfo
                                    |> Maybe.map .capturedVars
                                    |> Maybe.withDefault []

                            -- Generate macro: #define elm_name(args) elm_prefix_name(args, captured1, captured2, ...)
                            -- Use __VA_ARGS__ for flexibility
                            capturedArgs =
                                if List.isEmpty capturedVars then
                                    ""
                                else
                                    ", " ++ (capturedVars |> List.map (\v -> "elm_" ++ v) |> String.join ", ")
                        in
                        "#define elm_" ++ name ++ "(...) elm_" ++ prefix ++ "_" ++ name ++ "(__VA_ARGS__" ++ capturedArgs ++ ")"

                Src.Destruct pattern expr ->
                    generateDestructuring pattern expr

        -- Generate #undef for each local function after the body
        undefMacros =
            localFuncInfo
                |> List.map (\f -> "#undef elm_" ++ f.name)

        defStrs =
            List.map generateDef defs

        -- Use context-aware generation for the body to handle nested lets correctly
        bodyStr =
            generateStandaloneExprWithCtx { funcPrefix = prefix, modulePrefix = "", userFunctions = [] } body

        undefStrs =
            if List.isEmpty undefMacros then
                ""
            else
                "\n        " ++ String.join "\n        " undefMacros
    in
    "({\n        " ++ String.join "\n        " defStrs ++ "\n        " ++ bodyStr ++ ";" ++ undefStrs ++ "\n    })"


{-| Generate standalone C code for let bindings with full context for module prefixing
-}
generateStandaloneLetWithPrefixAndCtx : String -> ExprCtx -> List (Src.Located Src.Def) -> Src.Expr -> String
generateStandaloneLetWithPrefixAndCtx prefix ctx defs body =
    let
        -- Collect info about local functions for macro generation
        localFuncInfo =
            defs
                |> List.filterMap
                    (\(Src.At _ def) ->
                        case def of
                            Src.Define (Src.At _ name) args defBody _ ->
                                if not (List.isEmpty args) then
                                    let
                                        argNames =
                                            List.concatMap patternVars args

                                        allRefs =
                                            collectVarRefs defBody |> Shared.uniqueStrings

                                        letValueNames =
                                            defs
                                                |> List.filterMap
                                                    (\(Src.At _ d) ->
                                                        case d of
                                                            Src.Define (Src.At _ n) a _ _ ->
                                                                if List.isEmpty a then Just n else Nothing
                                                            _ ->
                                                                Nothing
                                                    )

                                        localFuncNames =
                                            defs
                                                |> List.filterMap
                                                    (\(Src.At _ d) ->
                                                        case d of
                                                            Src.Define (Src.At _ n) a _ _ ->
                                                                if not (List.isEmpty a) then Just n else Nothing
                                                            _ ->
                                                                Nothing
                                                    )

                                        stdlibNames =
                                            [ "String", "List", "Maybe", "Result", "Debug", "Basics" ]

                                        capturedVars =
                                            allRefs
                                                |> List.filter
                                                    (\v ->
                                                        not (List.member v argNames)
                                                            && not (List.member v localFuncNames)
                                                            && not (List.member v stdlibNames)
                                                    )
                                    in
                                    Just { name = name, numArgs = List.length args, capturedVars = capturedVars }

                                else
                                    Nothing

                            Src.Destruct _ _ ->
                                Nothing
                    )

        generateDef (Src.At _ def) =
            case def of
                Src.Define (Src.At _ name) args defBody _ ->
                    if List.isEmpty args then
                        let
                            ( varType, varInit ) =
                                inferCTypeAndInitWithCtx ctx defBody
                        in
                        varType ++ " elm_" ++ name ++ " = " ++ varInit ++ ";"

                    else
                        let
                            funcInfo =
                                localFuncInfo
                                    |> List.filter (\f -> f.name == name)
                                    |> List.head

                            capturedVars =
                                funcInfo
                                    |> Maybe.map .capturedVars
                                    |> Maybe.withDefault []

                            capturedArgs =
                                if List.isEmpty capturedVars then
                                    ""
                                else
                                    ", " ++ (capturedVars |> List.map (\v -> "elm_" ++ v) |> String.join ", ")
                        in
                        "#define elm_" ++ name ++ "(...) elm_" ++ prefix ++ "_" ++ name ++ "(__VA_ARGS__" ++ capturedArgs ++ ")"

                Src.Destruct pattern expr ->
                    generateDestructuringWithCtx ctx pattern expr

        undefMacros =
            localFuncInfo
                |> List.map (\f -> "#undef elm_" ++ f.name)

        defStrs =
            List.map generateDef defs

        -- Use full context for the body to handle module-prefixed function calls
        bodyStr =
            generateStandaloneExprWithCtx { funcPrefix = prefix, modulePrefix = ctx.modulePrefix, userFunctions = ctx.userFunctions } body

        undefStrs =
            if List.isEmpty undefMacros then
                ""
            else
                "\n        " ++ String.join "\n        " undefMacros
    in
    "({\n        " ++ String.join "\n        " defStrs ++ "\n        " ++ bodyStr ++ ";" ++ undefStrs ++ "\n    })"


{-| Generate C code for pattern destructuring in let bindings.

    Handles: let (a, b) = expr in ...
    Generates:
        elm_tuple2_t __destruct_N = <expr>;
        double elm_a = __destruct_N._0.d;
        double elm_b = __destruct_N._1.d;
-}
generateDestructuring : Src.Pattern -> Src.Expr -> String
generateDestructuring pattern expr =
    let
        exprStr =
            generateStandaloneExpr expr

        -- Generate unique temp variable name based on pattern hash
        tempName =
            "__destruct_" ++ String.fromInt (String.length exprStr |> modBy 1000)

        -- Check if expr is List.partition (needs special handling to avoid TCC struct return issue)
        partitionArgs =
            case expr of
                Src.At _ (Src.Call (Src.At _ (Src.VarQual _ "List" "partition")) args) ->
                    Just args

                _ ->
                    Nothing

        -- Check if expr is List.unzip (returns tuple of lists)
        isListUnzip =
            case expr of
                Src.At _ (Src.Call (Src.At _ (Src.VarQual _ "List" "unzip")) _) ->
                    True

                _ ->
                    False

        isListTuple =
            partitionArgs /= Nothing || isListUnzip
    in
    -- Special case: List.partition destructuring - inline the partition logic to avoid TCC struct return issue
    case ( pattern, partitionArgs ) of
        ( Src.At _ (Src.PTuple (Src.At _ (Src.PVar name1)) (Src.At _ (Src.PVar name2)) []), Just [ fnExpr, listExpr ] ) ->
            let
                listStr =
                    generateStandaloneExpr listExpr

                fnAppStr =
                    case fnExpr of
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                            "({ double elm_" ++ pname ++ " = __part_lst.data[__i].d; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                        -- Constructor pattern: \(Src.At _ v) -> ...
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ elm_union_t __elem = (elm_union_t)__part_lst.data[__i].d; typeof(__elem.data) elm_" ++ innerName ++ " = __elem.data; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                        -- Qualified constructor pattern: \(Src.At _ v) -> ...
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                            "({ elm_union_t __elem = (elm_union_t)__part_lst.data[__i].d; typeof(__elem.data) elm_" ++ innerName ++ " = __elem.data; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                        -- Tuple pattern: \(a, b, c) -> ...
                        Src.At _ (Src.Lambda [ Src.At _ (Src.PTuple (Src.At _ first) (Src.At _ second) rest) ] lambdaBody) ->
                            let
                                elemType = if List.isEmpty rest then "elm_tuple2_t" else "elm_tuple3_t"
                                bindOne idx pat =
                                    case pat of
                                        Src.PVar vn ->
                                            "typeof(__part_telem._" ++ String.fromInt idx ++ ".d) elm_" ++ vn ++ " = __part_telem._" ++ String.fromInt idx ++ ".d;"
                                        Src.PAnything -> ""
                                        _ -> ""
                                bindings = [ bindOne 0 first, bindOne 1 second ]
                                    ++ (rest |> List.indexedMap (\i (Src.At _ p) -> bindOne (i + 2) p))
                                    |> List.filter (\s -> s /= "")
                                    |> String.join " "
                            in
                            "({ " ++ elemType ++ " __part_telem = *(" ++ elemType ++ "*)&(__part_lst.data[__i].d); " ++ bindings ++ " " ++ generateStandaloneExpr lambdaBody ++ "; })"

                        -- Generic lambda - use helper
                        Src.At _ (Src.Lambda patterns lambdaBody) ->
                            generateInlineLambdaBody patterns lambdaBody "__part_lst.data[__i].d"

                        _ ->
                            generateStandaloneExpr fnExpr ++ "(__part_lst.data[__i].d)"
            in
            "elm_list_t elm_" ++ name1 ++ "; elm_" ++ name1 ++ ".length = 0; elm_list_t elm_" ++ name2 ++ "; elm_" ++ name2 ++ ".length = 0; { elm_list_t __part_lst = " ++ listStr ++ "; for (int __i = 0; __i < __part_lst.length; __i++) { if (" ++ fnAppStr ++ ") elm_" ++ name1 ++ ".data[elm_" ++ name1 ++ ".length++] = __part_lst.data[__i]; else elm_" ++ name2 ++ ".data[elm_" ++ name2 ++ ".length++] = __part_lst.data[__i]; } }"

        _ ->
            -- Standard tuple destructuring
            case pattern of
                Src.At _ (Src.PTuple first second rest) ->
                    let
                        numElements =
                            2 + List.length rest

                        tupleType =
                            if isListTuple && numElements == 2 then
                                "struct { elm_list_t _0; elm_list_t _1; }"
                            else if numElements == 2 then
                                "elm_tuple2_t"
                            else if numElements == 3 then
                                "elm_tuple3_t"
                            else
                                "elm_tuple" ++ String.fromInt numElements ++ "_t"

                        -- Generate the temp tuple variable
                        tupleDecl =
                            tupleType ++ " " ++ tempName ++ " = " ++ exprStr ++ ";"

                        -- Generate bindings for each element
                        allPatterns =
                            first :: second :: rest

                        generateBinding idx pat =
                            case pat of
                                Src.At _ (Src.PVar name) ->
                                    -- For list tuples, elements are elm_list_t, accessed directly
                                    -- For regular tuples, elements are elm_elem_t union, access .d
                                    if isListTuple then
                                        Just ("elm_list_t elm_" ++ name ++ " = " ++ tempName ++ "._" ++ String.fromInt idx ++ ";")
                                    else
                                        Just ("double elm_" ++ name ++ " = " ++ tempName ++ "._" ++ String.fromInt idx ++ ".d;")

                                Src.At _ Src.PAnything ->
                                    -- Wildcard, skip
                                    Nothing

                                Src.At _ (Src.PTuple innerFirst innerSecond innerRest) ->
                                    -- Nested tuple - generate intermediate variable and recurse
                                    let
                                        innerNumElements =
                                            2 + List.length innerRest

                                        innerTupleType =
                                            if innerNumElements == 2 then
                                                "elm_tuple2_t"
                                            else
                                                "elm_tuple3_t"

                                        innerTempName =
                                            tempName ++ "_" ++ String.fromInt idx

                                        innerDecl =
                                            innerTupleType ++ " " ++ innerTempName ++ " = " ++ tempName ++ "._" ++ String.fromInt idx ++ ";"

                                        innerBindings =
                                            (innerFirst :: innerSecond :: innerRest)
                                                |> List.indexedMap (generateNestedBinding innerTempName)
                                                |> List.filterMap identity
                                    in
                                    Just (innerDecl ++ " " ++ String.join " " innerBindings)

                                _ ->
                                    -- Other patterns not yet supported
                                    Just ("/* unsupported pattern in tuple position " ++ String.fromInt idx ++ " */")

                        generateNestedBinding parentTemp idx pat =
                            case pat of
                                Src.At _ (Src.PVar name) ->
                                    Just ("double elm_" ++ name ++ " = " ++ parentTemp ++ "._" ++ String.fromInt idx ++ ".d;")

                                Src.At _ Src.PAnything ->
                                    Nothing

                                _ ->
                                    Just "/* deeply nested pattern not supported */"

                        bindings =
                            allPatterns
                                |> List.indexedMap generateBinding
                                |> List.filterMap identity
                    in
                    tupleDecl ++ " " ++ String.join " " bindings

                Src.At _ (Src.PVar name) ->
                    -- Simple variable binding (shouldn't happen via Destruct, but handle it)
                    "double elm_" ++ name ++ " = " ++ exprStr ++ ";"

                Src.At _ (Src.PRecord fields) ->
                    -- Record destructuring: let { x, y } = expr
                    let
                        fieldBindings =
                            fields
                                |> List.map (\(Src.At _ fieldName) ->
                                    "double elm_" ++ fieldName ++ " = (" ++ exprStr ++ ")." ++ fieldName ++ ";")
                    in
                    String.join " " fieldBindings

                -- Constructor pattern: let (Src.At _ x) = expr
                Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ->
                    "typeof(((elm_union_t)" ++ exprStr ++ ").data) elm_" ++ innerName ++ " = ((elm_union_t)" ++ exprStr ++ ").data;"

                -- Qualified constructor pattern: let (Src.At _ x) = expr
                Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ->
                    "typeof(((elm_union_t)" ++ exprStr ++ ").data) elm_" ++ innerName ++ " = ((elm_union_t)" ++ exprStr ++ ").data;"

                -- Constructor pattern with two bound variables: let (Src.At region x) = expr
                Src.At _ (Src.PCtor _ "At" [ Src.At _ (Src.PVar regionName), Src.At _ (Src.PVar innerName) ]) ->
                    -- For At patterns, region is the tag-related field and innerName is the data
                    "elm_union_t __at_tmp = (elm_union_t)" ++ exprStr ++ "; typeof(__at_tmp.tag) elm_" ++ regionName ++ " = __at_tmp.tag; typeof(__at_tmp.data) elm_" ++ innerName ++ " = __at_tmp.data;"

                -- Qualified constructor pattern with two bound variables
                Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ (Src.PVar regionName), Src.At _ (Src.PVar innerName) ]) ->
                    "elm_union_t __at_tmp = (elm_union_t)" ++ exprStr ++ "; typeof(__at_tmp.tag) elm_" ++ regionName ++ " = __at_tmp.tag; typeof(__at_tmp.data) elm_" ++ innerName ++ " = __at_tmp.data;"

                -- Constructor pattern extracting inner value: let (Just x) = expr
                Src.At _ (Src.PCtor _ ctorName [ Src.At _ (Src.PVar innerName) ]) ->
                    "typeof(((elm_union_t)" ++ exprStr ++ ").data.child->data.num) elm_" ++ innerName ++ " = ((elm_union_t)" ++ exprStr ++ ").data.child->data.num;"

                -- Wildcard pattern (just evaluate expr for side effects)
                Src.At _ Src.PAnything ->
                    "(void)(" ++ exprStr ++ ");"

                _ ->
                    "/* unsupported destructuring pattern */"


{-| Generate C code for pattern destructuring with context for module prefixing.
    This is a simplified version that uses context for expression generation.
-}
generateDestructuringWithCtx : ExprCtx -> Src.Pattern -> Src.Expr -> String
generateDestructuringWithCtx ctx pattern expr =
    let
        exprStr =
            generateStandaloneExprWithCtx ctx expr

        tempName =
            "__destruct_" ++ String.fromInt (String.length exprStr |> modBy 1000)
    in
    case pattern of
        Src.At _ (Src.PTuple first second rest) ->
            let
                numElements =
                    2 + List.length rest

                tupleType =
                    if numElements == 2 then
                        "elm_tuple2_t"
                    else if numElements == 3 then
                        "elm_tuple3_t"
                    else
                        "elm_tuple" ++ String.fromInt numElements ++ "_t"

                tupleDecl =
                    tupleType ++ " " ++ tempName ++ " = " ++ exprStr ++ ";"

                allPatterns =
                    first :: second :: rest

                generateBinding idx pat =
                    case pat of
                        Src.At _ (Src.PVar name) ->
                            Just ("double elm_" ++ name ++ " = " ++ tempName ++ "._" ++ String.fromInt idx ++ ".d;")
                        Src.At _ Src.PAnything ->
                            Nothing
                        _ ->
                            Just ("/* unsupported pattern in tuple position " ++ String.fromInt idx ++ " */")

                bindings =
                    allPatterns
                        |> List.indexedMap generateBinding
                        |> List.filterMap identity
            in
            tupleDecl ++ " " ++ String.join " " bindings

        Src.At _ (Src.PVar name) ->
            "double elm_" ++ name ++ " = " ++ exprStr ++ ";"

        Src.At _ (Src.PRecord fields) ->
            let
                fieldBindings =
                    fields
                        |> List.map (\(Src.At _ fieldName) ->
                            "double elm_" ++ fieldName ++ " = (" ++ exprStr ++ ")." ++ fieldName ++ ";")
            in
            String.join " " fieldBindings

        Src.At _ Src.PAnything ->
            "(void)(" ++ exprStr ++ ");"

        _ ->
            "/* unsupported destructuring pattern */"


{-| Generate standalone C code for if/else expressions using ternary operator
-}
generateStandaloneIf : List ( Src.Expr, Src.Expr ) -> Src.Expr -> String
generateStandaloneIf branches elseExpr =
    Expr.generateIfExpr generateStandaloneExpr branches elseExpr


{-| Generate if/else with function context
-}
generateStandaloneIfWithCtx : ExprCtx -> List ( Src.Expr, Src.Expr ) -> Src.Expr -> String
generateStandaloneIfWithCtx ctx branches elseExpr =
    Expr.generateIfExpr (generateStandaloneExprWithCtx ctx) branches elseExpr


{-| Generate standalone C code for case expressions
Supports pattern guards: `pattern if guard -> body`
-}
generateStandaloneCase : Src.Expr -> List ( Src.Pattern, Maybe Src.Expr, Src.Expr ) -> String
generateStandaloneCase scrutinee branches =
    -- First try the extracted pattern handlers
    case Pattern.generateCaseExpr generateStandaloneExpr scrutinee branches of
        Just result ->
            result

        Nothing ->
            -- Fall back to the full implementation
            generateStandaloneCaseFallback scrutinee branches


{-| Generate case expressions with context for module-prefixed function calls
-}
generateStandaloneCaseWithCtx : ExprCtx -> Src.Expr -> List ( Src.Pattern, Maybe Src.Expr, Src.Expr ) -> String
generateStandaloneCaseWithCtx ctx scrutinee branches =
    -- First try the extracted pattern handlers with context
    case Pattern.generateCaseExpr (generateStandaloneExprWithCtx ctx) scrutinee branches of
        Just result ->
            result

        Nothing ->
            -- Fall back to the full implementation with context
            generateStandaloneCaseFallbackWithCtx ctx scrutinee branches


{-| Fallback implementation for complex pattern matching not yet migrated
-}
generateStandaloneCaseFallback : Src.Expr -> List ( Src.Pattern, Maybe Src.Expr, Src.Expr ) -> String
generateStandaloneCaseFallback scrutinee branches =
    generateStandaloneCaseFallbackWithCtx Shared.defaultExprCtx scrutinee branches


{-| Context-aware fallback for complex pattern matching with module prefix support
-}
generateStandaloneCaseFallbackWithCtx : ExprCtx -> Src.Expr -> List ( Src.Pattern, Maybe Src.Expr, Src.Expr ) -> String
generateStandaloneCaseFallbackWithCtx ctx scrutinee branches =
    let
        scrutineeStr =
            generateStandaloneExprWithCtx ctx scrutinee

        -- Check if any branch uses a variable binding pattern
        hasVarBinding =
            List.any
                (\( Src.At _ pat, _, _ ) ->
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
                (\( Src.At _ pat, _, _ ) ->
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
                (\( Src.At _ pat, _, _ ) ->
                    case pat of
                        Src.PCtor _ name _ ->
                            name /= "True" && name /= "False"

                        Src.PCtorQual _ _ _ _ ->
                            True

                        _ ->
                            False
                )
                branches

        -- Check if this is a list case (has PList or PCons patterns)
        isListCase =
            List.any
                (\( Src.At _ pat, _, _ ) ->
                    case pat of
                        Src.PList _ ->
                            True

                        Src.PCons _ _ ->
                            True

                        _ ->
                            False
                )
                branches

        -- Check if this is a tuple case (has PTuple patterns)
        isTupleCase =
            List.any
                (\( Src.At _ pat, _, _ ) ->
                    case pat of
                        Src.PTuple _ _ _ ->
                            True

                        _ ->
                            False
                )
                branches

        -- Count tuple arity if it's a tuple case
        tupleArity =
            branches
                |> List.filterMap
                    (\( Src.At _ pat, _, _ ) ->
                        case pat of
                            Src.PTuple _ _ rest ->
                                Just (2 + List.length rest)

                            _ ->
                                Nothing
                    )
                |> List.head
                |> Maybe.withDefault 2

        -- Check if case expression returns a union type (by checking first branch's result)
        returnsUnion =
            case branches of
                ( _, _, firstResult ) :: _ ->
                    let
                        resultStr = generateStandaloneExprWithCtx ctx firstResult
                    in
                    String.contains "((elm_union_t){TAG_" resultStr
                        || String.contains "elm_union_t" resultStr

                [] ->
                    False

        -- Default fallback value based on detected return type
        fallbackValue =
            if returnsUnion then
                "((elm_union_t){0, 0}) /* no match */"
            else
                "0 /* no match */"

        -- Helper to wrap result with guard condition
        wrapWithGuard : Maybe Src.Expr -> String -> String -> String
        wrapWithGuard maybeGuard bodyStr restStr =
            case maybeGuard of
                Nothing ->
                    bodyStr

                Just guardExpr ->
                    let
                        guardStr = generateStandaloneExprWithCtx ctx guardExpr
                    in
                    "(" ++ guardStr ++ " ? " ++ bodyStr ++ " : " ++ restStr ++ ")"

        generateBranches : List ( Src.Pattern, Maybe Src.Expr, Src.Expr ) -> String
        generateBranches bs =
            case bs of
                [] ->
                    fallbackValue

                ( Src.At _ pattern, maybeGuard, resultExpr ) :: rest ->
                    case pattern of
                        Src.PAnything ->
                            -- Wildcard always matches (but guard may filter)
                            wrapWithGuard maybeGuard (generateStandaloneExprWithCtx ctx resultExpr) (generateBranches rest)

                        Src.PVar varName ->
                            -- Variable binding - bind scrutinee to variable name
                            -- Use appropriate type based on detected scrutinee type
                            let
                                varType =
                                    if isListCase then
                                        "elm_list_t"
                                    else if isCustomTypeCase then
                                        "elm_union_t"
                                    else
                                        "double"

                                bodyStr =
                                    "({\n            " ++ varType ++ " elm_"
                                        ++ varName
                                        ++ " = elm_case_scrutinee;\n            "
                                        ++ (case maybeGuard of
                                                Nothing ->
                                                    generateStandaloneExprWithCtx ctx resultExpr

                                                Just guardExpr ->
                                                    "(" ++ generateStandaloneExprWithCtx ctx guardExpr ++ " ? "
                                                        ++ generateStandaloneExprWithCtx ctx resultExpr
                                                        ++ " : " ++ generateBranches rest ++ ")"
                                           )
                                        ++ ";\n        })"
                            in
                            bodyStr

                        Src.PInt n ->
                            -- Integer pattern
                            let
                                condition = "elm_case_scrutinee == " ++ String.fromInt n
                                bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                fullBody = wrapWithGuard maybeGuard bodyStr (generateBranches rest)
                            in
                            "(" ++ condition ++ " ? " ++ fullBody ++ " : " ++ generateBranches rest ++ ")"

                        Src.PStr s ->
                            -- String pattern
                            let
                                condition = "strcmp(elm_case_scrutinee, \"" ++ escapeC s ++ "\") == 0"
                                bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                fullBody = wrapWithGuard maybeGuard bodyStr (generateBranches rest)
                            in
                            "(" ++ condition ++ " ? " ++ fullBody ++ " : " ++ generateBranches rest ++ ")"

                        Src.PChr c ->
                            -- Char pattern
                            let
                                condition = "elm_case_scrutinee == '" ++ escapeC c ++ "'"
                                bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                fullBody = wrapWithGuard maybeGuard bodyStr (generateBranches rest)
                            in
                            "(" ++ condition ++ " ? " ++ fullBody ++ " : " ++ generateBranches rest ++ ")"

                        Src.PTuple first second restPats ->
                            -- Tuple pattern - destructure and bind variables, check conditions
                            let
                                allPats =
                                    first :: second :: restPats

                                -- Generate conditions for integer patterns
                                conditions =
                                    allPats
                                        |> List.indexedMap
                                            (\i (Src.At _ pat) ->
                                                case pat of
                                                    Src.PInt n ->
                                                        Just ("elm_case_scrutinee._" ++ String.fromInt i ++ ".d == " ++ String.fromInt n)

                                                    _ ->
                                                        Nothing
                                            )
                                        |> List.filterMap identity

                                -- Generate bindings for variable patterns
                                bindings =
                                    allPats
                                        |> List.indexedMap
                                            (\i (Src.At _ pat) ->
                                                case pat of
                                                    Src.PVar varName ->
                                                        "double elm_" ++ varName ++ " = elm_case_scrutinee._" ++ String.fromInt i ++ ".d;"

                                                    _ ->
                                                        ""
                                            )
                                        |> List.filter (not << String.isEmpty)
                                        |> String.join "\n                "

                                bodyStr =
                                    generateStandaloneExprWithCtx ctx resultExpr

                                -- Add guard condition if present
                                guardedBodyStr =
                                    case maybeGuard of
                                        Nothing ->
                                            bodyStr

                                        Just guardExpr ->
                                            "(" ++ generateStandaloneExprWithCtx ctx guardExpr ++ " ? " ++ bodyStr ++ " : " ++ generateBranches rest ++ ")"
                            in
                            if List.isEmpty conditions then
                                -- No conditions - just bind variables and return
                                if String.isEmpty bindings then
                                    guardedBodyStr

                                else
                                    "({\n                "
                                        ++ bindings
                                        ++ "\n                "
                                        ++ guardedBodyStr
                                        ++ ";\n            })"

                            else
                                -- Has conditions - generate ternary with bindings
                                let
                                    conditionStr =
                                        String.join " && " conditions

                                    bodyWithBindings =
                                        if String.isEmpty bindings then
                                            guardedBodyStr

                                        else
                                            "({\n                "
                                                ++ bindings
                                                ++ "\n                "
                                                ++ guardedBodyStr
                                                ++ ";\n            })"
                                in
                                "(" ++ conditionStr ++ " ? " ++ bodyWithBindings ++ " : " ++ generateBranches rest ++ ")"

                        Src.PCtor _ ctorName ctorPatterns ->
                            -- Constructor pattern
                            case ctorName of
                                "True" ->
                                    let
                                        bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                        fullBody = wrapWithGuard maybeGuard bodyStr (generateBranches rest)
                                    in
                                    "(elm_case_scrutinee ? "
                                        ++ fullBody
                                        ++ " : "
                                        ++ generateBranches rest
                                        ++ ")"

                                "False" ->
                                    let
                                        bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                        fullBody = wrapWithGuard maybeGuard bodyStr (generateBranches rest)
                                    in
                                    "(!elm_case_scrutinee ? "
                                        ++ fullBody
                                        ++ " : "
                                        ++ generateBranches rest
                                        ++ ")"

                                _ ->
                                    if List.isEmpty ctorPatterns then
                                        -- Simple enum constructor - compare tag
                                        let
                                            bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                            fullBody = wrapWithGuard maybeGuard bodyStr (generateBranches rest)
                                        in
                                        "(elm_case_scrutinee.tag == TAG_"
                                            ++ ctorName
                                            ++ " ? "
                                            ++ fullBody
                                            ++ " : "
                                            ++ generateBranches rest
                                            ++ ")"

                                    else
                                        -- Constructor with data - bind variable and compare tag
                                        -- Check if the variable is used as a union (passed to function with elm_union_t param)
                                        let
                                            resultStr = generateStandaloneExprWithCtx ctx resultExpr

                                            -- Generate conditions for nested constructor patterns (e.g., Just North -> check inner tag)
                                            innerConditions =
                                                ctorPatterns
                                                    |> List.indexedMap
                                                        (\patIdx (Src.At _ pat) ->
                                                            let
                                                                accessor =
                                                                    if patIdx == 0 then
                                                                        "elm_case_scrutinee.data.child"
                                                                    else
                                                                        "elm_case_scrutinee.data2"
                                                            in
                                                            case pat of
                                                                Src.PCtor _ innerCtorName [] ->
                                                                    -- Nested nullary constructor (e.g., Just North)
                                                                    Just (accessor ++ "->tag == TAG_" ++ innerCtorName)

                                                                Src.PCtorQual _ innerModName innerCtorName [] ->
                                                                    -- Nested qualified nullary constructor
                                                                    Just (accessor ++ "->tag == TAG_" ++ innerModName ++ "_" ++ innerCtorName)

                                                                _ ->
                                                                    Nothing
                                                        )
                                                    |> List.filterMap identity

                                            bindings =
                                                ctorPatterns
                                                    |> List.indexedMap
                                                        (\patIdx (Src.At _ pat) ->
                                                            case pat of
                                                                Src.PVar varName ->
                                                                    -- Check if variable is used as union, string, or number in result expression
                                                                    let
                                                                        -- Check if used as string (string append, concatenation)
                                                                        -- Also check if other branches return string literals
                                                                        otherBranchesCode = generateBranches rest
                                                                        otherBranchesHaveStrings =
                                                                            String.contains "\"" otherBranchesCode
                                                                                && not (String.contains "elm_from_" otherBranchesCode)

                                                                        isUsedAsString =
                                                                            String.contains ("elm_str_append(elm_" ++ varName ++ ",") resultStr
                                                                                || String.contains ("elm_str_append(elm_" ++ varName ++ ")") resultStr
                                                                                || String.contains (", elm_" ++ varName ++ ")") resultStr
                                                                                    && String.contains "elm_str_" resultStr
                                                                                || String.contains ("\".\", elm_" ++ varName) resultStr
                                                                                || String.endsWith ("elm_" ++ varName ++ ")") resultStr
                                                                                    && String.contains "elm_str_append" resultStr
                                                                                -- Common string variable names
                                                                                || String.contains "Name" varName
                                                                                || String.contains "name" varName
                                                                                || varName == "s"
                                                                                || varName == "str"
                                                                                -- If result is just the variable and other branches return strings
                                                                                || (resultStr == "elm_" ++ varName && otherBranchesHaveStrings)

                                                                        isPassedToPrimitive =
                                                                            String.contains ("elm_from_int(elm_" ++ varName ++ ")") resultStr
                                                                                || String.contains ("elm_from_float(elm_" ++ varName ++ ")") resultStr
                                                                                || String.contains ("+ elm_" ++ varName ++ ")") resultStr
                                                                                || String.contains ("+ elm_" ++ varName ++ ";") resultStr
                                                                                || String.contains ("- elm_" ++ varName ++ ")") resultStr
                                                                                || String.contains ("- elm_" ++ varName ++ ";") resultStr
                                                                                || String.contains ("* elm_" ++ varName ++ ")") resultStr
                                                                                || String.contains ("* elm_" ++ varName ++ ";") resultStr
                                                                                || String.contains ("/ elm_" ++ varName ++ ")") resultStr
                                                                                || String.contains ("/ elm_" ++ varName ++ ";") resultStr

                                                                        isUsedAsUnion =
                                                                            not isPassedToPrimitive
                                                                                && not isUsedAsString
                                                                                && (String.contains ("(elm_" ++ varName ++ ")") resultStr
                                                                                    || String.contains ("(elm_" ++ varName ++ ",") resultStr
                                                                                    || String.contains (", elm_" ++ varName ++ ")") resultStr
                                                                                    || String.contains ("elm_" ++ varName ++ ".tag") resultStr)

                                                                        -- Check if used as a struct (has field accesses that aren't union/list fields)
                                                                        prefix = "elm_" ++ varName ++ "."

                                                                        -- Extract field names accessed on this variable
                                                                        extractStructFields : String -> List String
                                                                        extractStructFields str =
                                                                            let
                                                                                -- Helper to take characters while predicate is true
                                                                                takeWhileChars : (Char -> Bool) -> List Char -> List Char
                                                                                takeWhileChars pred chars =
                                                                                    case chars of
                                                                                        [] -> []
                                                                                        c :: rest2 ->
                                                                                            if pred c then
                                                                                                c :: takeWhileChars pred rest2
                                                                                            else
                                                                                                []

                                                                                -- Find all occurrences of elm_varName.fieldName
                                                                                findField remaining =
                                                                                    case String.indexes prefix remaining of
                                                                                        [] -> []
                                                                                        idx :: _ ->
                                                                                            let
                                                                                                afterPrefix = String.dropLeft (idx + String.length prefix) remaining
                                                                                                -- Extract field name (alphanumeric chars until non-alphanum)
                                                                                                fieldChars = String.toList afterPrefix
                                                                                                    |> takeWhileChars (\c -> Char.isAlphaNum c || c == '_')
                                                                                                fieldName = String.fromList fieldChars
                                                                                                restStr = String.dropLeft (idx + String.length prefix + String.length fieldName) remaining
                                                                                            in
                                                                                            if String.isEmpty fieldName then
                                                                                                findField restStr
                                                                                            else
                                                                                                fieldName :: findField restStr
                                                                            in
                                                                            findField str
                                                                                |> List.filter (\f -> f /= "tag" && f /= "data" && f /= "length" && f /= "child")
                                                                                |> List.foldr (\f acc -> if List.member f acc then acc else f :: acc) []

                                                                        structFields = extractStructFields resultStr

                                                                        -- Special case: At constructor's first arg is always a Region
                                                                        isAtFirstArg2 = ctorName == "At" && patIdx == 0

                                                                        isUsedAsStruct = not (List.isEmpty structFields) || isAtFirstArg2

                                                                        -- Determine struct type based on fields
                                                                        -- For Region type (has start/end with row/col), or At constructor first arg
                                                                        isRegionType = List.member "start" structFields || List.member "end" structFields || isAtFirstArg2

                                                                        -- For Position type (has row/col)
                                                                        isPositionType = (List.member "row" structFields || List.member "col" structFields)
                                                                            && not isRegionType

                                                                        structType =
                                                                            if isRegionType then
                                                                                "struct { struct { int row; int col; } start; struct { int row; int col; } end; }"
                                                                            else if isPositionType then
                                                                                "struct { int row; int col; }"
                                                                            else
                                                                                -- Generic struct with double fields
                                                                                let
                                                                                    fieldDefs = structFields |> List.map (\f -> "double " ++ f) |> String.join "; "
                                                                                in
                                                                                "struct { " ++ fieldDefs ++ "; }"

                                                                        -- First arg from data.child, second arg from data2
                                                                        accessor =
                                                                            if patIdx == 0 then
                                                                                "elm_case_scrutinee.data.child"
                                                                            else
                                                                                "elm_case_scrutinee.data2"
                                                                    in
                                                                    if isUsedAsUnion then
                                                                        "elm_union_t elm_" ++ varName ++ " = *" ++ accessor ++ ";"
                                                                    else if isUsedAsString then
                                                                        "const char *elm_" ++ varName ++ " = " ++ accessor ++ "->data.str;"
                                                                    else if isUsedAsStruct then
                                                                        -- Cast data.ptr to struct pointer and dereference
                                                                        structType ++ " elm_" ++ varName ++ " = *(" ++ structType ++ "*)" ++ accessor ++ "->data.ptr;"
                                                                    else
                                                                        "double elm_" ++ varName ++ " = " ++ accessor ++ "->data.num;"

                                                                _ ->
                                                                    ""
                                                        )
                                                    |> List.filter (not << String.isEmpty)
                                                    |> String.join " "

                                            -- Apply guard if present
                                            guardedResultStr =
                                                case maybeGuard of
                                                    Nothing ->
                                                        resultStr

                                                    Just guardExpr ->
                                                        "(" ++ generateStandaloneExprWithCtx ctx guardExpr ++ " ? " ++ resultStr ++ " : " ++ generateBranches rest ++ ")"

                                            -- Combine outer tag condition with inner conditions
                                            fullCondition =
                                                ("elm_case_scrutinee.tag == TAG_" ++ ctorName)
                                                    :: innerConditions
                                                    |> String.join " && "
                                        in
                                        "(" ++ fullCondition
                                            ++ " ? ({ "
                                            ++ bindings
                                            ++ " "
                                            ++ guardedResultStr
                                            ++ "; }) : "
                                            ++ generateBranches rest
                                            ++ ")"

                        -- Qualified constructor pattern (e.g., Src.Str, Src.Int)
                        Src.PCtorQual _ moduleName ctorName ctorPatterns ->
                            let
                                fullCtorName = moduleName ++ "_" ++ ctorName
                            in
                            if List.isEmpty ctorPatterns then
                                -- Simple enum constructor - compare tag
                                let
                                    bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                    fullBody = wrapWithGuard maybeGuard bodyStr (generateBranches rest)
                                in
                                "(elm_case_scrutinee.tag == TAG_"
                                    ++ fullCtorName
                                    ++ " ? "
                                    ++ fullBody
                                    ++ " : "
                                    ++ generateBranches rest
                                    ++ ")"

                            else if List.all (\(Src.At _ p) -> p == Src.PAnything) ctorPatterns then
                                -- Constructor with wildcard args - just compare tag
                                let
                                    bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                    fullBody = wrapWithGuard maybeGuard bodyStr (generateBranches rest)
                                in
                                "(elm_case_scrutinee.tag == TAG_"
                                    ++ fullCtorName
                                    ++ " ? "
                                    ++ fullBody
                                    ++ " : "
                                    ++ generateBranches rest
                                    ++ ")"

                            else
                                -- Constructor with data - bind variable and compare tag
                                let
                                    resultStr = generateStandaloneExprWithCtx ctx resultExpr

                                    -- Generate conditions for nested constructor patterns (e.g., Just North -> check inner tag)
                                    innerConditions2 =
                                        ctorPatterns
                                            |> List.indexedMap
                                                (\patIdx (Src.At _ pat) ->
                                                    let
                                                        accessor =
                                                            if patIdx == 0 then
                                                                "elm_case_scrutinee.data.child"
                                                            else
                                                                "elm_case_scrutinee.data2"
                                                    in
                                                    case pat of
                                                        Src.PCtor _ innerCtorName [] ->
                                                            -- Nested nullary constructor (e.g., Just North)
                                                            Just (accessor ++ "->tag == TAG_" ++ innerCtorName)

                                                        Src.PCtorQual _ innerModName innerCtorName [] ->
                                                            -- Nested qualified nullary constructor
                                                            Just (accessor ++ "->tag == TAG_" ++ innerModName ++ "_" ++ innerCtorName)

                                                        _ ->
                                                            Nothing
                                                )
                                            |> List.filterMap identity

                                    bindings =
                                        ctorPatterns
                                            |> List.indexedMap
                                                (\patIdx (Src.At _ pat) ->
                                                    case pat of
                                                        Src.PVar varName ->
                                                            -- Check if variable is used as union, string, or number in result expression
                                                            let
                                                                -- Check if used as string (string append, concatenation)
                                                                -- Also check if other branches return string literals
                                                                otherBranchesCode2 = generateBranches rest
                                                                otherBranchesHaveStrings2 =
                                                                    String.contains "\"" otherBranchesCode2
                                                                        && not (String.contains "elm_from_" otherBranchesCode2)

                                                                isUsedAsString =
                                                                    String.contains ("elm_str_append(elm_" ++ varName ++ ",") resultStr
                                                                        || String.contains ("elm_str_append(elm_" ++ varName ++ ")") resultStr
                                                                        || String.contains (", elm_" ++ varName ++ ")") resultStr
                                                                            && String.contains "elm_str_" resultStr
                                                                        || String.contains ("\".\", elm_" ++ varName) resultStr
                                                                        || String.endsWith ("elm_" ++ varName ++ ")") resultStr
                                                                            && String.contains "elm_str_append" resultStr
                                                                        -- Common string variable names
                                                                        || String.contains "Name" varName
                                                                        || String.contains "name" varName
                                                                        || varName == "s"
                                                                        || varName == "str"
                                                                        -- If result is just the variable and other branches return strings
                                                                        || (resultStr == "elm_" ++ varName && otherBranchesHaveStrings2)

                                                                isPassedToPrimitive =
                                                                    String.contains ("elm_from_int(elm_" ++ varName) resultStr
                                                                        || String.contains ("elm_from_float(elm_" ++ varName) resultStr
                                                                        || String.contains ("+ elm_" ++ varName) resultStr
                                                                        || String.contains ("- elm_" ++ varName) resultStr
                                                                        || String.contains ("* elm_" ++ varName) resultStr
                                                                        || String.contains ("/ elm_" ++ varName) resultStr

                                                                isUsedAsUnion =
                                                                    not isPassedToPrimitive
                                                                        && not isUsedAsString
                                                                        && (String.contains ("(elm_" ++ varName ++ ")") resultStr
                                                                            || String.contains ("(elm_" ++ varName ++ ",") resultStr
                                                                            || String.contains (", elm_" ++ varName ++ ")") resultStr
                                                                            || String.contains ("elm_" ++ varName ++ ".tag") resultStr)

                                                                -- Check if used as a struct (has field accesses that aren't union/list fields)
                                                                prefix2 = "elm_" ++ varName ++ "."

                                                                -- Extract field names accessed on this variable
                                                                extractStructFields2 : String -> List String
                                                                extractStructFields2 str =
                                                                    let
                                                                        -- Helper to take characters while predicate is true
                                                                        takeWhileChars2 : (Char -> Bool) -> List Char -> List Char
                                                                        takeWhileChars2 pred chars =
                                                                            case chars of
                                                                                [] -> []
                                                                                c :: rest3 ->
                                                                                    if pred c then
                                                                                        c :: takeWhileChars2 pred rest3
                                                                                    else
                                                                                        []

                                                                        -- Find all occurrences of elm_varName.fieldName
                                                                        findField2 remaining =
                                                                            case String.indexes prefix2 remaining of
                                                                                [] -> []
                                                                                idx :: _ ->
                                                                                    let
                                                                                        afterPrefix = String.dropLeft (idx + String.length prefix2) remaining
                                                                                        -- Extract field name (alphanumeric chars until non-alphanum)
                                                                                        fieldChars = String.toList afterPrefix
                                                                                            |> takeWhileChars2 (\c -> Char.isAlphaNum c || c == '_')
                                                                                        fieldName = String.fromList fieldChars
                                                                                        restStr2 = String.dropLeft (idx + String.length prefix2 + String.length fieldName) remaining
                                                                                    in
                                                                                    if String.isEmpty fieldName then
                                                                                        findField2 restStr2
                                                                                    else
                                                                                        fieldName :: findField2 restStr2
                                                                    in
                                                                    findField2 str
                                                                        |> List.filter (\f -> f /= "tag" && f /= "data" && f /= "length" && f /= "child")
                                                                        |> List.foldr (\f acc -> if List.member f acc then acc else f :: acc) []

                                                                structFields2 = extractStructFields2 resultStr

                                                                -- Special case: At constructor's first arg is always a Region
                                                                isAtFirstArg = ctorName == "At" && patIdx == 0

                                                                isUsedAsStruct2 = not (List.isEmpty structFields2) || isAtFirstArg

                                                                -- Determine struct type based on fields
                                                                -- For Region type (has start/end with row/col), or At constructor first arg
                                                                isRegionType2 = List.member "start" structFields2 || List.member "end" structFields2 || isAtFirstArg

                                                                -- For Position type (has row/col)
                                                                isPositionType2 = (List.member "row" structFields2 || List.member "col" structFields2)
                                                                    && not isRegionType2

                                                                structType2 =
                                                                    if isRegionType2 then
                                                                        "struct { struct { int row; int col; } start; struct { int row; int col; } end; }"
                                                                    else if isPositionType2 then
                                                                        "struct { int row; int col; }"
                                                                    else
                                                                        -- Generic struct with double fields
                                                                        let
                                                                            fieldDefs2 = structFields2 |> List.map (\f -> "double " ++ f) |> String.join "; "
                                                                        in
                                                                        "struct { " ++ fieldDefs2 ++ "; }"

                                                                -- First arg from data.child, second arg from data2
                                                                accessor =
                                                                    if patIdx == 0 then
                                                                        "elm_case_scrutinee.data.child"
                                                                    else
                                                                        "elm_case_scrutinee.data2"
                                                            in
                                                            if isUsedAsUnion then
                                                                "elm_union_t elm_" ++ varName ++ " = *" ++ accessor ++ ";"
                                                            else if isUsedAsString then
                                                                "const char *elm_" ++ varName ++ " = " ++ accessor ++ "->data.str;"
                                                            else if isUsedAsStruct2 then
                                                                -- Cast data.ptr to struct pointer and dereference
                                                                structType2 ++ " elm_" ++ varName ++ " = *(" ++ structType2 ++ "*)" ++ accessor ++ "->data.ptr;"
                                                            else
                                                                "double elm_" ++ varName ++ " = " ++ accessor ++ "->data.num;"

                                                        _ ->
                                                            ""
                                                )
                                            |> List.filter (not << String.isEmpty)
                                            |> String.join " "

                                    -- Apply guard if present
                                    guardedResultStr =
                                        case maybeGuard of
                                            Nothing ->
                                                resultStr

                                            Just guardExpr ->
                                                "(" ++ generateStandaloneExprWithCtx ctx guardExpr ++ " ? " ++ resultStr ++ " : " ++ generateBranches rest ++ ")"

                                    -- Combine outer tag condition with inner conditions
                                    fullCondition2 =
                                        ("elm_case_scrutinee.tag == TAG_" ++ fullCtorName)
                                            :: innerConditions2
                                            |> String.join " && "
                                in
                                "(" ++ fullCondition2
                                    ++ " ? ({ "
                                    ++ bindings
                                    ++ " "
                                    ++ guardedResultStr
                                    ++ "; }) : "
                                    ++ generateBranches rest
                                    ++ ")"

                        -- Empty list pattern
                        Src.PList [] ->
                            let
                                bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                fullBody = wrapWithGuard maybeGuard bodyStr (generateBranches rest)
                            in
                            "(elm_case_scrutinee.length == 0 ? "
                                ++ fullBody
                                ++ " : "
                                ++ generateBranches rest
                                ++ ")"

                        -- Non-empty list pattern [x], [x, y], etc.
                        Src.PList pats ->
                            let
                                patLen =
                                    List.length pats

                                -- Generate bindings for each element
                                bindings =
                                    pats
                                        |> List.indexedMap
                                            (\i (Src.At _ elemPat) ->
                                                let
                                                    elemExpr = "elm_case_scrutinee.data[" ++ String.fromInt i ++ "].d"
                                                in
                                                case elemPat of
                                                    Src.PVar vName ->
                                                        "double elm_" ++ vName ++ " = " ++ elemExpr ++ ";"

                                                    Src.PAnything ->
                                                        ""

                                                    -- Tuple pattern: (a, b)
                                                    Src.PTuple (Src.At _ first) (Src.At _ second) extraPats ->
                                                        let
                                                            elemType = if List.isEmpty extraPats then "elm_tuple2_t" else "elm_tuple3_t"
                                                            tupleDecl = elemType ++ " __elem_" ++ String.fromInt i ++ " = *(" ++ elemType ++ "*)&(" ++ elemExpr ++ ");"
                                                            bindOne idx pat =
                                                                case pat of
                                                                    Src.PVar vn ->
                                                                        "typeof(__elem_" ++ String.fromInt i ++ "._" ++ String.fromInt idx ++ ".d) elm_" ++ vn ++ " = __elem_" ++ String.fromInt i ++ "._" ++ String.fromInt idx ++ ".d;"
                                                                    Src.PAnything -> ""
                                                                    _ -> ""
                                                            allBindings = [ tupleDecl, bindOne 0 first, bindOne 1 second ]
                                                                ++ (extraPats |> List.indexedMap (\idx (Src.At _ p) -> bindOne (idx + 2) p))
                                                                |> List.filter (\s -> s /= "")
                                                                |> String.join " "
                                                        in
                                                        allBindings

                                                    -- Constructor pattern: (Src.At _ x)
                                                    Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ] ->
                                                        "elm_union_t __elem_" ++ String.fromInt i ++ " = (elm_union_t)" ++ elemExpr ++ "; typeof(__elem_" ++ String.fromInt i ++ ".data) elm_" ++ innerName ++ " = __elem_" ++ String.fromInt i ++ ".data;"

                                                    Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ] ->
                                                        "elm_union_t __elem_" ++ String.fromInt i ++ " = (elm_union_t)" ++ elemExpr ++ "; typeof(__elem_" ++ String.fromInt i ++ ".data) elm_" ++ innerName ++ " = __elem_" ++ String.fromInt i ++ ".data;"

                                                    _ ->
                                                        "/* unsupported list element pattern */"
                                            )
                                        |> List.filter (not << String.isEmpty)
                                        |> String.join " "

                                resultStr =
                                    generateStandaloneExprWithCtx ctx resultExpr

                                -- Apply guard if present
                                guardedResultStr =
                                    case maybeGuard of
                                        Nothing ->
                                            resultStr

                                        Just guardExpr ->
                                            "(" ++ generateStandaloneExprWithCtx ctx guardExpr ++ " ? " ++ resultStr ++ " : " ++ generateBranches rest ++ ")"

                                condition =
                                    "elm_case_scrutinee.length == " ++ String.fromInt patLen
                            in
                            "(" ++ condition ++ " ? ({ " ++ bindings ++ " " ++ guardedResultStr ++ "; }) : " ++ generateBranches rest ++ ")"

                        -- Cons pattern (head :: tail)
                        -- NOTE: PCons stores (tail, head) due to how the parser builds cons chains
                        Src.PCons (Src.At _ tailPat) (Src.At _ headPat) ->
                            let
                                -- Helper to infer type and accessor based on variable name
                                inferVarTypeAndAccessor : String -> String -> ( String, String )
                                inferVarTypeAndAccessor vName elemExpr =
                                    if String.contains "Name" vName || String.contains "name" vName
                                        || vName == "s" || vName == "str" || String.contains "Str" vName then
                                        ( "const char *", "((elm_union_t)" ++ elemExpr ++ ").data.str" )
                                    else
                                        ( "double", "((elm_union_t)" ++ elemExpr ++ ").data.num" )

                                -- Extract bindings from a pattern, returning (bindings, condition)
                                extractPatternBindings : String -> Src.Pattern_ -> ( String, String )
                                extractPatternBindings elemExpr pat =
                                    case pat of
                                        Src.PVar hName ->
                                            ( "double elm_" ++ hName ++ " = " ++ elemExpr ++ ";", "" )

                                        Src.PAnything ->
                                            ( "", "" )

                                        -- Nested constructor pattern: Ctor arg1 arg2 ...
                                        Src.PCtor _ ctorName ctorArgs ->
                                            let
                                                tagCheck = "((elm_union_t)" ++ elemExpr ++ ").tag == TAG_" ++ ctorName
                                                innerBindings =
                                                    ctorArgs
                                                        |> List.indexedMap
                                                            (\i (Src.At _ innerPat) ->
                                                                case innerPat of
                                                                    Src.PVar vName ->
                                                                        let
                                                                            ( cType, accessor ) = inferVarTypeAndAccessor vName elemExpr
                                                                        in
                                                                        cType ++ " elm_" ++ vName ++ " = " ++ accessor ++ ";"
                                                                    _ ->
                                                                        ""
                                                            )
                                                        |> List.filter (not << String.isEmpty)
                                                        |> String.join " "
                                            in
                                            ( innerBindings, tagCheck )

                                        -- Qualified constructor pattern: Module.Ctor arg1 arg2 ...
                                        -- Handle Src.At _ (Src.Accessor fieldName) pattern specifically
                                        Src.PCtorQual _ modName ctorName ctorArgs ->
                                            let
                                                fullTag = "TAG_" ++ modName ++ "_" ++ ctorName
                                                tagCheck = "((elm_union_t)" ++ elemExpr ++ ").tag == " ++ fullTag

                                                -- For Src.At pattern, the data is the inner expression
                                                -- For Src.Accessor pattern, the data is the field name
                                                innerBindings =
                                                    case ctorArgs of
                                                        -- Src.At _ innerExpr -> extract from the second argument (wildcard for first)
                                                        [ Src.At _ Src.PAnything, Src.At _ innerPat ] ->
                                                            case innerPat of
                                                                Src.PVar vName ->
                                                                    "double elm_" ++ vName ++ " = ((elm_union_t)" ++ elemExpr ++ ").data.num;"

                                                                -- Nested constructor: Src.At _ (Src.Accessor fieldName)
                                                                -- Note: Nested union extraction is limited - just extract first level data
                                                                Src.PCtor _ innerCtorName innerCtorArgs ->
                                                                    innerCtorArgs
                                                                        |> List.map
                                                                            (\(Src.At _ p) ->
                                                                                case p of
                                                                                    Src.PVar vName ->
                                                                                        "const char *elm_" ++ vName ++ " = ((elm_union_t)" ++ elemExpr ++ ").data.str;"
                                                                                    _ ->
                                                                                        ""
                                                                            )
                                                                        |> List.filter (not << String.isEmpty)
                                                                        |> String.join " "

                                                                Src.PCtorQual _ innerMod innerCtor innerArgs ->
                                                                    -- Note: Nested union extraction is limited - just extract first level data
                                                                    innerArgs
                                                                        |> List.map
                                                                            (\(Src.At _ p) ->
                                                                                case p of
                                                                                    Src.PVar vName ->
                                                                                        "const char *elm_" ++ vName ++ " = ((elm_union_t)" ++ elemExpr ++ ").data.str;"
                                                                                    _ ->
                                                                                        ""
                                                                            )
                                                                        |> List.filter (not << String.isEmpty)
                                                                        |> String.join " "

                                                                _ ->
                                                                    ""

                                                        -- Src.At pos (NestedCtor args) -> bind pos and extract from nested constructor
                                                        [ Src.At _ (Src.PVar posVar), Src.At _ innerPat ] ->
                                                            let
                                                                -- Note: Nested union extraction is limited - all bind to same first-level data
                                                                posBinding = "double elm_" ++ posVar ++ " = ((elm_union_t)" ++ elemExpr ++ ").data.num;"
                                                                nestedBindings =
                                                                    case innerPat of
                                                                        Src.PVar vName ->
                                                                            "double elm_" ++ vName ++ " = ((elm_union_t)" ++ elemExpr ++ ").data.num;"

                                                                        Src.PCtor _ _ nestedArgs ->
                                                                            nestedArgs
                                                                                |> List.map
                                                                                    (\(Src.At _ p) ->
                                                                                        case p of
                                                                                            Src.PVar vName ->
                                                                                                "double elm_" ++ vName ++ " = ((elm_union_t)" ++ elemExpr ++ ").data.num;"
                                                                                            _ ->
                                                                                                ""
                                                                                    )
                                                                                |> List.filter (not << String.isEmpty)
                                                                                |> String.join " "

                                                                        Src.PCtorQual _ _ _ nestedArgs ->
                                                                            nestedArgs
                                                                                |> List.map
                                                                                    (\(Src.At _ p) ->
                                                                                        case p of
                                                                                            Src.PVar vName ->
                                                                                                "double elm_" ++ vName ++ " = ((elm_union_t)" ++ elemExpr ++ ").data.num;"
                                                                                            _ ->
                                                                                                ""
                                                                                    )
                                                                                |> List.filter (not << String.isEmpty)
                                                                                |> String.join " "

                                                                        _ ->
                                                                            ""
                                                            in
                                                            [ posBinding, nestedBindings ]
                                                                |> List.filter (not << String.isEmpty)
                                                                |> String.join " "

                                                        -- Single arg patterns
                                                        [ Src.At _ (Src.PVar vName) ] ->
                                                            "double elm_" ++ vName ++ " = ((elm_union_t)" ++ elemExpr ++ ").data.num;"

                                                        _ ->
                                                            ctorArgs
                                                                |> List.indexedMap
                                                                    (\i (Src.At _ p) ->
                                                                        case p of
                                                                            Src.PVar vName ->
                                                                                "double elm_" ++ vName ++ " = ((elm_union_t)" ++ elemExpr ++ ").data.num;"
                                                                            Src.PAnything ->
                                                                                ""
                                                                            _ ->
                                                                                ""
                                                                    )
                                                                |> List.filter (not << String.isEmpty)
                                                                |> String.join " "
                                            in
                                            ( innerBindings, tagCheck )

                                        _ ->
                                            ( "", "" )

                                -- For list elements, determine accessor based on pattern type AND usage
                                -- If the variable is used with .tag access, it's a union type
                                -- If the variable is used with string functions, it's a string
                                -- Otherwise, default to double
                                resultStr = generateStandaloneExprWithCtx ctx resultExpr

                                -- Check if head variable is used as a union type
                                headVarUsedAsUnion =
                                    case headPat of
                                        Src.PVar hName ->
                                            String.contains ("elm_" ++ hName ++ ".tag") resultStr
                                                || String.contains ("elm_" ++ hName ++ ".data") resultStr
                                        _ -> False

                                -- Check if head variable is used as a string
                                headVarUsedAsString =
                                    case headPat of
                                        Src.PVar hName ->
                                            String.contains ("elm_str_append(elm_" ++ hName) resultStr
                                                || String.contains ("elm_str_append(" ++ "\"") resultStr && String.contains (", elm_" ++ hName ++ ")") resultStr
                                                || String.contains ("strlen(elm_" ++ hName) resultStr
                                        _ -> False

                                -- Check if head variable is used as a list (nested list case)
                                headVarUsedAsList =
                                    case headPat of
                                        Src.PVar hName ->
                                            String.contains ("elm_" ++ hName ++ ".length") resultStr
                                                || String.contains ("elm_" ++ hName ++ ".data") resultStr
                                                || String.contains ("(elm_" ++ hName ++ ")") resultStr
                                                || String.contains ("elm_list_t elm_case_scrutinee = elm_" ++ hName) resultStr
                                        _ -> False

                                headAccessor =
                                    case headPat of
                                        Src.PVar _ ->
                                            if headVarUsedAsUnion then
                                                "(elm_case_scrutinee.data[0].u)"
                                            else if headVarUsedAsString then
                                                "(elm_case_scrutinee.data[0].str)"
                                            else if headVarUsedAsList then
                                                "(*elm_case_scrutinee.data[0].lst)"
                                            else
                                                "(elm_case_scrutinee.data[0].d)"
                                        Src.PAnything -> "(elm_case_scrutinee.data[0].d)"
                                        _ -> "(elm_case_scrutinee.data[0].u)"

                                -- Update extractPatternBindings for different types
                                extractPatternBindingsWithType : String -> Src.Pattern_ -> Bool -> Bool -> Bool -> ( String, String )
                                extractPatternBindingsWithType elemExpr pat isUnion isString isList =
                                    case pat of
                                        Src.PVar hName ->
                                            if isUnion then
                                                ( "elm_union_t elm_" ++ hName ++ " = " ++ elemExpr ++ ";", "" )
                                            else if isString then
                                                ( "const char *elm_" ++ hName ++ " = " ++ elemExpr ++ ";", "" )
                                            else if isList then
                                                ( "elm_list_t elm_" ++ hName ++ " = " ++ elemExpr ++ ";", "" )
                                            else
                                                ( "double elm_" ++ hName ++ " = " ++ elemExpr ++ ";", "" )
                                        _ ->
                                            extractPatternBindings elemExpr pat

                                ( headBinding, headCondition ) =
                                    extractPatternBindingsWithType headAccessor headPat headVarUsedAsUnion headVarUsedAsString headVarUsedAsList

                                tailBinding =
                                    case tailPat of
                                        Src.PVar tName ->
                                            "elm_list_t elm_" ++ tName ++ " = { .length = elm_case_scrutinee.length - 1 }; for (int __i = 1; __i < elm_case_scrutinee.length; __i++) elm_" ++ tName ++ ".data[__i - 1] = elm_case_scrutinee.data[__i];"

                                        Src.PAnything ->
                                            ""

                                        _ ->
                                            ""

                                allBindings =
                                    [ headBinding, tailBinding ]
                                        |> List.filter (not << String.isEmpty)
                                        |> String.join " "

                                condition =
                                    if String.isEmpty headCondition then
                                        "elm_case_scrutinee.length > 0"
                                    else
                                        "elm_case_scrutinee.length > 0 && " ++ headCondition

                                -- Apply guard if present
                                guardedResultStr =
                                    case maybeGuard of
                                        Nothing ->
                                            resultStr

                                        Just guardExpr ->
                                            "(" ++ generateStandaloneExprWithCtx ctx guardExpr ++ " ? " ++ resultStr ++ " : " ++ generateBranches rest ++ ")"
                            in
                            "(" ++ condition ++ " ? ({ "
                                ++ allBindings
                                ++ " "
                                ++ guardedResultStr
                                ++ "; }) : "
                                ++ generateBranches rest
                                ++ ")"

                        -- As pattern: (pattern as name)
                        Src.PAlias (Src.At _ innerPat) (Src.At _ aliasName) ->
                            let
                                -- Determine type based on case context
                                aliasType =
                                    if isListCase then
                                        "elm_list_t"
                                    else if isCustomTypeCase then
                                        "elm_union_t"
                                    else
                                        "double"

                                aliasBinding =
                                    aliasType ++ " elm_" ++ aliasName ++ " = elm_case_scrutinee;"

                                aliasResultStr =
                                    generateStandaloneExprWithCtx ctx resultExpr

                                -- Apply guard if present
                                aliasGuardedResultStr =
                                    case maybeGuard of
                                        Nothing ->
                                            aliasResultStr

                                        Just guardExpr ->
                                            "(" ++ generateStandaloneExprWithCtx ctx guardExpr ++ " ? " ++ aliasResultStr ++ " : " ++ generateBranches rest ++ ")"

                                -- Generate inner pattern handling
                                innerResult =
                                    case innerPat of
                                        Src.PVar vName ->
                                            -- Inner variable binding
                                            aliasType ++ " elm_" ++ vName ++ " = elm_case_scrutinee; " ++ aliasGuardedResultStr

                                        Src.PCons (Src.At _ tailPat) (Src.At _ headPat) ->
                                            -- List cons pattern: (first :: _) as whole
                                            let
                                                headBinding =
                                                    case headPat of
                                                        Src.PVar hName ->
                                                            "double elm_" ++ hName ++ " = elm_case_scrutinee.data[0].d; "
                                                        _ ->
                                                            ""

                                                tailBinding =
                                                    case tailPat of
                                                        Src.PVar tName ->
                                                            "elm_list_t elm_" ++ tName ++ " = { .length = elm_case_scrutinee.length - 1 }; for (int __i = 1; __i < elm_case_scrutinee.length; __i++) elm_" ++ tName ++ ".data[__i - 1] = elm_case_scrutinee.data[__i]; "
                                                        Src.PAnything ->
                                                            ""
                                                        _ ->
                                                            ""
                                            in
                                            headBinding ++ tailBinding ++ aliasGuardedResultStr

                                        Src.PAnything ->
                                            aliasGuardedResultStr

                                        _ ->
                                            "/* unsupported inner pattern in as */ " ++ aliasGuardedResultStr
                            in
                            "(elm_case_scrutinee.length > 0 ? ({ " ++ aliasBinding ++ " " ++ innerResult ++ "; }) : " ++ generateBranches rest ++ ")"

                        _ ->
                            "/* unsupported pattern */ " ++ generateBranches rest

        -- Check for large string-pattern case (optimization to avoid stack overflow)
        isAllStringOrWildcard =
            branches
                |> List.all
                    (\( Src.At _ pat, _, _ ) ->
                        case pat of
                            Src.PStr _ -> True
                            Src.PAnything -> True
                            Src.PVar _ -> True
                            _ -> False
                    )

        isLargeCase =
            List.length branches > 50

        -- Generate iterative if-else chain for large string cases
        generateLargeStringCase : String -> List ( Src.Pattern, Maybe Src.Expr, Src.Expr ) -> String
        generateLargeStringCase scrut bs =
            let
                -- Split into string patterns and the final wildcard/var
                stringBranches =
                    bs |> List.filter (\( Src.At _ p, _, _ ) ->
                        case p of
                            Src.PStr _ -> True
                            _ -> False
                    )

                wildcardBranch =
                    bs |> List.filter (\( Src.At _ p, _, _ ) ->
                        case p of
                            Src.PAnything -> True
                            Src.PVar _ -> True
                            _ -> False
                    ) |> List.head

                -- Generate if-else chain iteratively (no recursion)
                ifElseChain =
                    stringBranches
                        |> List.map
                            (\( Src.At _ pat, maybeGuard, resultExpr ) ->
                                case pat of
                                    Src.PStr s ->
                                        let
                                            cond = "strcmp(__str_scrutinee, \"" ++ escapeC s ++ "\") == 0"
                                            body = generateStandaloneExprWithCtx ctx resultExpr
                                            guardedBody =
                                                case maybeGuard of
                                                    Nothing -> body
                                                    Just g -> "(" ++ generateStandaloneExprWithCtx ctx g ++ " ? " ++ body ++ " : __default_result)"
                                        in
                                        "if (" ++ cond ++ ") __result = " ++ guardedBody ++ ";"

                                    _ ->
                                        ""
                            )
                        |> List.filter (not << String.isEmpty)
                        |> String.join "\n        else "

                defaultResult =
                    case wildcardBranch of
                        Just ( Src.At _ (Src.PVar vn), maybeGuard, resultExpr ) ->
                            let
                                body = generateStandaloneExprWithCtx ctx resultExpr
                            in
                            case maybeGuard of
                                Nothing -> body
                                Just g -> "(" ++ generateStandaloneExprWithCtx ctx g ++ " ? " ++ body ++ " : " ++ fallbackValue ++ ")"

                        Just ( _, maybeGuard, resultExpr ) ->
                            let
                                body = generateStandaloneExprWithCtx ctx resultExpr
                            in
                            case maybeGuard of
                                Nothing -> body
                                Just g -> "(" ++ generateStandaloneExprWithCtx ctx g ++ " ? " ++ body ++ " : " ++ fallbackValue ++ ")"

                        Nothing ->
                            fallbackValue
            in
            "({\n        const char *__str_scrutinee = " ++ scrut ++ ";\n        "
                ++ (if returnsUnion then "elm_union_t" else "typeof(" ++ defaultResult ++ ")")
                ++ " __default_result = " ++ defaultResult ++ ";\n        "
                ++ (if returnsUnion then "elm_union_t" else "typeof(__default_result)")
                ++ " __result = __default_result;\n        "
                ++ ifElseChain
                ++ "\n        __result;\n    })"
    in
    if isAllStringOrWildcard && isLargeCase then
        -- Use iterative if-else chain to avoid stack overflow
        generateLargeStringCase scrutineeStr branches

    else if hasVarBinding || hasCtorWithData || isListCase || isTupleCase then
        -- Use compound statement to bind scrutinee to a variable
        let
            scrutineeType =
                if isListCase then
                    "elm_list_t"

                else if isCustomTypeCase then
                    "elm_union_t"

                else if isTupleCase then
                    if tupleArity == 3 then
                        "elm_tuple3_t"

                    else
                        "elm_tuple2_t"

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

            -- Helper to wrap result with guard condition
            wrapSimpleWithGuard : Maybe Src.Expr -> String -> String -> String
            wrapSimpleWithGuard maybeGuard bodyStr restStr =
                case maybeGuard of
                    Nothing ->
                        bodyStr

                    Just guardExpr ->
                        let
                            guardStr = generateStandaloneExprWithCtx ctx guardExpr
                        in
                        "(" ++ guardStr ++ " ? " ++ bodyStr ++ " : " ++ restStr ++ ")"

            generateSimpleBranches : List ( Src.Pattern, Maybe Src.Expr, Src.Expr ) -> String
            generateSimpleBranches bs =
                case bs of
                    [] ->
                        fallbackValue

                    ( Src.At _ pattern, maybeGuard, resultExpr ) :: rest ->
                        case pattern of
                            Src.PAnything ->
                                wrapSimpleWithGuard maybeGuard (generateStandaloneExprWithCtx ctx resultExpr) (generateSimpleBranches rest)

                            Src.PInt n ->
                                let
                                    bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                    fullBody = wrapSimpleWithGuard maybeGuard bodyStr (generateSimpleBranches rest)
                                in
                                "("
                                    ++ inlineScrutinee
                                    ++ " == "
                                    ++ String.fromInt n
                                    ++ " ? "
                                    ++ fullBody
                                    ++ " : "
                                    ++ generateSimpleBranches rest
                                    ++ ")"

                            Src.PStr s ->
                                let
                                    bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                    fullBody = wrapSimpleWithGuard maybeGuard bodyStr (generateSimpleBranches rest)
                                in
                                "(strcmp("
                                    ++ inlineScrutinee
                                    ++ ", \""
                                    ++ escapeC s
                                    ++ "\") == 0 ? "
                                    ++ fullBody
                                    ++ " : "
                                    ++ generateSimpleBranches rest
                                    ++ ")"

                            Src.PChr c ->
                                let
                                    bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                    fullBody = wrapSimpleWithGuard maybeGuard bodyStr (generateSimpleBranches rest)
                                in
                                "("
                                    ++ inlineScrutinee
                                    ++ " == '"
                                    ++ escapeC c
                                    ++ "' ? "
                                    ++ fullBody
                                    ++ " : "
                                    ++ generateSimpleBranches rest
                                    ++ ")"

                            Src.PCtor _ ctorName ctorPatterns ->
                                case ctorName of
                                    "True" ->
                                        let
                                            bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                            fullBody = wrapSimpleWithGuard maybeGuard bodyStr (generateSimpleBranches rest)
                                        in
                                        "("
                                            ++ inlineScrutinee
                                            ++ " ? "
                                            ++ fullBody
                                            ++ " : "
                                            ++ generateSimpleBranches rest
                                            ++ ")"

                                    "False" ->
                                        let
                                            bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                            fullBody = wrapSimpleWithGuard maybeGuard bodyStr (generateSimpleBranches rest)
                                        in
                                        "(!"
                                            ++ inlineScrutinee
                                            ++ " ? "
                                            ++ fullBody
                                            ++ " : "
                                            ++ generateSimpleBranches rest
                                            ++ ")"

                                    _ ->
                                        if List.isEmpty ctorPatterns then
                                            -- Simple enum constructor - compare tag
                                            let
                                                bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                                fullBody = wrapSimpleWithGuard maybeGuard bodyStr (generateSimpleBranches rest)
                                            in
                                            "("
                                                ++ inlineScrutinee
                                                ++ ".tag == TAG_"
                                                ++ ctorName
                                                ++ " ? "
                                                ++ fullBody
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
                                                                        "double elm_" ++ varName ++ " = " ++ inlineScrutinee ++ ".data.num;"

                                                                    _ ->
                                                                        ""
                                                            )
                                                        |> List.filter (not << String.isEmpty)
                                                        |> String.join " "

                                                resultStr = generateStandaloneExprWithCtx ctx resultExpr

                                                guardedResultStr =
                                                    case maybeGuard of
                                                        Nothing ->
                                                            resultStr

                                                        Just guardExpr ->
                                                            "(" ++ generateStandaloneExprWithCtx ctx guardExpr ++ " ? " ++ resultStr ++ " : " ++ generateSimpleBranches rest ++ ")"
                                            in
                                            "("
                                                ++ inlineScrutinee
                                                ++ ".tag == TAG_"
                                                ++ ctorName
                                                ++ " ? ({ "
                                                ++ bindings
                                                ++ " "
                                                ++ guardedResultStr
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
                                                            "double elm_" ++ varName ++ " = " ++ inlineScrutinee ++ "._" ++ String.fromInt i ++ ";"

                                                        Src.PAnything ->
                                                            ""

                                                        _ ->
                                                            "/* unsupported tuple element pattern */"
                                                )
                                            |> List.filter (not << String.isEmpty)
                                            |> String.join "\n            "

                                    bodyStr =
                                        generateStandaloneExprWithCtx ctx resultExpr

                                    guardedBodyStr =
                                        case maybeGuard of
                                            Nothing ->
                                                bodyStr

                                            Just guardExpr ->
                                                "(" ++ generateStandaloneExprWithCtx ctx guardExpr ++ " ? " ++ bodyStr ++ " : " ++ generateSimpleBranches rest ++ ")"
                                in
                                "({\n            "
                                    ++ bindings
                                    ++ "\n            "
                                    ++ guardedBodyStr
                                    ++ ";\n        })"

                            -- Qualified constructor pattern (e.g., Src.Str, Src.Int)
                            Src.PCtorQual _ moduleName ctorName ctorPatterns ->
                                let
                                    fullCtorName = moduleName ++ "_" ++ ctorName
                                in
                                if List.isEmpty ctorPatterns then
                                    -- Simple enum constructor - compare tag
                                    let
                                        bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                        fullBody = wrapSimpleWithGuard maybeGuard bodyStr (generateSimpleBranches rest)
                                    in
                                    "("
                                        ++ inlineScrutinee
                                        ++ ".tag == TAG_"
                                        ++ fullCtorName
                                        ++ " ? "
                                        ++ fullBody
                                        ++ " : "
                                        ++ generateSimpleBranches rest
                                        ++ ")"

                                else if List.all (\(Src.At _ p) -> p == Src.PAnything) ctorPatterns then
                                    -- Constructor with wildcard args - just compare tag
                                    let
                                        bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                        fullBody = wrapSimpleWithGuard maybeGuard bodyStr (generateSimpleBranches rest)
                                    in
                                    "("
                                        ++ inlineScrutinee
                                        ++ ".tag == TAG_"
                                        ++ fullCtorName
                                        ++ " ? "
                                        ++ fullBody
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
                                                                "double elm_" ++ varName ++ " = " ++ inlineScrutinee ++ ".data.num;"

                                                            _ ->
                                                                ""
                                                    )
                                                |> List.filter (not << String.isEmpty)
                                                |> String.join " "

                                        resultStr = generateStandaloneExprWithCtx ctx resultExpr

                                        guardedResultStr =
                                            case maybeGuard of
                                                Nothing ->
                                                    resultStr

                                                Just guardExpr ->
                                                    "(" ++ generateStandaloneExprWithCtx ctx guardExpr ++ " ? " ++ resultStr ++ " : " ++ generateSimpleBranches rest ++ ")"
                                    in
                                    "("
                                        ++ inlineScrutinee
                                        ++ ".tag == TAG_"
                                        ++ fullCtorName
                                        ++ " ? ({ "
                                        ++ bindings
                                        ++ " "
                                        ++ guardedResultStr
                                        ++ "; }) : "
                                        ++ generateSimpleBranches rest
                                        ++ ")"

                            -- Empty list pattern
                            Src.PList [] ->
                                let
                                    bodyStr = generateStandaloneExprWithCtx ctx resultExpr
                                    fullBody = wrapSimpleWithGuard maybeGuard bodyStr (generateSimpleBranches rest)
                                in
                                "("
                                    ++ inlineScrutinee
                                    ++ ".length == 0 ? "
                                    ++ fullBody
                                    ++ " : "
                                    ++ generateSimpleBranches rest
                                    ++ ")"

                            -- Cons pattern (head :: tail)
                            -- NOTE: PCons stores (tail, head) due to how the parser builds cons chains
                            Src.PCons (Src.At _ tailPat) (Src.At _ headPat) ->
                                let
                                    headBinding =
                                        case headPat of
                                            Src.PVar hName ->
                                                "double elm_" ++ hName ++ " = " ++ inlineScrutinee ++ ".data[0];"

                                            Src.PAnything ->
                                                ""

                                            _ ->
                                                ""

                                    tailBinding =
                                        case tailPat of
                                            Src.PVar tName ->
                                                "elm_list_t elm_" ++ tName ++ " = { .length = " ++ inlineScrutinee ++ ".length - 1 }; for (int __i = 1; __i < " ++ inlineScrutinee ++ ".length; __i++) elm_" ++ tName ++ ".data[__i - 1] = " ++ inlineScrutinee ++ ".data[__i];"

                                            Src.PAnything ->
                                                ""

                                            _ ->
                                                ""

                                    allBindings =
                                        [ headBinding, tailBinding ]
                                            |> List.filter (not << String.isEmpty)
                                            |> String.join " "

                                    resultStr = generateStandaloneExprWithCtx ctx resultExpr

                                    guardedResultStr =
                                        case maybeGuard of
                                            Nothing ->
                                                resultStr

                                            Just guardExpr ->
                                                "(" ++ generateStandaloneExprWithCtx ctx guardExpr ++ " ? " ++ resultStr ++ " : " ++ generateSimpleBranches rest ++ ")"
                                in
                                "("
                                    ++ inlineScrutinee
                                    ++ ".length > 0 ? ({ "
                                    ++ allBindings
                                    ++ " "
                                    ++ guardedResultStr
                                    ++ "; }) : "
                                    ++ generateSimpleBranches rest
                                    ++ ")"

                            _ ->
                                "/* unsupported pattern */ " ++ generateSimpleBranches rest
        in
        generateSimpleBranches branches


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
