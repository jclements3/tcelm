module Target.RTEMS exposing
    ( generateCode
    , CodegenConfig
    , runtimePreamble
    , standalonePreamble
    , -- Modular RTEMS preambles
      coreTypes
    , taskModule
    , clockModule
    , semaphoreModule
    , messageModule
    , timerModule
    , eventModule
    , rmsModule
    , interruptModule
    , -- Elm runtime helpers
      elmStringHelpers
    , elmExtendedStringHelpers
    , elmUnionTypes
    , elmTupleTypes
    , elmListHelpers
    , elmMathHelpers
    , elmMemoryOps
    )

{-| RTEMS target code generation.

This module handles generation of C code for the RTEMS real-time
operating system target.

## RTEMS Modules

RTEMS has a modular design where you include only what you need:

  - **coreTypes** - Basic RTEMS types (always required)
  - **taskModule** - Task management (usually required)
  - **clockModule** - Clock and time services
  - **semaphoreModule** - Semaphores for synchronization
  - **messageModule** - Message queues for IPC
  - **timerModule** - Software timers
  - **eventModule** - Event flags
  - **rmsModule** - Rate Monotonic Scheduler (for periodic tasks)
  - **interruptModule** - Interrupt management

See <https://docs.rtems.org/branches/master/c-user/config/index.html>
for full configuration documentation.

-}

import AST.Source as Src
import Codegen.Lambda exposing (LiftedFunc)
import Codegen.Shared exposing (MainValue(..), escapeC, getModuleName, getModulePrefix, collectUserFunctionNames, patternVars)
import Codegen.Union as Union


{-| Configuration for code generation callbacks.
These callbacks are provided by Cli.elm to inject shared generation functions.
-}
type alias CodegenConfig =
    { extractMain : Src.Module -> MainValue
    , generateImportCode : List Src.Import -> String
    , generateUserFunction : String -> List String -> String -> List Src.Pattern -> Src.Expr -> String
    , collectLocalFunctionsWithScope : String -> List String -> Src.Expr -> List LiftedFunc
    , generateLiftedFunction : String -> String -> List Src.Pattern -> Src.Expr -> List String -> String
    }


{-| Generate RTEMS-compatible C code for a module.
Takes a config with callback functions for code generation.
-}
generateCode : CodegenConfig -> Src.Module -> String
generateCode config ast =
    let
        moduleName =
            getModuleName ast

        modulePrefix =
            getModulePrefix ast

        -- Process imports to generate includes
        importCode =
            config.generateImportCode ast.imports

        mainValue =
            config.extractMain ast

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
                            Just (config.generateUserFunction modulePrefix userFunctionNames name value.args value.body)

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
                        config.collectLocalFunctionsWithScope name funcParamNames value.body
                    )
                |> List.map
                    (\lf ->
                        config.generateLiftedFunction lf.prefix lf.name lf.args lf.body lf.capturedVars
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
                , standalonePreamble
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
                , fontData
                , ""
                , framebufferCode
                , ""
                , rmsCode
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


{-| Font 8x16 bitmap data for framebuffer text output.
-}
fontData : String
fontData =
    String.join "\n"
        [ "/* Font 8x16 bitmap data */"
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
        ]


{-| Framebuffer graphics code for RTEMS.
-}
framebufferCode : String
framebufferCode =
    String.join "\n"
        [ "/* Framebuffer graphics */"
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
        ]


{-| Rate Monotonic Scheduler support code for RTEMS.
-}
rmsCode : String
rmsCode =
    String.join "\n"
        [ "/* Rate Monotonic Scheduler (RMS) support */"
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
        ]


{-| Complete runtime preamble for typical RTEMS applications.
Includes core types, task module, clock module, and Elm helpers.
-}
runtimePreamble : String
runtimePreamble =
    String.join "\n"
        [ coreTypes
        , taskModule
        , clockModule
        , elmStringHelpers
        , elmUnionTypes
        , elmListHelpers
        ]



-- RTEMS MODULES


{-| Core RTEMS types - always required.
Includes rtems\_id, rtems\_status\_code, rtems\_name, etc.
-}
coreTypes : String
coreTypes =
    String.join "\n"
        [ "/* RTEMS Core Types */"
        , "#include <stdint.h>"
        , "#include <stdbool.h>"
        , "#include <stddef.h>"
        , ""
        , "typedef uint32_t rtems_id;"
        , "typedef uint32_t rtems_status_code;"
        , "typedef uint32_t rtems_name;"
        , "typedef uint32_t rtems_interval;"
        , "typedef uint32_t rtems_task_priority;"
        , "typedef uint32_t rtems_mode;"
        , "typedef uint32_t rtems_attribute;"
        , "typedef uint32_t rtems_option;"
        , ""
        , "#define RTEMS_SUCCESSFUL 0"
        , "#define RTEMS_TIMEOUT 6"
        , "#define RTEMS_SELF 0"
        , "#define RTEMS_NO_TIMEOUT 0"
        , "#define RTEMS_WAIT 0"
        , "#define RTEMS_NO_WAIT 1"
        , ""
        , "#define rtems_build_name(c1,c2,c3,c4) \\"
        , "    ((uint32_t)(c1)<<24|(uint32_t)(c2)<<16|(uint32_t)(c3)<<8|(uint32_t)(c4))"
        ]


{-| Task management module.
Provides rtems\_task\_create, rtems\_task\_start, rtems\_task\_delete, etc.
-}
taskModule : String
taskModule =
    String.join "\n"
        [ ""
        , "/* RTEMS Task Management */"
        , "typedef uintptr_t rtems_task_argument;"
        , "typedef void (*rtems_task_entry)(rtems_task_argument);"
        , ""
        , "#define RTEMS_MINIMUM_STACK_SIZE 4096"
        , "#define RTEMS_DEFAULT_MODES 0"
        , "#define RTEMS_DEFAULT_ATTRIBUTES 0"
        , ""
        , "extern rtems_status_code rtems_task_create("
        , "    rtems_name name, rtems_task_priority priority,"
        , "    size_t stack_size, rtems_mode modes,"
        , "    rtems_attribute attributes, rtems_id *id);"
        , "extern rtems_status_code rtems_task_start("
        , "    rtems_id id, rtems_task_entry entry, rtems_task_argument arg);"
        , "extern rtems_status_code rtems_task_delete(rtems_id id);"
        , "extern rtems_status_code rtems_task_suspend(rtems_id id);"
        , "extern rtems_status_code rtems_task_resume(rtems_id id);"
        , "extern rtems_status_code rtems_task_set_priority("
        , "    rtems_id id, rtems_task_priority new_priority,"
        , "    rtems_task_priority *old_priority);"
        , "extern rtems_status_code rtems_task_wake_after(rtems_interval ticks);"
        , "extern rtems_id rtems_task_self(void);"
        ]


{-| Clock and time module.
Provides rtems\_clock\_get\_ticks\_per\_second, rtems\_clock\_get\_uptime, etc.
-}
clockModule : String
clockModule =
    String.join "\n"
        [ ""
        , "/* RTEMS Clock Services */"
        , "typedef long time_t;"
        , "struct timespec { time_t tv_sec; long tv_nsec; };"
        , ""
        , "extern rtems_interval rtems_clock_get_ticks_since_boot(void);"
        , "extern rtems_interval rtems_clock_get_ticks_per_second(void);"
        , "extern rtems_status_code rtems_clock_get_uptime(struct timespec *uptime);"
        , ""
        , "#define RTEMS_MILLISECONDS_TO_TICKS(ms) \\"
        , "    ((ms) * rtems_clock_get_ticks_per_second() / 1000)"
        ]


{-| Semaphore module.
Provides rtems\_semaphore\_create, rtems\_semaphore\_obtain, etc.
-}
semaphoreModule : String
semaphoreModule =
    String.join "\n"
        [ ""
        , "/* RTEMS Semaphores */"
        , "#define RTEMS_BINARY_SEMAPHORE 0x10"
        , "#define RTEMS_COUNTING_SEMAPHORE 0"
        , "#define RTEMS_PRIORITY 0x04"
        , "#define RTEMS_FIFO 0"
        , "#define RTEMS_INHERIT_PRIORITY 0x40"
        , ""
        , "extern rtems_status_code rtems_semaphore_create("
        , "    rtems_name name, uint32_t count, rtems_attribute attrs,"
        , "    rtems_task_priority ceiling, rtems_id *id);"
        , "extern rtems_status_code rtems_semaphore_delete(rtems_id id);"
        , "extern rtems_status_code rtems_semaphore_obtain("
        , "    rtems_id id, rtems_option options, rtems_interval timeout);"
        , "extern rtems_status_code rtems_semaphore_release(rtems_id id);"
        ]


{-| Message queue module.
Provides rtems\_message\_queue\_create, rtems\_message\_queue\_send, etc.
-}
messageModule : String
messageModule =
    String.join "\n"
        [ ""
        , "/* RTEMS Message Queues */"
        , "extern rtems_status_code rtems_message_queue_create("
        , "    rtems_name name, uint32_t count, size_t max_size,"
        , "    rtems_attribute attrs, rtems_id *id);"
        , "extern rtems_status_code rtems_message_queue_delete(rtems_id id);"
        , "extern rtems_status_code rtems_message_queue_send("
        , "    rtems_id id, const void *buffer, size_t size);"
        , "extern rtems_status_code rtems_message_queue_receive("
        , "    rtems_id id, void *buffer, size_t *size,"
        , "    rtems_option options, rtems_interval timeout);"
        , "extern rtems_status_code rtems_message_queue_broadcast("
        , "    rtems_id id, const void *buffer, size_t size, uint32_t *count);"
        ]


{-| Timer module.
Provides rtems\_timer\_create, rtems\_timer\_fire\_after, etc.
-}
timerModule : String
timerModule =
    String.join "\n"
        [ ""
        , "/* RTEMS Timers */"
        , "typedef void (*rtems_timer_service_routine)(rtems_id, void *);"
        , ""
        , "extern rtems_status_code rtems_timer_create("
        , "    rtems_name name, rtems_id *id);"
        , "extern rtems_status_code rtems_timer_delete(rtems_id id);"
        , "extern rtems_status_code rtems_timer_fire_after("
        , "    rtems_id id, rtems_interval ticks,"
        , "    rtems_timer_service_routine routine, void *user_data);"
        , "extern rtems_status_code rtems_timer_fire_when("
        , "    rtems_id id, rtems_time_of_day *wall_time,"
        , "    rtems_timer_service_routine routine, void *user_data);"
        , "extern rtems_status_code rtems_timer_cancel(rtems_id id);"
        , "extern rtems_status_code rtems_timer_reset(rtems_id id);"
        ]


{-| Event flags module.
Provides rtems\_event\_send, rtems\_event\_receive, etc.
-}
eventModule : String
eventModule =
    String.join "\n"
        [ ""
        , "/* RTEMS Events */"
        , "typedef uint32_t rtems_event_set;"
        , ""
        , "#define RTEMS_EVENT_0  0x00000001"
        , "#define RTEMS_EVENT_1  0x00000002"
        , "#define RTEMS_EVENT_2  0x00000004"
        , "#define RTEMS_EVENT_3  0x00000008"
        , "#define RTEMS_EVENT_ALL 0x00000000"
        , "#define RTEMS_EVENT_ANY 0x00000002"
        , ""
        , "extern rtems_status_code rtems_event_send("
        , "    rtems_id id, rtems_event_set event_in);"
        , "extern rtems_status_code rtems_event_receive("
        , "    rtems_event_set event_in, rtems_option options,"
        , "    rtems_interval timeout, rtems_event_set *event_out);"
        ]


{-| Rate Monotonic Scheduler module.
For periodic real-time tasks with deadline monitoring.
-}
rmsModule : String
rmsModule =
    String.join "\n"
        [ ""
        , "/* RTEMS Rate Monotonic Scheduler */"
        , "typedef struct {"
        , "    uint32_t count;"
        , "    uint32_t missed_count;"
        , "    uint32_t min_cpu_time;"
        , "    uint32_t max_cpu_time;"
        , "    uint32_t total_cpu_time;"
        , "} rtems_rate_monotonic_period_statistics;"
        , ""
        , "extern rtems_status_code rtems_rate_monotonic_create("
        , "    rtems_name name, rtems_id *id);"
        , "extern rtems_status_code rtems_rate_monotonic_delete(rtems_id id);"
        , "extern rtems_status_code rtems_rate_monotonic_period("
        , "    rtems_id id, rtems_interval length);"
        , "extern rtems_status_code rtems_rate_monotonic_cancel(rtems_id id);"
        , "extern rtems_status_code rtems_rate_monotonic_get_status("
        , "    rtems_id id, rtems_rate_monotonic_period_status *status);"
        , "extern rtems_status_code rtems_rate_monotonic_get_statistics("
        , "    rtems_id id, rtems_rate_monotonic_period_statistics *stats);"
        ]


{-| Interrupt management module.
-}
interruptModule : String
interruptModule =
    String.join "\n"
        [ ""
        , "/* RTEMS Interrupt Management */"
        , "typedef uint32_t rtems_interrupt_level;"
        , ""
        , "extern rtems_interrupt_level _rtems_interrupt_disable(void);"
        , "extern void _rtems_interrupt_enable(rtems_interrupt_level level);"
        , "extern bool rtems_interrupt_is_in_progress(void);"
        , ""
        , "#define rtems_interrupt_disable(level) \\"
        , "    do { (level) = _rtems_interrupt_disable(); } while(0)"
        , "#define rtems_interrupt_enable(level) \\"
        , "    _rtems_interrupt_enable(level)"
        ]



-- ELM RUNTIME HELPERS


{-| String helper functions for Elm.
Single-buffer versions (suitable for embedded with limited memory).
-}
elmStringHelpers : String
elmStringHelpers =
    String.join "\n"
        [ ""
        , "/* Elm String Helpers */"
        , "static int strcmp(const char *a, const char *b) {"
        , "    while (*a && *a == *b) { a++; b++; }"
        , "    return (unsigned char)*a - (unsigned char)*b;"
        , "}"
        , ""
        , "static double elm_strlen(const char *s) {"
        , "    int len = 0; while (*s++) len++;"
        , "    return len;"
        , "}"
        , ""
        , "static char __elm_fromint_buf[32];"
        , "static const char *elm_from_int(int n) {"
        , "    char tmp[32]; int i = 0, j = 0, neg = 0;"
        , "    if (n < 0) { neg = 1; n = -n; }"
        , "    if (n == 0) { __elm_fromint_buf[0] = '0'; __elm_fromint_buf[1] = 0; return __elm_fromint_buf; }"
        , "    while (n > 0) { tmp[i++] = '0' + (n % 10); n /= 10; }"
        , "    if (neg) __elm_fromint_buf[j++] = '-';"
        , "    while (i > 0) __elm_fromint_buf[j++] = tmp[--i];"
        , "    __elm_fromint_buf[j] = 0;"
        , "    return __elm_fromint_buf;"
        , "}"
        , ""
        , "static char __elm_append_buf[512];"
        , "static const char *elm_str_append(const char *a, const char *b) {"
        , "    int i = 0, j = 0;"
        , "    while (a[i] && i < 255) { __elm_append_buf[i] = a[i]; i++; }"
        , "    while (b[j] && i + j < 511) { __elm_append_buf[i + j] = b[j]; j++; }"
        , "    __elm_append_buf[i + j] = 0;"
        , "    return __elm_append_buf;"
        , "}"
        ]


{-| Union type support for Elm custom types (Maybe, Result, etc).
-}
elmUnionTypes : String
elmUnionTypes =
    String.join "\n"
        [ ""
        , "/* Elm Union Types */"
        , "struct elm_union_s;"
        , "typedef struct elm_union_s {"
        , "    int tag;"
        , "    union { double num; struct elm_union_s *child; const char *str; void *ptr; } data;"
        , "    struct elm_union_s *data2;"
        , "} elm_union_t;"
        , ""
        , "static elm_union_t *elm_alloc_union(elm_union_t val) {"
        , "    elm_union_t *p = (elm_union_t*)malloc(sizeof(elm_union_t));"
        , "    *p = val;"
        , "    return p;"
        , "}"
        , ""
        , "/* Built-in type tags */"
        , "#define TAG_Nothing 0"
        , "#define TAG_Just 1"
        , "#define TAG_Err 0"
        , "#define TAG_Ok 1"
        , "#define TAG_LT 0"
        , "#define TAG_EQ 1"
        , "#define TAG_GT 2"
        , ""
        , "static elm_union_t elm_Nothing(void) {"
        , "    elm_union_t r = { .tag = TAG_Nothing, .data = {.num = 0}, .data2 = 0 };"
        , "    return r;"
        , "}"
        , "static elm_union_t elm_Just(elm_union_t v) {"
        , "    elm_union_t r = { .tag = TAG_Just, .data = {.child = elm_alloc_union(v)}, .data2 = 0 };"
        , "    return r;"
        , "}"
        , "static elm_union_t elm_Ok(elm_union_t v) {"
        , "    elm_union_t r = { .tag = TAG_Ok, .data = {.child = elm_alloc_union(v)}, .data2 = 0 };"
        , "    return r;"
        , "}"
        , "static elm_union_t elm_Err(elm_union_t v) {"
        , "    elm_union_t r = { .tag = TAG_Err, .data = {.child = elm_alloc_union(v)}, .data2 = 0 };"
        , "    return r;"
        , "}"
        ]


{-| List type support for Elm.
Fixed-size array implementation suitable for embedded.
-}
elmListHelpers : String
elmListHelpers =
    String.join "\n"
        [ ""
        , "/* Elm List Type */"
        , "#define ELM_LIST_MAX 16"
        , "typedef struct { int length; int data[ELM_LIST_MAX]; } elm_list_t;"
        , ""
        , "static elm_union_t elm_List_head(elm_list_t lst) {"
        , "    if (lst.length > 0) {"
        , "        elm_union_t inner = { .tag = 0, .data = {.num = lst.data[0]}, .data2 = 0 };"
        , "        return elm_Just(inner);"
        , "    }"
        , "    return elm_Nothing();"
        , "}"
        ]


{-| Math helper functions for embedded use.
-}
elmMathHelpers : String
elmMathHelpers =
    String.join "\n"
        [ ""
        , "/* Math Helpers */"
        , "static double elm_pow(int base, int exp) {"
        , "    int result = 1;"
        , "    while (exp > 0) {"
        , "        if (exp & 1) result *= base;"
        , "        exp >>= 1; base *= base;"
        , "    }"
        , "    return result;"
        , "}"
        , ""
        , "static double elm_isqrt(int x) {"
        , "    if (x <= 0) return 0;"
        , "    int guess = x;"
        , "    while (1) {"
        , "        int next = (guess + x / guess) / 2;"
        , "        if (next >= guess) return guess;"
        , "        guess = next;"
        , "    }"
        , "}"
        , ""
        , "static double elm_ilog2(int x) {"
        , "    if (x <= 0) return 0;"
        , "    int r = 0;"
        , "    while (x > 1) { x >>= 1; r++; }"
        , "    return r;"
        , "}"
        ]


{-| Memory operations for embedded (no libc).
-}
elmMemoryOps : String
elmMemoryOps =
    String.join "\n"
        [ ""
        , "/* Memory Operations */"
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
        ]


{-| Extended string functions for RTEMS standalone mode.
-}
elmExtendedStringHelpers : String
elmExtendedStringHelpers =
    String.join "\n"
        [ ""
        , "/* Extended String Functions */"
        , "static char __elm_reverse_buf[256];"
        , "static const char *elm_str_reverse(const char *s) {"
        , "    int len = 0; while (s[len]) len++;"
        , "    for (int i = 0; i < len; i++) __elm_reverse_buf[i] = s[len - 1 - i];"
        , "    __elm_reverse_buf[len] = 0;"
        , "    return __elm_reverse_buf;"
        , "}"
        , ""
        , "static char __elm_fromchar_buf[2];"
        , "static const char *elm_str_from_char(char c) {"
        , "    __elm_fromchar_buf[0] = c; __elm_fromchar_buf[1] = 0;"
        , "    return __elm_fromchar_buf;"
        , "}"
        , ""
        , "static char __elm_cons_buf[256];"
        , "static const char *elm_str_cons(char c, const char *s) {"
        , "    __elm_cons_buf[0] = c;"
        , "    int i = 0; while (s[i] && i < 254) { __elm_cons_buf[i+1] = s[i]; i++; }"
        , "    __elm_cons_buf[i+1] = 0;"
        , "    return __elm_cons_buf;"
        , "}"
        , ""
        , "static char __elm_left_buf[256];"
        , "static const char *elm_str_left(int n, const char *s) {"
        , "    int len = 0; while (s[len]) len++;"
        , "    int take = n < len ? n : len;"
        , "    for (int i = 0; i < take; i++) __elm_left_buf[i] = s[i];"
        , "    __elm_left_buf[take] = 0;"
        , "    return __elm_left_buf;"
        , "}"
        , ""
        , "static char __elm_right_buf[256];"
        , "static const char *elm_str_right(int n, const char *s) {"
        , "    int len = 0; while (s[len]) len++;"
        , "    int start = n < len ? len - n : 0;"
        , "    int j = 0;"
        , "    for (int i = start; i < len; i++) __elm_right_buf[j++] = s[i];"
        , "    __elm_right_buf[j] = 0;"
        , "    return __elm_right_buf;"
        , "}"
        , ""
        , "static char __elm_repeat_buf[256];"
        , "static const char *elm_str_repeat(int n, const char *s) {"
        , "    int slen = 0; while (s[slen]) slen++;"
        , "    int pos = 0;"
        , "    for (int i = 0; i < n && pos + slen < 255; i++)"
        , "        for (int j = 0; j < slen; j++) __elm_repeat_buf[pos++] = s[j];"
        , "    __elm_repeat_buf[pos] = 0;"
        , "    return __elm_repeat_buf;"
        , "}"
        , ""
        , "static char __elm_slice_buf[256];"
        , "static const char *elm_str_slice(int start, int end, const char *s) {"
        , "    int len = 0; while (s[len]) len++;"
        , "    if (start < 0) start = len + start; if (start < 0) start = 0;"
        , "    if (end < 0) end = len + end; if (end < 0) end = 0;"
        , "    if (start > len) start = len; if (end > len) end = len;"
        , "    if (start >= end) { __elm_slice_buf[0] = 0; return __elm_slice_buf; }"
        , "    int j = 0;"
        , "    for (int i = start; i < end; i++) __elm_slice_buf[j++] = s[i];"
        , "    __elm_slice_buf[j] = 0;"
        , "    return __elm_slice_buf;"
        , "}"
        , ""
        , "static char __elm_trim_buf[256];"
        , "static const char *elm_str_trim(const char *s) {"
        , "    while (*s == ' ' || *s == '\\t' || *s == '\\n' || *s == '\\r') s++;"
        , "    int len = 0; while (s[len]) len++;"
        , "    while (len > 0 && (s[len-1] == ' ' || s[len-1] == '\\t' || s[len-1] == '\\n' || s[len-1] == '\\r')) len--;"
        , "    for (int i = 0; i < len; i++) __elm_trim_buf[i] = s[i];"
        , "    __elm_trim_buf[len] = 0;"
        , "    return __elm_trim_buf;"
        , "}"
        ]


{-| Tuple types for RTEMS.
-}
elmTupleTypes : String
elmTupleTypes =
    String.join "\n"
        [ ""
        , "/* Tuple Types */"
        , "typedef struct { int _0; int _1; } elm_tuple2_t;"
        , "typedef struct { int _0; int _1; int _2; } elm_tuple3_t;"
        ]


{-| Complete standalone preamble for RTEMS applications.
Includes all runtime helpers needed for standalone execution.
-}
standalonePreamble : String
standalonePreamble =
    String.join "\n"
        [ "#include <stdlib.h>"
        , ""
        , "/* RTEMS entry point */"
        , "typedef unsigned int rtems_task_argument;"
        , "typedef unsigned int rtems_id;"
        , "#define RTEMS_SELF 0"
        , "extern void rtems_task_delete(rtems_id id);"
        , elmStringHelpers
        , elmMathHelpers
        , elmMemoryOps
        , elmExtendedStringHelpers
        , elmUnionTypes
        , elmTupleTypes
        , elmListHelpers
        ]
