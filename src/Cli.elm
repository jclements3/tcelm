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
                , "/* String length */"
                , "static int elm_strlen(const char *s) {"
                , "    int len = 0;"
                , "    while (*s++) len++;"
                , "    return len;"
                , "}"
                , ""
                , "/* Integer power function */"
                , "static int elm_pow(int base, int exp) {"
                , "    int result = 1;"
                , "    while (exp > 0) {"
                , "        if (exp & 1) result *= base;"
                , "        exp >>= 1;"
                , "        base *= base;"
                , "    }"
                , "    return result;"
                , "}"
                , ""
                , "/* String.fromInt - convert int to string */"
                , "static char __elm_fromint_buf[32];"
                , "static const char *elm_from_int(int n) {"
                , "    char tmp[32];"
                , "    int i = 0, j = 0;"
                , "    int neg = 0;"
                , "    if (n < 0) { neg = 1; n = -n; }"
                , "    if (n == 0) { __elm_fromint_buf[0] = '0'; __elm_fromint_buf[1] = 0; return __elm_fromint_buf; }"
                , "    while (n > 0) { tmp[i++] = '0' + (n % 10); n /= 10; }"
                , "    if (neg) __elm_fromint_buf[j++] = '-';"
                , "    while (i > 0) __elm_fromint_buf[j++] = tmp[--i];"
                , "    __elm_fromint_buf[j] = 0;"
                , "    return __elm_fromint_buf;"
                , "}"
                , ""
                , "/* String.reverse - reverse a string */"
                , "static char __elm_reverse_buf[256];"
                , "static const char *elm_str_reverse(const char *s) {"
                , "    int len = 0;"
                , "    while (s[len]) len++;"
                , "    for (int i = 0; i < len; i++) __elm_reverse_buf[i] = s[len - 1 - i];"
                , "    __elm_reverse_buf[len] = 0;"
                , "    return __elm_reverse_buf;"
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
                , "/* Integer square root using Newton-Raphson */"
                , "static int elm_isqrt(int x) {"
                , "    if (x <= 0) return 0;"
                , "    int guess = x;"
                , "    while (1) {"
                , "        int next = (guess + x / guess) / 2;"
                , "        if (next >= guess) return guess;"
                , "        guess = next;"
                , "    }"
                , "}"
                , ""
                , "/* Generic tagged union type for custom types */"
                , "typedef struct { int tag; int data; } elm_union_t;"
                , ""
                , "/* Built-in tuple type */"
                , "typedef struct { int _0; int _1; } elm_tuple2_t;"
                , "typedef struct { int _0; int _1; int _2; } elm_tuple3_t;"
                , ""
                , "/* Built-in Order type tags (for compare function) */"
                , "#define TAG_LT 0"
                , "#define TAG_EQ 1"
                , "#define TAG_GT 2"
                , ""
                , "/* Built-in Maybe type tags */"
                , "#define TAG_Nothing 0"
                , "#define TAG_Just 1"
                , ""
                , "/* String.fromChar - convert char to single-char string */"
                , "static char __elm_fromchar_buf[2];"
                , "static const char *elm_str_from_char(char c) {"
                , "    __elm_fromchar_buf[0] = c;"
                , "    __elm_fromchar_buf[1] = 0;"
                , "    return __elm_fromchar_buf;"
                , "}"
                , ""
                , "/* String.cons - prepend char to string */"
                , "static char __elm_cons_buf[256];"
                , "static const char *elm_str_cons(char c, const char *s) {"
                , "    __elm_cons_buf[0] = c;"
                , "    int i = 0;"
                , "    while (s[i] && i < 254) { __elm_cons_buf[i+1] = s[i]; i++; }"
                , "    __elm_cons_buf[i+1] = 0;"
                , "    return __elm_cons_buf;"
                , "}"
                , ""
                , "/* String.left - take first n characters */"
                , "static char __elm_left_buf[256];"
                , "static const char *elm_str_left(int n, const char *s) {"
                , "    int len = 0; while (s[len]) len++;"
                , "    int take = n < len ? n : len;"
                , "    for (int i = 0; i < take; i++) __elm_left_buf[i] = s[i];"
                , "    __elm_left_buf[take] = 0;"
                , "    return __elm_left_buf;"
                , "}"
                , ""
                , "/* String.right - take last n characters */"
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
                , "/* String.append - concatenate two strings */"
                , "static char __elm_append_buf[512];"
                , "static const char *elm_str_append(const char *a, const char *b) {"
                , "    int i = 0, j = 0;"
                , "    while (a[i] && i < 255) { __elm_append_buf[i] = a[i]; i++; }"
                , "    while (b[j] && i + j < 511) { __elm_append_buf[i + j] = b[j]; j++; }"
                , "    __elm_append_buf[i + j] = 0;"
                , "    return __elm_append_buf;"
                , "}"
                , ""
                , "/* String.repeat - repeat string n times */"
                , "static char __elm_repeat_buf[256];"
                , "static const char *elm_str_repeat(int n, const char *s) {"
                , "    int slen = 0; while (s[slen]) slen++;"
                , "    int pos = 0;"
                , "    for (int i = 0; i < n && pos + slen < 255; i++) {"
                , "        for (int j = 0; j < slen; j++) __elm_repeat_buf[pos++] = s[j];"
                , "    }"
                , "    __elm_repeat_buf[pos] = 0;"
                , "    return __elm_repeat_buf;"
                , "}"
                , ""
                , "/* String.slice - extract substring from start to end index */"
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
                , "/* String.trim - remove leading and trailing whitespace */"
                , "static char __elm_trim_buf[256];"
                , "static const char *elm_str_trim(const char *s) {"
                , "    while (*s == ' ' || *s == '\\t' || *s == '\\n' || *s == '\\r') s++;"
                , "    int len = 0; while (s[len]) len++;"
                , "    while (len > 0 && (s[len-1] == ' ' || s[len-1] == '\\t' || s[len-1] == '\\n' || s[len-1] == '\\r')) len--;"
                , "    for (int i = 0; i < len; i++) __elm_trim_buf[i] = s[i];"
                , "    __elm_trim_buf[len] = 0;"
                , "    return __elm_trim_buf;"
                , "}"
                , ""
                , "/* String.toUpper - convert to uppercase */"
                , "static char __elm_toupper_buf[256];"
                , "static const char *elm_str_to_upper(const char *s) {"
                , "    int i = 0;"
                , "    for (; s[i]; i++) {"
                , "        __elm_toupper_buf[i] = (s[i] >= 'a' && s[i] <= 'z') ? s[i] - 32 : s[i];"
                , "    }"
                , "    __elm_toupper_buf[i] = 0;"
                , "    return __elm_toupper_buf;"
                , "}"
                , ""
                , "/* String.toLower - convert to lowercase */"
                , "static char __elm_tolower_buf[256];"
                , "static const char *elm_str_to_lower(const char *s) {"
                , "    int i = 0;"
                , "    for (; s[i]; i++) {"
                , "        __elm_tolower_buf[i] = (s[i] >= 'A' && s[i] <= 'Z') ? s[i] + 32 : s[i];"
                , "    }"
                , "    __elm_tolower_buf[i] = 0;"
                , "    return __elm_tolower_buf;"
                , "}"
                , ""
                , "/* String.padLeft - pad string on the left with char */"
                , "static char __elm_padleft_buf[256];"
                , "static const char *elm_str_pad_left(int n, char c, const char *s) {"
                , "    int len = 0; while (s[len]) len++;"
                , "    int pad = n - len; if (pad < 0) pad = 0;"
                , "    if (pad + len > 255) pad = 255 - len;"
                , "    for (int i = 0; i < pad; i++) __elm_padleft_buf[i] = c;"
                , "    for (int i = 0; i < len; i++) __elm_padleft_buf[pad + i] = s[i];"
                , "    __elm_padleft_buf[pad + len] = 0;"
                , "    return __elm_padleft_buf;"
                , "}"
                , ""
                , "/* String.padRight - pad string on the right with char */"
                , "static char __elm_padright_buf[256];"
                , "static const char *elm_str_pad_right(int n, char c, const char *s) {"
                , "    int len = 0; while (s[len]) len++;"
                , "    int pad = n - len; if (pad < 0) pad = 0;"
                , "    if (len + pad > 255) pad = 255 - len;"
                , "    for (int i = 0; i < len; i++) __elm_padright_buf[i] = s[i];"
                , "    for (int i = 0; i < pad; i++) __elm_padright_buf[len + i] = c;"
                , "    __elm_padright_buf[len + pad] = 0;"
                , "    return __elm_padright_buf;"
                , "}"
                , ""
                , "/* String.startsWith - check if string starts with prefix */"
                , "static int elm_str_starts_with(const char *prefix, const char *s) {"
                , "    while (*prefix && *s && *prefix == *s) { prefix++; s++; }"
                , "    return !*prefix;"
                , "}"
                , ""
                , "/* String.endsWith - check if string ends with suffix */"
                , "static int elm_str_ends_with(const char *suffix, const char *s) {"
                , "    int slen = 0, sufflen = 0;"
                , "    while (s[slen]) slen++;"
                , "    while (suffix[sufflen]) sufflen++;"
                , "    if (sufflen > slen) return 0;"
                , "    return !strcmp(s + slen - sufflen, suffix);"
                , "}"
                , ""
                , "/* String.contains - check if substring exists */"
                , "static int elm_str_contains(const char *needle, const char *haystack) {"
                , "    if (!*needle) return 1;"
                , "    for (; *haystack; haystack++) {"
                , "        const char *h = haystack, *n = needle;"
                , "        while (*h && *n && *h == *n) { h++; n++; }"
                , "        if (!*n) return 1;"
                , "    }"
                , "    return 0;"
                , "}"
                , ""
                , "/* String.toInt - parse string to Maybe Int */"
                , "static elm_union_t elm_str_to_int(const char *s) {"
                , "    int result = 0, neg = 0, i = 0;"
                , "    if (!s || !*s) return (elm_union_t){TAG_Nothing, 0};"
                , "    if (s[0] == '-') { neg = 1; i = 1; }"
                , "    else if (s[0] == '+') { i = 1; }"
                , "    if (!s[i]) return (elm_union_t){TAG_Nothing, 0};"
                , "    for (; s[i]; i++) {"
                , "        if (s[i] < '0' || s[i] > '9') return (elm_union_t){TAG_Nothing, 0};"
                , "        result = result * 10 + (s[i] - '0');"
                , "    }"
                , "    return (elm_union_t){TAG_Just, neg ? -result : result};"
                , "}"
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
            -- Check for power operator (needs special handling)
            isPowerOp =
                List.all (\( _, Src.At _ op ) -> op == "^") pairs

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

        else if isStringEquality then
            -- String comparison using strcmp
            case pairs of
                [ ( leftExpr, Src.At _ "==" ) ] ->
                    "(strcmp(" ++ generateStandaloneExpr leftExpr ++ ", " ++ finalTerm ++ ") == 0)"

                [ ( leftExpr, Src.At _ "/=" ) ] ->
                    "(strcmp(" ++ generateStandaloneExpr leftExpr ++ ", " ++ finalTerm ++ ") != 0)"

                _ ->
                    "(" ++ buildExpr terms ++ ")"

        else
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

                ( Src.CapVar, "Nothing" ) ->
                    -- Built-in Maybe Nothing constructor
                    "((elm_union_t){TAG_Nothing, 0})"

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

        Src.At _ (Src.Var _ "identity") ->
            -- identity x = x
            case args of
                [ value ] ->
                    generateStandaloneExpr value

                _ ->
                    "/* identity wrong arity */ 0"

        Src.At _ (Src.Var _ "always") ->
            -- always x y = x (returns first arg, ignores second)
            case args of
                [ value, _ ] ->
                    generateStandaloneExpr value

                _ ->
                    "/* always wrong arity */ 0"

        Src.At _ (Src.Var _ "not") ->
            -- not x = boolean negation
            case args of
                [ value ] ->
                    "(!" ++ generateStandaloneExpr value ++ ")"

                _ ->
                    "/* not wrong arity */ 0"

        Src.At _ (Src.Var _ "clamp") ->
            -- clamp low high value = value clamped to [low, high]
            case args of
                [ low, high, value ] ->
                    let
                        lowStr = generateStandaloneExpr low
                        highStr = generateStandaloneExpr high
                        valStr = generateStandaloneExpr value
                    in
                    "((" ++ valStr ++ " < " ++ lowStr ++ ") ? " ++ lowStr ++ " : ((" ++ valStr ++ " > " ++ highStr ++ ") ? " ++ highStr ++ " : " ++ valStr ++ "))"

                _ ->
                    "/* clamp wrong arity */ 0"

        Src.At _ (Src.Var _ "xor") ->
            -- xor a b = logical XOR (true if exactly one is true)
            case args of
                [ a, b ] ->
                    "((" ++ generateStandaloneExpr a ++ ") != (" ++ generateStandaloneExpr b ++ "))"

                _ ->
                    "/* xor wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Tuple" "first") ->
            -- Tuple.first (a, b) = a
            case args of
                [ tuple ] ->
                    "(" ++ generateStandaloneExpr tuple ++ "._0)"

                _ ->
                    "/* Tuple.first wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Tuple" "second") ->
            -- Tuple.second (a, b) = b
            case args of
                [ tuple ] ->
                    "(" ++ generateStandaloneExpr tuple ++ "._1)"

                _ ->
                    "/* Tuple.second wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Tuple" "pair") ->
            -- Tuple.pair a b = (a, b)
            case args of
                [ a, b ] ->
                    "((elm_tuple2_t){" ++ generateStandaloneExpr a ++ ", " ++ generateStandaloneExpr b ++ "})"

                _ ->
                    "/* Tuple.pair wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Tuple" "mapFirst") ->
            -- Tuple.mapFirst f (a, b) = (f a, b)
            case args of
                [ fnExpr, tupleExpr ] ->
                    let
                        tupleStr = generateStandaloneExpr tupleExpr

                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    let
                                        bodyStr = generateStandaloneExpr lambdaBody
                                    in
                                    "({ int elm_" ++ pname ++ " = __tuple_in._0; " ++ bodyStr ++ "; })"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__tuple_in._0)"
                    in
                    "({ elm_tuple2_t __tuple_in = " ++ tupleStr ++ "; elm_tuple2_t __tuple_out; __tuple_out._0 = " ++ fnAppStr ++ "; __tuple_out._1 = __tuple_in._1; __tuple_out; })"

                _ ->
                    "/* Tuple.mapFirst wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Tuple" "mapSecond") ->
            -- Tuple.mapSecond f (a, b) = (a, f b)
            case args of
                [ fnExpr, tupleExpr ] ->
                    let
                        tupleStr = generateStandaloneExpr tupleExpr

                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    let
                                        bodyStr = generateStandaloneExpr lambdaBody
                                    in
                                    "({ int elm_" ++ pname ++ " = __tuple_in._1; " ++ bodyStr ++ "; })"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__tuple_in._1)"
                    in
                    "({ elm_tuple2_t __tuple_in = " ++ tupleStr ++ "; elm_tuple2_t __tuple_out; __tuple_out._0 = __tuple_in._0; __tuple_out._1 = " ++ fnAppStr ++ "; __tuple_out; })"

                _ ->
                    "/* Tuple.mapSecond wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Tuple" "mapBoth") ->
            -- Tuple.mapBoth f g (a, b) = (f a, g b)
            case args of
                [ fnFirst, fnSecond, tupleExpr ] ->
                    let
                        tupleStr = generateStandaloneExpr tupleExpr

                        fnFirstAppStr =
                            case fnFirst of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    let
                                        bodyStr = generateStandaloneExpr lambdaBody
                                    in
                                    "({ int elm_" ++ pname ++ " = __tuple_in._0; " ++ bodyStr ++ "; })"

                                _ ->
                                    generateStandaloneExpr fnFirst ++ "(__tuple_in._0)"

                        fnSecondAppStr =
                            case fnSecond of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    let
                                        bodyStr = generateStandaloneExpr lambdaBody
                                    in
                                    "({ int elm_" ++ pname ++ " = __tuple_in._1; " ++ bodyStr ++ "; })"

                                _ ->
                                    generateStandaloneExpr fnSecond ++ "(__tuple_in._1)"
                    in
                    "({ elm_tuple2_t __tuple_in = " ++ tupleStr ++ "; elm_tuple2_t __tuple_out; __tuple_out._0 = " ++ fnFirstAppStr ++ "; __tuple_out._1 = " ++ fnSecondAppStr ++ "; __tuple_out; })"

                _ ->
                    "/* Tuple.mapBoth wrong arity */ 0"

        Src.At _ (Src.Var _ "compare") ->
            -- compare a b = Order (LT, EQ, or GT)
            case args of
                [ a, b ] ->
                    let
                        aStr = generateStandaloneExpr a
                        bStr = generateStandaloneExpr b
                    in
                    "((" ++ aStr ++ " < " ++ bStr ++ ") ? ((elm_union_t){TAG_LT, 0}) : ((" ++ aStr ++ " > " ++ bStr ++ ") ? ((elm_union_t){TAG_GT, 0}) : ((elm_union_t){TAG_EQ, 0})))"

                _ ->
                    "/* compare wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Char" "toCode") ->
            -- Char.toCode c = ASCII code of char
            case args of
                [ c ] ->
                    "((int)" ++ generateStandaloneExpr c ++ ")"

                _ ->
                    "/* Char.toCode wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Char" "fromCode") ->
            -- Char.fromCode n = char with ASCII code n
            case args of
                [ n ] ->
                    "((char)" ++ generateStandaloneExpr n ++ ")"

                _ ->
                    "/* Char.fromCode wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Char" "isDigit") ->
            -- Char.isDigit c = True if c is 0-9
            case args of
                [ c ] ->
                    let cStr = generateStandaloneExpr c
                    in "(" ++ cStr ++ " >= '0' && " ++ cStr ++ " <= '9')"

                _ ->
                    "/* Char.isDigit wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Char" "isAlpha") ->
            -- Char.isAlpha c = True if c is a-z or A-Z
            case args of
                [ c ] ->
                    let cStr = generateStandaloneExpr c
                    in "((" ++ cStr ++ " >= 'a' && " ++ cStr ++ " <= 'z') || (" ++ cStr ++ " >= 'A' && " ++ cStr ++ " <= 'Z'))"

                _ ->
                    "/* Char.isAlpha wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Char" "isUpper") ->
            -- Char.isUpper c = True if c is A-Z
            case args of
                [ c ] ->
                    let cStr = generateStandaloneExpr c
                    in "(" ++ cStr ++ " >= 'A' && " ++ cStr ++ " <= 'Z')"

                _ ->
                    "/* Char.isUpper wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Char" "isLower") ->
            -- Char.isLower c = True if c is a-z
            case args of
                [ c ] ->
                    let cStr = generateStandaloneExpr c
                    in "(" ++ cStr ++ " >= 'a' && " ++ cStr ++ " <= 'z')"

                _ ->
                    "/* Char.isLower wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Char" "isAlphaNum") ->
            -- Char.isAlphaNum c = True if c is alphanumeric
            case args of
                [ c ] ->
                    let cStr = generateStandaloneExpr c
                    in "((" ++ cStr ++ " >= 'a' && " ++ cStr ++ " <= 'z') || (" ++ cStr ++ " >= 'A' && " ++ cStr ++ " <= 'Z') || (" ++ cStr ++ " >= '0' && " ++ cStr ++ " <= '9'))"

                _ ->
                    "/* Char.isAlphaNum wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Char" "isHexDigit") ->
            -- Char.isHexDigit c = True if c is 0-9, a-f, or A-F
            case args of
                [ c ] ->
                    let cStr = generateStandaloneExpr c
                    in "((" ++ cStr ++ " >= '0' && " ++ cStr ++ " <= '9') || (" ++ cStr ++ " >= 'a' && " ++ cStr ++ " <= 'f') || (" ++ cStr ++ " >= 'A' && " ++ cStr ++ " <= 'F'))"

                _ ->
                    "/* Char.isHexDigit wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Char" "isOctDigit") ->
            -- Char.isOctDigit c = True if c is 0-7
            case args of
                [ c ] ->
                    let cStr = generateStandaloneExpr c
                    in "(" ++ cStr ++ " >= '0' && " ++ cStr ++ " <= '7')"

                _ ->
                    "/* Char.isOctDigit wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Char" "toUpper") ->
            -- Char.toUpper c = uppercase version of c
            case args of
                [ c ] ->
                    let cStr = generateStandaloneExpr c
                    in "((" ++ cStr ++ " >= 'a' && " ++ cStr ++ " <= 'z') ? " ++ cStr ++ " - 32 : " ++ cStr ++ ")"

                _ ->
                    "/* Char.toUpper wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Char" "toLower") ->
            -- Char.toLower c = lowercase version of c
            case args of
                [ c ] ->
                    let cStr = generateStandaloneExpr c
                    in "((" ++ cStr ++ " >= 'A' && " ++ cStr ++ " <= 'Z') ? " ++ cStr ++ " + 32 : " ++ cStr ++ ")"

                _ ->
                    "/* Char.toLower wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "fromChar") ->
            -- String.fromChar c = single-character string
            case args of
                [ c ] ->
                    "elm_str_from_char(" ++ generateStandaloneExpr c ++ ")"

                _ ->
                    "/* String.fromChar wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "cons") ->
            -- String.cons c s = prepend char c to string s
            case args of
                [ c, s ] ->
                    "elm_str_cons(" ++ generateStandaloneExpr c ++ ", " ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.cons wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "length") ->
            -- String.length s = number of characters in s
            case args of
                [ s ] ->
                    "elm_strlen(" ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.length wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "isEmpty") ->
            -- String.isEmpty s = True if s is empty
            case args of
                [ s ] ->
                    "(*(" ++ generateStandaloneExpr s ++ ") == '\\0')"

                _ ->
                    "/* String.isEmpty wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "fromInt") ->
            -- String.fromInt n = string representation of n
            case args of
                [ n ] ->
                    "elm_from_int(" ++ generateStandaloneExpr n ++ ")"

                _ ->
                    "/* String.fromInt wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "reverse") ->
            -- String.reverse s = reversed string
            case args of
                [ s ] ->
                    "elm_str_reverse(" ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.reverse wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "toInt") ->
            -- String.toInt s = Maybe Int
            case args of
                [ s ] ->
                    "elm_str_to_int(" ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.toInt wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "left") ->
            -- String.left n s = first n characters
            case args of
                [ n, s ] ->
                    "elm_str_left(" ++ generateStandaloneExpr n ++ ", " ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.left wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "right") ->
            -- String.right n s = last n characters
            case args of
                [ n, s ] ->
                    "elm_str_right(" ++ generateStandaloneExpr n ++ ", " ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.right wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "append") ->
            -- String.append a b = concatenate a and b
            case args of
                [ a, b ] ->
                    "elm_str_append(" ++ generateStandaloneExpr a ++ ", " ++ generateStandaloneExpr b ++ ")"

                _ ->
                    "/* String.append wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "contains") ->
            -- String.contains needle haystack = True if needle in haystack
            case args of
                [ needle, haystack ] ->
                    "elm_str_contains(" ++ generateStandaloneExpr needle ++ ", " ++ generateStandaloneExpr haystack ++ ")"

                _ ->
                    "/* String.contains wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "startsWith") ->
            -- String.startsWith prefix s = True if s starts with prefix
            case args of
                [ prefix, s ] ->
                    "elm_str_starts_with(" ++ generateStandaloneExpr prefix ++ ", " ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.startsWith wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "endsWith") ->
            -- String.endsWith suffix s = True if s ends with suffix
            case args of
                [ suffix, s ] ->
                    "elm_str_ends_with(" ++ generateStandaloneExpr suffix ++ ", " ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.endsWith wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "toUpper") ->
            -- String.toUpper s = uppercase version
            case args of
                [ s ] ->
                    "elm_str_to_upper(" ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.toUpper wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "toLower") ->
            -- String.toLower s = lowercase version
            case args of
                [ s ] ->
                    "elm_str_to_lower(" ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.toLower wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "trim") ->
            -- String.trim s = remove leading/trailing whitespace
            case args of
                [ s ] ->
                    "elm_str_trim(" ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.trim wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "repeat") ->
            -- String.repeat n s = repeat s n times
            case args of
                [ n, s ] ->
                    "elm_str_repeat(" ++ generateStandaloneExpr n ++ ", " ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.repeat wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "slice") ->
            -- String.slice start end s = substring from start to end
            case args of
                [ start, end, s ] ->
                    "elm_str_slice(" ++ generateStandaloneExpr start ++ ", " ++ generateStandaloneExpr end ++ ", " ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.slice wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "dropLeft") ->
            -- String.dropLeft n s = drop first n characters
            case args of
                [ n, s ] ->
                    let
                        sStr = generateStandaloneExpr s
                        nStr = generateStandaloneExpr n
                    in
                    "elm_str_slice(" ++ nStr ++ ", elm_strlen(" ++ sStr ++ "), " ++ sStr ++ ")"

                _ ->
                    "/* String.dropLeft wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "dropRight") ->
            -- String.dropRight n s = drop last n characters
            case args of
                [ n, s ] ->
                    let
                        sStr = generateStandaloneExpr s
                        nStr = generateStandaloneExpr n
                    in
                    "elm_str_slice(0, elm_strlen(" ++ sStr ++ ") - " ++ nStr ++ ", " ++ sStr ++ ")"

                _ ->
                    "/* String.dropRight wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "padLeft") ->
            -- String.padLeft n char s = pad s on the left with char to length n
            case args of
                [ n, c, s ] ->
                    "elm_str_pad_left(" ++ generateStandaloneExpr n ++ ", " ++ generateStandaloneExpr c ++ ", " ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.padLeft wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "padRight") ->
            -- String.padRight n char s = pad s on the right with char to length n
            case args of
                [ n, c, s ] ->
                    "elm_str_pad_right(" ++ generateStandaloneExpr n ++ ", " ++ generateStandaloneExpr c ++ ", " ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.padRight wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "any") ->
            -- String.any pred s = True if any char satisfies pred
            case args of
                [ predExpr, strExpr ] ->
                    let
                        strStr = generateStandaloneExpr strExpr

                        -- Generate predicate application for a char
                        predAppStr =
                            case predExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    let
                                        bodyStr = generateStandaloneExpr lambdaBody
                                    in
                                    "({ int elm_" ++ pname ++ " = __str[__i]; " ++ bodyStr ++ "; })"

                                Src.At _ (Src.VarQual _ "Char" fnName) ->
                                    -- Handle Char.isDigit, Char.isAlpha, etc.
                                    generateCharPredCall fnName "__str[__i]"

                                _ ->
                                    generateStandaloneExpr predExpr ++ "(__str[__i])"
                    in
                    "({ const char *__str = " ++ strStr ++ "; int __result = 0; for (int __i = 0; __str[__i]; __i++) { if (" ++ predAppStr ++ ") { __result = 1; break; } } __result; })"

                _ ->
                    "/* String.any wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "all") ->
            -- String.all pred s = True if all chars satisfy pred
            case args of
                [ predExpr, strExpr ] ->
                    let
                        strStr = generateStandaloneExpr strExpr

                        -- Generate predicate application for a char
                        predAppStr =
                            case predExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    let
                                        bodyStr = generateStandaloneExpr lambdaBody
                                    in
                                    "({ int elm_" ++ pname ++ " = __str[__i]; " ++ bodyStr ++ "; })"

                                Src.At _ (Src.VarQual _ "Char" fnName) ->
                                    -- Handle Char.isDigit, Char.isAlpha, etc.
                                    generateCharPredCall fnName "__str[__i]"

                                _ ->
                                    generateStandaloneExpr predExpr ++ "(__str[__i])"
                    in
                    "({ const char *__str = " ++ strStr ++ "; int __result = 1; for (int __i = 0; __str[__i]; __i++) { if (!(" ++ predAppStr ++ ")) { __result = 0; break; } } __result; })"

                _ ->
                    "/* String.all wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "foldl") ->
            -- String.foldl f init str = fold from left
            case args of
                [ fnExpr, initExpr, strExpr ] ->
                    let
                        strStr = generateStandaloneExpr strExpr
                        initStr = generateStandaloneExpr initExpr

                        -- Generate function application: f char acc
                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar charName), Src.At _ (Src.PVar accName) ] lambdaBody) ->
                                    let
                                        bodyStr = generateStandaloneExpr lambdaBody
                                    in
                                    "({ int elm_" ++ charName ++ " = __str[__i]; int elm_" ++ accName ++ " = __acc; " ++ bodyStr ++ "; })"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__str[__i], __acc)"
                    in
                    "({ const char *__str = " ++ strStr ++ "; int __acc = " ++ initStr ++ "; for (int __i = 0; __str[__i]; __i++) { __acc = " ++ fnAppStr ++ "; } __acc; })"

                _ ->
                    "/* String.foldl wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "foldr") ->
            -- String.foldr f init str = fold from right
            case args of
                [ fnExpr, initExpr, strExpr ] ->
                    let
                        strStr = generateStandaloneExpr strExpr
                        initStr = generateStandaloneExpr initExpr

                        -- Generate function application: f char acc
                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar charName), Src.At _ (Src.PVar accName) ] lambdaBody) ->
                                    let
                                        bodyStr = generateStandaloneExpr lambdaBody
                                    in
                                    "({ int elm_" ++ charName ++ " = __str[__i]; int elm_" ++ accName ++ " = __acc; " ++ bodyStr ++ "; })"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__str[__i], __acc)"
                    in
                    "({ const char *__str = " ++ strStr ++ "; int __len = 0; while (__str[__len]) __len++; int __acc = " ++ initStr ++ "; for (int __i = __len - 1; __i >= 0; __i--) { __acc = " ++ fnAppStr ++ "; } __acc; })"

                _ ->
                    "/* String.foldr wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "filter") ->
            -- String.filter pred str = filter characters by predicate
            case args of
                [ predExpr, strExpr ] ->
                    let
                        strStr = generateStandaloneExpr strExpr

                        predAppStr =
                            case predExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    let
                                        bodyStr = generateStandaloneExpr lambdaBody
                                    in
                                    "({ int elm_" ++ pname ++ " = __str[__i]; " ++ bodyStr ++ "; })"

                                Src.At _ (Src.VarQual _ "Char" fnName) ->
                                    generateCharPredCall fnName "__str[__i]"

                                _ ->
                                    generateStandaloneExpr predExpr ++ "(__str[__i])"
                    in
                    "({ static char __filter_buf[256]; const char *__str = " ++ strStr ++ "; int __j = 0; for (int __i = 0; __str[__i] && __j < 255; __i++) { if (" ++ predAppStr ++ ") { __filter_buf[__j++] = __str[__i]; } } __filter_buf[__j] = 0; __filter_buf; })"

                _ ->
                    "/* String.filter wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "map") ->
            -- String.map f str = apply f to each character
            case args of
                [ fnExpr, strExpr ] ->
                    let
                        strStr = generateStandaloneExpr strExpr

                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    let
                                        bodyStr = generateStandaloneExpr lambdaBody
                                    in
                                    "({ int elm_" ++ pname ++ " = __str[__i]; " ++ bodyStr ++ "; })"

                                Src.At _ (Src.VarQual _ "Char" "toUpper") ->
                                    "((__str[__i] >= 'a' && __str[__i] <= 'z') ? __str[__i] - 32 : __str[__i])"

                                Src.At _ (Src.VarQual _ "Char" "toLower") ->
                                    "((__str[__i] >= 'A' && __str[__i] <= 'Z') ? __str[__i] + 32 : __str[__i])"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__str[__i])"
                    in
                    "({ static char __map_buf[256]; const char *__str = " ++ strStr ++ "; int __i = 0; for (; __str[__i] && __i < 255; __i++) { __map_buf[__i] = " ++ fnAppStr ++ "; } __map_buf[__i] = 0; __map_buf; })"

                _ ->
                    "/* String.map wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Bitwise" "and") ->
            -- Bitwise.and a b = a & b
            case args of
                [ a, b ] ->
                    "(" ++ generateStandaloneExpr a ++ " & " ++ generateStandaloneExpr b ++ ")"

                _ ->
                    "/* Bitwise.and wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Bitwise" "or") ->
            -- Bitwise.or a b = a | b
            case args of
                [ a, b ] ->
                    "(" ++ generateStandaloneExpr a ++ " | " ++ generateStandaloneExpr b ++ ")"

                _ ->
                    "/* Bitwise.or wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Bitwise" "xor") ->
            -- Bitwise.xor a b = a ^ b
            case args of
                [ a, b ] ->
                    "(" ++ generateStandaloneExpr a ++ " ^ " ++ generateStandaloneExpr b ++ ")"

                _ ->
                    "/* Bitwise.xor wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Bitwise" "complement") ->
            -- Bitwise.complement a = ~a
            case args of
                [ a ] ->
                    "(~" ++ generateStandaloneExpr a ++ ")"

                _ ->
                    "/* Bitwise.complement wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Bitwise" "shiftLeftBy") ->
            -- Bitwise.shiftLeftBy n x = x << n
            case args of
                [ n, x ] ->
                    "(" ++ generateStandaloneExpr x ++ " << " ++ generateStandaloneExpr n ++ ")"

                _ ->
                    "/* Bitwise.shiftLeftBy wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Bitwise" "shiftRightBy") ->
            -- Bitwise.shiftRightBy n x = x >> n (arithmetic shift)
            case args of
                [ n, x ] ->
                    "(" ++ generateStandaloneExpr x ++ " >> " ++ generateStandaloneExpr n ++ ")"

                _ ->
                    "/* Bitwise.shiftRightBy wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Bitwise" "shiftRightZfBy") ->
            -- Bitwise.shiftRightZfBy n x = x >>> n (logical shift - zero fill)
            case args of
                [ n, x ] ->
                    "((unsigned int)" ++ generateStandaloneExpr x ++ " >> " ++ generateStandaloneExpr n ++ ")"

                _ ->
                    "/* Bitwise.shiftRightZfBy wrong arity */ 0"

        Src.At _ (Src.Var _ "floor") ->
            -- floor x = largest int <= x
            case args of
                [ x ] ->
                    "((int)" ++ generateStandaloneExpr x ++ ")"

                _ ->
                    "/* floor wrong arity */ 0"

        Src.At _ (Src.Var _ "ceiling") ->
            -- ceiling x = smallest int >= x
            case args of
                [ x ] ->
                    let
                        xStr = generateStandaloneExpr x
                    in
                    "((int)" ++ xStr ++ " + (" ++ xStr ++ " > (int)" ++ xStr ++ " ? 1 : 0))"

                _ ->
                    "/* ceiling wrong arity */ 0"

        Src.At _ (Src.Var _ "round") ->
            -- round x = nearest int (round half away from zero)
            case args of
                [ x ] ->
                    let
                        xStr = generateStandaloneExpr x
                    in
                    "(" ++ xStr ++ " >= 0 ? (int)(" ++ xStr ++ " + 0.5) : (int)(" ++ xStr ++ " - 0.5))"

                _ ->
                    "/* round wrong arity */ 0"

        Src.At _ (Src.Var _ "truncate") ->
            -- truncate x = int towards zero
            case args of
                [ x ] ->
                    "((int)" ++ generateStandaloneExpr x ++ ")"

                _ ->
                    "/* truncate wrong arity */ 0"

        Src.At _ (Src.Var _ "sqrt") ->
            -- sqrt x = square root of x (integer approximation)
            case args of
                [ x ] ->
                    "elm_isqrt((int)" ++ generateStandaloneExpr x ++ ")"

                _ ->
                    "/* sqrt wrong arity */ 0"

        Src.At _ (Src.Var _ "toFloat") ->
            -- toFloat n = convert int to float
            case args of
                [ n ] ->
                    "((double)" ++ generateStandaloneExpr n ++ ")"

                _ ->
                    "/* toFloat wrong arity */ 0"

        Src.At _ (Src.Var _ "isEven") ->
            -- isEven n = True if n is even
            case args of
                [ n ] ->
                    "((" ++ generateStandaloneExpr n ++ " % 2) == 0)"

                _ ->
                    "/* isEven wrong arity */ 0"

        Src.At _ (Src.Var _ "isOdd") ->
            -- isOdd n = True if n is odd
            case args of
                [ n ] ->
                    "((" ++ generateStandaloneExpr n ++ " % 2) != 0)"

                _ ->
                    "/* isOdd wrong arity */ 0"

        Src.At _ (Src.Var Src.CapVar "Just") ->
            -- Built-in Maybe Just constructor
            case args of
                [ value ] ->
                    "((elm_union_t){TAG_Just, " ++ generateStandaloneExpr value ++ "})"

                _ ->
                    "/* Just wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Maybe" "withDefault") ->
            -- Maybe.withDefault default maybe = value or default
            case args of
                [ defaultVal, maybeVal ] ->
                    let
                        defStr = generateStandaloneExpr defaultVal
                        maybeStr = generateStandaloneExpr maybeVal
                    in
                    "((" ++ maybeStr ++ ").tag == TAG_Just ? (" ++ maybeStr ++ ").data : " ++ defStr ++ ")"

                _ ->
                    "/* Maybe.withDefault wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Maybe" "map") ->
            -- Maybe.map f maybe = apply f to value if Just, else Nothing
            case args of
                [ fnExpr, maybeVal ] ->
                    let
                        maybeStr = generateStandaloneExpr maybeVal
                        -- Generate the function application to __maybe_val.data
                        fnAppStr =
                            case fnExpr of
                                Src.At pos (Src.Lambda patterns body) ->
                                    -- Inline lambda: bind __maybe_val.data to pattern and evaluate body
                                    case patterns of
                                        [ Src.At _ (Src.PVar varName) ] ->
                                            "({ int elm_" ++ varName ++ " = __maybe_val.data; " ++ generateStandaloneExpr body ++ "; })"

                                        _ ->
                                            "/* unsupported lambda pattern in Maybe.map */ 0"

                                _ ->
                                    -- Regular function call
                                    generateStandaloneExpr fnExpr ++ "(__maybe_val.data)"
                    in
                    "({ elm_union_t __maybe_val = " ++ maybeStr ++ "; __maybe_val.tag == TAG_Just ? ((elm_union_t){TAG_Just, " ++ fnAppStr ++ "}) : ((elm_union_t){TAG_Nothing, 0}); })"

                _ ->
                    "/* Maybe.map wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Maybe" "andThen") ->
            -- Maybe.andThen f maybe = apply f (returns Maybe) if Just, else Nothing
            case args of
                [ fnExpr, maybeVal ] ->
                    let
                        maybeStr = generateStandaloneExpr maybeVal
                        -- Generate the function application to __maybe_val.data (returns Maybe)
                        fnAppStr =
                            case fnExpr of
                                Src.At pos (Src.Lambda patterns body) ->
                                    -- Inline lambda: bind __maybe_val.data to pattern and evaluate body
                                    case patterns of
                                        [ Src.At _ (Src.PVar varName) ] ->
                                            "({ int elm_" ++ varName ++ " = __maybe_val.data; " ++ generateStandaloneExpr body ++ "; })"

                                        _ ->
                                            "/* unsupported lambda pattern in Maybe.andThen */ ((elm_union_t){TAG_Nothing, 0})"

                                _ ->
                                    -- Regular function call
                                    generateStandaloneExpr fnExpr ++ "(__maybe_val.data)"
                    in
                    "({ elm_union_t __maybe_val = " ++ maybeStr ++ "; __maybe_val.tag == TAG_Just ? " ++ fnAppStr ++ " : ((elm_union_t){TAG_Nothing, 0}); })"

                _ ->
                    "/* Maybe.andThen wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Maybe" "map2") ->
            -- Maybe.map2 f maybeA maybeB = apply f if both are Just
            case args of
                [ fnExpr, maybeA, maybeB ] ->
                    let
                        maybeAStr = generateStandaloneExpr maybeA
                        maybeBStr = generateStandaloneExpr maybeB

                        -- Generate function application with two args
                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda patterns lambdaBody) ->
                                    case patterns of
                                        [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PVar pname2) ] ->
                                            -- Simple two-arg lambda: (\a b -> body)
                                            let
                                                bodyStr = generateStandaloneExpr lambdaBody
                                            in
                                            "({ int elm_" ++ pname1 ++ " = __maybe_a.data; int elm_" ++ pname2 ++ " = __maybe_b.data; " ++ bodyStr ++ "; })"

                                        _ ->
                                            "/* unsupported lambda pattern in Maybe.map2 */ 0"

                                _ ->
                                    -- Regular function call
                                    generateStandaloneExpr fnExpr ++ "(__maybe_a.data, __maybe_b.data)"
                    in
                    "({ elm_union_t __maybe_a = " ++ maybeAStr ++ "; elm_union_t __maybe_b = " ++ maybeBStr ++ "; (__maybe_a.tag == TAG_Just && __maybe_b.tag == TAG_Just) ? ((elm_union_t){TAG_Just, " ++ fnAppStr ++ "}) : ((elm_union_t){TAG_Nothing, 0}); })"

                _ ->
                    "/* Maybe.map2 wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Maybe" "map3") ->
            -- Maybe.map3 f maybeA maybeB maybeC = apply f if all three are Just
            case args of
                [ fnExpr, maybeA, maybeB, maybeC ] ->
                    let
                        maybeAStr = generateStandaloneExpr maybeA
                        maybeBStr = generateStandaloneExpr maybeB
                        maybeCStr = generateStandaloneExpr maybeC

                        -- Generate function application with three args
                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda patterns lambdaBody) ->
                                    case patterns of
                                        [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PVar pname2), Src.At _ (Src.PVar pname3) ] ->
                                            -- Simple three-arg lambda: (\a b c -> body)
                                            let
                                                bodyStr = generateStandaloneExpr lambdaBody
                                            in
                                            "({ int elm_" ++ pname1 ++ " = __maybe_a.data; int elm_" ++ pname2 ++ " = __maybe_b.data; int elm_" ++ pname3 ++ " = __maybe_c.data; " ++ bodyStr ++ "; })"

                                        _ ->
                                            "/* unsupported lambda pattern in Maybe.map3 */ 0"

                                _ ->
                                    -- Regular function call
                                    generateStandaloneExpr fnExpr ++ "(__maybe_a.data, __maybe_b.data, __maybe_c.data)"
                    in
                    "({ elm_union_t __maybe_a = " ++ maybeAStr ++ "; elm_union_t __maybe_b = " ++ maybeBStr ++ "; elm_union_t __maybe_c = " ++ maybeCStr ++ "; (__maybe_a.tag == TAG_Just && __maybe_b.tag == TAG_Just && __maybe_c.tag == TAG_Just) ? ((elm_union_t){TAG_Just, " ++ fnAppStr ++ "}) : ((elm_union_t){TAG_Nothing, 0}); })"

                _ ->
                    "/* Maybe.map3 wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Maybe" "map4") ->
            -- Maybe.map4 f maybeA maybeB maybeC maybeD = apply f if all four are Just
            case args of
                [ fnExpr, maybeA, maybeB, maybeC, maybeD ] ->
                    let
                        maybeAStr = generateStandaloneExpr maybeA
                        maybeBStr = generateStandaloneExpr maybeB
                        maybeCStr = generateStandaloneExpr maybeC
                        maybeDStr = generateStandaloneExpr maybeD

                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda patterns lambdaBody) ->
                                    case patterns of
                                        [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PVar pname2), Src.At _ (Src.PVar pname3), Src.At _ (Src.PVar pname4) ] ->
                                            let
                                                bodyStr = generateStandaloneExpr lambdaBody
                                            in
                                            "({ int elm_" ++ pname1 ++ " = __maybe_a.data; int elm_" ++ pname2 ++ " = __maybe_b.data; int elm_" ++ pname3 ++ " = __maybe_c.data; int elm_" ++ pname4 ++ " = __maybe_d.data; " ++ bodyStr ++ "; })"

                                        _ ->
                                            "/* unsupported lambda pattern in Maybe.map4 */ 0"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__maybe_a.data, __maybe_b.data, __maybe_c.data, __maybe_d.data)"
                    in
                    "({ elm_union_t __maybe_a = " ++ maybeAStr ++ "; elm_union_t __maybe_b = " ++ maybeBStr ++ "; elm_union_t __maybe_c = " ++ maybeCStr ++ "; elm_union_t __maybe_d = " ++ maybeDStr ++ "; (__maybe_a.tag == TAG_Just && __maybe_b.tag == TAG_Just && __maybe_c.tag == TAG_Just && __maybe_d.tag == TAG_Just) ? ((elm_union_t){TAG_Just, " ++ fnAppStr ++ "}) : ((elm_union_t){TAG_Nothing, 0}); })"

                _ ->
                    "/* Maybe.map4 wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Maybe" "map5") ->
            -- Maybe.map5 f a b c d e = apply f if all five are Just
            case args of
                [ fnExpr, maybeA, maybeB, maybeC, maybeD, maybeE ] ->
                    let
                        maybeAStr = generateStandaloneExpr maybeA
                        maybeBStr = generateStandaloneExpr maybeB
                        maybeCStr = generateStandaloneExpr maybeC
                        maybeDStr = generateStandaloneExpr maybeD
                        maybeEStr = generateStandaloneExpr maybeE

                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda patterns lambdaBody) ->
                                    case patterns of
                                        [ Src.At _ (Src.PVar p1), Src.At _ (Src.PVar p2), Src.At _ (Src.PVar p3), Src.At _ (Src.PVar p4), Src.At _ (Src.PVar p5) ] ->
                                            let
                                                bodyStr = generateStandaloneExpr lambdaBody
                                            in
                                            "({ int elm_" ++ p1 ++ " = __maybe_a.data; int elm_" ++ p2 ++ " = __maybe_b.data; int elm_" ++ p3 ++ " = __maybe_c.data; int elm_" ++ p4 ++ " = __maybe_d.data; int elm_" ++ p5 ++ " = __maybe_e.data; " ++ bodyStr ++ "; })"

                                        _ ->
                                            "/* unsupported lambda pattern in Maybe.map5 */ 0"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__maybe_a.data, __maybe_b.data, __maybe_c.data, __maybe_d.data, __maybe_e.data)"
                    in
                    "({ elm_union_t __maybe_a = " ++ maybeAStr ++ "; elm_union_t __maybe_b = " ++ maybeBStr ++ "; elm_union_t __maybe_c = " ++ maybeCStr ++ "; elm_union_t __maybe_d = " ++ maybeDStr ++ "; elm_union_t __maybe_e = " ++ maybeEStr ++ "; (__maybe_a.tag == TAG_Just && __maybe_b.tag == TAG_Just && __maybe_c.tag == TAG_Just && __maybe_d.tag == TAG_Just && __maybe_e.tag == TAG_Just) ? ((elm_union_t){TAG_Just, " ++ fnAppStr ++ "}) : ((elm_union_t){TAG_Nothing, 0}); })"

                _ ->
                    "/* Maybe.map5 wrong arity */ 0"

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


{-| Generate inline code for Char predicates
-}
generateCharPredCall : String -> String -> String
generateCharPredCall fnName charExpr =
    case fnName of
        "isDigit" ->
            "(" ++ charExpr ++ " >= '0' && " ++ charExpr ++ " <= '9')"

        "isAlpha" ->
            "((" ++ charExpr ++ " >= 'a' && " ++ charExpr ++ " <= 'z') || (" ++ charExpr ++ " >= 'A' && " ++ charExpr ++ " <= 'Z'))"

        "isUpper" ->
            "(" ++ charExpr ++ " >= 'A' && " ++ charExpr ++ " <= 'Z')"

        "isLower" ->
            "(" ++ charExpr ++ " >= 'a' && " ++ charExpr ++ " <= 'z')"

        "isAlphaNum" ->
            "((" ++ charExpr ++ " >= 'a' && " ++ charExpr ++ " <= 'z') || (" ++ charExpr ++ " >= 'A' && " ++ charExpr ++ " <= 'Z') || (" ++ charExpr ++ " >= '0' && " ++ charExpr ++ " <= '9'))"

        "isHexDigit" ->
            "((" ++ charExpr ++ " >= '0' && " ++ charExpr ++ " <= '9') || (" ++ charExpr ++ " >= 'a' && " ++ charExpr ++ " <= 'f') || (" ++ charExpr ++ " >= 'A' && " ++ charExpr ++ " <= 'F'))"

        "isOctDigit" ->
            "(" ++ charExpr ++ " >= '0' && " ++ charExpr ++ " <= '7')"

        _ ->
            "/* unknown Char predicate: " ++ fnName ++ " */ 0"


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
