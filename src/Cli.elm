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
                    if target == "rtems" then
                        generateRtemsCode ast

                    else if target == "native" then
                        generateNativeCode ast

                    else if target == "native-worker" then
                        C.generateNativeWorkerModule ast

                    else if target == "tcc" then
                        generateTccCode ast

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


{-| Generate C code for RTEMS target (x86_64) with Init wrapper
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
        -- Using recursive pattern instead of List.any for self-hosting compatibility
        unionHasData union =
            ctorListHasData union.ctors

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
                , "#include <stdlib.h>"
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
                , "static double elm_strlen(const char *s) {"
                , "    int len = 0;"
                , "    while (*s++) len++;"
                , "    return len;"
                , "}"
                , ""
                , "/* Integer power function */"
                , "static double elm_pow(int base, int exp) {"
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
                , "/* Integer log base 2 (floor) */"
                , "static double elm_ilog2(int x) {"
                , "    if (x <= 0) return 0;"
                , "    int r = 0;"
                , "    while (x > 1) { x >>= 1; r++; }"
                , "    return r;"
                , "}"
                , ""
                , "/* Forward declaration for recursive union type */"
                , "struct elm_union_s;"
                , ""
                , "/* Generic tagged union type for custom types */"
                , "/* Supports both primitive values (.num) and nested unions (.child) */"
                , "typedef struct elm_union_s { int tag; union { double num; struct elm_union_s *child; const char *str; } data; struct elm_union_s *data2; } elm_union_t;"
                , ""
                , "/* Helper to allocate a nested union on the heap */"
                , "static elm_union_t *elm_alloc_union(elm_union_t val) {"
                , "    elm_union_t *p = (elm_union_t*)malloc(sizeof(elm_union_t));"
                , "    *p = val;"
                , "    return p;"
                , "}"
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
                , "/* Built-in Result type tags */"
                , "#define TAG_Err 0"
                , "#define TAG_Ok 1"
                , ""
                , "/* Built-in List type - fixed-size array (max 16 elements) */"
                , "#define ELM_LIST_MAX 16"
                , "typedef struct { int length; int data[ELM_LIST_MAX]; } elm_list_t;"
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
                , "/* String.trimLeft - remove leading whitespace */"
                , "static char __elm_triml_buf[256];"
                , "static const char *elm_str_trim_left(const char *s) {"
                , "    while (*s == ' ' || *s == '\\t' || *s == '\\n' || *s == '\\r') s++;"
                , "    int i = 0; for (; s[i] && i < 255; i++) __elm_triml_buf[i] = s[i];"
                , "    __elm_triml_buf[i] = 0;"
                , "    return __elm_triml_buf;"
                , "}"
                , ""
                , "/* String.trimRight - remove trailing whitespace */"
                , "static char __elm_trimr_buf[256];"
                , "static const char *elm_str_trim_right(const char *s) {"
                , "    int len = 0; while (s[len]) len++;"
                , "    while (len > 0 && (s[len-1] == ' ' || s[len-1] == '\\t' || s[len-1] == '\\n' || s[len-1] == '\\r')) len--;"
                , "    for (int i = 0; i < len && i < 255; i++) __elm_trimr_buf[i] = s[i];"
                , "    __elm_trimr_buf[len < 255 ? len : 255] = 0;"
                , "    return __elm_trimr_buf;"
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
                , "static double elm_str_starts_with(const char *prefix, const char *s) {"
                , "    while (*prefix && *s && *prefix == *s) { prefix++; s++; }"
                , "    return !*prefix;"
                , "}"
                , ""
                , "/* String.endsWith - check if string ends with suffix */"
                , "static double elm_str_ends_with(const char *suffix, const char *s) {"
                , "    int slen = 0, sufflen = 0;"
                , "    while (s[slen]) slen++;"
                , "    while (suffix[sufflen]) sufflen++;"
                , "    if (sufflen > slen) return 0;"
                , "    return !strcmp(s + slen - sufflen, suffix);"
                , "}"
                , ""
                , "/* String.contains - check if substring exists */"
                , "static double elm_str_contains(const char *needle, const char *haystack) {"
                , "    if (!*needle) return 1;"
                , "    for (; *haystack; haystack++) {"
                , "        const char *h = haystack, *n = needle;"
                , "        while (*h && *n && *h == *n) { h++; n++; }"
                , "        if (!*n) return 1;"
                , "    }"
                , "    return 0;"
                , "}"
                , ""
                , "/* String.replace - replace all occurrences of target with replacement */"
                , "static char __elm_replace_buf[512];"
                , "static const char *elm_str_replace(const char *target, const char *replacement, const char *src) {"
                , "    int tlen = 0, rlen = 0, slen = 0;"
                , "    while (target[tlen]) tlen++;"
                , "    while (replacement[rlen]) rlen++;"
                , "    while (src[slen]) slen++;"
                , "    if (tlen == 0) { for (int i = 0; i <= slen && i < 511; i++) __elm_replace_buf[i] = src[i]; return __elm_replace_buf; }"
                , "    int j = 0;"
                , "    for (int i = 0; src[i] && j < 510; ) {"
                , "        int match = 1;"
                , "        for (int k = 0; k < tlen && match; k++) {"
                , "            if (src[i + k] != target[k]) match = 0;"
                , "        }"
                , "        if (match) {"
                , "            for (int k = 0; k < rlen && j < 510; k++) __elm_replace_buf[j++] = replacement[k];"
                , "            i += tlen;"
                , "        } else {"
                , "            __elm_replace_buf[j++] = src[i++];"
                , "        }"
                , "    }"
                , "    __elm_replace_buf[j] = 0;"
                , "    return __elm_replace_buf;"
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
                , "/* String.toFloat - parse float string (returns integer part) */"
                , "static elm_union_t elm_str_to_float(const char *s) {"
                , "    int result = 0, neg = 0, i = 0;"
                , "    if (!s || !*s) return (elm_union_t){TAG_Nothing, 0};"
                , "    if (s[0] == '-') { neg = 1; i = 1; }"
                , "    else if (s[0] == '+') { i = 1; }"
                , "    if (!s[i]) return (elm_union_t){TAG_Nothing, 0};"
                , "    for (; s[i] && s[i] != '.'; i++) {"
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


{-| Generate C code for TCC (Tiny C Compiler)
    Uses lambda lifting to avoid GCC-specific nested functions.
    Similar to RTEMS code but without RTEMS-specific runtime.
-}
generateTccCode : Src.Module -> String
generateTccCode ast =
    let
        moduleName =
            ast.name
                |> Maybe.map (\(Src.At _ n) -> n)
                |> Maybe.withDefault "Main"

        mainValue =
            extractMain ast

        -- Detect if expression is simple (can be used as static initializer)
        isSimpleExpr (Src.At _ e) =
            case e of
                Src.Str _ -> True
                Src.Int _ -> True
                Src.Float _ -> True
                Src.Chr _ -> True
                _ -> False

        -- Generate module-level constants (non-main values without arguments)
        -- Simple expressions become static const, complex ones become getter functions
        ( simpleConstants, complexConstants ) =
            ast.values
                |> List.filterMap
                    (\(Src.At _ value) ->
                        let
                            (Src.At _ name) =
                                value.name
                        in
                        if name /= "main" && List.isEmpty value.args then
                            Just ( name, value.body, isSimpleExpr value.body )
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

        -- Replace complex constant references with function calls in an expression
        -- NOTE: For self-hosting, we skip this transformation to avoid closure capture issues
        -- This may cause issues with complex constants, but enables bootstrapping
        fixComplexConstantRefs code =
            code

        complexConstantsCode =
            complexConstants
                |> List.map
                    (\( name, body, _ ) ->
                        let
                            cExpr = generateStandaloneExpr body
                            isString = String.contains "elm_str_" cExpr || String.contains "\"" cExpr
                            isRecord = String.contains "((struct {" cExpr
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
                                if isString then "const char *"
                                else if isRecord then recordType
                                else "double"
                        in
                        -- For records, generate static constants instead of functions
                        -- This allows `model = init` to work correctly
                        if isRecord then
                            let
                                -- Extract initializer from ((struct {...}){...})
                                initStartMarker = "){"
                                initStartIdx = String.indexes initStartMarker cExpr |> List.head |> Maybe.withDefault 0
                                afterInitStart = String.dropLeft (initStartIdx + String.length initStartMarker) cExpr
                                initEndIdx = String.length afterInitStart - 2
                                initializer = String.left initEndIdx afterInitStart
                            in
                            "static " ++ recordType ++ " elm_" ++ name ++ " = {" ++ initializer ++ "};"
                        else
                            "static " ++ cType ++ " elm_" ++ name ++ "(void) {\n    return " ++ fixComplexConstantRefs cExpr ++ ";\n}"
                    )
                |> String.join "\n\n"

        moduleConstantsCode =
            let
                simple = if String.isEmpty simpleConstantsCode then "" else "/* Simple constants */\n" ++ simpleConstantsCode ++ "\n\n"
                complex = if String.isEmpty complexConstantsCode then "" else "/* Computed constants (as functions) */\n" ++ complexConstantsCode ++ "\n\n"
            in
            simple ++ complex

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
                            Just (fixComplexConstantRefs (generateUserFunction name value.args value.body))

                        else
                            Nothing
                    )
                |> String.join "\n\n"

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
            ast.values
                |> List.filterMap
                    (\(Src.At _ value) ->
                        let
                            (Src.At _ name) =
                                value.name
                        in
                        if name /= "main" && not (List.isEmpty value.args) then
                            let
                                -- Generate implementation to infer types
                                implCode = generateUserFunction name value.args value.body
                                -- Extract signature parts from implementation
                                signatureEnd = String.indexes "(" implCode |> List.head |> Maybe.withDefault 0
                                paramsStart = signatureEnd + 1
                                paramsEnd = String.indexes ")" implCode |> List.head |> Maybe.withDefault 0
                                returnTypeStart = 7 -- Length of "static "
                                -- Find " elm_<name>(" to locate the function name position
                                funcNameMarker = " elm_" ++ name ++ "("
                                returnTypeEnd = String.indexes funcNameMarker implCode |> List.head |> Maybe.withDefault 0
                                funcReturnType = String.slice returnTypeStart returnTypeEnd implCode
                                params = String.slice paramsStart paramsEnd implCode
                                -- Skip forward declarations for functions with anonymous structs
                                -- In C, anonymous structs are always different types
                                isStructReturn = String.contains "struct {" funcReturnType
                                isStructParam = String.contains "struct {" params
                            in
                            if isStructReturn || isStructParam then
                                Nothing
                            else
                                Just ("static " ++ funcReturnType ++ " elm_" ++ name ++ "(" ++ params ++ ");")
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
            , "#include <stdio.h>"
            , "#include <stdlib.h>"
            , "#include <string.h>"
            , "#include <math.h>"
            , ""
            , "/* String.fromInt - convert int to string (uses rotating buffer pool) */"
            , "#define ELM_FROMINT_POOL_SIZE 64"
            , "static char __elm_fromint_pool[ELM_FROMINT_POOL_SIZE][32];"
            , "static int __elm_fromint_idx = 0;"
            , "static const char *elm_from_int(int n) {"
            , "    char *buf = __elm_fromint_pool[__elm_fromint_idx];"
            , "    __elm_fromint_idx = (__elm_fromint_idx + 1) % ELM_FROMINT_POOL_SIZE;"
            , "    char tmp[32];"
            , "    int i = 0, j = 0;"
            , "    int neg = 0;"
            , "    if (n < 0) { neg = 1; n = -n; }"
            , "    if (n == 0) { buf[0] = '0'; buf[1] = 0; return buf; }"
            , "    while (n > 0) { tmp[i++] = '0' + (n % 10); n /= 10; }"
            , "    if (neg) buf[j++] = '-';"
            , "    while (i > 0) buf[j++] = tmp[--i];"
            , "    buf[j] = 0;"
            , "    return buf;"
            , "}"
            , ""
            , "/* String.fromFloat - convert double to string (uses rotating buffer pool) */"
            , "#define ELM_FROMFLOAT_POOL_SIZE 64"
            , "static char __elm_fromfloat_pool[ELM_FROMFLOAT_POOL_SIZE][64];"
            , "static int __elm_fromfloat_idx = 0;"
            , "static const char *elm_from_float(double f) {"
            , "    char *buf = __elm_fromfloat_pool[__elm_fromfloat_idx];"
            , "    __elm_fromfloat_idx = (__elm_fromfloat_idx + 1) % ELM_FROMFLOAT_POOL_SIZE;"
            , "    snprintf(buf, 64, \"%g\", f);"
            , "    return buf;"
            , "}"
            , ""
            , "/* String.append - concatenate two strings (uses rotating buffer pool) */"
            , "#define ELM_STR_POOL_SIZE 32"
            , "#define ELM_STR_BUF_SIZE 4096"
            , "static char __elm_str_pool[ELM_STR_POOL_SIZE][ELM_STR_BUF_SIZE];"
            , "static int __elm_str_pool_idx = 0;"
            , "static const char *elm_str_append(const char *a, const char *b) {"
            , "    char *buf = __elm_str_pool[__elm_str_pool_idx];"
            , "    __elm_str_pool_idx = (__elm_str_pool_idx + 1) % ELM_STR_POOL_SIZE;"
            , "    int i = 0, j = 0;"
            , "    while (a[i] && i < ELM_STR_BUF_SIZE - 1) { buf[i] = a[i]; i++; }"
            , "    while (b[j] && i + j < ELM_STR_BUF_SIZE - 1) { buf[i + j] = b[j]; j++; }"
            , "    buf[i + j] = 0;"
            , "    return buf;"
            , "}"
            , ""
            , "/* Power function */"
            , "static double elm_pow(double base, double exp) {"
            , "    return pow(base, exp);"
            , "}"
            , ""
            , "/* String length */"
            , "static double elm_strlen(const char *s) {"
            , "    return (double)strlen(s);"
            , "}"
            , ""
            , "/* String.endsWith - check if string ends with suffix */"
            , "static int elm_str_ends_with(const char *suffix, const char *s) {"
            , "    int slen = 0, suflen = 0;"
            , "    while (s[slen]) slen++;"
            , "    while (suffix[suflen]) suflen++;"
            , "    if (suflen > slen) return 0;"
            , "    for (int i = 0; i < suflen; i++) {"
            , "        if (s[slen - suflen + i] != suffix[i]) return 0;"
            , "    }"
            , "    return 1;"
            , "}"
            , ""
            , "/* String.contains - check if string contains substring */"
            , "static int elm_str_contains(const char *needle, const char *haystack) {"
            , "    int nlen = 0, hlen = 0;"
            , "    while (needle[nlen]) nlen++;"
            , "    while (haystack[hlen]) hlen++;"
            , "    if (nlen == 0) return 1;"
            , "    for (int i = 0; i <= hlen - nlen; i++) {"
            , "        int match = 1;"
            , "        for (int j = 0; j < nlen && match; j++) {"
            , "            if (haystack[i + j] != needle[j]) match = 0;"
            , "        }"
            , "        if (match) return 1;"
            , "    }"
            , "    return 0;"
            , "}"
            , ""
            , "/* Forward declarations for recursive types */"
            , "struct elm_list_s;"
            , "struct elm_union_s;"
            , ""
            , "/* Generic tagged union type for custom types */"
            , "/* Supports both primitive values (.num) and nested unions (.child) */"
            , "/* data2 is optional second argument for binary constructors like Add Expr Expr */"
            , "typedef struct elm_union_s { int tag; union { double num; struct elm_union_s *child; const char *str; } data; struct elm_union_s *data2; } elm_union_t;"
            , ""
            , "/* Helper to allocate a nested union on the heap */"
            , "static elm_union_t *elm_alloc_union(elm_union_t val) {"
            , "    elm_union_t *p = (elm_union_t*)malloc(sizeof(elm_union_t));"
            , "    *p = val;"
            , "    return p;"
            , "}"
            , ""
            , "/* Built-in tuple types - use flexible data union for elements */"
            , "typedef union { double d; void *ptr; struct elm_list_s *lst; const char *str; } elm_elem_t;"
            , "typedef struct { elm_elem_t _0; elm_elem_t _1; } elm_tuple2_t;"
            , "typedef struct { elm_elem_t _0; elm_elem_t _1; elm_elem_t _2; } elm_tuple3_t;"
            , ""
            , "/* Built-in Maybe type tags */"
            , "#define TAG_Nothing 0"
            , "#define TAG_Just 1"
            , ""
            , "/* Built-in Result type tags */"
            , "#define TAG_Err 0"
            , "#define TAG_Ok 1"
            , ""
            , "/* Built-in Order type tags */"
            , "#define TAG_LT 0"
            , "#define TAG_EQ 1"
            , "#define TAG_GT 2"
            , ""
            , "/* Src module AST type tags (for self-hosting) */"
            , "#define TAG_Src_Str 0"
            , "#define TAG_Src_Int 1"
            , "#define TAG_Src_Float 2"
            , "#define TAG_Src_Chr 3"
            , "#define TAG_Src_Var 4"
            , "#define TAG_Src_VarQual 5"
            , "#define TAG_Src_List 6"
            , "#define TAG_Src_Tuple 7"
            , "#define TAG_Src_Record 8"
            , "#define TAG_Src_Update 9"
            , "#define TAG_Src_Access 10"
            , "#define TAG_Src_Accessor 11"
            , "#define TAG_Src_If 12"
            , "#define TAG_Src_Let 13"
            , "#define TAG_Src_Case 14"
            , "#define TAG_Src_Lambda 15"
            , "#define TAG_Src_Binops 16"
            , "#define TAG_Src_Call 17"
            , "#define TAG_Src_Negate 18"
            , "#define TAG_Src_At 19"
            , "#define TAG_Src_PVar 0"
            , "#define TAG_Src_PAnything 1"
            , "#define TAG_Src_PInt 2"
            , "#define TAG_Src_PStr 3"
            , "#define TAG_Src_PChr 4"
            , "#define TAG_Src_PList 5"
            , "#define TAG_Src_PCons 6"
            , "#define TAG_Src_PTuple 7"
            , "#define TAG_Src_PCtor 8"
            , "#define TAG_Src_PCtorQual 9"
            , "#define TAG_Src_PRecord 10"
            , "#define TAG_Src_Define 0"
            , "#define TAG_Src_Destruct 1"
            , "#define TAG_Src_LowVar 0"
            , "#define TAG_Src_CapVar 1"
            , "#define TAG_Src_Unit 20"
            , "#define TAG_Src_Op 21"
            , "#define TAG_Src_PUnit 11"
            , "#define TAG_Src_PAlias 12"
            , ""
            , "/* Built-in List type - fixed-size array with flexible element storage */"
            , "#define ELM_LIST_MAX 64"
            , "typedef union { double d; elm_tuple2_t t2; elm_tuple3_t t3; void *ptr; struct elm_list_s *lst; elm_union_t u; const char *str; } elm_data_t;"
            , "typedef struct elm_list_s { int length; elm_data_t data[ELM_LIST_MAX]; } elm_list_t;"
            , ""
            , "/* String.replace - replace all occurrences */"
            , "static char __elm_replace_buf[4096];"
            , "static const char *elm_str_replace(const char *target, const char *replacement, const char *src) {"
            , "    int tlen = 0, rlen = 0, slen = 0;"
            , "    while (target[tlen]) tlen++;"
            , "    while (replacement[rlen]) rlen++;"
            , "    while (src[slen]) slen++;"
            , "    if (tlen == 0) { for (int i = 0; i <= slen && i < 4095; i++) __elm_replace_buf[i] = src[i]; return __elm_replace_buf; }"
            , "    int j = 0;"
            , "    for (int i = 0; src[i] && j < 4094; ) {"
            , "        int match = 1;"
            , "        for (int k = 0; k < tlen && match; k++) if (src[i + k] != target[k]) match = 0;"
            , "        if (match) { for (int k = 0; k < rlen && j < 4094; k++) __elm_replace_buf[j++] = replacement[k]; i += tlen; }"
            , "        else { __elm_replace_buf[j++] = src[i++]; }"
            , "    }"
            , "    __elm_replace_buf[j] = 0;"
            , "    return __elm_replace_buf;"
            , "}"
            , ""
            , "/* Rate Monotonic Scheduler (RMS) stubs for TCC testing */"
            , "typedef enum { RMS_ON_TIME = 0, RMS_MISSED = 1, RMS_NOT_STARTED = 2 } rms_deadline_status_t;"
            , "typedef struct { unsigned int count, missed_count, min_cpu_time_us, max_cpu_time_us, avg_cpu_time_us, period_ms; rms_deadline_status_t last_status; } rms_stats_t;"
            , "typedef struct { unsigned int period_id, period_ms, period_ticks; int started; rms_deadline_status_t last_status; unsigned int local_missed_count; } rms_period_t;"
            , "static rms_period_t *rms_create(unsigned int period_ms) {"
            , "    rms_period_t *p = (rms_period_t *)malloc(sizeof(rms_period_t));"
            , "    if (!p) return 0;"
            , "    p->period_id = 1; p->period_ms = period_ms; p->period_ticks = period_ms;"
            , "    p->started = 0; p->last_status = RMS_NOT_STARTED; p->local_missed_count = 0;"
            , "    return p;"
            , "}"
            , "static rms_deadline_status_t rms_wait_period(rms_period_t *p) {"
            , "    if (!p) return RMS_NOT_STARTED;"
            , "    if (!p->started) { p->started = 1; return RMS_NOT_STARTED; }"
            , "    p->last_status = RMS_ON_TIME; return RMS_ON_TIME; /* Stub: always on time */"
            , "}"
            , "static int rms_get_stats(rms_period_t *p, rms_stats_t *s) {"
            , "    if (!p || !s) return -1; memset(s, 0, sizeof(*s));"
            , "    s->missed_count = p->local_missed_count; s->period_ms = p->period_ms; s->last_status = p->last_status;"
            , "    return 0;"
            , "}"
            , "static unsigned int rms_get_missed_count(rms_period_t *p) { return p ? p->local_missed_count : 0; }"
            , "static void rms_delete(rms_period_t *p) { if (p) free(p); }"
            , "static int rms_assign_priority(unsigned int period_ms) { (void)period_ms; return 0; }"
            , "static unsigned int rms_global_missed_count = 0;"
            , "static unsigned int rms_global_get_missed(void) { return rms_global_missed_count; }"
            , "static void rms_global_reset_missed(void) { rms_global_missed_count = 0; }"
            , ""
            , "/* Record types for AST and internal structures */"
            , "typedef struct { double name; elm_list_t values; elm_list_t unions; } elm_module_t;"
            , "typedef struct { double name; elm_list_t args; double body; } elm_value_t;"
            , "typedef struct { double name; elm_list_t ctors; } elm_src_union_t;"
            , "typedef struct { const char *prefix; const char *name; elm_list_t args; double body; } elm_local_func_t;"
            , "typedef struct { const char *target; } elm_flags_t;"
            , "typedef struct { const char *target; } elm_model_t;"
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
            ++ [ constructorDefinesCode ++ forwardDeclsCode ++ moduleConstantsCode ++ liftedFunctionsCode ++ userFunctionsCode ++ "/* Elm main value */" ]
            ++ mainImpl
        )


{-| Generate forward declaration for a standalone function
-}
generateStandaloneForwardDecl : Src.Located Src.Value -> String
generateStandaloneForwardDecl (Src.At _ value) =
    let
        (Src.At _ name) = value.name
        -- Generate the implementation first to infer types
        implCode = generateUserFunction name value.args value.body
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


{-| Type representing the main value's type and code
-}
type MainValue
    = MainString String
    | MainInt Int
    | MainExpr String String  -- (C type, C expression)


{-| Extract the main value from a module
-}
ctorListHasData : List ( Src.Located String, List Src.Type ) -> Bool
ctorListHasData ctors =
    case ctors of
        [] ->
            False

        ( _, args ) :: rest ->
            if List.isEmpty args then
                ctorListHasData rest

            else
                True


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
        Just (exprToMainValueWithType value.type_ value.body)

    else
        Nothing


{-| Check if a type annotation represents String type
-}
mainTypeIsString : Maybe Src.Type -> Bool
mainTypeIsString maybeType =
    case maybeType of
        Just (Src.At _ (Src.TType _ "String" [])) ->
            True

        _ ->
            False


{-| Convert an expression to a MainValue, using type annotation if available
-}
exprToMainValueWithType : Maybe Src.Type -> Src.Expr -> MainValue
exprToMainValueWithType maybeType expr =
    let
        baseValue =
            exprToMainValue expr
    in
    -- If type annotation says String, override int inference
    if mainTypeIsString maybeType then
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
                inferredType =
                    if String.contains "elm_str_" cCode
                        || String.contains "elm_from_int" cCode
                        || String.contains "elm_from_float" cCode
                        || String.startsWith "\"" cCode then
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


{-| Generate comma-separated argument string from list of expressions.
    This is used instead of List.map/String.join to avoid generating
    elm_List_map calls during self-hosting.
-}
generateArgsString : List Src.Expr -> String
generateArgsString exprs =
    case exprs of
        [] ->
            ""

        [ single ] ->
            generateStandaloneExpr single

        first :: rest ->
            generateStandaloneExpr first ++ ", " ++ generateArgsString rest


{-| Generate binary operations with function context (for nested Let handling)
-}
generateStandaloneBinopsWithCtx : ExprCtx -> List ( Src.Expr, Src.Located String ) -> Src.Expr -> String
generateStandaloneBinopsWithCtx ctx pairs finalExpr =
    -- For now, delegate to non-context version
    -- The context is mainly needed for Let handling, which is handled separately
    generateStandaloneBinops pairs finalExpr


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
                        generateStandaloneExpr expr

                    [] ->
                        generateStandaloneExpr finalExpr

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
                                -- Regular function call
                                buildPipe (generateStandaloneExpr first ++ "(" ++ arg ++ ")") rest
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

                    first :: rest ->
                        -- Check for accessor pattern using helper
                        case extractAccessor first of
                            Just fieldName ->
                                -- Accessor: innerArg.field
                                buildBackPipe (innerArg ++ "." ++ fieldName) rest

                            Nothing ->
                                -- Regular function call
                                buildBackPipe (generateStandaloneExpr first ++ "(" ++ innerArg ++ ")") rest
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
                                generateStandaloneExpr headExpr
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
                        ( generateStandaloneExpr expr, op ) :: buildTerms rest

            terms = buildTerms pairs
            finalTerm = generateStandaloneExpr finalExpr

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
                    "(strcmp(" ++ generateStandaloneExpr leftExpr ++ ", " ++ finalTerm ++ ") == 0)"

                [ ( leftExpr, Src.At _ "/=" ) ] ->
                    "(strcmp(" ++ generateStandaloneExpr leftExpr ++ ", " ++ finalTerm ++ ") != 0)"

                _ ->
                    "(" ++ buildExpr terms ++ ")"

        else
            "(" ++ buildExpr terms ++ ")"


{-| Helper to determine if an expression generates a union value
-}
isUnionValue : String -> Bool
isUnionValue exprStr =
    String.startsWith "((elm_union_t)" exprStr
        || String.startsWith "elm_alloc_union" exprStr
        || String.contains "elm_union_t" exprStr


{-| Check if an expression generates a string value
-}
isStringValue : String -> Bool
isStringValue exprStr =
    String.startsWith "\"" exprStr
        || String.startsWith "elm_str_" exprStr
        || String.startsWith "elm_from_int" exprStr
        || String.startsWith "elm_from_float" exprStr
        || String.contains "elm_str_append" exprStr


{-| Wrap a value for use in elm_union_t constructor
    - Union values use .child with elm_alloc_union
    - String values use .str
    - Primitive values use .num
-}
wrapUnionData : String -> String
wrapUnionData valueStr =
    if isUnionValue valueStr then
        "{.child = elm_alloc_union(" ++ valueStr ++ ")}"
    else if isStringValue valueStr then
        "{.str = " ++ valueStr ++ "}"
    else
        "{.num = " ++ valueStr ++ "}"


{-| Generate a union constructor expression
-}
makeUnionCtor : String -> String -> String
makeUnionCtor tag dataValue =
    if dataValue == "0" || dataValue == "" then
        "((elm_union_t){" ++ tag ++ ", {.num = 0}})"
    else
        "((elm_union_t){" ++ tag ++ ", " ++ wrapUnionData dataValue ++ "})"


{-| Context for expression generation, tracks enclosing function name
-}
type alias ExprCtx =
    { funcPrefix : String  -- The enclosing function name for lifted local functions
    }


{-| Default context for top-level or main expressions
-}
defaultExprCtx : ExprCtx
defaultExprCtx =
    { funcPrefix = "main" }


{-| Generate standalone C code for a single expression (no runtime)
-}
generateStandaloneExpr : Src.Expr -> String
generateStandaloneExpr expr =
    generateStandaloneExprWithCtx defaultExprCtx expr


{-| Generate standalone C code with function context for lifted local functions
-}
generateStandaloneExprWithCtx : ExprCtx -> Src.Expr -> String
generateStandaloneExprWithCtx ctx (Src.At _ expr) =
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
                    makeUnionCtor "TAG_Nothing" "0"

                ( Src.CapVar, _ ) ->
                    -- Constructor - generate union tag for nullary constructor
                    makeUnionCtor ("TAG_" ++ name) "0"

                _ ->
                    "elm_" ++ name

        Src.VarQual varType moduleName name ->
            case varType of
                Src.CapVar ->
                    -- Qualified constructor - generate tag value
                    -- For nullary constructors like Src.LowVar, Src.CapVar
                    makeUnionCtor ("TAG_" ++ moduleName ++ "_" ++ name) "0"

                Src.LowVar ->
                    -- Qualified variable - reference as function/value
                    "elm_" ++ moduleName ++ "_" ++ name

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
                        |> List.map
                            (\( Src.At _ fieldName, fieldValue ) ->
                                inferFieldType fieldValue ++ " " ++ fieldName
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

        Src.List elements ->
            -- Generate list literal as elm_list_t
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

                wrapDataElement elemExpr =
                    let
                        genExpr = generateStandaloneExpr elemExpr
                        isUnion = isUnionExpr elemExpr ||
                                  String.startsWith "((elm_union_t)" genExpr ||
                                  String.contains "elm_union_t" genExpr
                    in
                    if isUnion then
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
            else
                "((elm_list_t){ .length = " ++ String.fromInt numElements ++ ", .data = { " ++ values ++ " } })"

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

        Src.At _ (Src.Var _ "never") ->
            -- never x = unreachable code (halts)
            case args of
                [ _ ] ->
                    "({ while(1); 0; })"

                _ ->
                    "/* never wrong arity */ 0"

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
                                    "({ double elm_" ++ pname ++ " = __tuple_in._0; " ++ bodyStr ++ "; })"

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
                                    "({ double elm_" ++ pname ++ " = __tuple_in._1; " ++ bodyStr ++ "; })"

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
                                    "({ double elm_" ++ pname ++ " = __tuple_in._0; " ++ bodyStr ++ "; })"

                                _ ->
                                    generateStandaloneExpr fnFirst ++ "(__tuple_in._0)"

                        fnSecondAppStr =
                            case fnSecond of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    let
                                        bodyStr = generateStandaloneExpr lambdaBody
                                    in
                                    "({ double elm_" ++ pname ++ " = __tuple_in._1; " ++ bodyStr ++ "; })"

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

        Src.At _ (Src.VarQual _ "Char" "isSpace") ->
            -- Char.isSpace c = True if c is whitespace (space, tab, newline, etc.)
            case args of
                [ c ] ->
                    let cStr = generateStandaloneExpr c
                    in "(" ++ cStr ++ " == ' ' || " ++ cStr ++ " == '\\t' || " ++ cStr ++ " == '\\n' || " ++ cStr ++ " == '\\r')"

                _ ->
                    "/* Char.isSpace wrong arity */ 0"

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

        Src.At _ (Src.VarQual _ "Char" "toLocaleUpper") ->
            -- Char.toLocaleUpper c = uppercase (same as toUpper for ASCII)
            case args of
                [ c ] ->
                    let cStr = generateStandaloneExpr c
                    in "((" ++ cStr ++ " >= 'a' && " ++ cStr ++ " <= 'z') ? " ++ cStr ++ " - 32 : " ++ cStr ++ ")"

                _ ->
                    "/* Char.toLocaleUpper wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Char" "toLocaleLower") ->
            -- Char.toLocaleLower c = lowercase (same as toLower for ASCII)
            case args of
                [ c ] ->
                    let cStr = generateStandaloneExpr c
                    in "((" ++ cStr ++ " >= 'A' && " ++ cStr ++ " <= 'Z') ? " ++ cStr ++ " + 32 : " ++ cStr ++ ")"

                _ ->
                    "/* Char.toLocaleLower wrong arity */ 0"

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

        Src.At _ (Src.VarQual _ "String" "fromInt") ->
            -- String.fromInt n = string representation
            case args of
                [ n ] ->
                    "elm_from_int(" ++ generateStandaloneExpr n ++ ")"

                _ ->
                    "/* String.fromInt wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "toFloat") ->
            -- String.toFloat s = Maybe Float (returns integer part)
            case args of
                [ s ] ->
                    "elm_str_to_float(" ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.toFloat wrong arity */ 0"

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

        Src.At _ (Src.VarQual _ "String" "trimLeft") ->
            -- String.trimLeft s = remove leading whitespace
            case args of
                [ s ] ->
                    "elm_str_trim_left(" ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.trimLeft wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "trimRight") ->
            -- String.trimRight s = remove trailing whitespace
            case args of
                [ s ] ->
                    "elm_str_trim_right(" ++ generateStandaloneExpr s ++ ")"

                _ ->
                    "/* String.trimRight wrong arity */ 0"

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
                                    "({ double elm_" ++ pname ++ " = __str[__i]; " ++ bodyStr ++ "; })"

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
                                    "({ double elm_" ++ pname ++ " = __str[__i]; " ++ bodyStr ++ "; })"

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
                                    "({ double elm_" ++ charName ++ " = __str[__i]; double elm_" ++ accName ++ " = __acc; " ++ bodyStr ++ "; })"

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
                                    "({ double elm_" ++ charName ++ " = __str[__i]; double elm_" ++ accName ++ " = __acc; " ++ bodyStr ++ "; })"

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
                                    "({ double elm_" ++ pname ++ " = __str[__i]; " ++ bodyStr ++ "; })"

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
                                    "({ double elm_" ++ pname ++ " = __str[__i]; " ++ bodyStr ++ "; })"

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

        Src.At _ (Src.VarQual _ "String" "replace") ->
            -- String.replace target replacement str
            case args of
                [ target, replacement, str ] ->
                    "elm_str_replace(" ++ generateStandaloneExpr target ++ ", " ++ generateStandaloneExpr replacement ++ ", " ++ generateStandaloneExpr str ++ ")"

                _ ->
                    "/* String.replace wrong arity */ 0"

        Src.At _ (Src.VarQual _ "String" "join") ->
            -- String.join sep list = join strings with separator
            case args of
                [ sepExpr, listExpr ] ->
                    let
                        sepStr =
                            generateStandaloneExpr sepExpr

                        listStr =
                            generateStandaloneExpr listExpr
                    in
                    "({ static char __join_buf[1024]; elm_list_t __lst = " ++ listStr ++ "; const char *__sep = " ++ sepStr ++ "; int __pos = 0; for (int __i = 0; __i < __lst.length && __pos < 1023; __i++) { if (__i > 0) { int __seplen = 0; while (__sep[__seplen]) __seplen++; for (int __j = 0; __j < __seplen && __pos < 1023; __j++) __join_buf[__pos++] = __sep[__j]; } const char *__s = (const char *)(long)__lst.data[__i]; while (*__s && __pos < 1023) __join_buf[__pos++] = *__s++; } __join_buf[__pos] = 0; __join_buf; })"

                _ ->
                    "/* String.join wrong arity */ 0"

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
            -- sqrt x = square root of x (uses C standard library sqrt)
            case args of
                [ x ] ->
                    "sqrt(" ++ generateStandaloneExpr x ++ ")"

                _ ->
                    "/* sqrt wrong arity */ 0"

        Src.At _ (Src.Var _ "logBase") ->
            -- logBase base x = log of x to given base (integer approximation)
            case args of
                [ base, x ] ->
                    let
                        baseStr = generateStandaloneExpr base
                        xStr = generateStandaloneExpr x
                    in
                    "({ int __b = " ++ baseStr ++ ", __x = " ++ xStr ++ ", __r = 0; if (__b > 1 && __x > 0) { while (__x >= __b) { __x /= __b; __r++; } } __r; })"

                _ ->
                    "/* logBase wrong arity */ 0"

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
                                            "({ double elm_" ++ varName ++ " = __maybe_val.data; " ++ generateStandaloneExpr body ++ "; })"

                                        -- Constructor pattern: \(Src.At _ n) -> n
                                        [ Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] ->
                                            "({ elm_union_t __inner = __maybe_val.data; double elm_" ++ innerName ++ " = __inner.data; " ++ generateStandaloneExpr body ++ "; })"

                                        -- Qualified constructor pattern: \(Src.At _ n) -> n
                                        [ Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] ->
                                            "({ elm_union_t __inner = __maybe_val.data; double elm_" ++ innerName ++ " = __inner.data; " ++ generateStandaloneExpr body ++ "; })"

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
                                            "({ double elm_" ++ varName ++ " = __maybe_val.data; " ++ generateStandaloneExpr body ++ "; })"

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
                                            "({ double elm_" ++ pname1 ++ " = __maybe_a.data; double elm_" ++ pname2 ++ " = __maybe_b.data; " ++ bodyStr ++ "; })"

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
                                            "({ double elm_" ++ pname1 ++ " = __maybe_a.data; double elm_" ++ pname2 ++ " = __maybe_b.data; double elm_" ++ pname3 ++ " = __maybe_c.data; " ++ bodyStr ++ "; })"

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
                                            "({ double elm_" ++ pname1 ++ " = __maybe_a.data; double elm_" ++ pname2 ++ " = __maybe_b.data; double elm_" ++ pname3 ++ " = __maybe_c.data; double elm_" ++ pname4 ++ " = __maybe_d.data; " ++ bodyStr ++ "; })"

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
                                            "({ double elm_" ++ p1 ++ " = __maybe_a.data; double elm_" ++ p2 ++ " = __maybe_b.data; double elm_" ++ p3 ++ " = __maybe_c.data; double elm_" ++ p4 ++ " = __maybe_d.data; double elm_" ++ p5 ++ " = __maybe_e.data; " ++ bodyStr ++ "; })"

                                        _ ->
                                            "/* unsupported lambda pattern in Maybe.map5 */ 0"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__maybe_a.data, __maybe_b.data, __maybe_c.data, __maybe_d.data, __maybe_e.data)"
                    in
                    "({ elm_union_t __maybe_a = " ++ maybeAStr ++ "; elm_union_t __maybe_b = " ++ maybeBStr ++ "; elm_union_t __maybe_c = " ++ maybeCStr ++ "; elm_union_t __maybe_d = " ++ maybeDStr ++ "; elm_union_t __maybe_e = " ++ maybeEStr ++ "; (__maybe_a.tag == TAG_Just && __maybe_b.tag == TAG_Just && __maybe_c.tag == TAG_Just && __maybe_d.tag == TAG_Just && __maybe_e.tag == TAG_Just) ? ((elm_union_t){TAG_Just, " ++ fnAppStr ++ "}) : ((elm_union_t){TAG_Nothing, 0}); })"

                _ ->
                    "/* Maybe.map5 wrong arity */ 0"

        Src.At _ (Src.Var _ "Ok") ->
            -- Ok constructor
            case args of
                [ value ] ->
                    "((elm_union_t){TAG_Ok, " ++ generateStandaloneExpr value ++ "})"

                _ ->
                    "/* Ok wrong arity */ 0"

        Src.At _ (Src.Var _ "Err") ->
            -- Err constructor
            case args of
                [ value ] ->
                    "((elm_union_t){TAG_Err, " ++ generateStandaloneExpr value ++ "})"

                _ ->
                    "/* Err wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Result" "withDefault") ->
            -- Result.withDefault def result
            case args of
                [ def, result ] ->
                    let
                        defStr = generateStandaloneExpr def
                        resultStr = generateStandaloneExpr result
                    in
                    "((" ++ resultStr ++ ").tag == TAG_Ok ? (" ++ resultStr ++ ").data : " ++ defStr ++ ")"

                _ ->
                    "/* Result.withDefault wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Result" "map") ->
            -- Result.map fn result
            case args of
                [ fnExpr, resultExpr ] ->
                    let
                        resultStr = generateStandaloneExpr resultExpr
                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    "({ double elm_" ++ pname ++ " = __result_val.data; " ++ generateStandaloneExpr lambdaBody ++ "; })"
                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__result_val.data)"
                    in
                    "({ elm_union_t __result_val = " ++ resultStr ++ "; __result_val.tag == TAG_Ok ? ((elm_union_t){TAG_Ok, " ++ fnAppStr ++ "}) : __result_val; })"

                _ ->
                    "/* Result.map wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Result" "mapError") ->
            -- Result.mapError fn result
            case args of
                [ fnExpr, resultExpr ] ->
                    let
                        resultStr = generateStandaloneExpr resultExpr
                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    "({ double elm_" ++ pname ++ " = __result_val.data; " ++ generateStandaloneExpr lambdaBody ++ "; })"
                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__result_val.data)"
                    in
                    "({ elm_union_t __result_val = " ++ resultStr ++ "; __result_val.tag == TAG_Err ? ((elm_union_t){TAG_Err, " ++ fnAppStr ++ "}) : __result_val; })"

                _ ->
                    "/* Result.mapError wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Result" "toMaybe") ->
            -- Result.toMaybe result
            case args of
                [ resultExpr ] ->
                    let
                        resultStr = generateStandaloneExpr resultExpr
                    in
                    "((" ++ resultStr ++ ").tag == TAG_Ok ? ((elm_union_t){TAG_Just, (" ++ resultStr ++ ").data}) : ((elm_union_t){TAG_Nothing, 0}))"

                _ ->
                    "/* Result.toMaybe wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Result" "andThen") ->
            -- Result.andThen fn result
            case args of
                [ fnExpr, resultExpr ] ->
                    let
                        resultStr = generateStandaloneExpr resultExpr
                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    "({ double elm_" ++ pname ++ " = __result_val.data; " ++ generateStandaloneExpr lambdaBody ++ "; })"
                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__result_val.data)"
                    in
                    "({ elm_union_t __result_val = " ++ resultStr ++ "; __result_val.tag == TAG_Ok ? " ++ fnAppStr ++ " : __result_val; })"

                _ ->
                    "/* Result.andThen wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Result" "fromMaybe") ->
            -- Result.fromMaybe err maybe
            case args of
                [ errExpr, maybeExpr ] ->
                    let
                        errStr = generateStandaloneExpr errExpr
                        maybeStr = generateStandaloneExpr maybeExpr
                    in
                    "((" ++ maybeStr ++ ").tag == TAG_Just ? ((elm_union_t){TAG_Ok, (" ++ maybeStr ++ ").data}) : ((elm_union_t){TAG_Err, " ++ errStr ++ "}))"

                _ ->
                    "/* Result.fromMaybe wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Debug" "log") ->
            -- Debug.log tag value = prints tag:value and returns value
            -- On embedded target, just return the value
            case args of
                [ _, value ] ->
                    generateStandaloneExpr value

                _ ->
                    "/* Debug.log wrong arity */ 0"

        Src.At _ (Src.VarQual _ "Debug" "todo") ->
            -- Debug.todo msg = halt execution (for unimplemented code)
            case args of
                [ _ ] ->
                    "({ while(1); 0; })"

                _ ->
                    "/* Debug.todo wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "length") ->
            -- List.length list = number of elements
            case args of
                [ listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr
                        -- Use direct .length for known list types:
                        -- - List literals: ((elm_list_t){...})
                        -- - Case scrutinee: elm_case_scrutinee
                        -- - Known list parameter names
                        -- - Compound expressions with parens
                        isKnownListType =
                            String.contains "elm_list_t" listStr
                                || String.startsWith "elm_case_scrutinee" listStr
                                || String.startsWith "elm_exprs" listStr
                                || String.startsWith "elm_list" listStr
                                || String.startsWith "elm_items" listStr
                                || String.startsWith "elm_elements" listStr
                                || String.startsWith "elm_ts" listStr
                                || String.startsWith "elm_ps" listStr
                                || String.startsWith "__lst" listStr
                                || String.contains "(" listStr  -- Compound expressions
                        -- Only use lst->length for specific tuple extraction patterns
                        -- These are local variables from ._0 or ._1 extraction
                        isTupleExtractedVar =
                            String.startsWith "elm_args" listStr
                                || String.startsWith "elm_ctors" listStr
                    in
                    if isKnownListType then
                        "(" ++ listStr ++ ").length"
                    else if isTupleExtractedVar then
                        -- Variable from tuple extraction, use lst pointer
                        "(" ++ listStr ++ ").lst->length"
                    else
                        -- Default to direct length access
                        "(" ++ listStr ++ ").length"

                _ ->
                    "/* List.length wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "isEmpty") ->
            -- List.isEmpty list = True if list is empty
            case args of
                [ listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr
                        -- Use direct .length for known list types
                        isKnownListType =
                            String.contains "elm_list_t" listStr
                                || String.startsWith "elm_case_scrutinee" listStr
                                || String.startsWith "elm_exprs" listStr
                                || String.startsWith "elm_list" listStr
                                || String.startsWith "elm_items" listStr
                                || String.startsWith "elm_elements" listStr
                                || String.startsWith "elm_ts" listStr
                                || String.startsWith "elm_ps" listStr
                                || String.startsWith "__lst" listStr
                                || String.contains "(" listStr
                        -- Only use lst->length for specific tuple extraction patterns
                        isTupleExtractedVar =
                            String.startsWith "elm_args" listStr
                                || String.startsWith "elm_ctors" listStr
                    in
                    if isKnownListType then
                        "((" ++ listStr ++ ").length == 0)"
                    else if isTupleExtractedVar then
                        "((" ++ listStr ++ ").lst->length == 0)"
                    else
                        "((" ++ listStr ++ ").length == 0)"

                _ ->
                    "/* List.isEmpty wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "head") ->
            -- List.head list = Maybe first element
            case args of
                [ listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; __lst.length > 0 ? ((elm_union_t){TAG_Just, __lst.data[0]}) : ((elm_union_t){TAG_Nothing, 0}); })"

                _ ->
                    "/* List.head wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "tail") ->
            -- List.tail list = Maybe rest of list after first element
            case args of
                [ listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; elm_union_t __result; if (__lst.length == 0) { __result = (elm_union_t){TAG_Nothing, 0}; } else { __result = (elm_union_t){TAG_Just, __lst.length - 1}; } __result; })"

                _ ->
                    "/* List.tail wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "last") ->
            -- List.last list = Maybe last element
            case args of
                [ listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; __lst.length > 0 ? ((elm_union_t){TAG_Just, __lst.data[__lst.length - 1]}) : ((elm_union_t){TAG_Nothing, 0}); })"

                _ ->
                    "/* List.last wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "sum") ->
            -- List.sum list = sum of all elements
            case args of
                [ listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; int __sum = 0; for (int __i = 0; __i < __lst.length; __i++) __sum += __lst.data[__i]; __sum; })"

                _ ->
                    "/* List.sum wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "product") ->
            -- List.product list = product of all elements
            case args of
                [ listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; int __prod = 1; for (int __i = 0; __i < __lst.length; __i++) __prod *= __lst.data[__i]; __prod; })"

                _ ->
                    "/* List.product wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "maximum") ->
            -- List.maximum list = Maybe maximum element
            case args of
                [ listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; elm_union_t __result; if (__lst.length == 0) { __result = (elm_union_t){TAG_Nothing, 0}; } else { int __max = __lst.data[0]; for (int __i = 1; __i < __lst.length; __i++) if (__lst.data[__i] > __max) __max = __lst.data[__i]; __result = (elm_union_t){TAG_Just, __max}; } __result; })"

                _ ->
                    "/* List.maximum wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "minimum") ->
            -- List.minimum list = Maybe minimum element
            case args of
                [ listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; elm_union_t __result; if (__lst.length == 0) { __result = (elm_union_t){TAG_Nothing, 0}; } else { int __min = __lst.data[0]; for (int __i = 1; __i < __lst.length; __i++) if (__lst.data[__i] < __min) __min = __lst.data[__i]; __result = (elm_union_t){TAG_Just, __min}; } __result; })"

                _ ->
                    "/* List.minimum wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "reverse") ->
            -- List.reverse list = reversed list
            case args of
                [ listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; elm_list_t __rev; __rev.length = __lst.length; for (int __i = 0; __i < __lst.length; __i++) __rev.data[__i] = __lst.data[__lst.length - 1 - __i]; __rev; })"

                _ ->
                    "/* List.reverse wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "member") ->
            -- List.member elem list = True if elem is in list
            case args of
                [ elemExpr, listExpr ] ->
                    let
                        elemStr = generateStandaloneExpr elemExpr
                        listStr = generateStandaloneExpr listExpr
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; int __elem = " ++ elemStr ++ "; int __found = 0; for (int __i = 0; __i < __lst.length && !__found; __i++) if (__lst.data[__i] == __elem) __found = 1; __found; })"

                _ ->
                    "/* List.member wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "range") ->
            -- List.range lo hi = list from lo to hi inclusive
            case args of
                [ loExpr, hiExpr ] ->
                    let
                        loStr = generateStandaloneExpr loExpr
                        hiStr = generateStandaloneExpr hiExpr
                    in
                    "({ int __lo = " ++ loStr ++ ", __hi = " ++ hiStr ++ "; elm_list_t __lst; __lst.length = __hi >= __lo ? __hi - __lo + 1 : 0; if (__lst.length > ELM_LIST_MAX) __lst.length = ELM_LIST_MAX; for (int __i = 0; __i < __lst.length; __i++) __lst.data[__i] = __lo + __i; __lst; })"

                _ ->
                    "/* List.range wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "take") ->
            -- List.take n list = first n elements
            case args of
                [ nExpr, listExpr ] ->
                    let
                        nStr = generateStandaloneExpr nExpr
                        listStr = generateStandaloneExpr listExpr
                    in
                    "({ int __n = " ++ nStr ++ "; elm_list_t __lst = " ++ listStr ++ "; elm_list_t __result; __result.length = __n < __lst.length ? __n : __lst.length; if (__result.length < 0) __result.length = 0; for (int __i = 0; __i < __result.length; __i++) __result.data[__i] = __lst.data[__i]; __result; })"

                _ ->
                    "/* List.take wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "drop") ->
            -- List.drop n list = drop first n elements
            case args of
                [ nExpr, listExpr ] ->
                    let
                        nStr = generateStandaloneExpr nExpr
                        listStr = generateStandaloneExpr listExpr
                    in
                    "({ int __n = " ++ nStr ++ "; elm_list_t __lst = " ++ listStr ++ "; elm_list_t __result; int __start = __n < __lst.length ? __n : __lst.length; if (__start < 0) __start = 0; __result.length = __lst.length - __start; for (int __i = 0; __i < __result.length; __i++) __result.data[__i] = __lst.data[__start + __i]; __result; })"

                _ ->
                    "/* List.drop wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "append") ->
            -- List.append listA listB = concatenate two lists
            case args of
                [ listAExpr, listBExpr ] ->
                    let
                        listAStr = generateStandaloneExpr listAExpr
                        listBStr = generateStandaloneExpr listBExpr
                    in
                    "({ elm_list_t __a = " ++ listAStr ++ ", __b = " ++ listBStr ++ "; elm_list_t __result; __result.length = __a.length + __b.length; if (__result.length > ELM_LIST_MAX) __result.length = ELM_LIST_MAX; int __i; for (__i = 0; __i < __a.length && __i < __result.length; __i++) __result.data[__i] = __a.data[__i]; for (int __j = 0; __i < __result.length; __i++, __j++) __result.data[__i] = __b.data[__j]; __result; })"

                _ ->
                    "/* List.append wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "singleton") ->
            -- List.singleton x = single-element list
            case args of
                [ elemExpr ] ->
                    let
                        elemStr = generateStandaloneExpr elemExpr
                        isUnionExpr =
                            case elemExpr of
                                Src.At _ (Src.Var Src.CapVar _) -> True
                                Src.At _ (Src.VarQual Src.CapVar _ _) -> True
                                Src.At _ (Src.Call (Src.At _ (Src.Var Src.CapVar _)) _) -> True
                                Src.At _ (Src.Call (Src.At _ (Src.VarQual Src.CapVar _ _)) _) -> True
                                Src.At _ (Src.Var Src.LowVar name) ->
                                    String.contains "Expr" name ||
                                    String.contains "expr" name ||
                                    String.contains "Pat" name ||
                                    String.contains "pat" name ||
                                    String.contains "Type" name
                                _ -> False
                        -- Check if this is an elm_union_t value
                        isUnion = isUnionExpr ||
                                  String.startsWith "((elm_union_t)" elemStr ||
                                  String.contains "elm_union_t" elemStr
                        wrapElem =
                            if isUnion then
                                "{.u = " ++ elemStr ++ "}"
                            else if String.startsWith "\"" elemStr then
                                "{.str = " ++ elemStr ++ "}"
                            else
                                elemStr
                    in
                    "((elm_list_t){ .length = 1, .data = { " ++ wrapElem ++ " } })"

                _ ->
                    "/* List.singleton wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "repeat") ->
            -- List.repeat n elem = list with n copies of elem
            case args of
                [ nExpr, elemExpr ] ->
                    let
                        nStr = generateStandaloneExpr nExpr
                        elemStr = generateStandaloneExpr elemExpr
                    in
                    "({ int __n = " ++ nStr ++ ", __elem = " ++ elemStr ++ "; elm_list_t __lst; __lst.length = __n > 0 ? (__n > ELM_LIST_MAX ? ELM_LIST_MAX : __n) : 0; for (int __i = 0; __i < __lst.length; __i++) __lst.data[__i] = __elem; __lst; })"

                _ ->
                    "/* List.repeat wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "map") ->
            -- List.map f list = apply f to each element
            case args of
                [ fnExpr, listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr

                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    "({ double elm_" ++ pname ++ " = __lst.data[__i]; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Lambda with Located pattern: \(Src.At _ x) -> ...
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                                    "({ double elm_" ++ innerName ++ " = ((elm_union_t)__lst.data[__i]).data; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Lambda with qualified Located pattern: \(Src.At _ x) -> ...
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                                    "({ double elm_" ++ innerName ++ " = ((elm_union_t)__lst.data[__i]).data; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Lambda with tuple pattern: \(a, b) -> ...
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PTuple (Src.At _ (Src.PVar pname1)) (Src.At _ (Src.PVar pname2)) []) ] lambdaBody) ->
                                    "({ elm_tuple2_t __elem = __lst.data[__i]; double elm_" ++ pname1 ++ " = __elem._0; double elm_" ++ pname2 ++ " = __elem._1; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__lst.data[__i])"
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; elm_list_t __result; __result.length = __lst.length; for (int __i = 0; __i < __lst.length; __i++) __result.data[__i] = " ++ fnAppStr ++ "; __result; })"

                _ ->
                    "/* List.map wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "map2") ->
            -- List.map2 f listA listB = apply f to pairs of elements
            case args of
                [ fnExpr, listAExpr, listBExpr ] ->
                    let
                        listAStr = generateStandaloneExpr listAExpr
                        listBStr = generateStandaloneExpr listBExpr

                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PVar pname2) ] lambdaBody) ->
                                    "({ double elm_" ++ pname1 ++ " = __lstA.data[__i], elm_" ++ pname2 ++ " = __lstB.data[__i]; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__lstA.data[__i], __lstB.data[__i])"
                    in
                    "({ elm_list_t __lstA = " ++ listAStr ++ ", __lstB = " ++ listBStr ++ "; elm_list_t __result; __result.length = __lstA.length < __lstB.length ? __lstA.length : __lstB.length; for (int __i = 0; __i < __result.length; __i++) __result.data[__i] = " ++ fnAppStr ++ "; __result; })"

                _ ->
                    "/* List.map2 wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "map3") ->
            -- List.map3 f listA listB listC = apply f to triples of elements
            case args of
                [ fnExpr, listAExpr, listBExpr, listCExpr ] ->
                    let
                        listAStr = generateStandaloneExpr listAExpr
                        listBStr = generateStandaloneExpr listBExpr
                        listCStr = generateStandaloneExpr listCExpr

                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PVar pname2), Src.At _ (Src.PVar pname3) ] lambdaBody) ->
                                    "({ double elm_" ++ pname1 ++ " = __lstA.data[__i], elm_" ++ pname2 ++ " = __lstB.data[__i], elm_" ++ pname3 ++ " = __lstC.data[__i]; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__lstA.data[__i], __lstB.data[__i], __lstC.data[__i])"
                    in
                    "({ elm_list_t __lstA = " ++ listAStr ++ ", __lstB = " ++ listBStr ++ ", __lstC = " ++ listCStr ++ "; elm_list_t __result; int __minLen = __lstA.length; if (__lstB.length < __minLen) __minLen = __lstB.length; if (__lstC.length < __minLen) __minLen = __lstC.length; __result.length = __minLen; for (int __i = 0; __i < __result.length; __i++) __result.data[__i] = " ++ fnAppStr ++ "; __result; })"

                _ ->
                    "/* List.map3 wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "filter") ->
            -- List.filter pred list = keep elements where pred returns true
            case args of
                [ fnExpr, listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr

                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    "({ double elm_" ++ pname ++ " = __lst.data[__i]; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Handle not << String.isEmpty composition
                                Src.At _ (Src.Binops [ ( Src.At _ (Src.Var _ "not"), Src.At _ "<<" ) ] (Src.At _ (Src.VarQual _ "String" "isEmpty"))) ->
                                    "(*(const char *)(long)__lst.data[__i] != '\\0')"

                                -- Handle not << f composition (negate any function)
                                Src.At _ (Src.Binops [ ( Src.At _ (Src.Var _ "not"), Src.At _ "<<" ) ] innerFn) ->
                                    "!(" ++ generateStandaloneExpr innerFn ++ "((const char *)(long)__lst.data[__i]))"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__lst.data[__i])"
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; elm_list_t __result; __result.length = 0; for (int __i = 0; __i < __lst.length; __i++) if (" ++ fnAppStr ++ ") __result.data[__result.length++] = __lst.data[__i]; __result; })"

                _ ->
                    "/* List.filter wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "foldl") ->
            -- List.foldl f init list = fold from left
            case args of
                [ fnExpr, initExpr, listExpr ] ->
                    let
                        initStr = generateStandaloneExpr initExpr
                        listStr = generateStandaloneExpr listExpr

                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PVar pname2) ] lambdaBody) ->
                                    "({ double elm_" ++ pname1 ++ " = __lst.data[__i], elm_" ++ pname2 ++ " = __acc; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__lst.data[__i], __acc)"
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; int __acc = " ++ initStr ++ "; for (int __i = 0; __i < __lst.length; __i++) __acc = " ++ fnAppStr ++ "; __acc; })"

                _ ->
                    "/* List.foldl wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "foldr") ->
            -- List.foldr f init list = fold from right
            case args of
                [ fnExpr, initExpr, listExpr ] ->
                    let
                        initStr = generateStandaloneExpr initExpr
                        listStr = generateStandaloneExpr listExpr

                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PVar pname2) ] lambdaBody) ->
                                    "({ double elm_" ++ pname1 ++ " = __lst.data[__i], elm_" ++ pname2 ++ " = __acc; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__lst.data[__i], __acc)"
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; int __acc = " ++ initStr ++ "; for (int __i = __lst.length - 1; __i >= 0; __i--) __acc = " ++ fnAppStr ++ "; __acc; })"

                _ ->
                    "/* List.foldr wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "all") ->
            -- List.all pred list = True if all elements satisfy pred
            case args of
                [ fnExpr, listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr

                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    "({ double elm_" ++ pname ++ " = __lst.data[__i].d; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Lambda with tuple pattern: \(a, b) -> ...
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PTuple (Src.At _ (Src.PVar pname1)) (Src.At _ (Src.PVar pname2)) []) ] lambdaBody) ->
                                    "({ elm_tuple2_t __elem = __lst.data[__i].t2; double elm_" ++ pname1 ++ " = __elem._0; double elm_" ++ pname2 ++ " = __elem._1; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__lst.data[__i].d)"
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; int __result = 1; for (int __i = 0; __i < __lst.length && __result; __i++) if (!(" ++ fnAppStr ++ ")) __result = 0; __result; })"

                _ ->
                    "/* List.all wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "any") ->
            -- List.any pred list = True if any element satisfies pred
            case args of
                [ fnExpr, listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr

                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    "({ double elm_" ++ pname ++ " = __lst.data[__i].d; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Lambda with tuple pattern: \(a, b) -> ...
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PTuple (Src.At _ (Src.PVar pname1)) (Src.At _ (Src.PVar pname2)) []) ] lambdaBody) ->
                                    "({ elm_tuple2_t __elem = __lst.data[__i].t2; elm_elem_t elm_" ++ pname1 ++ " = __elem._0; elm_elem_t elm_" ++ pname2 ++ " = __elem._1; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Lambda with tuple pattern with wildcard first: \(_, b) -> ...
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PTuple (Src.At _ Src.PAnything) (Src.At _ (Src.PVar pname2)) []) ] lambdaBody) ->
                                    "({ elm_tuple2_t __elem = __lst.data[__i].t2; elm_elem_t elm_" ++ pname2 ++ " = __elem._1; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Lambda with tuple pattern with wildcard second: \(a, _) -> ...
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PTuple (Src.At _ (Src.PVar pname1)) (Src.At _ Src.PAnything) []) ] lambdaBody) ->
                                    "({ elm_tuple2_t __elem = __lst.data[__i].t2; elm_elem_t elm_" ++ pname1 ++ " = __elem._0; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__lst.data[__i].d)"
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; int __result = 0; for (int __i = 0; __i < __lst.length && !__result; __i++) if (" ++ fnAppStr ++ ") __result = 1; __result; })"

                _ ->
                    "/* List.any wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "sort") ->
            -- List.sort list = sorted list (ascending, insertion sort)
            case args of
                [ listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; for (int __i = 1; __i < __lst.length; __i++) { int __key = __lst.data[__i], __j = __i - 1; while (__j >= 0 && __lst.data[__j] > __key) { __lst.data[__j + 1] = __lst.data[__j]; __j--; } __lst.data[__j + 1] = __key; } __lst; })"

                _ ->
                    "/* List.sort wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "indexedMap") ->
            -- List.indexedMap f list = apply f(index, elem) to each element
            case args of
                [ fnExpr, listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr

                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PVar pname2) ] lambdaBody) ->
                                    "({ double elm_" ++ pname1 ++ " = __i, elm_" ++ pname2 ++ " = __lst.data[__i]; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Lambda with index and Located pattern: \i (Src.At _ x) -> ...
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                                    "({ double elm_" ++ pname1 ++ " = __i; double elm_" ++ innerName ++ " = ((elm_union_t)__lst.data[__i]).data; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Lambda with index and qualified Located pattern: \i (Src.At _ x) -> ...
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                                    "({ double elm_" ++ pname1 ++ " = __i; double elm_" ++ innerName ++ " = ((elm_union_t)__lst.data[__i]).data; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Lambda with index and tuple pattern: \i (a, b) -> ...
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PTuple (Src.At _ (Src.PVar pname2)) (Src.At _ (Src.PVar pname3)) []) ] lambdaBody) ->
                                    "({ double elm_" ++ pname1 ++ " = __i; elm_tuple2_t __elem = __lst.data[__i]; double elm_" ++ pname2 ++ " = __elem._0; double elm_" ++ pname3 ++ " = __elem._1; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Lambda with index and tuple with Located first element: \i (Src.At _ name, args) -> ...
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname1), Src.At _ (Src.PTuple (Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ])) (Src.At _ (Src.PVar pname3)) []) ] lambdaBody) ->
                                    "({ double elm_" ++ pname1 ++ " = __i; elm_tuple2_t __elem = __lst.data[__i]; elm_union_t __loc = __elem._0; double elm_" ++ innerName ++ " = __loc.data; double elm_" ++ pname3 ++ " = __elem._1; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Lambda with ignored index and Located pattern: \_ (Src.At _ x) -> ...
                                Src.At _ (Src.Lambda [ Src.At _ Src.PAnything, Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                                    "({ double elm_" ++ innerName ++ " = ((elm_union_t)__lst.data[__i]).data; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Lambda with ignored index and Located pattern (non-qualified): \_ (Src.At _ x) -> ...
                                Src.At _ (Src.Lambda [ Src.At _ Src.PAnything, Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                                    "({ double elm_" ++ innerName ++ " = ((elm_union_t)__lst.data[__i]).data; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__i, __lst.data[__i])"
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; elm_list_t __result; __result.length = __lst.length; for (int __i = 0; __i < __lst.length; __i++) __result.data[__i] = " ++ fnAppStr ++ "; __result; })"

                _ ->
                    "/* List.indexedMap wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "concat") ->
            -- List.concat [[a], [b, c]] = [a, b, c] - flatten list of lists
            -- For embedded, we just return the first non-empty sublist or empty
            -- This is a simplification since nested lists are complex
            case args of
                [ listExpr ] ->
                    "/* List.concat not fully supported */ ((elm_list_t){ .length = 0 })"

                _ ->
                    "/* List.concat wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "intersperse") ->
            -- List.intersperse sep list = insert sep between elements
            case args of
                [ sepExpr, listExpr ] ->
                    let
                        sepStr = generateStandaloneExpr sepExpr
                        listStr = generateStandaloneExpr listExpr
                    in
                    "({ int __sep = " ++ sepStr ++ "; elm_list_t __lst = " ++ listStr ++ "; elm_list_t __result; if (__lst.length == 0) { __result.length = 0; } else { __result.length = __lst.length * 2 - 1; if (__result.length > ELM_LIST_MAX) __result.length = ELM_LIST_MAX; int __j = 0; for (int __i = 0; __i < __lst.length && __j < __result.length; __i++) { if (__i > 0 && __j < __result.length) __result.data[__j++] = __sep; if (__j < __result.length) __result.data[__j++] = __lst.data[__i]; } __result.length = __j; } __result; })"

                _ ->
                    "/* List.intersperse wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "filterMap") ->
            -- List.filterMap f list = map and keep Just values
            case args of
                [ fnExpr, listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr

                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    "({ double elm_" ++ pname ++ " = __lst.data[__i]; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Lambda with Located pattern: \(Src.At _ x) -> ...
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                                    "({ elm_union_t __elem = __lst.data[__i]; double elm_" ++ innerName ++ " = __elem.data; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Lambda with qualified Located pattern: \(Src.At _ x) -> ...
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                                    "({ elm_union_t __elem = __lst.data[__i]; double elm_" ++ innerName ++ " = __elem.data; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__lst.data[__i])"
                    in
                    "({ elm_list_t __lst = " ++ listStr ++ "; elm_list_t __result; __result.length = 0; for (int __i = 0; __i < __lst.length; __i++) { elm_union_t __maybe = " ++ fnAppStr ++ "; if (__maybe.tag == TAG_Just) __result.data[__result.length++] = __maybe.data; } __result; })"

                _ ->
                    "/* List.filterMap wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "concatMap") ->
            -- List.concatMap f list = flatten(map f list)
            -- Applies f to each element (f returns list) and concatenates results
            case args of
                [ fnExpr, listExpr ] ->
                    let
                        listStr = generateStandaloneExpr listExpr

                        fnAppStr =
                            case fnExpr of
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PVar pname) ] lambdaBody) ->
                                    "({ double elm_" ++ pname ++ " = __src.data[__i]; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Lambda with Located pattern (Src.At _ x)
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PCtor _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                                    "({ elm_union_t __elem = __src.data[__i]; double elm_" ++ innerName ++ " = __elem.data; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                -- Lambda with qualified Located pattern (Src.At _ x)
                                Src.At _ (Src.Lambda [ Src.At _ (Src.PCtorQual _ _ "At" [ Src.At _ Src.PAnything, Src.At _ (Src.PVar innerName) ]) ] lambdaBody) ->
                                    "({ elm_union_t __elem = __src.data[__i]; double elm_" ++ innerName ++ " = __elem.data; " ++ generateStandaloneExpr lambdaBody ++ "; })"

                                _ ->
                                    generateStandaloneExpr fnExpr ++ "(__src.data[__i])"
                    in
                    "({ elm_list_t __src = " ++ listStr ++ "; elm_list_t __result; __result.length = 0; for (int __i = 0; __i < __src.length; __i++) { elm_list_t __sub = " ++ fnAppStr ++ "; for (int __j = 0; __j < __sub.length; __j++) __result.data[__result.length++] = __sub.data[__j]; } __result; })"

                _ ->
                    "/* List.concatMap wrong arity */ 0"

        Src.At _ (Src.VarQual _ "List" "unzip") ->
            -- List.unzip cannot be fully supported as it returns tuple of lists
            -- Simplified: return empty tuple indicator
            case args of
                [ _ ] ->
                    "/* List.unzip not fully supported */ 0"

                _ ->
                    "/* List.unzip wrong arity */ 0"

        _ ->
            -- Check if this is a constructor call (CapVar) vs function call (LowVar)
            case fn of
                Src.At _ (Src.Var Src.CapVar ctorName) ->
                    -- Unqualified constructor call: Ctor arg1 arg2
                    let
                        -- Wrap each argument as elm_union_t if it's a primitive or string
                        wrapAsUnion argExpr =
                            let
                                argStr = generateStandaloneExpr argExpr
                            in
                            if isUnionValue argStr then
                                argStr
                            else if isStringValue argStr then
                                -- Wrap string as union with tag 0 (generic)
                                "((elm_union_t){0, {.str = " ++ argStr ++ "}, 0})"
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
                        -- Wrap each argument as elm_union_t if it's a primitive or string
                        wrapAsUnion argExpr =
                            let
                                argStr = generateStandaloneExpr argExpr
                            in
                            if isUnionValue argStr then
                                argStr
                            else if isStringValue argStr then
                                -- Wrap string as union with tag 0 (generic)
                                "((elm_union_t){0, {.str = " ++ argStr ++ "}, 0})"
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
                                    "elm_" ++ name

                                Src.At _ (Src.VarQual _ moduleName funcName) ->
                                    -- Qualified function: Module.function -> elm_Module_function
                                    "elm_" ++ moduleName ++ "_" ++ funcName

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


{-| Generate inlined lambda with string arguments (for pipe handling)
    This avoids needing to construct AST nodes dynamically.
-}
generateInlinedLambdaWithStringArgs : List Src.Pattern -> List String -> Src.Expr -> String
generateInlinedLambdaWithStringArgs patterns argStrings body =
    let
        -- Create bindings for each pattern/arg string pair
        bindings =
            List.map2
                (\(Src.At _ pat) argStr ->
                    case pat of
                        Src.PVar varName ->
                            "double elm_" ++ varName ++ " = " ++ argStr ++ ";"

                        _ ->
                            "/* unsupported pattern in lambda */"
                )
                patterns
                argStrings

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
        -- Generate function body with correct context for lifted local functions
        bodyExpr =
            generateStandaloneExprWithCtx { funcPrefix = name } body

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

                                    -- Check if parameter is used as a list (has .length or .data access, or is bound as elm_list_t)
                                    isListType =
                                        String.contains ("elm_" ++ varName ++ ".length") bodyExpr
                                            || String.contains ("elm_" ++ varName ++ ".data") bodyExpr
                                            || String.contains ("(elm_" ++ varName ++ ").length") bodyExpr
                                            || String.contains ("elm_list_t elm_case_scrutinee = elm_" ++ varName) bodyExpr

                                    -- Check if parameter is used as a string (with string functions)
                                    -- Use more precise patterns with trailing delimiter to avoid matching function name prefixes
                                    isStringType =
                                        String.contains ("elm_str_append(elm_" ++ varName ++ ",") bodyExpr
                                            || String.contains ("elm_str_append(elm_" ++ varName ++ ")") bodyExpr
                                            || String.contains ("elm_str_replace(") bodyExpr && String.contains (", elm_" ++ varName ++ ")") bodyExpr
                                            || String.contains ("elm_str_ends_with(") bodyExpr && String.contains (", elm_" ++ varName ++ ")") bodyExpr
                                            || String.contains ("elm_str_contains(") bodyExpr && String.contains (", elm_" ++ varName ++ ")") bodyExpr
                                            || String.contains ("strlen(elm_" ++ varName ++ ")") bodyExpr

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
                                in
                                if isListType then
                                    "elm_list_t elm_" ++ varName
                                else if isStringType then
                                    "const char *elm_" ++ varName
                                else if isUnionType then
                                    "elm_union_t elm_" ++ varName
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

                            _ ->
                                "double /* unsupported pattern */"
                    )
                |> String.join ", "

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

        isListReturn =
            String.contains "((elm_list_t){" bodyExpr
                || String.contains "elm_list_t __" bodyExpr

        -- Check for record returns - generates ((struct { ... }){...})
        isRecordReturn =
            String.contains "((struct {" bodyExpr

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

        returnType =
            if isStringReturn then
                "const char *"
            else if isUnionReturn then
                "elm_union_t"
            else if isListReturn then
                "elm_list_t"
            else if isRecordReturn then
                recordType
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
        "static " ++ recordType ++ " elm_" ++ name ++ " = {" ++ initializer ++ "};"
    else if isRecordReturn then
        -- For functions returning records, use a typedef to ensure type consistency
        -- TCC and standard C treat each anonymous struct as a distinct type
        let
            typedefName = "elm_" ++ name ++ "_return_t"
            initStartMarker = "){"
            initStartIdx = String.indexes initStartMarker bodyExpr |> List.head |> Maybe.withDefault 0
            afterInitStart = String.dropLeft (initStartIdx + String.length initStartMarker) bodyExpr
            initEndIdx = String.length afterInitStart - 2
            initializer = String.left initEndIdx afterInitStart
        in
        "typedef " ++ recordType ++ " " ++ typedefName ++ ";\n" ++
        "static " ++ typedefName ++ " elm_" ++ name ++ "(" ++ params ++ ") {\n    return (" ++ typedefName ++ "){" ++ initializer ++ "};\n}"
    else
        "static " ++ returnType ++ " elm_" ++ name ++ "(" ++ params ++ ") {\n    return " ++ bodyExpr ++ ";\n}"


{-| A local function to be lifted to module level
-}
type alias LiftedFunc =
    { prefix : String
    , name : String
    , args : List Src.Pattern
    , body : Src.Expr
    , capturedVars : List String  -- Variables captured from outer scope
    }


{-| Collect all variable references from an expression
-}
collectVarRefs : Src.Expr -> List String
collectVarRefs (Src.At _ expr) =
    case expr of
        Src.Var _ name ->
            [ name ]

        Src.VarQual _ _ name ->
            [ name ]

        Src.Call fn fnArgs ->
            collectVarRefs fn ++ List.concatMap collectVarRefs fnArgs

        Src.Binops pairs final ->
            List.concatMap (\( e, _ ) -> collectVarRefs e) pairs ++ collectVarRefs final

        Src.If branches elseExpr ->
            List.concatMap (\( cond, thenE ) -> collectVarRefs cond ++ collectVarRefs thenE) branches
                ++ collectVarRefs elseExpr

        Src.Let defs letBody ->
            let
                -- Collect refs from def bodies (but not the names being defined)
                defRefs =
                    defs
                        |> List.concatMap
                            (\(Src.At _ def) ->
                                case def of
                                    Src.Define _ _ defBody _ ->
                                        collectVarRefs defBody

                                    Src.Destruct _ e ->
                                        collectVarRefs e
                            )
            in
            defRefs ++ collectVarRefs letBody

        Src.Case scrutinee branches ->
            collectVarRefs scrutinee
                ++ List.concatMap (\( _, branchExpr ) -> collectVarRefs branchExpr) branches

        Src.Lambda _ lambdaBody ->
            collectVarRefs lambdaBody

        Src.Record fields ->
            fields |> List.concatMap (\( _, fieldExpr ) -> collectVarRefs fieldExpr)

        Src.Update (Src.At _ baseName) fields ->
            baseName :: List.concatMap (\( _, fieldExpr ) -> collectVarRefs fieldExpr) fields

        Src.Access inner _ ->
            collectVarRefs inner

        Src.Negate inner ->
            collectVarRefs inner

        Src.Tuple first second rest ->
            collectVarRefs first ++ collectVarRefs second ++ List.concatMap collectVarRefs rest

        Src.List items ->
            List.concatMap collectVarRefs items

        _ ->
            []


{-| Extract variable names bound by a pattern
-}
patternVars : Src.Pattern -> List String
patternVars (Src.At _ pat) =
    case pat of
        Src.PVar name ->
            [ name ]

        Src.PCtor _ _ subPats ->
            List.concatMap patternVars subPats

        Src.PCtorQual _ _ _ subPats ->
            List.concatMap patternVars subPats

        Src.PList subPats ->
            List.concatMap patternVars subPats

        Src.PCons headPat tailPat ->
            patternVars headPat ++ patternVars tailPat

        Src.PTuple first second rest ->
            patternVars first ++ patternVars second ++ List.concatMap patternVars rest

        Src.PRecord fields ->
            List.map (\(Src.At _ name) -> name) fields

        Src.PAlias innerPat (Src.At _ aliasName) ->
            aliasName :: patternVars innerPat

        _ ->
            []


{-| Remove duplicates from a list
-}
uniqueStrings : List String -> List String
uniqueStrings list =
    List.foldl
        (\item acc ->
            if List.member item acc then
                acc
            else
                acc ++ [ item ]
        )
        []
        list


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
                                                    collectVarRefs defBody |> uniqueStrings

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
                            (\( pat, branchExpr ) ->
                                -- Add pattern vars to scope for this branch
                                let
                                    patVars = patternVars pat
                                in
                                collectLocalFunctionsWithScope prefix (scope ++ patVars) branchExpr
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

                            _ ->
                                "double /* unsupported pattern */"
                    )
                |> String.join ", "

        -- Detect return type based on the final expression in the body
        -- Be careful to check what's actually being returned, not just what's used in the body
        -- Check for record returns - generates ((struct { ... }){...})
        isRecordReturn =
            String.contains "((struct {" bodyExpr

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


{-| Generate standalone C code for let bindings using GCC compound statements
-}
generateStandaloneLet : List (Src.Located Src.Def) -> Src.Expr -> String
generateStandaloneLet defs body =
    generateStandaloneLetWithPrefix "main" defs body


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
                                            collectVarRefs defBody |> uniqueStrings

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

                Src.Destruct _ _ ->
                    "/* pattern destructuring not supported */"

        -- Generate #undef for each local function after the body
        undefMacros =
            localFuncInfo
                |> List.map (\f -> "#undef elm_" ++ f.name)

        defStrs =
            List.map generateDef defs

        -- Use context-aware generation for the body to handle nested lets correctly
        bodyStr =
            generateStandaloneExprWithCtx { funcPrefix = prefix } body

        undefStrs =
            if List.isEmpty undefMacros then
                ""
            else
                "\n        " ++ String.join "\n        " undefMacros
    in
    "({\n        " ++ String.join "\n        " defStrs ++ "\n        " ++ bodyStr ++ ";" ++ undefStrs ++ "\n    })"


{-| Generate standalone C code for if/else expressions using ternary operator
-}
generateStandaloneIf : List ( Src.Expr, Src.Expr ) -> Src.Expr -> String
generateStandaloneIf branches elseExpr =
    generateStandaloneIfWithCtx defaultExprCtx branches elseExpr


{-| Generate if/else with function context
-}
generateStandaloneIfWithCtx : ExprCtx -> List ( Src.Expr, Src.Expr ) -> Src.Expr -> String
generateStandaloneIfWithCtx ctx branches elseExpr =
    case branches of
        [] ->
            generateStandaloneExprWithCtx ctx elseExpr

        ( condition, thenExpr ) :: rest ->
            "("
                ++ generateStandaloneExprWithCtx ctx condition
                ++ " ? "
                ++ generateStandaloneExprWithCtx ctx thenExpr
                ++ " : "
                ++ generateStandaloneIfWithCtx ctx rest elseExpr
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

                        Src.PCtorQual _ _ _ _ ->
                            True

                        _ ->
                            False
                )
                branches

        -- Check if this is a list case (has PList or PCons patterns)
        isListCase =
            List.any
                (\( Src.At _ pat, _ ) ->
                    case pat of
                        Src.PList _ ->
                            True

                        Src.PCons _ _ ->
                            True

                        _ ->
                            False
                )
                branches

        -- Check if case expression returns a union type (by checking first branch's result)
        returnsUnion =
            case branches of
                ( _, firstResult ) :: _ ->
                    let
                        resultStr = generateStandaloneExpr firstResult
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

        generateBranches : List ( Src.Pattern, Src.Expr ) -> String
        generateBranches bs =
            case bs of
                [] ->
                    fallbackValue

                ( Src.At _ pattern, resultExpr ) :: rest ->
                    case pattern of
                        Src.PAnything ->
                            -- Wildcard always matches
                            generateStandaloneExpr resultExpr

                        Src.PVar varName ->
                            -- Variable binding - bind scrutinee to variable name
                            "({\n            double elm_"
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
                                                        "double elm_" ++ varName ++ " = elm_case_scrutinee._" ++ String.fromInt i ++ ";"

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
                                        -- Check if the variable is used as a union (passed to function with elm_union_t param)
                                        let
                                            resultStr = generateStandaloneExpr resultExpr

                                            bindings =
                                                ctorPatterns
                                                    |> List.indexedMap
                                                        (\patIdx (Src.At _ pat) ->
                                                            case pat of
                                                                Src.PVar varName ->
                                                                    -- Check if variable is used as union, string, or number in result expression
                                                                    let
                                                                        -- Check if used as string (string append, concatenation)
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
                                                                    else
                                                                        "double elm_" ++ varName ++ " = " ++ accessor ++ "->data.num;"

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
                                            ++ resultStr
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
                                "(elm_case_scrutinee.tag == TAG_"
                                    ++ fullCtorName
                                    ++ " ? "
                                    ++ generateStandaloneExpr resultExpr
                                    ++ " : "
                                    ++ generateBranches rest
                                    ++ ")"

                            else if List.all (\(Src.At _ p) -> p == Src.PAnything) ctorPatterns then
                                -- Constructor with wildcard args - just compare tag
                                "(elm_case_scrutinee.tag == TAG_"
                                    ++ fullCtorName
                                    ++ " ? "
                                    ++ generateStandaloneExpr resultExpr
                                    ++ " : "
                                    ++ generateBranches rest
                                    ++ ")"

                            else
                                -- Constructor with data - bind variable and compare tag
                                let
                                    resultStr = generateStandaloneExpr resultExpr

                                    bindings =
                                        ctorPatterns
                                            |> List.indexedMap
                                                (\patIdx (Src.At _ pat) ->
                                                    case pat of
                                                        Src.PVar varName ->
                                                            -- Check if variable is used as union, string, or number in result expression
                                                            let
                                                                -- Check if used as string (string append, concatenation)
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
                                                            else
                                                                "double elm_" ++ varName ++ " = " ++ accessor ++ "->data.num;"

                                                        _ ->
                                                            ""
                                                )
                                            |> List.filter (not << String.isEmpty)
                                            |> String.join " "
                                in
                                "(elm_case_scrutinee.tag == TAG_"
                                    ++ fullCtorName
                                    ++ " ? ({ "
                                    ++ bindings
                                    ++ " "
                                    ++ resultStr
                                    ++ "; }) : "
                                    ++ generateBranches rest
                                    ++ ")"

                        -- Empty list pattern
                        Src.PList [] ->
                            "(elm_case_scrutinee.length == 0 ? "
                                ++ generateStandaloneExpr resultExpr
                                ++ " : "
                                ++ generateBranches rest
                                ++ ")"

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
                                resultStr = generateStandaloneExpr resultExpr

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
                            in
                            "(" ++ condition ++ " ? ({ "
                                ++ allBindings
                                ++ " "
                                ++ generateStandaloneExpr resultExpr
                                ++ "; }) : "
                                ++ generateBranches rest
                                ++ ")"

                        _ ->
                            "/* unsupported pattern */ " ++ generateBranches rest
    in
    if hasVarBinding || hasCtorWithData || isListCase then
        -- Use compound statement to bind scrutinee to a variable
        let
            scrutineeType =
                if isListCase then
                    "elm_list_t"

                else if isCustomTypeCase then
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
                        fallbackValue

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
                                                                        "double elm_" ++ varName ++ " = " ++ inlineScrutinee ++ ".data.num;"

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
                                                            "double elm_" ++ varName ++ " = " ++ inlineScrutinee ++ "._" ++ String.fromInt i ++ ";"

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

                            -- Qualified constructor pattern (e.g., Src.Str, Src.Int)
                            Src.PCtorQual _ moduleName ctorName ctorPatterns ->
                                let
                                    fullCtorName = moduleName ++ "_" ++ ctorName
                                in
                                if List.isEmpty ctorPatterns then
                                    -- Simple enum constructor - compare tag
                                    "("
                                        ++ inlineScrutinee
                                        ++ ".tag == TAG_"
                                        ++ fullCtorName
                                        ++ " ? "
                                        ++ generateStandaloneExpr resultExpr
                                        ++ " : "
                                        ++ generateSimpleBranches rest
                                        ++ ")"

                                else if List.all (\(Src.At _ p) -> p == Src.PAnything) ctorPatterns then
                                    -- Constructor with wildcard args - just compare tag
                                    "("
                                        ++ inlineScrutinee
                                        ++ ".tag == TAG_"
                                        ++ fullCtorName
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
                                                                "double elm_" ++ varName ++ " = " ++ inlineScrutinee ++ ".data.num;"

                                                            _ ->
                                                                ""
                                                    )
                                                |> List.filter (not << String.isEmpty)
                                                |> String.join " "
                                    in
                                    "("
                                        ++ inlineScrutinee
                                        ++ ".tag == TAG_"
                                        ++ fullCtorName
                                        ++ " ? ({ "
                                        ++ bindings
                                        ++ " "
                                        ++ generateStandaloneExpr resultExpr
                                        ++ "; }) : "
                                        ++ generateSimpleBranches rest
                                        ++ ")"

                            -- Empty list pattern
                            Src.PList [] ->
                                "("
                                    ++ inlineScrutinee
                                    ++ ".length == 0 ? "
                                    ++ generateStandaloneExpr resultExpr
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
                                in
                                "("
                                    ++ inlineScrutinee
                                    ++ ".length > 0 ? ({ "
                                    ++ allBindings
                                    ++ " "
                                    ++ generateStandaloneExpr resultExpr
                                    ++ "; }) : "
                                    ++ generateSimpleBranches rest
                                    ++ ")"

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

                    '\'' ->
                        "\\'"

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
