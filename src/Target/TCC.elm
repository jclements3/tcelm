module Target.TCC exposing
    ( generateCode
    , generateLibCode
    , generateHeader
    , runtimePreamble
    )

{-| TCC target code generation.

This module handles generation of C code for the TCC (Tiny C Compiler) target.
TCC requires standard C without GCC extensions like nested functions.

-}

import AST.Source as Src


{-| Generate TCC-compatible C code for a module.
Currently delegates to Cli.generateTccCode.
-}
generateCode : Src.Module -> String
generateCode ast =
    -- This is a placeholder - the actual implementation remains in Cli.elm
    -- for now, to avoid breaking the build. Full migration will happen in
    -- subsequent refactoring.
    "/* TCC target - see Cli.generateTccCode */"


{-| Generate library code with module-qualified names.
-}
generateLibCode : Src.Module -> String
generateLibCode ast =
    "/* TCC lib target - see Cli.generateTccLibCode */"


{-| Generate header file with extern declarations.
-}
generateHeader : Src.Module -> String
generateHeader ast =
    "/* TCC header target - see Cli.generateTccHeader */"


{-| Standard runtime preamble for TCC targets.
This includes common includes, type definitions, and helper functions.
-}
runtimePreamble : String
runtimePreamble =
    String.join "\n"
        [ "#include <stdio.h>"
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
        ]
