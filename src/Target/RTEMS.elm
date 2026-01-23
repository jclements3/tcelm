module Target.RTEMS exposing
    ( generateCode
    , runtimePreamble
    )

{-| RTEMS target code generation.

This module handles generation of C code for the RTEMS real-time
operating system target, including Init wrapper and serial/framebuffer output.

-}

import AST.Source as Src


{-| Generate RTEMS-compatible C code for a module.
Currently delegates to Cli.generateRtemsCode.
-}
generateCode : Src.Module -> String
generateCode ast =
    -- This is a placeholder - the actual implementation remains in Cli.elm
    -- for now, to avoid breaking the build. Full migration will happen in
    -- subsequent refactoring.
    "/* RTEMS target - see Cli.generateRtemsCode */"


{-| RTEMS-specific runtime preamble.
Includes RTEMS type definitions and serial/framebuffer output functions.
-}
runtimePreamble : String
runtimePreamble =
    String.join "\n"
        [ "/* RTEMS entry point */"
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
        ]
