module Target.Native exposing
    ( generateCode
    )

{-| Native target code generation.

This module handles generation of C code for native execution
(standard desktop/server environments).

-}

import AST.Source as Src


{-| Generate native C code for a module.
Currently delegates to Cli.generateNativeCode.
-}
generateCode : Src.Module -> String
generateCode ast =
    -- This is a placeholder - the actual implementation remains in Cli.elm
    -- for now, to avoid breaking the build. Full migration will happen in
    -- subsequent refactoring.
    "/* Native target - see Cli.generateNativeCode */"
