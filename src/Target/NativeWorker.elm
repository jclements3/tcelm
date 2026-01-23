module Target.NativeWorker exposing
    ( generateCode
    )

{-| Native Worker target code generation.

This module handles generation of C code for the native worker target,
which is used for background processing tasks.

-}

import AST.Source as Src


{-| Generate native worker C code for a module.
Currently delegates to Generate.C.generateNativeWorkerModule.
-}
generateCode : Src.Module -> String
generateCode ast =
    -- This is a placeholder - the actual implementation remains in Generate/C.elm
    -- for now, to avoid breaking the build. Full migration will happen in
    -- subsequent refactoring.
    "/* Native Worker target - see Generate.C.generateNativeWorkerModule */"
