module Codegen.Union exposing
    ( generateUnionDef
    , generateTagDefines
    , generateTypeDef
    , generateConstructorFuncs
    )

{-| Union type code generation.

This module handles generation of C code for Elm custom types (union types),
including tag defines, type definitions, and constructor functions.

-}

import AST.Source as Src


{-| Generate complete union type definition including tags, typedef, and constructors.
-}
generateUnionDef : Src.Located Src.Union -> String
generateUnionDef (Src.At _ union) =
    let
        (Src.At _ typeName) =
            union.name

        tagDefines =
            generateTagDefines union.ctors

        structDef =
            generateTypeDef typeName

        ctorFuncs =
            generateConstructorFuncs union.ctors
    in
    tagDefines ++ "\n" ++ structDef ++ ctorFuncs


{-| Generate #define statements for constructor tags.
-}
generateTagDefines : List ( Src.Located String, List Src.Type ) -> String
generateTagDefines ctors =
    ctors
        |> List.indexedMap
            (\i ( Src.At _ ctorName, _ ) ->
                "#define TAG_" ++ ctorName ++ " " ++ String.fromInt i
            )
        |> String.join "\n"


{-| Generate typedef for a union type.
-}
generateTypeDef : String -> String
generateTypeDef typeName =
    "typedef elm_union_t elm_" ++ typeName ++ ";\n"


{-| Generate constructor functions for a union type.
-}
generateConstructorFuncs : List ( Src.Located String, List Src.Type ) -> String
generateConstructorFuncs ctors =
    ctors
        |> List.map
            (\( Src.At _ ctorName, ctorArgs ) ->
                generateConstructorFunc ctorName (List.length ctorArgs)
            )
        |> String.join "\n\n"


{-| Generate a single constructor function.
-}
generateConstructorFunc : String -> Int -> String
generateConstructorFunc ctorName argCount =
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


{-| Generate union definitions for a list of unions.
-}
generateUnionDefs : List (Src.Located Src.Union) -> String
generateUnionDefs unions =
    unions
        |> List.map generateUnionDef
        |> String.join "\n\n"
