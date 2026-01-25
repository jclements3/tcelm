module Target.TCC exposing
    ( CodegenConfig
    , generateCode
    , generateHeader
    , generateLibCode
    , runtimePreamble
    )

{-| TCC target code generation.

This module handles generation of C code for the TCC (Tiny C Compiler) target.
TCC requires standard C without GCC extensions like nested functions.

-}

import AST.Source as Src
import Codegen.Lambda exposing (LiftedFunc)
import Codegen.Shared exposing (MainValue(..), collectAllFunctionNames, collectUserFunctionNames, escapeC, getModuleName, getModulePrefix, isSimpleLiteral, patternVars)


{-| Configuration for TCC code generation.
Provides callbacks to shared code generation functions in Cli.elm.
-}
type alias CodegenConfig =
    { extractMain : Src.Module -> MainValue
    , generateImportCode : List Src.Import -> String
    , generateUserFunction : String -> List String -> String -> List Src.Pattern -> Src.Expr -> String
    , collectLocalFunctionsWithScope : String -> List String -> Src.Expr -> List LiftedFunc
    , generateLiftedFunction : String -> String -> List Src.Pattern -> Src.Expr -> List String -> String
    , generateStandaloneExpr : Src.Expr -> String
    , generateStandaloneExprWithPrefix : String -> String -> List String -> Src.Expr -> String
    , filterReachableValues : List (Src.Located Src.Value) -> List (Src.Located Src.Value)
    }


{-| Generate C code for TCC (Tiny C Compiler).
Uses lambda lifting to avoid GCC-specific nested functions.
-}
generateCode : CodegenConfig -> Src.Module -> String
generateCode config ast =
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
                |> List.any
                    (\(Src.At _ v) ->
                        case v.name of
                            Src.At _ "main" ->
                                True

                            _ ->
                                False
                    )

        -- Dead Code Elimination: only keep functions reachable from main
        -- Only apply DCE if module has main (library modules keep all functions)
        values =
            if hasMain then
                config.filterReachableValues ast.values

            else
                ast.values

        -- Process imports to generate includes and extern declarations
        importCode =
            config.generateImportCode ast.imports

        mainValue =
            config.extractMain ast

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
                                            funcName =
                                                "elm_" ++ recordName ++ "_" ++ fieldName

                                            params =
                                                patterns
                                                    |> List.map
                                                        (\(Src.At _ p) ->
                                                            case p of
                                                                Src.PVar vn ->
                                                                    "double elm_" ++ vn

                                                                Src.PRecord fns ->
                                                                    fns |> List.map (\(Src.At _ fn) -> "double elm_" ++ fn) |> String.join ", "

                                                                _ ->
                                                                    "double __arg"
                                                        )
                                                    |> String.join ", "

                                            bodyStr =
                                                config.generateStandaloneExpr body

                                            -- Infer return type from body
                                            lambdaReturnType =
                                                if String.contains "elm_str_" bodyStr || String.startsWith "\"" bodyStr then
                                                    "const char *"

                                                else if String.contains "elm_union_t" bodyStr || String.contains "TAG_" bodyStr then
                                                    "elm_union_t"

                                                else
                                                    "double"

                                            funcDef =
                                                "static " ++ lambdaReturnType ++ " " ++ funcName ++ "(" ++ params ++ ") {\n    return " ++ bodyStr ++ ";\n}"
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
                            (Src.At _ name) =
                                value.name
                        in
                        if name /= "main" && List.isEmpty value.args then
                            let
                                lambdas =
                                    extractRecordLambdas name value.body
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
                            cExpr =
                                config.generateStandaloneExpr body

                            isString =
                                String.startsWith "\"" cExpr

                            cType =
                                if isString then
                                    "const char *"

                                else
                                    "double"
                        in
                        "static " ++ cType ++ " elm_" ++ name ++ " = " ++ cExpr ++ ";"
                    )
                |> String.join "\n"

        -- Replace complex constant references with function calls in an expression
        -- NOTE: For self-hosting, we skip this transformation to avoid closure capture issues
        fixComplexConstantRefs code =
            code

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
                            cExpr =
                                config.generateStandaloneExprWithPrefix name modulePrefix userFunctionNames body

                            -- Check for record list first (special marker format)
                            isRecordList =
                                String.contains "/*RECORD_LIST:" cExpr

                            -- Check for list type (before record, since list of records contains struct)
                            isList =
                                not isRecordList && (String.contains "elm_list_t" cExpr || String.startsWith "((elm_list_t)" cExpr)

                            -- Check for union type (Maybe, Result, etc.) - contains TAG_ usage or elm_union_t
                            isUnion =
                                not isList && not isRecordList && (String.contains "TAG_" cExpr || String.contains "elm_union_t" cExpr || String.contains "elm_Just" cExpr || String.contains "elm_Nothing" cExpr)

                            -- String detection - only if it's a string expression, not just contains a string literal argument
                            isString =
                                not isUnion && not isList && not isRecordList && (String.contains "elm_str_" cExpr || (String.startsWith "\"" cExpr && String.endsWith "\"" cExpr))

                            isRecord =
                                not isList && not isRecordList && String.contains "((struct {" cExpr

                            -- Extract record type from the expression
                            recordType =
                                if isRecord then
                                    let
                                        startMarker =
                                            "((struct {"

                                        startIdx =
                                            String.indexes startMarker cExpr |> List.head |> Maybe.withDefault 0

                                        searchStart =
                                            startIdx + String.length startMarker

                                        afterStart =
                                            String.dropLeft searchStart cExpr

                                        endIdx =
                                            String.indexes "})" afterStart |> List.head |> Maybe.withDefault 0

                                        fieldDefs =
                                            String.left endIdx afterStart
                                    in
                                    "struct {" ++ fieldDefs ++ "}"

                                else
                                    ""

                            cType =
                                if isList then
                                    "elm_list_t"

                                else if isUnion then
                                    "elm_union_t"

                                else if isString then
                                    "const char *"

                                else if isRecord then
                                    recordType

                                else
                                    "double"
                        in
                        -- For record lists, generate typedef and static array
                        if isRecordList then
                            let
                                -- Extract record type from /*RECORD_LIST:struct {...}*/
                                typeStartMarker =
                                    "/*RECORD_LIST:"

                                typeStartIdx =
                                    String.indexes typeStartMarker cExpr |> List.head |> Maybe.withDefault 0

                                afterTypeStart =
                                    String.dropLeft (typeStartIdx + String.length typeStartMarker) cExpr

                                typeEndMarker =
                                    "*/ "

                                typeEndIdx =
                                    String.indexes typeEndMarker afterTypeStart |> List.head |> Maybe.withDefault 0

                                recType =
                                    String.left typeEndIdx afterTypeStart

                                -- Extract count from /*END_RECORD_LIST:N*/
                                countStartMarker =
                                    "/*END_RECORD_LIST:"

                                countStartIdx =
                                    String.indexes countStartMarker cExpr |> List.head |> Maybe.withDefault 0

                                afterCountStart =
                                    String.dropLeft (countStartIdx + String.length countStartMarker) cExpr

                                countEndIdx =
                                    String.indexes "*/" afterCountStart |> List.head |> Maybe.withDefault 0

                                countStr =
                                    String.left countEndIdx afterCountStart

                                -- Extract the data after */ { ... } /*END
                                afterTypeComment =
                                    String.dropLeft (typeEndIdx + String.length typeEndMarker) afterTypeStart

                                -- Now find the opening { and extract content until } /*END
                                dataStartIdx =
                                    String.indexes "{ " afterTypeComment |> List.head |> Maybe.withDefault 0

                                afterDataStart =
                                    String.dropLeft (dataStartIdx + 2) afterTypeComment

                                dataEndIdx =
                                    String.indexes " } /*END" afterDataStart |> List.head |> Maybe.withDefault (String.length afterDataStart)

                                dataContent =
                                    String.left dataEndIdx afterDataStart

                                -- Generate typedef name from constant name
                                typedefName =
                                    "elm_" ++ modulePrefix ++ "_" ++ name ++ "_elem_t"

                                listTypeName =
                                    "elm_" ++ modulePrefix ++ "_" ++ name ++ "_list_t"

                                dataArrayName =
                                    "elm_" ++ modulePrefix ++ "_" ++ name ++ "_data"

                                varName =
                                    "elm_" ++ modulePrefix ++ "_" ++ name
                            in
                            -- Generate typedef for element
                            "typedef "
                                ++ recType
                                ++ " "
                                ++ typedefName
                                ++ ";\n"
                                -- Generate static array
                                ++ "static "
                                ++ typedefName
                                ++ " "
                                ++ dataArrayName
                                ++ "[] = { "
                                ++ dataContent
                                ++ " };\n"
                                -- Generate list wrapper type with typed data pointer
                                ++ "typedef struct { int length; "
                                ++ typedefName
                                ++ " *data; } "
                                ++ listTypeName
                                ++ ";\n"
                                -- Generate wrapper variable that points to the array
                                ++ "static "
                                ++ listTypeName
                                ++ " "
                                ++ varName
                                ++ " = { "
                                ++ countStr
                                ++ ", "
                                ++ dataArrayName
                                ++ " };\n"
                                -- Generate alias for backward compatibility
                                ++ "#define elm_"
                                ++ name
                                ++ " "
                                ++ varName

                        else if isList then
                            let
                                -- Extract initializer from ((elm_list_t){...})
                                initStartMarker =
                                    "((elm_list_t){"

                                initStartIdx =
                                    String.indexes initStartMarker cExpr |> List.head |> Maybe.withDefault 0

                                afterInitStart =
                                    String.dropLeft (initStartIdx + String.length initStartMarker) cExpr

                                initEndIdx =
                                    String.length afterInitStart - 2

                                initializer =
                                    String.left initEndIdx afterInitStart
                            in
                            "static elm_list_t elm_" ++ name ++ " = {" ++ initializer ++ "};"

                        else if isRecord then
                            let
                                -- Extract initializer from ((struct {...}){...})
                                initStartMarker =
                                    "){"

                                initStartIdx =
                                    String.indexes initStartMarker cExpr |> List.head |> Maybe.withDefault 0

                                afterInitStart =
                                    String.dropLeft (initStartIdx + String.length initStartMarker) cExpr

                                initEndIdx =
                                    String.length afterInitStart - 2

                                initializer =
                                    String.left initEndIdx afterInitStart

                                -- Replace lambda markers with function names
                                fixedInitializer =
                                    replaceLambdaMarkers name initializer
                            in
                            "static " ++ recordType ++ " elm_" ++ name ++ " = {" ++ fixedInitializer ++ "};"

                        else
                            "static " ++ cType ++ " elm_" ++ name ++ "(void) {\n    return " ++ fixComplexConstantRefs cExpr ++ ";\n}"
                    )
                |> String.join "\n\n"

        -- Early constants code (doesn't depend on lifted functions)
        earlyConstantsCode =
            let
                lambdas =
                    if String.isEmpty recordLambdaFunctionsCode then
                        ""

                    else
                        "/* Lambda functions lifted from records */\n" ++ recordLambdaFunctionsCode ++ "\n\n"

                simple =
                    if String.isEmpty simpleConstantsCode then
                        ""

                    else
                        "/* Simple constants */\n" ++ simpleConstantsCode ++ "\n\n"
            in
            lambdas ++ simple

        -- Complex constants code (depends on lifted functions, output after them)
        complexConstantsSection =
            if String.isEmpty complexConstantsCode then
                ""

            else
                "/* Computed constants (as functions) */\n" ++ complexConstantsCode ++ "\n\n"

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
                            Just (fixComplexConstantRefs (config.generateUserFunction modulePrefix userFunctionNames name value.args value.body))

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
                "/* Lifted local functions */\n" ++ fixComplexConstantRefs liftedFunctions ++ "\n\n"

        userFunctionsCode =
            if String.isEmpty userFunctions then
                ""

            else
                "/* User-defined functions */\n" ++ userFunctions ++ "\n\n"

        -- Generate custom type definitions (tags and constructors)
        -- Skip built-in types (Maybe, Result, Order) as they're already defined in the header
        builtinTypes =
            [ "Maybe", "Result", "Order" ]

        customTypeCode =
            ast.unions
                |> List.filter
                    (\(Src.At _ union) ->
                        let
                            (Src.At _ typeName) =
                                union.name
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
                                        (\_ ( Src.At _ ctorName, ctorArgs ) ->
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
                                implCode =
                                    config.generateUserFunction modulePrefix userFunctionNames name value.args value.body

                                -- Extract signature parts from implementation
                                signatureEnd =
                                    String.indexes "(" implCode |> List.head |> Maybe.withDefault 0

                                paramsStart =
                                    signatureEnd + 1

                                paramsEnd =
                                    String.indexes ")" implCode |> List.head |> Maybe.withDefault 0

                                returnTypeStart =
                                    7

                                -- Length of "static "
                                -- Find " elm_<name>(" to locate the function name position
                                funcNameMarker =
                                    " elm_" ++ prefixedName ++ "("

                                returnTypeEnd =
                                    String.indexes funcNameMarker implCode |> List.head |> Maybe.withDefault 0

                                funcReturnType =
                                    String.slice returnTypeStart returnTypeEnd implCode

                                params =
                                    String.slice paramsStart paramsEnd implCode

                                -- Skip forward declarations for functions with anonymous structs
                                isStructReturn =
                                    String.contains "struct {" funcReturnType

                                isStructParam =
                                    String.contains "struct {" params

                                -- Skip typeof return types (needs the type to be declared first)
                                isTypeofReturn =
                                    String.contains "typeof(" funcReturnType
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
            , runtimePreamble
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
generateLibCode : CodegenConfig -> Src.Module -> String
generateLibCode config ast =
    let
        moduleName =
            getModuleName ast

        -- Convert module name to C identifier (replace dots with underscores)
        cModuleName =
            getModulePrefix ast

        -- Process imports
        importCode =
            config.generateImportCode ast.imports

        -- Generate module-qualified function names
        qualifyName name =
            "elm_" ++ cModuleName ++ "_" ++ name

        -- Generate module-level constants with qualified names
        ( simpleConstants, complexConstants ) =
            ast.values
                |> List.filterMap
                    (\(Src.At _ value) ->
                        let
                            (Src.At _ name) =
                                value.name
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
                            cExpr =
                                config.generateStandaloneExpr body

                            isString =
                                String.startsWith "\"" cExpr

                            cType =
                                if isString then
                                    "const char *"

                                else
                                    "double"
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
                            cExpr =
                                config.generateStandaloneExprWithPrefix name cModuleName userFunctionNames body

                            isString =
                                String.contains "elm_str_" cExpr || String.contains "\"" cExpr

                            cType =
                                if isString then
                                    "const char *"

                                else
                                    "double"
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
                            (Src.At _ name) =
                                value.name
                        in
                        if not (List.isEmpty value.args) then
                            Just (config.generateUserFunction cModuleName userFunctionNames name value.args value.body)

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
                            (Src.At _ name) =
                                value.name

                            funcParamNames =
                                List.concatMap patternVars value.args
                        in
                        config.collectLocalFunctionsWithScope name funcParamNames value.body
                    )
                |> List.map
                    (\lf ->
                        let
                            funcCode =
                                config.generateLiftedFunction lf.prefix lf.name lf.args lf.body lf.capturedVars

                            -- Qualify lifted function names too
                            qualifiedCode =
                                String.replace ("elm_" ++ lf.prefix ++ "_") (qualifyName (lf.prefix ++ "_")) funcCode
                        in
                        qualifiedCode
                    )
                |> String.join "\n\n"

        -- Generate custom type constructors
        constructorDefines =
            ast.unions
                |> List.concatMap
                    (\(Src.At _ union) ->
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
            ++ [ if String.isEmpty constructorDefines then
                    ""

                 else
                    "/* Type tags */\n" ++ constructorDefines ++ "\n"
               ]
            ++ [ if String.isEmpty simpleConstantsCode then
                    ""

                 else
                    "/* Constants */\n" ++ simpleConstantsCode ++ "\n"
               ]
            ++ [ if String.isEmpty liftedFunctions then
                    ""

                 else
                    "/* Local functions */\n" ++ liftedFunctions ++ "\n"
               ]
            ++ [ if String.isEmpty complexConstantsCode then
                    ""

                 else
                    "/* Computed values */\n" ++ complexConstantsCode ++ "\n"
               ]
            ++ [ if String.isEmpty userFunctions then
                    ""

                 else
                    "/* Functions */\n" ++ userFunctions ++ "\n"
               ]
            ++ footer
        )


{-| Generate C header file with extern declarations for a module's exports.
-}
generateHeader : CodegenConfig -> Src.Module -> String
generateHeader config ast =
    let
        moduleName =
            getModuleName ast

        cModuleName =
            getModulePrefix ast

        -- Collect user function names for context
        userFunctionNames =
            collectAllFunctionNames ast.values

        -- Get exported names
        (Src.At _ exports) =
            ast.exports

        exportedNames =
            case exports of
                Src.Open ->
                    -- Export all values
                    ast.values
                        |> List.map
                            (\(Src.At _ v) ->
                                let
                                    (Src.At _ name) =
                                        v.name
                                in
                                name
                            )

                Src.Explicit exposed ->
                    exposed
                        |> List.filterMap
                            (\e ->
                                case e of
                                    Src.Lower (Src.At _ name) ->
                                        Just name

                                    _ ->
                                        Nothing
                            )

        -- Qualify function name
        qualifyName name =
            "elm_" ++ cModuleName ++ "_" ++ name

        -- Generate extern declaration for a value
        generateExtern : Src.Located Src.Value -> Maybe String
        generateExtern (Src.At _ value) =
            let
                (Src.At _ name) =
                    value.name
            in
            if List.member name exportedNames then
                let
                    -- Generate the function to infer return type
                    funcCode =
                        config.generateUserFunction cModuleName userFunctionNames name value.args value.body

                    -- Extract return type (format: static TYPE elm_NAME...)
                    returnTypeStart =
                        7

                    -- "static "
                    returnTypeEnd =
                        String.indexes " elm_" funcCode |> List.head |> Maybe.withDefault 0

                    returnType =
                        String.slice returnTypeStart returnTypeEnd funcCode

                    -- Generate parameter types
                    hasArgs =
                        not (List.isEmpty value.args)

                    params =
                        if hasArgs then
                            let
                                paramStart =
                                    String.indexes "(" funcCode |> List.head |> Maybe.map ((+) 1) |> Maybe.withDefault 0

                                paramEnd =
                                    String.indexes ")" funcCode |> List.head |> Maybe.withDefault 0
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
            ++ [ if String.isEmpty typeTagExterns then
                    ""

                 else
                    "/* Type tags */\n" ++ typeTagExterns ++ "\n"
               ]
            ++ [ if String.isEmpty externDecls then
                    ""

                 else
                    "/* Function declarations */\n" ++ externDecls
               ]
            ++ footer
        )


{-| Complete runtime preamble for TCC targets.
This includes all necessary type definitions, helper functions, and runtime support.
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
        , "typedef struct elm_union_s { int tag; union { double num; struct elm_union_s *child; const char *str; void *ptr; } data; struct elm_union_s *data2; } elm_union_t;"
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
        , "/* Built-in Maybe constructor functions */"
        , "static elm_union_t elm_Nothing(void) {"
        , "    elm_union_t result = { .tag = TAG_Nothing, .data = {.num = 0}, .data2 = 0 };"
        , "    return result;"
        , "}"
        , "static elm_union_t elm_Just(elm_union_t v1) {"
        , "    elm_union_t result = { .tag = TAG_Just, .data = {.child = elm_alloc_union(v1)}, .data2 = 0 };"
        , "    return result;"
        , "}"
        , ""
        , "/* Built-in Result type tags */"
        , "#define TAG_Err 0"
        , "#define TAG_Ok 1"
        , ""
        , "/* Built-in Result constructor functions */"
        , "static elm_union_t elm_Ok(elm_union_t v1) {"
        , "    elm_union_t result = { .tag = TAG_Ok, .data = {.child = elm_alloc_union(v1)}, .data2 = 0 };"
        , "    return result;"
        , "}"
        , "static elm_union_t elm_Err(elm_union_t v1) {"
        , "    elm_union_t result = { .tag = TAG_Err, .data = {.child = elm_alloc_union(v1)}, .data2 = 0 };"
        , "    return result;"
        , "}"
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
        , "/* Built-in List functions */"
        , "static elm_union_t elm_List_head(elm_list_t lst) {"
        , "    if (lst.length > 0) {"
        , "        elm_union_t inner = { .tag = 0, .data = {.num = lst.data[0].d}, .data2 = 0 };"
        , "        return elm_Just(inner);"
        , "    }"
        , "    return elm_Nothing();"
        , "}"
        , "static elm_union_t elm_List_tail(elm_list_t lst) {"
        , "    if (lst.length > 1) {"
        , "        elm_list_t *tail = (elm_list_t*)malloc(sizeof(elm_list_t));"
        , "        tail->length = lst.length - 1;"
        , "        for (int i = 1; i < lst.length; i++) tail->data[i-1] = lst.data[i];"
        , "        elm_union_t inner = { .tag = 0, .data = {.ptr = tail}, .data2 = 0 };"
        , "        return elm_Just(inner);"
        , "    } else if (lst.length == 1) {"
        , "        elm_list_t *empty = (elm_list_t*)malloc(sizeof(elm_list_t));"
        , "        empty->length = 0;"
        , "        elm_union_t inner = { .tag = 0, .data = {.ptr = empty}, .data2 = 0 };"
        , "        return elm_Just(inner);"
        , "    }"
        , "    return elm_Nothing();"
        , "}"
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
        ]
