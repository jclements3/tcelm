module Codegen.C exposing
    ( generateC
    , generateModule
    , Options
    , defaultOptions
    )

{-| C code generation from Core IR.

This module generates C code from the Core intermediate representation.
The generated code is designed to be:
- TCC compatible (no GCC extensions)
- Suitable for embedded systems (RTEMS, bare metal)
- Efficient for the target use cases

Key design decisions:
- Closures are heap-allocated structs
- Type class dictionaries are passed explicitly
- Tail calls are optimized where possible
- Pattern matching compiles to efficient switch/if chains
-}

import Core
import Types exposing (Type(..), Scheme(..), Constraint(..))
import Dict exposing (Dict)
import Set


-- OPTIONS


type alias Options =
    { targetName : String
    , emitDebugInfo : Bool
    , optimizeLevel : Int
    , maxClosureArgs : Int
    , maxTupleSize : Int
    }


defaultOptions : Options
defaultOptions =
    { targetName = "tcc"
    , emitDebugInfo = False
    , optimizeLevel = 1
    , maxClosureArgs = 8
    , maxTupleSize = 8
    }



-- CONTEXT


type alias GenCtx =
    { options : Options
    , moduleName : String
    , indent : Int
    , freshId : Int
    , dataTypes : Dict String Core.DataDef
    , functions : Dict String Core.FuncDef
    , foreignFunctions : Dict String Core.ForeignDef
    , classes : Dict String Core.ClassDef
    , instances : List Core.InstDef
    , closureParams : Set.Set String  -- Function parameters that are closures
    , localVars : Set.Set String       -- Local variables in scope (for free var computation)
    , liftedLambdas : List LiftedLambda -- Accumulated lifted lambda functions
    }


{-| A lambda that has been lifted to a top-level function.
-}
type alias LiftedLambda =
    { name : String           -- Generated function name (e.g., "__lambda_0")
    , definition : String     -- Full C function definition
    }


emptyCtx : Options -> GenCtx
emptyCtx opts =
    { options = opts
    , moduleName = "Main"
    , indent = 0
    , freshId = 0
    , dataTypes = Dict.empty
    , functions = Dict.empty
    , foreignFunctions = Dict.empty
    , classes = Dict.empty
    , instances = []
    , closureParams = Set.empty
    , localVars = Set.empty
    , liftedLambdas = []
    }


freshName : String -> GenCtx -> ( String, GenCtx )
freshName prefix ctx =
    ( prefix ++ "_" ++ String.fromInt ctx.freshId
    , { ctx | freshId = ctx.freshId + 1 }
    )


withIndent : GenCtx -> GenCtx
withIndent ctx =
    { ctx | indent = ctx.indent + 1 }


indentStr : GenCtx -> String
indentStr ctx =
    String.repeat (ctx.indent * 4) " "



-- MAIN ENTRY POINT


generateC : Options -> Core.Module -> String
generateC opts module_ =
    let
        ctx = collectDecls (emptyCtx opts) module_.decls
        ctx1 = { ctx | moduleName = module_.name }

        -- First pass: collect all lifted lambdas by generating functions
        ( funcCode, finalCtx ) = generateFunctionsWithLambdas ctx1

        -- Generate lambda forward declarations and definitions
        lambdaForwardDecls =
            finalCtx.liftedLambdas
                |> List.reverse  -- Reverse to get declaration order
                |> List.map .name
                |> List.map (\name -> "static elm_value_t " ++ name ++ "();")
                |> String.join "\n"

        lambdaDefinitions =
            finalCtx.liftedLambdas
                |> List.reverse
                |> List.map .definition
                |> String.join "\n\n"
    in
    let
        foreignWrappers = generateForeignWrappers finalCtx
    in
    String.join "\n"
        [ generateHeader ctx1
        , ""
        , generateRuntime ctx1
        , ""
        , generateTypeDecls ctx1
        , ""
        , generateForeignExterns finalCtx
        , ""
        , generateForwardDecls ctx1
        , if String.isEmpty lambdaForwardDecls then "" else "\n/* Lifted lambda forward declarations */\n" ++ lambdaForwardDecls
        , ""
        , if String.isEmpty lambdaDefinitions then "" else "/* ===== LIFTED LAMBDAS ===== */\n\n" ++ lambdaDefinitions
        , ""
        , if String.isEmpty foreignWrappers then "" else "/* ===== FOREIGN FUNCTION WRAPPERS ===== */\n\n" ++ foreignWrappers
        , ""
        , "/* ===== FUNCTIONS ===== */\n\n" ++ funcCode
        , ""
        , generateMain finalCtx
        ]


generateModule : Core.Module -> String
generateModule =
    generateC defaultOptions


collectDecls : GenCtx -> List Core.Decl -> GenCtx
collectDecls ctx decls =
    List.foldl collectDecl ctx decls


collectDecl : Core.Decl -> GenCtx -> GenCtx
collectDecl decl ctx =
    case decl of
        Core.FuncDecl funcDef ->
            { ctx | functions = Dict.insert funcDef.name funcDef ctx.functions }

        Core.DataDecl dataDef ->
            { ctx | dataTypes = Dict.insert dataDef.name dataDef ctx.dataTypes }

        Core.ClassDecl classDef ->
            { ctx | classes = Dict.insert classDef.name classDef ctx.classes }

        Core.InstDecl instDef ->
            { ctx | instances = instDef :: ctx.instances }

        Core.ForeignDecl foreignDef ->
            { ctx | foreignFunctions = Dict.insert foreignDef.name foreignDef ctx.foreignFunctions }



-- HEADER


generateHeader : GenCtx -> String
generateHeader ctx =
    String.join "\n"
        [ "/*"
        , " * Generated by tcelm v2 from " ++ ctx.moduleName
        , " * Target: " ++ ctx.options.targetName
        , " */"
        , ""
        , "#include <stdio.h>"
        , "#include <stdlib.h>"
        , "#include <string.h>"
        , "#include <stdint.h>"
        , "#include <stdbool.h>"
        , "#include <math.h>"
        , "#include <time.h>"
        ]



-- RUNTIME


generateRuntime : GenCtx -> String
generateRuntime _ =
    String.join "\n"
        [ "/* ===== RUNTIME ===== */"
        , ""
        , "/* Tagged union for algebraic data types */"
        , "typedef struct elm_value_s {"
        , "    uint32_t tag;"
        , "    union {"
        , "        int64_t i;"
        , "        double f;"
        , "        const char *s;"
        , "        void *p;"
        , "        struct elm_value_s *c;  /* child */"
        , "    } data;"
        , "    struct elm_value_s *next;   /* for lists, additional fields */"
        , "} elm_value_t;"
        , ""
        , "/* Closure for partial application */"
        , "typedef struct elm_closure_s {"
        , "    void *func;                 /* function pointer */"
        , "    uint8_t arity;              /* total arity */"
        , "    uint8_t applied;            /* args already applied */"
        , "    elm_value_t args[8];        /* captured arguments */"
        , "} elm_closure_t;"
        , ""
        , "/* Type class dictionary */"
        , "typedef struct elm_dict_s {"
        , "    void *methods[16];          /* method function pointers */"
        , "} elm_dict_t;"
        , ""
        , "/* Memory allocation */"
        , "static elm_value_t *elm_alloc(void) {"
        , "    return (elm_value_t *)malloc(sizeof(elm_value_t));"
        , "}"
        , ""
        , "static elm_closure_t *elm_alloc_closure(void) {"
        , "    return (elm_closure_t *)malloc(sizeof(elm_closure_t));"
        , "}"
        , ""
        , "/* Value constructors */"
        , "static elm_value_t elm_int(int64_t n) {"
        , "    return (elm_value_t){ .tag = 0, .data.i = n, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_float(double f) {"
        , "    return (elm_value_t){ .tag = 1, .data.f = f, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_string(const char *s) {"
        , "    return (elm_value_t){ .tag = 2, .data.s = s, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_char(char c) {"
        , "    return (elm_value_t){ .tag = 3, .data.i = (int64_t)c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_bool(bool b) {"
        , "    return (elm_value_t){ .tag = b ? 5 : 4, .data.i = b, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_unit(void) {"
        , "    return (elm_value_t){ .tag = 6, .data.i = 0, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_ptr(void *p) {"
        , "    return (elm_value_t){ .tag = 7, .data.p = p, .next = NULL };"
        , "}"
        , ""
        , "/* List operations */"
        , "static elm_value_t elm_nil(void) {"
        , "    return (elm_value_t){ .tag = 100, .data.p = NULL, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_cons(elm_value_t head, elm_value_t tail) {"
        , "    elm_value_t *h = elm_alloc();"
        , "    elm_value_t *t = elm_alloc();"
        , "    *h = head;"
        , "    *t = tail;"
        , "    return (elm_value_t){ .tag = 101, .data.c = h, .next = t };"
        , "}"
        , ""
        , "/* Maybe operations */"
        , "static elm_value_t elm_nothing(void) {"
        , "    return (elm_value_t){ .tag = 200, .data.p = NULL, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_just(elm_value_t val) {"
        , "    elm_value_t *v = elm_alloc();"
        , "    *v = val;"
        , "    return (elm_value_t){ .tag = 201, .data.c = v, .next = NULL };"
        , "}"
        , ""
        , "/* Result operations */"
        , "static elm_value_t elm_err(elm_value_t val) {"
        , "    elm_value_t *v = elm_alloc();"
        , "    *v = val;"
        , "    return (elm_value_t){ .tag = 300, .data.c = v, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_ok(elm_value_t val) {"
        , "    elm_value_t *v = elm_alloc();"
        , "    *v = val;"
        , "    return (elm_value_t){ .tag = 301, .data.c = v, .next = NULL };"
        , "}"
        , ""
        , "/* Record field node for linked list representation */"
        , "typedef struct elm_record_field_s {"
        , "    const char *name;"
        , "    elm_value_t value;"
        , "    struct elm_record_field_s *next;"
        , "} elm_record_field_t;"
        , ""
        , "/* Record operations - records are linked lists of (field_name, value) pairs */"
        , "static elm_value_t elm_record_end(void) {"
        , "    return (elm_value_t){ .tag = 500, .data.p = NULL, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_record_field(const char *name, elm_value_t value, elm_value_t rest) {"
        , "    elm_record_field_t *f = (elm_record_field_t *)malloc(sizeof(elm_record_field_t));"
        , "    f->name = name;"
        , "    f->value = value;"
        , "    f->next = (elm_record_field_t *)rest.data.p;"
        , "    return (elm_value_t){ .tag = 500, .data.p = f, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_record_get(const char *name, elm_value_t record) {"
        , "    elm_record_field_t *f = (elm_record_field_t *)record.data.p;"
        , "    while (f != NULL) {"
        , "        if (strcmp(f->name, name) == 0) {"
        , "            return f->value;"
        , "        }"
        , "        f = f->next;"
        , "    }"
        , "    return elm_unit(); /* field not found */"
        , "}"
        , ""
        , "/* Tuple operations - use special tag 600 for 2-tuples */"
        , "static elm_value_t elm_tuple2(elm_value_t a, elm_value_t b) {"
        , "    elm_value_t *first = elm_alloc();"
        , "    elm_value_t *second = elm_alloc();"
        , "    *first = a;"
        , "    *second = b;"
        , "    return (elm_value_t){ .tag = 600, .data.c = first, .next = second };"
        , "}"
        , ""
        , "/* Closure application */"
        , "static elm_value_t elm_apply1(elm_closure_t *c, elm_value_t arg) {"
        , "    if (c->applied + 1 < c->arity) {"
        , "        /* Still partial - create new closure */"
        , "        elm_closure_t *c2 = elm_alloc_closure();"
        , "        *c2 = *c;"
        , "        c2->args[c2->applied++] = arg;"
        , "        return (elm_value_t){ .tag = 400, .data.p = c2, .next = NULL };"
        , "    }"
        , "    /* Full application */"
        , "    switch (c->arity) {"
        , "        case 1: return ((elm_value_t (*)(elm_value_t))c->func)(arg);"
        , "        case 2: return ((elm_value_t (*)(elm_value_t, elm_value_t))c->func)(c->args[0], arg);"
        , "        case 3: return ((elm_value_t (*)(elm_value_t, elm_value_t, elm_value_t))c->func)(c->args[0], c->args[1], arg);"
        , "        case 4: return ((elm_value_t (*)(elm_value_t, elm_value_t, elm_value_t, elm_value_t))c->func)(c->args[0], c->args[1], c->args[2], arg);"
        , "        default: return elm_unit();"
        , "    }"
        , "}"
        , ""
        , "/* String operations */"
        , "static elm_value_t elm_str_append(elm_value_t a, elm_value_t b) {"
        , "    size_t la = strlen(a.data.s);"
        , "    size_t lb = strlen(b.data.s);"
        , "    char *buf = malloc(la + lb + 1);"
        , "    memcpy(buf, a.data.s, la);"
        , "    memcpy(buf + la, b.data.s, lb + 1);"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "static elm_value_t elm_str_length(elm_value_t s) {"
        , "    return elm_int((int64_t)strlen(s.data.s));"
        , "}"
        , ""
        , "/* Arithmetic operators (prefixed with _op_ to avoid user variable collisions) */"
        , "static elm_value_t elm__op_add(elm_value_t a, elm_value_t b) {"
        , "    return elm_int(a.data.i + b.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm__op_sub(elm_value_t a, elm_value_t b) {"
        , "    return elm_int(a.data.i - b.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm__op_mul(elm_value_t a, elm_value_t b) {"
        , "    return elm_int(a.data.i * b.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm__op_div(elm_value_t a, elm_value_t b) {"
        , "    return elm_int(a.data.i / b.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm__op_mod(elm_value_t a, elm_value_t b) {"
        , "    return elm_int(a.data.i % b.data.i);"
        , "}"
        , ""
        , "/* modBy : Int -> Int -> Int -- always positive result */"
        , "static elm_value_t elm_modBy(elm_value_t divisor, elm_value_t n) {"
        , "    int64_t d = divisor.data.i;"
        , "    int64_t x = n.data.i;"
        , "    int64_t r = x % d;"
        , "    return elm_int((r < 0) ? r + (d < 0 ? -d : d) : r);"
        , "}"
        , ""
        , "/* remainderBy : Int -> Int -> Int -- same sign as dividend */"
        , "static elm_value_t elm_remainderBy(elm_value_t divisor, elm_value_t n) {"
        , "    return elm_int(n.data.i % divisor.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm__op_intDiv(elm_value_t a, elm_value_t b) {"
        , "    return elm_int(a.data.i / b.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm__op_pow(elm_value_t a, elm_value_t b) {"
        , "    int64_t result = 1;"
        , "    int64_t base = a.data.i;"
        , "    int64_t exp = b.data.i;"
        , "    while (exp > 0) { if (exp & 1) result *= base; base *= base; exp >>= 1; }"
        , "    return elm_int(result);"
        , "}"
        , ""
        , "/* Comparison operators */"
        , "static elm_value_t elm__op_eq(elm_value_t a, elm_value_t b) {"
        , "    return elm_bool(a.data.i == b.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm__op_neq(elm_value_t a, elm_value_t b) {"
        , "    return elm_bool(a.data.i != b.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm__op_lt(elm_value_t a, elm_value_t b) {"
        , "    return elm_bool(a.data.i < b.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm__op_gt(elm_value_t a, elm_value_t b) {"
        , "    return elm_bool(a.data.i > b.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm__op_lte(elm_value_t a, elm_value_t b) {"
        , "    return elm_bool(a.data.i <= b.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm__op_gte(elm_value_t a, elm_value_t b) {"
        , "    return elm_bool(a.data.i >= b.data.i);"
        , "}"
        , ""
        , "/* Boolean operators */"
        , "static elm_value_t elm__op_and(elm_value_t a, elm_value_t b) {"
        , "    return elm_bool(a.data.i && b.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm__op_or(elm_value_t a, elm_value_t b) {"
        , "    return elm_bool(a.data.i || b.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm_not(elm_value_t a) {"
        , "    return elm_bool(!a.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm_negate(elm_value_t a) {"
        , "    return elm_int(-a.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm_xor(elm_value_t a, elm_value_t b) {"
        , "    return elm_bool((a.data.i && !b.data.i) || (!a.data.i && b.data.i));"
        , "}"
        , ""
        , "static elm_value_t elm_isNaN(elm_value_t f) {"
        , "    return elm_bool(isnan(f.data.f));"
        , "}"
        , ""
        , "static elm_value_t elm_isInfinite(elm_value_t f) {"
        , "    return elm_bool(isinf(f.data.f));"
        , "}"
        , ""
        , "/* Math functions */"
        , "static elm_value_t elm_sqrt(elm_value_t x) {"
        , "    return elm_float(sqrt(x.data.f));"
        , "}"
        , ""
        , "static elm_value_t elm_sin(elm_value_t x) {"
        , "    return elm_float(sin(x.data.f));"
        , "}"
        , ""
        , "static elm_value_t elm_cos(elm_value_t x) {"
        , "    return elm_float(cos(x.data.f));"
        , "}"
        , ""
        , "static elm_value_t elm_tan(elm_value_t x) {"
        , "    return elm_float(tan(x.data.f));"
        , "}"
        , ""
        , "static elm_value_t elm_asin(elm_value_t x) {"
        , "    return elm_float(asin(x.data.f));"
        , "}"
        , ""
        , "static elm_value_t elm_acos(elm_value_t x) {"
        , "    return elm_float(acos(x.data.f));"
        , "}"
        , ""
        , "static elm_value_t elm_atan(elm_value_t x) {"
        , "    return elm_float(atan(x.data.f));"
        , "}"
        , ""
        , "static elm_value_t elm_atan2(elm_value_t y, elm_value_t x) {"
        , "    return elm_float(atan2(y.data.f, x.data.f));"
        , "}"
        , ""
        , "static elm_value_t elm_logBase(elm_value_t base, elm_value_t n) {"
        , "    return elm_float(log(n.data.f) / log(base.data.f));"
        , "}"
        , ""
        , "static elm_value_t elm_e(void) {"
        , "    return elm_float(2.718281828459045);"
        , "}"
        , ""
        , "static elm_value_t elm_pi(void) {"
        , "    return elm_float(3.141592653589793);"
        , "}"
        , ""
        , "/* Angle conversions */"
        , "static elm_value_t elm_degrees(elm_value_t deg) {"
        , "    return elm_float(deg.data.f * 3.141592653589793 / 180.0);"
        , "}"
        , ""
        , "static elm_value_t elm_radians(elm_value_t rad) {"
        , "    return elm_float(rad.data.f);"
        , "}"
        , ""
        , "static elm_value_t elm_turns(elm_value_t t) {"
        , "    return elm_float(t.data.f * 2.0 * 3.141592653589793);"
        , "}"
        , ""
        , "static elm_value_t elm_ceiling(elm_value_t x) {"
        , "    return elm_int((int64_t)ceil(x.data.f));"
        , "}"
        , ""
        , "static elm_value_t elm_floor(elm_value_t x) {"
        , "    return elm_int((int64_t)floor(x.data.f));"
        , "}"
        , ""
        , "static elm_value_t elm_round(elm_value_t x) {"
        , "    return elm_int((int64_t)round(x.data.f));"
        , "}"
        , ""
        , "static elm_value_t elm_truncate(elm_value_t x) {"
        , "    return elm_int((int64_t)trunc(x.data.f));"
        , "}"
        , ""
        , "static elm_value_t elm_toFloat(elm_value_t n) {"
        , "    return elm_float((double)n.data.i);"
        , "}"
        , ""
        , "/* List operators */"
        , "static elm_value_t elm__op_cons(elm_value_t head, elm_value_t tail) {"
        , "    return elm_cons(head, tail);"
        , "}"
        , ""
        , "/* Forward declaration for append */"
        , "static elm_value_t elm_List_reverse(elm_value_t xs);"
        , "static elm_value_t elm__op_append(elm_value_t xs, elm_value_t ys) {"
        , "    if (xs.tag == 100) return ys;"
        , "    xs = elm_List_reverse(xs);"
        , "    while (xs.tag == 101) {"
        , "        ys = elm_cons(*xs.data.c, ys);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return ys;"
        , "}"
        , ""
        , "/* ===== STANDARD LIBRARY ===== */"
        , ""
        , "/* Basics */"
        , "static elm_value_t elm_identity(elm_value_t x) {"
        , "    return x;"
        , "}"
        , ""
        , "static elm_value_t elm_always(elm_value_t x, elm_value_t _y) {"
        , "    return x;"
        , "}"
        , ""
        , "static elm_value_t elm_flip(elm_value_t f, elm_value_t b, elm_value_t a) {"
        , "    return elm_apply1((elm_closure_t *)elm_apply1((elm_closure_t *)f.data.p, a).data.p, b);"
        , "}"
        , ""
        , "/* Order type - LT (700), EQ (701), GT (702) */"
        , "static elm_value_t elm_lt(void) { return (elm_value_t){ .tag = 700, .data.p = NULL, .next = NULL }; }"
        , "static elm_value_t elm_eq(void) { return (elm_value_t){ .tag = 701, .data.p = NULL, .next = NULL }; }"
        , "static elm_value_t elm_gt(void) { return (elm_value_t){ .tag = 702, .data.p = NULL, .next = NULL }; }"
        , ""
        , "static elm_value_t elm_compare(elm_value_t a, elm_value_t b) {"
        , "    /* Compare integers (tag 0) */"
        , "    if (a.tag == 0 && b.tag == 0) {"
        , "        if (a.data.i < b.data.i) return elm_lt();"
        , "        if (a.data.i > b.data.i) return elm_gt();"
        , "        return elm_eq();"
        , "    }"
        , "    /* Compare floats (tag 1) */"
        , "    if (a.tag == 1 && b.tag == 1) {"
        , "        if (a.data.f < b.data.f) return elm_lt();"
        , "        if (a.data.f > b.data.f) return elm_gt();"
        , "        return elm_eq();"
        , "    }"
        , "    /* Compare strings (tag 2) */"
        , "    if (a.tag == 2 && b.tag == 2) {"
        , "        int cmp = strcmp(a.data.s, b.data.s);"
        , "        if (cmp < 0) return elm_lt();"
        , "        if (cmp > 0) return elm_gt();"
        , "        return elm_eq();"
        , "    }"
        , "    /* Compare chars (tag 3) */"
        , "    if (a.tag == 3 && b.tag == 3) {"
        , "        if (a.data.i < b.data.i) return elm_lt();"
        , "        if (a.data.i > b.data.i) return elm_gt();"
        , "        return elm_eq();"
        , "    }"
        , "    /* Default: EQ for same tag, LT otherwise */"
        , "    return elm_eq();"
        , "}"
        , ""
        , "static elm_value_t elm_curry(elm_value_t f, elm_value_t a, elm_value_t b) {"
        , "    /* curry : ((a, b) -> c) -> a -> b -> c */"
        , "    return elm_apply1((elm_closure_t *)f.data.p, elm_tuple2(a, b));"
        , "}"
        , ""
        , "static elm_value_t elm_uncurry(elm_value_t f, elm_value_t tuple) {"
        , "    /* uncurry : (a -> b -> c) -> (a, b) -> c */"
        , "    elm_value_t a = *tuple.data.c;"
        , "    elm_value_t b = *tuple.next;"
        , "    elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, a);"
        , "    return elm_apply1((elm_closure_t *)f1.data.p, b);"
        , "}"
        , ""
        , "static elm_value_t elm_min(elm_value_t a, elm_value_t b) {"
        , "    return a.data.i < b.data.i ? a : b;"
        , "}"
        , ""
        , "static elm_value_t elm_max(elm_value_t a, elm_value_t b) {"
        , "    return a.data.i > b.data.i ? a : b;"
        , "}"
        , ""
        , "static elm_value_t elm_clamp(elm_value_t lo, elm_value_t hi, elm_value_t x) {"
        , "    if (x.data.i < lo.data.i) return lo;"
        , "    if (x.data.i > hi.data.i) return hi;"
        , "    return x;"
        , "}"
        , ""
        , "static elm_value_t elm_abs(elm_value_t x) {"
        , "    return elm_int(x.data.i < 0 ? -x.data.i : x.data.i);"
        , "}"
        , ""
        , "/* List module */"
        , "static elm_value_t elm_List_isEmpty(elm_value_t xs) {"
        , "    return elm_bool(xs.tag == 100);"
        , "}"
        , ""
        , "static elm_value_t elm_List_length(elm_value_t xs) {"
        , "    int64_t n = 0;"
        , "    while (xs.tag == 101) {"
        , "        n++;"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_int(n);"
        , "}"
        , ""
        , "static elm_value_t elm_List_reverse(elm_value_t xs) {"
        , "    elm_value_t acc = elm_nil();"
        , "    while (xs.tag == 101) {"
        , "        acc = elm_cons(*xs.data.c, acc);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return acc;"
        , "}"
        , ""
        , "static elm_value_t elm_List_member(elm_value_t x, elm_value_t xs) {"
        , "    while (xs.tag == 101) {"
        , "        if (xs.data.c->data.i == x.data.i) return elm_bool(true);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_bool(false);"
        , "}"
        , ""
        , "static elm_value_t elm_List_head(elm_value_t xs) {"
        , "    if (xs.tag == 100) return elm_nothing();"
        , "    return elm_just(*xs.data.c);"
        , "}"
        , ""
        , "static elm_value_t elm_List_last(elm_value_t xs) {"
        , "    if (xs.tag == 100) return elm_nothing();"
        , "    while (xs.next && xs.next->tag == 101) {"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_just(*xs.data.c);"
        , "}"
        , ""
        , "static elm_value_t elm_List_tail(elm_value_t xs) {"
        , "    if (xs.tag == 100) return elm_nothing();"
        , "    return elm_just(*xs.next);"
        , "}"
        , ""
        , "static elm_value_t elm_List_take(elm_value_t n, elm_value_t xs) {"
        , "    elm_value_t acc = elm_nil();"
        , "    int64_t count = n.data.i;"
        , "    while (count > 0 && xs.tag == 101) {"
        , "        acc = elm_cons(*xs.data.c, acc);"
        , "        xs = *xs.next;"
        , "        count--;"
        , "    }"
        , "    return elm_List_reverse(acc);"
        , "}"
        , ""
        , "static elm_value_t elm_List_drop(elm_value_t n, elm_value_t xs) {"
        , "    int64_t count = n.data.i;"
        , "    while (count > 0 && xs.tag == 101) {"
        , "        xs = *xs.next;"
        , "        count--;"
        , "    }"
        , "    return xs;"
        , "}"
        , ""
        , "static elm_value_t elm_List_sum(elm_value_t xs) {"
        , "    int64_t total = 0;"
        , "    while (xs.tag == 101) {"
        , "        total += xs.data.c->data.i;"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_int(total);"
        , "}"
        , ""
        , "static elm_value_t elm_List_product(elm_value_t xs) {"
        , "    int64_t total = 1;"
        , "    while (xs.tag == 101) {"
        , "        total *= xs.data.c->data.i;"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_int(total);"
        , "}"
        , ""
        , "static elm_value_t elm_List_maximum(elm_value_t xs) {"
        , "    if (xs.tag == 100) return elm_nothing();"
        , "    int64_t m = xs.data.c->data.i;"
        , "    xs = *xs.next;"
        , "    while (xs.tag == 101) {"
        , "        if (xs.data.c->data.i > m) m = xs.data.c->data.i;"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_just(elm_int(m));"
        , "}"
        , ""
        , "static elm_value_t elm_List_minimum(elm_value_t xs) {"
        , "    if (xs.tag == 100) return elm_nothing();"
        , "    int64_t m = xs.data.c->data.i;"
        , "    xs = *xs.next;"
        , "    while (xs.tag == 101) {"
        , "        if (xs.data.c->data.i < m) m = xs.data.c->data.i;"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_just(elm_int(m));"
        , "}"
        , ""
        , "static elm_value_t elm_List_append(elm_value_t xs, elm_value_t ys) {"
        , "    if (xs.tag == 100) return ys;"
        , "    xs = elm_List_reverse(xs);"
        , "    while (xs.tag == 101) {"
        , "        ys = elm_cons(*xs.data.c, ys);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return ys;"
        , "}"
        , ""
        , "static elm_value_t elm_List_concat(elm_value_t xss) {"
        , "    elm_value_t result = elm_nil();"
        , "    while (xss.tag == 101) {"
        , "        result = elm_List_append(result, *xss.data.c);"
        , "        xss = *xss.next;"
        , "    }"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t elm_List_intersperse(elm_value_t sep, elm_value_t xs) {"
        , "    if (xs.tag == 100 || xs.next->tag == 100) return xs;"
        , "    elm_value_t acc = elm_nil();"
        , "    acc = elm_cons(*xs.data.c, acc);"
        , "    xs = *xs.next;"
        , "    while (xs.tag == 101) {"
        , "        acc = elm_cons(*xs.data.c, elm_cons(sep, acc));"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_List_reverse(acc);"
        , "}"
        , ""
        , "static elm_value_t elm_List_range(elm_value_t lo, elm_value_t hi) {"
        , "    elm_value_t acc = elm_nil();"
        , "    for (int64_t i = hi.data.i; i >= lo.data.i; i--) {"
        , "        acc = elm_cons(elm_int(i), acc);"
        , "    }"
        , "    return acc;"
        , "}"
        , ""
        , "static elm_value_t elm_List_repeat(elm_value_t n, elm_value_t x) {"
        , "    elm_value_t acc = elm_nil();"
        , "    for (int64_t i = 0; i < n.data.i; i++) {"
        , "        acc = elm_cons(x, acc);"
        , "    }"
        , "    return acc;"
        , "}"
        , ""
        , "static elm_value_t elm_List_singleton(elm_value_t x) {"
        , "    return elm_cons(x, elm_nil());"
        , "}"
        , ""
        , "/* Higher-order List functions */"
        , "static elm_value_t elm_List_map(elm_value_t f, elm_value_t xs) {"
        , "    elm_value_t acc = elm_nil();"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t mapped = elm_apply1((elm_closure_t *)f.data.p, *xs.data.c);"
        , "        acc = elm_cons(mapped, acc);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_List_reverse(acc);"
        , "}"
        , ""
        , "static elm_value_t elm_List_filter(elm_value_t pred, elm_value_t xs) {"
        , "    elm_value_t acc = elm_nil();"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t keep = elm_apply1((elm_closure_t *)pred.data.p, *xs.data.c);"
        , "        if (keep.data.i) acc = elm_cons(*xs.data.c, acc);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_List_reverse(acc);"
        , "}"
        , ""
        , "static elm_value_t elm_List_filterMap(elm_value_t f, elm_value_t xs) {"
        , "    elm_value_t acc = elm_nil();"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t result = elm_apply1((elm_closure_t *)f.data.p, *xs.data.c);"
        , "        if (result.tag == 201) acc = elm_cons(*result.data.c, acc);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_List_reverse(acc);"
        , "}"
        , ""
        , "static elm_value_t elm_List_foldl(elm_value_t f, elm_value_t acc, elm_value_t xs) {"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, *xs.data.c);"
        , "        acc = elm_apply1((elm_closure_t *)f1.data.p, acc);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return acc;"
        , "}"
        , ""
        , "static elm_value_t elm_List_foldr(elm_value_t f, elm_value_t acc, elm_value_t xs) {"
        , "    xs = elm_List_reverse(xs);"
        , "    return elm_List_foldl(f, acc, xs);"
        , "}"
        , ""
        , "static elm_value_t elm_List_any(elm_value_t pred, elm_value_t xs) {"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t result = elm_apply1((elm_closure_t *)pred.data.p, *xs.data.c);"
        , "        if (result.data.i) return elm_bool(true);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_bool(false);"
        , "}"
        , ""
        , "static elm_value_t elm_List_all(elm_value_t pred, elm_value_t xs) {"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t result = elm_apply1((elm_closure_t *)pred.data.p, *xs.data.c);"
        , "        if (!result.data.i) return elm_bool(false);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_bool(true);"
        , "}"
        , ""
        , "static elm_value_t elm_List_concatMap(elm_value_t f, elm_value_t xs) {"
        , "    return elm_List_concat(elm_List_map(f, xs));"
        , "}"
        , ""
        , "static elm_value_t elm_List_indexedMap(elm_value_t f, elm_value_t xs) {"
        , "    elm_value_t acc = elm_nil();"
        , "    int64_t i = 0;"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, elm_int(i));"
        , "        elm_value_t mapped = elm_apply1((elm_closure_t *)f1.data.p, *xs.data.c);"
        , "        acc = elm_cons(mapped, acc);"
        , "        xs = *xs.next;"
        , "        i++;"
        , "    }"
        , "    return elm_List_reverse(acc);"
        , "}"
        , ""
        , "/* List.sort - insertion sort for simplicity (assumes Int elements) */"
        , "static elm_value_t elm_List_sort(elm_value_t xs) {"
        , "    if (xs.tag == 100) return elm_nil();"
        , "    elm_value_t sorted = elm_nil();"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t elem = *xs.data.c;"
        , "        elm_value_t prev = elm_nil();"
        , "        elm_value_t curr = sorted;"
        , "        /* Find insertion point */"
        , "        while (curr.tag == 101 && curr.data.c->data.i < elem.data.i) {"
        , "            prev = elm_cons(*curr.data.c, prev);"
        , "            curr = *curr.next;"
        , "        }"
        , "        /* Build new sorted list: reverse(prev) ++ [elem] ++ curr */"
        , "        elm_value_t result = elm_cons(elem, curr);"
        , "        while (prev.tag == 101) {"
        , "            result = elm_cons(*prev.data.c, result);"
        , "            prev = *prev.next;"
        , "        }"
        , "        sorted = result;"
        , "        xs = *xs.next;"
        , "    }"
        , "    return sorted;"
        , "}"
        , ""
        , "/* List.sortBy - sort using a key function */"
        , "static elm_value_t elm_List_sortBy(elm_value_t keyFn, elm_value_t xs) {"
        , "    if (xs.tag == 100) return elm_nil();"
        , "    elm_value_t sorted = elm_nil();"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t elem = *xs.data.c;"
        , "        elm_value_t elemKey = elm_apply1((elm_closure_t *)keyFn.data.p, elem);"
        , "        elm_value_t prev = elm_nil();"
        , "        elm_value_t curr = sorted;"
        , "        /* Find insertion point by comparing keys */"
        , "        while (curr.tag == 101) {"
        , "            elm_value_t currKey = elm_apply1((elm_closure_t *)keyFn.data.p, *curr.data.c);"
        , "            if (currKey.data.i >= elemKey.data.i) break;"
        , "            prev = elm_cons(*curr.data.c, prev);"
        , "            curr = *curr.next;"
        , "        }"
        , "        elm_value_t result = elm_cons(elem, curr);"
        , "        while (prev.tag == 101) {"
        , "            result = elm_cons(*prev.data.c, result);"
        , "            prev = *prev.next;"
        , "        }"
        , "        sorted = result;"
        , "        xs = *xs.next;"
        , "    }"
        , "    return sorted;"
        , "}"
        , ""
        , "/* List.sortWith - sort using a custom comparator function */"
        , "static elm_value_t elm_List_sortWith(elm_value_t cmp, elm_value_t xs) {"
        , "    if (xs.tag == 100) return elm_nil();"
        , "    elm_value_t sorted = elm_nil();"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t elem = *xs.data.c;"
        , "        elm_value_t prev = elm_nil();"
        , "        elm_value_t curr = sorted;"
        , "        /* Find insertion point by calling comparator */"
        , "        while (curr.tag == 101) {"
        , "            elm_value_t cmp1 = elm_apply1((elm_closure_t *)cmp.data.p, elem);"
        , "            elm_value_t order = elm_apply1((elm_closure_t *)cmp1.data.p, *curr.data.c);"
        , "            /* LT = 700, EQ = 701, GT = 702 */"
        , "            if (order.tag <= 701) break; /* elem <= curr, insert here */"
        , "            prev = elm_cons(*curr.data.c, prev);"
        , "            curr = *curr.next;"
        , "        }"
        , "        elm_value_t result = elm_cons(elem, curr);"
        , "        while (prev.tag == 101) {"
        , "            result = elm_cons(*prev.data.c, result);"
        , "            prev = *prev.next;"
        , "        }"
        , "        sorted = result;"
        , "        xs = *xs.next;"
        , "    }"
        , "    return sorted;"
        , "}"
        , ""
        , "/* List.partition - split list by predicate */"
        , "static elm_value_t elm_List_partition(elm_value_t pred, elm_value_t xs) {"
        , "    elm_value_t trues = elm_nil();"
        , "    elm_value_t falses = elm_nil();"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t elem = *xs.data.c;"
        , "        elm_value_t keep = elm_apply1((elm_closure_t *)pred.data.p, elem);"
        , "        if (keep.data.i) {"
        , "            trues = elm_cons(elem, trues);"
        , "        } else {"
        , "            falses = elm_cons(elem, falses);"
        , "        }"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_tuple2(elm_List_reverse(trues), elm_List_reverse(falses));"
        , "}"
        , ""
        , "/* List.unzip - convert list of tuples to tuple of lists */"
        , "static elm_value_t elm_List_unzip(elm_value_t xs) {"
        , "    elm_value_t firsts = elm_nil();"
        , "    elm_value_t seconds = elm_nil();"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t tuple = *xs.data.c;"
        , "        firsts = elm_cons(*tuple.data.c, firsts);"
        , "        seconds = elm_cons(*tuple.next, seconds);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_tuple2(elm_List_reverse(firsts), elm_List_reverse(seconds));"
        , "}"
        , ""
        , "/* List.map2 - combine two lists with a function */"
        , "static elm_value_t elm_List_map2(elm_value_t f, elm_value_t xs, elm_value_t ys) {"
        , "    elm_value_t acc = elm_nil();"
        , "    while (xs.tag == 101 && ys.tag == 101) {"
        , "        elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, *xs.data.c);"
        , "        elm_value_t mapped = elm_apply1((elm_closure_t *)f1.data.p, *ys.data.c);"
        , "        acc = elm_cons(mapped, acc);"
        , "        xs = *xs.next;"
        , "        ys = *ys.next;"
        , "    }"
        , "    return elm_List_reverse(acc);"
        , "}"
        , ""
        , "/* List.map3 - combine three lists with a function */"
        , "static elm_value_t elm_List_map3(elm_value_t f, elm_value_t xs, elm_value_t ys, elm_value_t zs) {"
        , "    elm_value_t acc = elm_nil();"
        , "    while (xs.tag == 101 && ys.tag == 101 && zs.tag == 101) {"
        , "        elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, *xs.data.c);"
        , "        elm_value_t f2 = elm_apply1((elm_closure_t *)f1.data.p, *ys.data.c);"
        , "        elm_value_t mapped = elm_apply1((elm_closure_t *)f2.data.p, *zs.data.c);"
        , "        acc = elm_cons(mapped, acc);"
        , "        xs = *xs.next;"
        , "        ys = *ys.next;"
        , "        zs = *zs.next;"
        , "    }"
        , "    return elm_List_reverse(acc);"
        , "}"
        , ""
        , "/* List.map4 - combine four lists with a function */"
        , "static elm_value_t elm_List_map4(elm_value_t f, elm_value_t xs, elm_value_t ys, elm_value_t zs, elm_value_t ws) {"
        , "    elm_value_t acc = elm_nil();"
        , "    while (xs.tag == 101 && ys.tag == 101 && zs.tag == 101 && ws.tag == 101) {"
        , "        elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, *xs.data.c);"
        , "        elm_value_t f2 = elm_apply1((elm_closure_t *)f1.data.p, *ys.data.c);"
        , "        elm_value_t f3 = elm_apply1((elm_closure_t *)f2.data.p, *zs.data.c);"
        , "        elm_value_t mapped = elm_apply1((elm_closure_t *)f3.data.p, *ws.data.c);"
        , "        acc = elm_cons(mapped, acc);"
        , "        xs = *xs.next;"
        , "        ys = *ys.next;"
        , "        zs = *zs.next;"
        , "        ws = *ws.next;"
        , "    }"
        , "    return elm_List_reverse(acc);"
        , "}"
        , ""
        , "/* List.map5 - combine five lists with a function */"
        , "static elm_value_t elm_List_map5(elm_value_t f, elm_value_t xs, elm_value_t ys, elm_value_t zs, elm_value_t ws, elm_value_t vs) {"
        , "    elm_value_t acc = elm_nil();"
        , "    while (xs.tag == 101 && ys.tag == 101 && zs.tag == 101 && ws.tag == 101 && vs.tag == 101) {"
        , "        elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, *xs.data.c);"
        , "        elm_value_t f2 = elm_apply1((elm_closure_t *)f1.data.p, *ys.data.c);"
        , "        elm_value_t f3 = elm_apply1((elm_closure_t *)f2.data.p, *zs.data.c);"
        , "        elm_value_t f4 = elm_apply1((elm_closure_t *)f3.data.p, *ws.data.c);"
        , "        elm_value_t mapped = elm_apply1((elm_closure_t *)f4.data.p, *vs.data.c);"
        , "        acc = elm_cons(mapped, acc);"
        , "        xs = *xs.next;"
        , "        ys = *ys.next;"
        , "        zs = *zs.next;"
        , "        ws = *ws.next;"
        , "        vs = *vs.next;"
        , "    }"
        , "    return elm_List_reverse(acc);"
        , "}"
        , ""
        , "/* List.getAt - get element at index (0-based) */"
        , "static elm_value_t elm_List_getAt(elm_value_t idx, elm_value_t xs) {"
        , "    int64_t i = idx.data.i;"
        , "    if (i < 0) return elm_nothing();"
        , "    while (xs.tag == 101 && i > 0) {"
        , "        xs = *xs.next;"
        , "        i--;"
        , "    }"
        , "    if (xs.tag == 100) return elm_nothing();"
        , "    return elm_just(*xs.data.c);"
        , "}"
        , ""
        , "/* List.zip - combine two lists into list of tuples */"
        , "static elm_value_t elm_List_zip(elm_value_t xs, elm_value_t ys) {"
        , "    elm_value_t result = elm_nil();"
        , "    while (xs.tag == 101 && ys.tag == 101) {"
        , "        elm_value_t tuple = elm_tuple2(*xs.data.c, *ys.data.c);"
        , "        result = elm_cons(tuple, result);"
        , "        xs = *xs.next;"
        , "        ys = *ys.next;"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "/* Maybe module */"
        , "static elm_value_t elm_Maybe_withDefault(elm_value_t def, elm_value_t maybe) {"
        , "    if (maybe.tag == 200) return def;"
        , "    return *maybe.data.c;"
        , "}"
        , ""
        , "static elm_value_t elm_Maybe_map(elm_value_t f, elm_value_t maybe) {"
        , "    if (maybe.tag == 200) return elm_nothing();"
        , "    return elm_just(elm_apply1((elm_closure_t *)f.data.p, *maybe.data.c));"
        , "}"
        , ""
        , "static elm_value_t elm_Maybe_andThen(elm_value_t f, elm_value_t maybe) {"
        , "    if (maybe.tag == 200) return elm_nothing();"
        , "    return elm_apply1((elm_closure_t *)f.data.p, *maybe.data.c);"
        , "}"
        , ""
        , "static elm_value_t elm_Maybe_map2(elm_value_t f, elm_value_t m1, elm_value_t m2) {"
        , "    if (m1.tag == 200 || m2.tag == 200) return elm_nothing();"
        , "    elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, *m1.data.c);"
        , "    return elm_just(elm_apply1((elm_closure_t *)f1.data.p, *m2.data.c));"
        , "}"
        , ""
        , "static elm_value_t elm_Maybe_map3(elm_value_t f, elm_value_t m1, elm_value_t m2, elm_value_t m3) {"
        , "    if (m1.tag == 200 || m2.tag == 200 || m3.tag == 200) return elm_nothing();"
        , "    elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, *m1.data.c);"
        , "    elm_value_t f2 = elm_apply1((elm_closure_t *)f1.data.p, *m2.data.c);"
        , "    return elm_just(elm_apply1((elm_closure_t *)f2.data.p, *m3.data.c));"
        , "}"
        , ""
        , "static elm_value_t elm_Maybe_map4(elm_value_t f, elm_value_t m1, elm_value_t m2, elm_value_t m3, elm_value_t m4) {"
        , "    if (m1.tag == 200 || m2.tag == 200 || m3.tag == 200 || m4.tag == 200) return elm_nothing();"
        , "    elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, *m1.data.c);"
        , "    elm_value_t f2 = elm_apply1((elm_closure_t *)f1.data.p, *m2.data.c);"
        , "    elm_value_t f3 = elm_apply1((elm_closure_t *)f2.data.p, *m3.data.c);"
        , "    return elm_just(elm_apply1((elm_closure_t *)f3.data.p, *m4.data.c));"
        , "}"
        , ""
        , "static elm_value_t elm_Maybe_map5(elm_value_t f, elm_value_t m1, elm_value_t m2, elm_value_t m3, elm_value_t m4, elm_value_t m5) {"
        , "    if (m1.tag == 200 || m2.tag == 200 || m3.tag == 200 || m4.tag == 200 || m5.tag == 200) return elm_nothing();"
        , "    elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, *m1.data.c);"
        , "    elm_value_t f2 = elm_apply1((elm_closure_t *)f1.data.p, *m2.data.c);"
        , "    elm_value_t f3 = elm_apply1((elm_closure_t *)f2.data.p, *m3.data.c);"
        , "    elm_value_t f4 = elm_apply1((elm_closure_t *)f3.data.p, *m4.data.c);"
        , "    return elm_just(elm_apply1((elm_closure_t *)f4.data.p, *m5.data.c));"
        , "}"
        , ""
        , "/* Maybe.filter - filter a Maybe with a predicate */"
        , "static elm_value_t elm_Maybe_filter(elm_value_t pred, elm_value_t maybe) {"
        , "    if (maybe.tag == 200) return elm_nothing();"
        , "    elm_value_t result = elm_apply1((elm_closure_t *)pred.data.p, *maybe.data.c);"
        , "    if (result.tag == 5) return maybe;"
        , "    return elm_nothing();"
        , "}"
        , ""
        , "/* Result module */"
        , "static elm_value_t elm_Result_withDefault(elm_value_t def, elm_value_t result) {"
        , "    if (result.tag == 300) return def;"
        , "    return *result.data.c;"
        , "}"
        , ""
        , "static elm_value_t elm_Result_map(elm_value_t f, elm_value_t result) {"
        , "    if (result.tag == 300) return result;"
        , "    return elm_ok(elm_apply1((elm_closure_t *)f.data.p, *result.data.c));"
        , "}"
        , ""
        , "static elm_value_t elm_Result_mapError(elm_value_t f, elm_value_t result) {"
        , "    if (result.tag == 301) return result;"
        , "    return elm_err(elm_apply1((elm_closure_t *)f.data.p, *result.data.c));"
        , "}"
        , ""
        , "static elm_value_t elm_Result_andThen(elm_value_t f, elm_value_t result) {"
        , "    if (result.tag == 300) return result;"
        , "    return elm_apply1((elm_closure_t *)f.data.p, *result.data.c);"
        , "}"
        , ""
        , "static elm_value_t elm_Result_toMaybe(elm_value_t result) {"
        , "    if (result.tag == 300) return elm_nothing();"
        , "    return elm_just(*result.data.c);"
        , "}"
        , ""
        , "static elm_value_t elm_Result_map2(elm_value_t f, elm_value_t r1, elm_value_t r2) {"
        , "    if (r1.tag == 300) return r1;"
        , "    if (r2.tag == 300) return r2;"
        , "    elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, *r1.data.c);"
        , "    return elm_ok(elm_apply1((elm_closure_t *)f1.data.p, *r2.data.c));"
        , "}"
        , ""
        , "static elm_value_t elm_Result_map3(elm_value_t f, elm_value_t r1, elm_value_t r2, elm_value_t r3) {"
        , "    if (r1.tag == 300) return r1;"
        , "    if (r2.tag == 300) return r2;"
        , "    if (r3.tag == 300) return r3;"
        , "    elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, *r1.data.c);"
        , "    elm_value_t f2 = elm_apply1((elm_closure_t *)f1.data.p, *r2.data.c);"
        , "    return elm_ok(elm_apply1((elm_closure_t *)f2.data.p, *r3.data.c));"
        , "}"
        , ""
        , "static elm_value_t elm_Result_fromMaybe(elm_value_t errVal, elm_value_t maybe) {"
        , "    if (maybe.tag == 200) return elm_err(errVal);"
        , "    return elm_ok(*maybe.data.c);"
        , "}"
        , ""
        , "/* Task module (synchronous execution) */"
        , "/* Tags: 800 = Task_Fail, 801 = Task_Succeed */"
        , "static elm_value_t elm_task_succeed(elm_value_t val) {"
        , "    elm_value_t result;"
        , "    result.tag = 801;"
        , "    result.data.c = malloc(sizeof(elm_value_t));"
        , "    *result.data.c = val;"
        , "    result.next = NULL;"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t elm_task_fail(elm_value_t err) {"
        , "    elm_value_t result;"
        , "    result.tag = 800;"
        , "    result.data.c = malloc(sizeof(elm_value_t));"
        , "    *result.data.c = err;"
        , "    result.next = NULL;"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t elm_Task_succeed(elm_value_t val) {"
        , "    return elm_task_succeed(val);"
        , "}"
        , ""
        , "static elm_value_t elm_Task_fail(elm_value_t err) {"
        , "    return elm_task_fail(err);"
        , "}"
        , ""
        , "static elm_value_t elm_Task_map(elm_value_t f, elm_value_t task) {"
        , "    if (task.tag == 800) return task;"
        , "    return elm_task_succeed(elm_apply1((elm_closure_t *)f.data.p, *task.data.c));"
        , "}"
        , ""
        , "static elm_value_t elm_Task_andThen(elm_value_t f, elm_value_t task) {"
        , "    if (task.tag == 800) return task;"
        , "    return elm_apply1((elm_closure_t *)f.data.p, *task.data.c);"
        , "}"
        , ""
        , "static elm_value_t elm_Task_mapError(elm_value_t f, elm_value_t task) {"
        , "    if (task.tag == 801) return task;"
        , "    return elm_task_fail(elm_apply1((elm_closure_t *)f.data.p, *task.data.c));"
        , "}"
        , ""
        , "static elm_value_t elm_Task_onError(elm_value_t f, elm_value_t task) {"
        , "    if (task.tag == 801) return task;"
        , "    return elm_apply1((elm_closure_t *)f.data.p, *task.data.c);"
        , "}"
        , ""
        , "static elm_value_t elm_Task_map2(elm_value_t f, elm_value_t t1, elm_value_t t2) {"
        , "    if (t1.tag == 800) return t1;"
        , "    if (t2.tag == 800) return t2;"
        , "    elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, *t1.data.c);"
        , "    return elm_task_succeed(elm_apply1((elm_closure_t *)f1.data.p, *t2.data.c));"
        , "}"
        , ""
        , "static elm_value_t elm_Task_map3(elm_value_t f, elm_value_t t1, elm_value_t t2, elm_value_t t3) {"
        , "    if (t1.tag == 800) return t1;"
        , "    if (t2.tag == 800) return t2;"
        , "    if (t3.tag == 800) return t3;"
        , "    elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, *t1.data.c);"
        , "    elm_value_t f2 = elm_apply1((elm_closure_t *)f1.data.p, *t2.data.c);"
        , "    return elm_task_succeed(elm_apply1((elm_closure_t *)f2.data.p, *t3.data.c));"
        , "}"
        , ""
        , "static elm_value_t elm_Task_sequence(elm_value_t tasks) {"
        , "    elm_value_t results = elm_nil();"
        , "    elm_value_t xs = tasks;"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t task = *xs.data.c;"
        , "        if (task.tag == 800) return task;"
        , "        results = elm_cons(*task.data.c, results);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_task_succeed(elm_List_reverse(results));"
        , "}"
        , ""
        , "/* Time module */"
        , "static elm_value_t elm_Time_now(void) {"
        , "    struct timespec ts;"
        , "    clock_gettime(CLOCK_REALTIME, &ts);"
        , "    int64_t millis = (int64_t)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;"
        , "    return elm_task_succeed(elm_int(millis));"
        , "}"
        , ""
        , "static elm_value_t elm_Time_posixToMillis(elm_value_t posix) {"
        , "    return posix; /* Posix is just Int in milliseconds */"
        , "}"
        , ""
        , "static elm_value_t elm_Time_millisToPosix(elm_value_t millis) {"
        , "    return millis; /* Posix is just Int in milliseconds */"
        , "}"
        , ""
        , "/* Process module */"
        , "static elm_value_t elm_Process_sleep(elm_value_t ms) {"
        , "    /* Sleep for given milliseconds (as Float for Elm compatibility) */"
        , "    double millis = ms.data.f;"
        , "    struct timespec ts;"
        , "    ts.tv_sec = (time_t)(millis / 1000);"
        , "    ts.tv_nsec = (long)((millis - ts.tv_sec * 1000) * 1000000);"
        , "    nanosleep(&ts, NULL);"
        , "    return elm_task_succeed(elm_unit());"
        , "}"
        , ""
        , "/* String module */"
        , "static elm_value_t elm_String_length(elm_value_t s) {"
        , "    return elm_int((int64_t)strlen(s.data.s));"
        , "}"
        , ""
        , "static elm_value_t elm_String_isEmpty(elm_value_t s) {"
        , "    return elm_bool(s.data.s[0] == '\\0');"
        , "}"
        , ""
        , "static elm_value_t elm_String_reverse(elm_value_t s) {"
        , "    size_t len = strlen(s.data.s);"
        , "    char *buf = malloc(len + 1);"
        , "    for (size_t i = 0; i < len; i++) buf[i] = s.data.s[len - 1 - i];"
        , "    buf[len] = '\\0';"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "static elm_value_t elm_String_concat(elm_value_t strs) {"
        , "    size_t totalLen = 0;"
        , "    elm_value_t xs = strs;"
        , "    while (xs.tag == 101) {"
        , "        totalLen += strlen(xs.data.c->data.s);"
        , "        xs = *xs.next;"
        , "    }"
        , "    char *buf = malloc(totalLen + 1);"
        , "    char *p = buf;"
        , "    xs = strs;"
        , "    while (xs.tag == 101) {"
        , "        size_t len = strlen(xs.data.c->data.s);"
        , "        memcpy(p, xs.data.c->data.s, len);"
        , "        p += len;"
        , "        xs = *xs.next;"
        , "    }"
        , "    *p = '\\0';"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "static elm_value_t elm_String_append(elm_value_t a, elm_value_t b) {"
        , "    return elm_str_append(a, b);"
        , "}"
        , ""
        , "static elm_value_t elm_String_join(elm_value_t sep, elm_value_t strs) {"
        , "    if (strs.tag == 100) return elm_string(\"\");"
        , "    size_t sepLen = strlen(sep.data.s);"
        , "    size_t totalLen = 0;"
        , "    int count = 0;"
        , "    elm_value_t xs = strs;"
        , "    while (xs.tag == 101) {"
        , "        totalLen += strlen(xs.data.c->data.s);"
        , "        count++;"
        , "        xs = *xs.next;"
        , "    }"
        , "    totalLen += sepLen * (count - 1);"
        , "    char *buf = malloc(totalLen + 1);"
        , "    char *p = buf;"
        , "    xs = strs;"
        , "    int first = 1;"
        , "    while (xs.tag == 101) {"
        , "        if (!first) { memcpy(p, sep.data.s, sepLen); p += sepLen; }"
        , "        first = 0;"
        , "        size_t len = strlen(xs.data.c->data.s);"
        , "        memcpy(p, xs.data.c->data.s, len);"
        , "        p += len;"
        , "        xs = *xs.next;"
        , "    }"
        , "    *p = '\\0';"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "static elm_value_t elm_String_fromInt(elm_value_t n) {"
        , "    char *buf = malloc(32);"
        , "    snprintf(buf, 32, \"%ld\", (long)n.data.i);"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "static elm_value_t elm_String_toInt(elm_value_t s) {"
        , "    char *end;"
        , "    long val = strtol(s.data.s, &end, 10);"
        , "    if (*end != '\\0') return elm_nothing();"
        , "    return elm_just(elm_int(val));"
        , "}"
        , ""
        , "static elm_value_t elm_String_fromFloat(elm_value_t f) {"
        , "    char *buf = malloc(64);"
        , "    snprintf(buf, 64, \"%g\", f.data.f);"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "static elm_value_t elm_String_toFloat(elm_value_t s) {"
        , "    char *end;"
        , "    double val = strtod(s.data.s, &end);"
        , "    if (*end != '\\0' || end == s.data.s) return elm_nothing();"
        , "    return elm_just(elm_float(val));"
        , "}"
        , ""
        , "static elm_value_t elm_String_left(elm_value_t n, elm_value_t s) {"
        , "    size_t len = strlen(s.data.s);"
        , "    size_t take = n.data.i < 0 ? 0 : (size_t)n.data.i;"
        , "    if (take >= len) return s;"
        , "    char *buf = malloc(take + 1);"
        , "    memcpy(buf, s.data.s, take);"
        , "    buf[take] = '\\0';"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "static elm_value_t elm_String_right(elm_value_t n, elm_value_t s) {"
        , "    size_t len = strlen(s.data.s);"
        , "    size_t take = n.data.i < 0 ? 0 : (size_t)n.data.i;"
        , "    if (take >= len) return s;"
        , "    const char *start = s.data.s + len - take;"
        , "    char *buf = malloc(take + 1);"
        , "    memcpy(buf, start, take + 1);"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "static elm_value_t elm_String_dropLeft(elm_value_t n, elm_value_t s) {"
        , "    size_t len = strlen(s.data.s);"
        , "    size_t drop = n.data.i < 0 ? 0 : (size_t)n.data.i;"
        , "    if (drop >= len) return elm_string(\"\");"
        , "    const char *start = s.data.s + drop;"
        , "    size_t newLen = len - drop;"
        , "    char *buf = malloc(newLen + 1);"
        , "    memcpy(buf, start, newLen + 1);"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "static elm_value_t elm_String_dropRight(elm_value_t n, elm_value_t s) {"
        , "    size_t len = strlen(s.data.s);"
        , "    size_t drop = n.data.i < 0 ? 0 : (size_t)n.data.i;"
        , "    if (drop >= len) return elm_string(\"\");"
        , "    size_t newLen = len - drop;"
        , "    char *buf = malloc(newLen + 1);"
        , "    memcpy(buf, s.data.s, newLen);"
        , "    buf[newLen] = '\\0';"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "static elm_value_t elm_String_contains(elm_value_t sub, elm_value_t s) {"
        , "    return elm_bool(strstr(s.data.s, sub.data.s) != NULL);"
        , "}"
        , ""
        , "static elm_value_t elm_String_startsWith(elm_value_t prefix, elm_value_t s) {"
        , "    size_t prefixLen = strlen(prefix.data.s);"
        , "    return elm_bool(strncmp(s.data.s, prefix.data.s, prefixLen) == 0);"
        , "}"
        , ""
        , "static elm_value_t elm_String_endsWith(elm_value_t suffix, elm_value_t s) {"
        , "    size_t suffixLen = strlen(suffix.data.s);"
        , "    size_t sLen = strlen(s.data.s);"
        , "    if (suffixLen > sLen) return elm_bool(false);"
        , "    return elm_bool(strcmp(s.data.s + sLen - suffixLen, suffix.data.s) == 0);"
        , "}"
        , ""
        , "static elm_value_t elm_String_split(elm_value_t sep, elm_value_t str) {"
        , "    elm_value_t result = elm_nil();"
        , "    const char *s = str.data.s;"
        , "    const char *sepStr = sep.data.s;"
        , "    size_t sepLen = strlen(sepStr);"
        , "    if (sepLen == 0) {"
        , "        /* Empty separator - split into individual characters */"
        , "        size_t len = strlen(s);"
        , "        for (size_t i = len; i > 0; i--) {"
        , "            char *buf = malloc(2);"
        , "            buf[0] = s[i - 1];"
        , "            buf[1] = '\\0';"
        , "            result = elm_cons(elm_string(buf), result);"
        , "        }"
        , "        return result;"
        , "    }"
        , "    /* Build list in reverse then reverse */"
        , "    elm_value_t parts = elm_nil();"
        , "    const char *start = s;"
        , "    const char *found;"
        , "    while ((found = strstr(start, sepStr)) != NULL) {"
        , "        size_t partLen = found - start;"
        , "        char *buf = malloc(partLen + 1);"
        , "        memcpy(buf, start, partLen);"
        , "        buf[partLen] = '\\0';"
        , "        parts = elm_cons(elm_string(buf), parts);"
        , "        start = found + sepLen;"
        , "    }"
        , "    /* Add final part */"
        , "    size_t finalLen = strlen(start);"
        , "    char *finalBuf = malloc(finalLen + 1);"
        , "    memcpy(finalBuf, start, finalLen + 1);"
        , "    parts = elm_cons(elm_string(finalBuf), parts);"
        , "    return elm_List_reverse(parts);"
        , "}"
        , ""
        , "/* String.indexes - find all occurrences of a substring */"
        , "static elm_value_t elm_String_indexes(elm_value_t sub, elm_value_t str) {"
        , "    elm_value_t result = elm_nil();"
        , "    const char *s = str.data.s;"
        , "    const char *subStr = sub.data.s;"
        , "    size_t subLen = strlen(subStr);"
        , "    if (subLen == 0) return elm_nil(); /* Empty substring - return empty list */"
        , "    const char *p = s;"
        , "    while ((p = strstr(p, subStr)) != NULL) {"
        , "        int64_t idx = p - s;"
        , "        result = elm_cons(elm_int(idx), result);"
        , "        p++; /* Move past current match to find overlapping matches */"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "/* String.indices - alias for String.indexes */"
        , "static elm_value_t elm_String_indices(elm_value_t sub, elm_value_t str) {"
        , "    return elm_String_indexes(sub, str);"
        , "}"
        , ""
        , "/* String.replace - replace all occurrences of a substring */"
        , "static elm_value_t elm_String_replace(elm_value_t from, elm_value_t to, elm_value_t str) {"
        , "    const char *s = str.data.s;"
        , "    const char *fromStr = from.data.s;"
        , "    const char *toStr = to.data.s;"
        , "    size_t fromLen = strlen(fromStr);"
        , "    size_t toLen = strlen(toStr);"
        , "    if (fromLen == 0) return str; /* Empty pattern - return unchanged */"
        , "    /* Count occurrences */"
        , "    size_t count = 0;"
        , "    const char *p = s;"
        , "    while ((p = strstr(p, fromStr)) != NULL) {"
        , "        count++;"
        , "        p += fromLen;"
        , "    }"
        , "    if (count == 0) return str; /* No matches - return unchanged */"
        , "    /* Allocate result buffer */"
        , "    size_t sLen = strlen(s);"
        , "    size_t newLen = sLen + count * (toLen - fromLen);"
        , "    char *result = malloc(newLen + 1);"
        , "    char *dest = result;"
        , "    const char *src = s;"
        , "    while (*src) {"
        , "        const char *found = strstr(src, fromStr);"
        , "        if (found) {"
        , "            size_t copyLen = found - src;"
        , "            memcpy(dest, src, copyLen);"
        , "            dest += copyLen;"
        , "            memcpy(dest, toStr, toLen);"
        , "            dest += toLen;"
        , "            src = found + fromLen;"
        , "        } else {"
        , "            strcpy(dest, src);"
        , "            break;"
        , "        }"
        , "    }"
        , "    return elm_string(result);"
        , "}"
        , ""
        , "static elm_value_t elm_String_slice(elm_value_t start, elm_value_t end, elm_value_t s) {"
        , "    int64_t len = (int64_t)strlen(s.data.s);"
        , "    int64_t i = start.data.i;"
        , "    int64_t j = end.data.i;"
        , "    /* Handle negative indices */"
        , "    if (i < 0) i = len + i;"
        , "    if (j < 0) j = len + j;"
        , "    /* Clamp to bounds */"
        , "    if (i < 0) i = 0;"
        , "    if (j < 0) j = 0;"
        , "    if (i > len) i = len;"
        , "    if (j > len) j = len;"
        , "    if (i >= j) return elm_string(\"\");"
        , "    size_t sliceLen = (size_t)(j - i);"
        , "    char *buf = malloc(sliceLen + 1);"
        , "    memcpy(buf, s.data.s + i, sliceLen);"
        , "    buf[sliceLen] = '\\0';"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "static elm_value_t elm_String_toUpper(elm_value_t s) {"
        , "    size_t len = strlen(s.data.s);"
        , "    char *buf = malloc(len + 1);"
        , "    for (size_t i = 0; i <= len; i++) {"
        , "        char c = s.data.s[i];"
        , "        buf[i] = (c >= 'a' && c <= 'z') ? c - 32 : c;"
        , "    }"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "static elm_value_t elm_String_toLower(elm_value_t s) {"
        , "    size_t len = strlen(s.data.s);"
        , "    char *buf = malloc(len + 1);"
        , "    for (size_t i = 0; i <= len; i++) {"
        , "        char c = s.data.s[i];"
        , "        buf[i] = (c >= 'A' && c <= 'Z') ? c + 32 : c;"
        , "    }"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "static elm_value_t elm_String_trim(elm_value_t s) {"
        , "    const char *str = s.data.s;"
        , "    size_t len = strlen(str);"
        , "    size_t start = 0;"
        , "    while (start < len && (str[start] == ' ' || str[start] == '\\t' || str[start] == '\\n' || str[start] == '\\r')) start++;"
        , "    size_t end = len;"
        , "    while (end > start && (str[end-1] == ' ' || str[end-1] == '\\t' || str[end-1] == '\\n' || str[end-1] == '\\r')) end--;"
        , "    size_t newLen = end - start;"
        , "    char *buf = malloc(newLen + 1);"
        , "    memcpy(buf, str + start, newLen);"
        , "    buf[newLen] = '\\0';"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "static elm_value_t elm_String_trimLeft(elm_value_t s) {"
        , "    const char *str = s.data.s;"
        , "    while (*str == ' ' || *str == '\\t' || *str == '\\n' || *str == '\\r') str++;"
        , "    size_t len = strlen(str);"
        , "    char *buf = malloc(len + 1);"
        , "    memcpy(buf, str, len + 1);"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "static elm_value_t elm_String_trimRight(elm_value_t s) {"
        , "    size_t len = strlen(s.data.s);"
        , "    while (len > 0 && (s.data.s[len-1] == ' ' || s.data.s[len-1] == '\\t' || s.data.s[len-1] == '\\n' || s.data.s[len-1] == '\\r')) len--;"
        , "    char *buf = malloc(len + 1);"
        , "    memcpy(buf, s.data.s, len);"
        , "    buf[len] = '\\0';"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "/* String.toList - convert string to list of chars (as ints) */"
        , "static elm_value_t elm_String_toList(elm_value_t s) {"
        , "    size_t len = strlen(s.data.s);"
        , "    elm_value_t result = elm_nil();"
        , "    for (size_t i = len; i > 0; i--) {"
        , "        result = elm_cons(elm_int((int64_t)(unsigned char)s.data.s[i-1]), result);"
        , "    }"
        , "    return result;"
        , "}"
        , ""
        , "/* String.fromList - convert list of chars (as ints) to string */"
        , "static elm_value_t elm_String_fromList(elm_value_t xs) {"
        , "    /* First count the list length */"
        , "    size_t len = 0;"
        , "    elm_value_t tmp = xs;"
        , "    while (tmp.tag == 101) {"
        , "        len++;"
        , "        tmp = *tmp.next;"
        , "    }"
        , "    char *buf = malloc(len + 1);"
        , "    size_t i = 0;"
        , "    while (xs.tag == 101) {"
        , "        buf[i++] = (char)xs.data.c->data.i;"
        , "        xs = *xs.next;"
        , "    }"
        , "    buf[len] = '\\0';"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "/* String.padLeft - pad string to length with char (as int) */"
        , "static elm_value_t elm_String_padLeft(elm_value_t n, elm_value_t ch, elm_value_t s) {"
        , "    size_t len = strlen(s.data.s);"
        , "    size_t target = (size_t)n.data.i;"
        , "    if (len >= target) return s;"
        , "    size_t pad = target - len;"
        , "    char *buf = malloc(target + 1);"
        , "    char c = (char)ch.data.i;"
        , "    for (size_t i = 0; i < pad; i++) buf[i] = c;"
        , "    memcpy(buf + pad, s.data.s, len);"
        , "    buf[target] = '\\0';"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "/* String.padRight - pad string to length with char (as int) */"
        , "static elm_value_t elm_String_padRight(elm_value_t n, elm_value_t ch, elm_value_t s) {"
        , "    size_t len = strlen(s.data.s);"
        , "    size_t target = (size_t)n.data.i;"
        , "    if (len >= target) return s;"
        , "    size_t pad = target - len;"
        , "    char *buf = malloc(target + 1);"
        , "    char c = (char)ch.data.i;"
        , "    memcpy(buf, s.data.s, len);"
        , "    for (size_t i = len; i < target; i++) buf[i] = c;"
        , "    buf[target] = '\\0';"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "/* String.cons - prepend a char to a string */"
        , "static elm_value_t elm_String_cons(elm_value_t ch, elm_value_t s) {"
        , "    size_t len = strlen(s.data.s);"
        , "    char *buf = malloc(len + 2);"
        , "    buf[0] = (char)ch.data.i;"
        , "    memcpy(buf + 1, s.data.s, len + 1);"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "/* String.fromChar - convert a single Char to String */"
        , "static elm_value_t elm_String_fromChar(elm_value_t ch) {"
        , "    char *buf = malloc(2);"
        , "    buf[0] = (char)ch.data.i;"
        , "    buf[1] = '\\0';"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "/* String.uncons - split first char from string */"
        , "static elm_value_t elm_String_uncons(elm_value_t s) {"
        , "    if (s.data.s[0] == '\\0') return elm_nothing();"
        , "    elm_value_t ch = elm_char(s.data.s[0]);"
        , "    char *rest = strdup(s.data.s + 1);"
        , "    return elm_just(elm_tuple2(ch, elm_string(rest)));"
        , "}"
        , ""
        , "/* String.repeat - repeat string n times */"
        , "static elm_value_t elm_String_repeat(elm_value_t n, elm_value_t s) {"
        , "    int64_t times = n.data.i;"
        , "    if (times <= 0) return elm_string(strdup(\"\"));"
        , "    size_t len = strlen(s.data.s);"
        , "    size_t total = len * times;"
        , "    char *buf = malloc(total + 1);"
        , "    for (int64_t i = 0; i < times; i++) {"
        , "        memcpy(buf + i * len, s.data.s, len);"
        , "    }"
        , "    buf[total] = '\\0';"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "/* String.words - split string by whitespace */"
        , "static elm_value_t elm_String_words(elm_value_t s) {"
        , "    elm_value_t result = elm_nil();"
        , "    const char *str = s.data.s;"
        , "    size_t len = strlen(str);"
        , "    size_t start = 0;"
        , "    for (size_t i = 0; i <= len; i++) {"
        , "        if (i == len || str[i] == ' ' || str[i] == '\\t' || str[i] == '\\n' || str[i] == '\\r') {"
        , "            if (i > start) {"
        , "                size_t word_len = i - start;"
        , "                char *word = malloc(word_len + 1);"
        , "                memcpy(word, str + start, word_len);"
        , "                word[word_len] = '\\0';"
        , "                result = elm_cons(elm_string(word), result);"
        , "            }"
        , "            start = i + 1;"
        , "        }"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "/* String.lines - split string by newlines */"
        , "static elm_value_t elm_String_lines(elm_value_t s) {"
        , "    elm_value_t result = elm_nil();"
        , "    const char *str = s.data.s;"
        , "    size_t len = strlen(str);"
        , "    size_t start = 0;"
        , "    for (size_t i = 0; i <= len; i++) {"
        , "        if (i == len || str[i] == '\\n') {"
        , "            size_t line_len = i - start;"
        , "            /* Skip \\r before \\n for Windows line endings */"
        , "            if (line_len > 0 && str[start + line_len - 1] == '\\r') line_len--;"
        , "            char *line = malloc(line_len + 1);"
        , "            memcpy(line, str + start, line_len);"
        , "            line[line_len] = '\\0';"
        , "            result = elm_cons(elm_string(line), result);"
        , "            start = i + 1;"
        , "        }"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "/* String.foldl - fold left over characters */"
        , "static elm_value_t elm_String_foldl(elm_value_t f, elm_value_t acc, elm_value_t s) {"
        , "    const char *str = s.data.s;"
        , "    for (size_t i = 0; str[i] != '\\0'; i++) {"
        , "        elm_value_t ch = elm_char(str[i]);"
        , "        elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, ch);"
        , "        acc = elm_apply1((elm_closure_t *)f1.data.p, acc);"
        , "    }"
        , "    return acc;"
        , "}"
        , ""
        , "/* String.foldr - fold right over characters */"
        , "static elm_value_t elm_String_foldr(elm_value_t f, elm_value_t acc, elm_value_t s) {"
        , "    const char *str = s.data.s;"
        , "    size_t len = strlen(str);"
        , "    for (size_t i = len; i > 0; i--) {"
        , "        elm_value_t ch = elm_char(str[i-1]);"
        , "        elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, ch);"
        , "        acc = elm_apply1((elm_closure_t *)f1.data.p, acc);"
        , "    }"
        , "    return acc;"
        , "}"
        , ""
        , "/* String.any - check if any character satisfies predicate */"
        , "static elm_value_t elm_String_any(elm_value_t pred, elm_value_t s) {"
        , "    const char *str = s.data.s;"
        , "    for (size_t i = 0; str[i] != '\\0'; i++) {"
        , "        elm_value_t ch = elm_char(str[i]);"
        , "        elm_value_t result = elm_apply1((elm_closure_t *)pred.data.p, ch);"
        , "        if (result.data.i) return elm_bool(true);"
        , "    }"
        , "    return elm_bool(false);"
        , "}"
        , ""
        , "/* String.all - check if all characters satisfy predicate */"
        , "static elm_value_t elm_String_all(elm_value_t pred, elm_value_t s) {"
        , "    const char *str = s.data.s;"
        , "    for (size_t i = 0; str[i] != '\\0'; i++) {"
        , "        elm_value_t ch = elm_char(str[i]);"
        , "        elm_value_t result = elm_apply1((elm_closure_t *)pred.data.p, ch);"
        , "        if (!result.data.i) return elm_bool(false);"
        , "    }"
        , "    return elm_bool(true);"
        , "}"
        , ""
        , "/* String.filter - keep only characters satisfying predicate */"
        , "static elm_value_t elm_String_filter(elm_value_t pred, elm_value_t s) {"
        , "    const char *str = s.data.s;"
        , "    size_t len = strlen(str);"
        , "    char *buf = malloc(len + 1);"
        , "    size_t j = 0;"
        , "    for (size_t i = 0; i < len; i++) {"
        , "        elm_value_t ch = elm_char(str[i]);"
        , "        elm_value_t keep = elm_apply1((elm_closure_t *)pred.data.p, ch);"
        , "        if (keep.data.i) buf[j++] = str[i];"
        , "    }"
        , "    buf[j] = '\\0';"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "/* String.map - transform each character */"
        , "static elm_value_t elm_String_map(elm_value_t f, elm_value_t s) {"
        , "    const char *str = s.data.s;"
        , "    size_t len = strlen(str);"
        , "    char *buf = malloc(len + 1);"
        , "    for (size_t i = 0; i < len; i++) {"
        , "        elm_value_t ch = elm_char(str[i]);"
        , "        elm_value_t newCh = elm_apply1((elm_closure_t *)f.data.p, ch);"
        , "        buf[i] = (char)newCh.data.i;"
        , "    }"
        , "    buf[len] = '\\0';"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "/* Tuple module */"
        , "static elm_value_t elm_Tuple_pair(elm_value_t a, elm_value_t b) {"
        , "    return elm_tuple2(a, b);"
        , "}"
        , ""
        , "static elm_value_t elm_Tuple_first(elm_value_t tuple) {"
        , "    return *tuple.data.c;"
        , "}"
        , ""
        , "static elm_value_t elm_Tuple_second(elm_value_t tuple) {"
        , "    return *tuple.next;"
        , "}"
        , ""
        , "static elm_value_t elm_Tuple_mapFirst(elm_value_t f, elm_value_t tuple) {"
        , "    elm_value_t first = *tuple.data.c;"
        , "    elm_value_t second = *tuple.next;"
        , "    elm_value_t newFirst = elm_apply1((elm_closure_t *)f.data.p, first);"
        , "    return elm_tuple2(newFirst, second);"
        , "}"
        , ""
        , "static elm_value_t elm_Tuple_mapSecond(elm_value_t f, elm_value_t tuple) {"
        , "    elm_value_t first = *tuple.data.c;"
        , "    elm_value_t second = *tuple.next;"
        , "    elm_value_t newSecond = elm_apply1((elm_closure_t *)f.data.p, second);"
        , "    return elm_tuple2(first, newSecond);"
        , "}"
        , ""
        , "static elm_value_t elm_Tuple_mapBoth(elm_value_t f, elm_value_t g, elm_value_t tuple) {"
        , "    elm_value_t first = *tuple.data.c;"
        , "    elm_value_t second = *tuple.next;"
        , "    elm_value_t newFirst = elm_apply1((elm_closure_t *)f.data.p, first);"
        , "    elm_value_t newSecond = elm_apply1((elm_closure_t *)g.data.p, second);"
        , "    return elm_tuple2(newFirst, newSecond);"
        , "}"
        , ""
        , "/* Dict module - implemented as association list of (key, value) tuples */"
        , "/* Dict is a list where each element is a tuple2 of (key, value) */"
        , "static elm_value_t elm_Dict_empty(void) {"
        , "    return elm_nil();"
        , "}"
        , ""
        , "static elm_value_t elm_Dict_singleton(elm_value_t key, elm_value_t value) {"
        , "    return elm_cons(elm_tuple2(key, value), elm_nil());"
        , "}"
        , ""
        , "/* Helper to compare keys (handles Int and String) */"
        , "static int elm_dict_key_eq(elm_value_t k1, elm_value_t k2) {"
        , "    if (k1.tag != k2.tag) return 0;"
        , "    switch (k1.tag) {"
        , "        case 0: return k1.data.i == k2.data.i; /* Int */"
        , "        case 2: return strcmp(k1.data.s, k2.data.s) == 0; /* String */"
        , "        default: return k1.data.i == k2.data.i; /* fallback to int comparison */"
        , "    }"
        , "}"
        , ""
        , "/* Helper to check if dict contains key */"
        , "static int elm_dict_has_key(elm_value_t key, elm_value_t dict) {"
        , "    while (dict.tag == 101) {"
        , "        elm_value_t entry = *dict.data.c;"
        , "        elm_value_t k = *entry.data.c;"
        , "        if (elm_dict_key_eq(k, key)) return 1;"
        , "        dict = *dict.next;"
        , "    }"
        , "    return 0;"
        , "}"
        , ""
        , "static elm_value_t elm_Dict_insert(elm_value_t key, elm_value_t value, elm_value_t dict) {"
        , "    /* Insert or update: keeps insertion order, removes old key if present */"
        , "    elm_value_t result = elm_nil();"
        , "    elm_value_t xs = dict;"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t entry = *xs.data.c;"
        , "        elm_value_t k = *entry.data.c;"
        , "        if (!elm_dict_key_eq(k, key)) {"
        , "            result = elm_cons(entry, result);"
        , "        }"
        , "        xs = *xs.next;"
        , "    }"
        , "    /* Append new entry at end to maintain insertion order */"
        , "    result = elm_List_append(elm_List_reverse(result), elm_cons(elm_tuple2(key, value), elm_nil()));"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t elm_Dict_get(elm_value_t key, elm_value_t dict) {"
        , "    elm_value_t xs = dict;"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t entry = *xs.data.c;"
        , "        elm_value_t k = *entry.data.c;"
        , "        elm_value_t v = *entry.next;"
        , "        if (elm_dict_key_eq(k, key)) {"
        , "            return elm_just(v);"
        , "        }"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_nothing();"
        , "}"
        , ""
        , "static elm_value_t elm_Dict_remove(elm_value_t key, elm_value_t dict) {"
        , "    elm_value_t result = elm_nil();"
        , "    elm_value_t xs = dict;"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t entry = *xs.data.c;"
        , "        elm_value_t k = *entry.data.c;"
        , "        if (!elm_dict_key_eq(k, key)) {"
        , "            result = elm_cons(entry, result);"
        , "        }"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "static elm_value_t elm_Dict_member(elm_value_t key, elm_value_t dict) {"
        , "    elm_value_t xs = dict;"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t entry = *xs.data.c;"
        , "        elm_value_t k = *entry.data.c;"
        , "        if (elm_dict_key_eq(k, key)) {"
        , "            return elm_bool(true);"
        , "        }"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_bool(false);"
        , "}"
        , ""
        , "static elm_value_t elm_Dict_size(elm_value_t dict) {"
        , "    return elm_List_length(dict);"
        , "}"
        , ""
        , "static elm_value_t elm_Dict_isEmpty(elm_value_t dict) {"
        , "    return elm_bool(dict.tag == 100);"
        , "}"
        , ""
        , "static elm_value_t elm_Dict_keys(elm_value_t dict) {"
        , "    elm_value_t result = elm_nil();"
        , "    elm_value_t xs = dict;"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t entry = *xs.data.c;"
        , "        elm_value_t k = *entry.data.c;"
        , "        result = elm_cons(k, result);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "static elm_value_t elm_Dict_values(elm_value_t dict) {"
        , "    elm_value_t result = elm_nil();"
        , "    elm_value_t xs = dict;"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t entry = *xs.data.c;"
        , "        elm_value_t v = *entry.next;"
        , "        result = elm_cons(v, result);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "static elm_value_t elm_Dict_toList(elm_value_t dict) {"
        , "    return dict;  /* Dict is already a list of tuples */"
        , "}"
        , ""
        , "static elm_value_t elm_Dict_fromList(elm_value_t list) {"
        , "    elm_value_t result = elm_nil();"
        , "    elm_value_t xs = list;"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t entry = *xs.data.c;"
        , "        elm_value_t key = *entry.data.c;"
        , "        elm_value_t value = *entry.next;"
        , "        result = elm_Dict_insert(key, value, result);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t elm_Dict_update(elm_value_t key, elm_value_t updater, elm_value_t dict) {"
        , "    elm_value_t maybeValue = elm_Dict_get(key, dict);"
        , "    elm_value_t newMaybeValue = elm_apply1((elm_closure_t *)updater.data.p, maybeValue);"
        , "    if (newMaybeValue.tag == 200) { /* Nothing */"
        , "        return elm_Dict_remove(key, dict);"
        , "    } else { /* Just value */"
        , "        return elm_Dict_insert(key, *newMaybeValue.data.c, dict);"
        , "    }"
        , "}"
        , ""
        , "static elm_value_t elm_Dict_map(elm_value_t f, elm_value_t dict) {"
        , "    elm_value_t result = elm_nil();"
        , "    elm_value_t xs = dict;"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t entry = *xs.data.c;"
        , "        elm_value_t k = *entry.data.c;"
        , "        elm_value_t v = *entry.next;"
        , "        elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, k);"
        , "        elm_value_t newV = elm_apply1((elm_closure_t *)f1.data.p, v);"
        , "        result = elm_cons(elm_tuple2(k, newV), result);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "static elm_value_t elm_Dict_filter(elm_value_t pred, elm_value_t dict) {"
        , "    elm_value_t result = elm_nil();"
        , "    elm_value_t xs = dict;"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t entry = *xs.data.c;"
        , "        elm_value_t k = *entry.data.c;"
        , "        elm_value_t v = *entry.next;"
        , "        elm_value_t pred1 = elm_apply1((elm_closure_t *)pred.data.p, k);"
        , "        elm_value_t keep = elm_apply1((elm_closure_t *)pred1.data.p, v);"
        , "        if (keep.tag == 5) { /* True */"
        , "            result = elm_cons(entry, result);"
        , "        }"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "static elm_value_t elm_Dict_foldl(elm_value_t f, elm_value_t acc, elm_value_t dict) {"
        , "    elm_value_t result = acc;"
        , "    elm_value_t xs = dict;"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t entry = *xs.data.c;"
        , "        elm_value_t k = *entry.data.c;"
        , "        elm_value_t v = *entry.next;"
        , "        elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, k);"
        , "        elm_value_t f2 = elm_apply1((elm_closure_t *)f1.data.p, v);"
        , "        result = elm_apply1((elm_closure_t *)f2.data.p, result);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t elm_Dict_foldr(elm_value_t f, elm_value_t acc, elm_value_t dict) {"
        , "    /* foldr processes in reverse order - reverse the list first */"
        , "    elm_value_t reversed = elm_List_reverse(dict);"
        , "    return elm_Dict_foldl(f, acc, reversed);"
        , "}"
        , ""
        , "static elm_value_t elm_Dict_partition(elm_value_t pred, elm_value_t dict) {"
        , "    elm_value_t trues = elm_nil();"
        , "    elm_value_t falses = elm_nil();"
        , "    while (dict.tag == 101) {"
        , "        elm_value_t entry = *dict.data.c; /* (key, value) tuple */"
        , "        elm_value_t key = *entry.data.c;"
        , "        elm_value_t value = *entry.next;"
        , "        elm_value_t pred1 = elm_apply1((elm_closure_t *)pred.data.p, key);"
        , "        elm_value_t keep = elm_apply1((elm_closure_t *)pred1.data.p, value);"
        , "        if (keep.data.i) {"
        , "            trues = elm_cons(entry, trues);"
        , "        } else {"
        , "            falses = elm_cons(entry, falses);"
        , "        }"
        , "        dict = *dict.next;"
        , "    }"
        , "    return elm_tuple2(elm_List_reverse(trues), elm_List_reverse(falses));"
        , "}"
        , ""
        , "/* Dict.union - combine two dicts (left-biased) */"
        , "static elm_value_t elm_Dict_union(elm_value_t dict1, elm_value_t dict2) {"
        , "    /* Add all entries from dict1, then entries from dict2 not in dict1 */"
        , "    elm_value_t result = dict1;"
        , "    while (dict2.tag == 101) {"
        , "        elm_value_t entry = *dict2.data.c;"
        , "        elm_value_t key = *entry.data.c;"
        , "        if (!elm_dict_has_key(key, dict1)) {"
        , "            result = elm_cons(entry, result);"
        , "        }"
        , "        dict2 = *dict2.next;"
        , "    }"
        , "    return result;"
        , "}"
        , ""
        , "/* Dict.diff - entries in dict1 not in dict2 */"
        , "static elm_value_t elm_Dict_diff(elm_value_t dict1, elm_value_t dict2) {"
        , "    elm_value_t result = elm_nil();"
        , "    while (dict1.tag == 101) {"
        , "        elm_value_t entry = *dict1.data.c;"
        , "        elm_value_t key = *entry.data.c;"
        , "        if (!elm_dict_has_key(key, dict2)) {"
        , "            result = elm_cons(entry, result);"
        , "        }"
        , "        dict1 = *dict1.next;"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "/* Dict.intersect - entries in both dicts (values from dict1) */"
        , "static elm_value_t elm_Dict_intersect(elm_value_t dict1, elm_value_t dict2) {"
        , "    elm_value_t result = elm_nil();"
        , "    while (dict1.tag == 101) {"
        , "        elm_value_t entry = *dict1.data.c;"
        , "        elm_value_t key = *entry.data.c;"
        , "        if (elm_dict_has_key(key, dict2)) {"
        , "            result = elm_cons(entry, result);"
        , "        }"
        , "        dict1 = *dict1.next;"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "/* ===== SET MODULE ===== */"
        , "/* Set implemented as a list of unique elements */"
        , ""
        , "/* Helper to check if element is in set (uses same comparison as Dict) */"
        , "static int elm_set_member_helper(elm_value_t elem, elm_value_t set) {"
        , "    elm_value_t xs = set;"
        , "    while (xs.tag == 101) {"
        , "        if (elm_dict_key_eq(*xs.data.c, elem)) {"
        , "            return 1;"
        , "        }"
        , "        xs = *xs.next;"
        , "    }"
        , "    return 0;"
        , "}"
        , ""
        , "static elm_value_t elm_Set_empty(void) {"
        , "    return elm_nil();"
        , "}"
        , ""
        , "static elm_value_t elm_Set_singleton(elm_value_t elem) {"
        , "    return elm_cons(elem, elm_nil());"
        , "}"
        , ""
        , "static elm_value_t elm_Set_insert(elm_value_t elem, elm_value_t set) {"
        , "    if (elm_set_member_helper(elem, set)) {"
        , "        return set;"
        , "    }"
        , "    return elm_cons(elem, set);"
        , "}"
        , ""
        , "static elm_value_t elm_Set_remove(elm_value_t elem, elm_value_t set) {"
        , "    elm_value_t result = elm_nil();"
        , "    elm_value_t xs = set;"
        , "    while (xs.tag == 101) {"
        , "        if (!elm_dict_key_eq(*xs.data.c, elem)) {"
        , "            result = elm_cons(*xs.data.c, result);"
        , "        }"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "static elm_value_t elm_Set_member(elm_value_t elem, elm_value_t set) {"
        , "    return elm_bool(elm_set_member_helper(elem, set));"
        , "}"
        , ""
        , "static elm_value_t elm_Set_size(elm_value_t set) {"
        , "    return elm_List_length(set);"
        , "}"
        , ""
        , "static elm_value_t elm_Set_isEmpty(elm_value_t set) {"
        , "    return elm_bool(set.tag == 100);"
        , "}"
        , ""
        , "static elm_value_t elm_Set_toList(elm_value_t set) {"
        , "    return set;"
        , "}"
        , ""
        , "static elm_value_t elm_Set_fromList(elm_value_t list) {"
        , "    elm_value_t result = elm_nil();"
        , "    elm_value_t xs = list;"
        , "    while (xs.tag == 101) {"
        , "        result = elm_Set_insert(*xs.data.c, result);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t elm_Set_union(elm_value_t set1, elm_value_t set2) {"
        , "    elm_value_t result = set2;"
        , "    elm_value_t xs = set1;"
        , "    while (xs.tag == 101) {"
        , "        result = elm_Set_insert(*xs.data.c, result);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t elm_Set_intersect(elm_value_t set1, elm_value_t set2) {"
        , "    elm_value_t result = elm_nil();"
        , "    elm_value_t xs = set1;"
        , "    while (xs.tag == 101) {"
        , "        if (elm_set_member_helper(*xs.data.c, set2)) {"
        , "            result = elm_cons(*xs.data.c, result);"
        , "        }"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "static elm_value_t elm_Set_diff(elm_value_t set1, elm_value_t set2) {"
        , "    elm_value_t result = elm_nil();"
        , "    elm_value_t xs = set1;"
        , "    while (xs.tag == 101) {"
        , "        if (!elm_set_member_helper(*xs.data.c, set2)) {"
        , "            result = elm_cons(*xs.data.c, result);"
        , "        }"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "static elm_value_t elm_Set_map(elm_value_t f, elm_value_t set) {"
        , "    elm_value_t result = elm_nil();"
        , "    elm_value_t xs = set;"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t mapped = elm_apply1((elm_closure_t *)f.data.p, *xs.data.c);"
        , "        result = elm_Set_insert(mapped, result);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t elm_Set_filter(elm_value_t pred, elm_value_t set) {"
        , "    elm_value_t result = elm_nil();"
        , "    elm_value_t xs = set;"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t keep = elm_apply1((elm_closure_t *)pred.data.p, *xs.data.c);"
        , "        if (keep.tag == 5) {"
        , "            result = elm_cons(*xs.data.c, result);"
        , "        }"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "static elm_value_t elm_Set_foldl(elm_value_t f, elm_value_t acc, elm_value_t set) {"
        , "    elm_value_t result = acc;"
        , "    elm_value_t xs = set;"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t f1 = elm_apply1((elm_closure_t *)f.data.p, *xs.data.c);"
        , "        result = elm_apply1((elm_closure_t *)f1.data.p, result);"
        , "        xs = *xs.next;"
        , "    }"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t elm_Set_foldr(elm_value_t f, elm_value_t acc, elm_value_t set) {"
        , "    elm_value_t reversed = elm_List_reverse(set);"
        , "    return elm_Set_foldl(f, acc, reversed);"
        , "}"
        , ""
        , "static elm_value_t elm_Set_partition(elm_value_t pred, elm_value_t set) {"
        , "    elm_value_t trues = elm_nil();"
        , "    elm_value_t falses = elm_nil();"
        , "    while (set.tag == 101) {"
        , "        elm_value_t elem = *set.data.c;"
        , "        elm_value_t keep = elm_apply1((elm_closure_t *)pred.data.p, elem);"
        , "        if (keep.data.i) {"
        , "            trues = elm_cons(elem, trues);"
        , "        } else {"
        , "            falses = elm_cons(elem, falses);"
        , "        }"
        , "        set = *set.next;"
        , "    }"
        , "    return elm_tuple2(elm_List_reverse(trues), elm_List_reverse(falses));"
        , "}"
        , ""
        , "/* ===== CHAR MODULE ===== */"
        , ""
        , "static elm_value_t elm_Char_toCode(elm_value_t c) {"
        , "    return elm_int(c.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm_Char_fromCode(elm_value_t code) {"
        , "    return (elm_value_t){ .tag = 3, .data.i = code.data.i, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Char_isDigit(elm_value_t c) {"
        , "    int ch = (int)c.data.i;"
        , "    return elm_bool(ch >= '0' && ch <= '9');"
        , "}"
        , ""
        , "static elm_value_t elm_Char_isLower(elm_value_t c) {"
        , "    int ch = (int)c.data.i;"
        , "    return elm_bool(ch >= 'a' && ch <= 'z');"
        , "}"
        , ""
        , "static elm_value_t elm_Char_isUpper(elm_value_t c) {"
        , "    int ch = (int)c.data.i;"
        , "    return elm_bool(ch >= 'A' && ch <= 'Z');"
        , "}"
        , ""
        , "static elm_value_t elm_Char_isAlpha(elm_value_t c) {"
        , "    int ch = (int)c.data.i;"
        , "    return elm_bool((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'));"
        , "}"
        , ""
        , "static elm_value_t elm_Char_isAlphaNum(elm_value_t c) {"
        , "    int ch = (int)c.data.i;"
        , "    return elm_bool((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9'));"
        , "}"
        , ""
        , "static elm_value_t elm_Char_toUpper(elm_value_t c) {"
        , "    int ch = (int)c.data.i;"
        , "    if (ch >= 'a' && ch <= 'z') ch = ch - 'a' + 'A';"
        , "    return (elm_value_t){ .tag = 3, .data.i = ch, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Char_toLower(elm_value_t c) {"
        , "    int ch = (int)c.data.i;"
        , "    if (ch >= 'A' && ch <= 'Z') ch = ch - 'A' + 'a';"
        , "    return (elm_value_t){ .tag = 3, .data.i = ch, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Char_isHexDigit(elm_value_t c) {"
        , "    int ch = (int)c.data.i;"
        , "    return elm_bool((ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F'));"
        , "}"
        , ""
        , "static elm_value_t elm_Char_isOctDigit(elm_value_t c) {"
        , "    int ch = (int)c.data.i;"
        , "    return elm_bool(ch >= '0' && ch <= '7');"
        , "}"
        , ""
        , "static elm_value_t elm_Char_isSpace(elm_value_t c) {"
        , "    int ch = (int)c.data.i;"
        , "    return elm_bool(ch == ' ' || ch == '\\t' || ch == '\\n' || ch == '\\r');"
        , "}"
        , ""
        , "/* ===== BITWISE MODULE ===== */"
        , ""
        , "static elm_value_t elm_Bitwise_and(elm_value_t a, elm_value_t b) {"
        , "    return elm_int(a.data.i & b.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm_Bitwise_or(elm_value_t a, elm_value_t b) {"
        , "    return elm_int(a.data.i | b.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm_Bitwise_xor(elm_value_t a, elm_value_t b) {"
        , "    return elm_int(a.data.i ^ b.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm_Bitwise_complement(elm_value_t a) {"
        , "    return elm_int(~a.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm_Bitwise_shiftLeftBy(elm_value_t bits, elm_value_t value) {"
        , "    return elm_int(value.data.i << bits.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm_Bitwise_shiftRightBy(elm_value_t bits, elm_value_t value) {"
        , "    return elm_int(value.data.i >> bits.data.i);"
        , "}"
        , ""
        , "static elm_value_t elm_Bitwise_shiftRightZfBy(elm_value_t bits, elm_value_t value) {"
        , "    /* Unsigned right shift - need to cast to unsigned first */"
        , "    return elm_int((int64_t)((uint64_t)value.data.i >> bits.data.i));"
        , "}"
        , ""
        , "/* ===== ARRAY MODULE ===== */"
        , "/* Array implemented as a list for simplicity - can optimize to true arrays later */"
        , ""
        , "static elm_value_t elm_Array_empty(void) {"
        , "    return elm_nil();"
        , "}"
        , ""
        , "static elm_value_t elm_Array_fromList(elm_value_t list) {"
        , "    return list;  /* Array is just a list for now */"
        , "}"
        , ""
        , "static elm_value_t elm_Array_initialize(elm_value_t n, elm_value_t f) {"
        , "    int64_t size = n.data.i;"
        , "    if (size <= 0) return elm_nil();"
        , "    elm_value_t result = elm_nil();"
        , "    for (int64_t i = size - 1; i >= 0; i--) {"
        , "        elm_value_t elem = elm_apply1((elm_closure_t *)f.data.p, elm_int(i));"
        , "        result = elm_cons(elem, result);"
        , "    }"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t elm_Array_repeat(elm_value_t n, elm_value_t value) {"
        , "    int64_t size = n.data.i;"
        , "    if (size <= 0) return elm_nil();"
        , "    elm_value_t result = elm_nil();"
        , "    for (int64_t i = 0; i < size; i++) {"
        , "        result = elm_cons(value, result);"
        , "    }"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t elm_Array_toList(elm_value_t arr) {"
        , "    return arr;  /* Array is just a list for now */"
        , "}"
        , ""
        , "static elm_value_t elm_Array_toIndexedList(elm_value_t arr) {"
        , "    elm_value_t result = elm_nil();"
        , "    elm_value_t xs = arr;"
        , "    int64_t i = 0;"
        , "    while (xs.tag == 101) {"
        , "        result = elm_cons(elm_tuple2(elm_int(i), *xs.data.c), result);"
        , "        i++;"
        , "        xs = *xs.next;"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "static elm_value_t elm_Array_length(elm_value_t arr) {"
        , "    return elm_List_length(arr);"
        , "}"
        , ""
        , "static elm_value_t elm_Array_isEmpty(elm_value_t arr) {"
        , "    return elm_bool(arr.tag == 100);"
        , "}"
        , ""
        , "static elm_value_t elm_Array_get(elm_value_t index, elm_value_t arr) {"
        , "    int64_t i = index.data.i;"
        , "    if (i < 0) return elm_nothing();"
        , "    elm_value_t xs = arr;"
        , "    while (xs.tag == 101 && i > 0) {"
        , "        xs = *xs.next;"
        , "        i--;"
        , "    }"
        , "    if (xs.tag == 101) return elm_just(*xs.data.c);"
        , "    return elm_nothing();"
        , "}"
        , ""
        , "static elm_value_t elm_Array_set(elm_value_t index, elm_value_t value, elm_value_t arr) {"
        , "    int64_t targetIdx = index.data.i;"
        , "    if (targetIdx < 0) return arr;"
        , "    elm_value_t result = elm_nil();"
        , "    elm_value_t xs = arr;"
        , "    int64_t i = 0;"
        , "    while (xs.tag == 101) {"
        , "        if (i == targetIdx) {"
        , "            result = elm_cons(value, result);"
        , "        } else {"
        , "            result = elm_cons(*xs.data.c, result);"
        , "        }"
        , "        xs = *xs.next;"
        , "        i++;"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "static elm_value_t elm_Array_push(elm_value_t value, elm_value_t arr) {"
        , "    return elm_List_append(arr, elm_cons(value, elm_nil()));"
        , "}"
        , ""
        , "static elm_value_t elm_Array_append(elm_value_t arr1, elm_value_t arr2) {"
        , "    return elm_List_append(arr1, arr2);"
        , "}"
        , ""
        , "static elm_value_t elm_Array_slice(elm_value_t start, elm_value_t end, elm_value_t arr) {"
        , "    int64_t s = start.data.i;"
        , "    int64_t e = end.data.i;"
        , "    int64_t len = elm_List_length(arr).data.i;"
        , "    /* Handle negative indices */"
        , "    if (s < 0) s = len + s;"
        , "    if (e < 0) e = len + e;"
        , "    if (s < 0) s = 0;"
        , "    if (e > len) e = len;"
        , "    if (s >= e) return elm_nil();"
        , "    elm_value_t result = elm_nil();"
        , "    elm_value_t xs = arr;"
        , "    int64_t i = 0;"
        , "    while (xs.tag == 101 && i < e) {"
        , "        if (i >= s) {"
        , "            result = elm_cons(*xs.data.c, result);"
        , "        }"
        , "        xs = *xs.next;"
        , "        i++;"
        , "    }"
        , "    return elm_List_reverse(result);"
        , "}"
        , ""
        , "static elm_value_t elm_Array_map(elm_value_t f, elm_value_t arr) {"
        , "    return elm_List_map(f, arr);"
        , "}"
        , ""
        , "static elm_value_t elm_Array_indexedMap(elm_value_t f, elm_value_t arr) {"
        , "    return elm_List_indexedMap(f, arr);"
        , "}"
        , ""
        , "static elm_value_t elm_Array_foldl(elm_value_t f, elm_value_t acc, elm_value_t arr) {"
        , "    return elm_List_foldl(f, acc, arr);"
        , "}"
        , ""
        , "static elm_value_t elm_Array_foldr(elm_value_t f, elm_value_t acc, elm_value_t arr) {"
        , "    return elm_List_foldr(f, acc, arr);"
        , "}"
        , ""
        , "static elm_value_t elm_Array_filter(elm_value_t pred, elm_value_t arr) {"
        , "    return elm_List_filter(pred, arr);"
        , "}"
        , ""
        , "/* ===== JSON ENCODE MODULE ===== */"
        , "/* Tags: 900=null, 901=int, 902=float, 903=string, 904=bool, 905=array, 906=object */"
        , ""
        , "static elm_value_t elm_Json_Encode_null(void) {"
        , "    return (elm_value_t){ .tag = 900, .data.i = 0, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Encode_int(elm_value_t n) {"
        , "    elm_value_t result;"
        , "    result.tag = 901;"
        , "    result.data.i = n.data.i;"
        , "    result.next = NULL;"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Encode_float(elm_value_t f) {"
        , "    elm_value_t result;"
        , "    result.tag = 902;"
        , "    result.data.f = f.data.f;"
        , "    result.next = NULL;"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Encode_string(elm_value_t s) {"
        , "    elm_value_t result;"
        , "    result.tag = 903;"
        , "    result.data.s = s.data.s;"
        , "    result.next = NULL;"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Encode_bool(elm_value_t b) {"
        , "    elm_value_t result;"
        , "    result.tag = 904;"
        , "    result.data.i = (b.tag == 5) ? 1 : 0;"
        , "    result.next = NULL;"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Encode_list(elm_value_t encoder, elm_value_t list) {"
        , "    elm_value_t result;"
        , "    result.tag = 905;"
        , "    /* Encode each element using the encoder function */"
        , "    elm_value_t encoded = elm_nil();"
        , "    elm_value_t xs = list;"
        , "    while (xs.tag == 101) {"
        , "        elm_value_t elem = elm_apply1((elm_closure_t *)encoder.data.p, *xs.data.c);"
        , "        encoded = elm_cons(elem, encoded);"
        , "        xs = *xs.next;"
        , "    }"
        , "    result.data.c = malloc(sizeof(elm_value_t));"
        , "    *result.data.c = elm_List_reverse(encoded);"
        , "    result.next = NULL;"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Encode_array(elm_value_t encoder, elm_value_t arr) {"
        , "    return elm_Json_Encode_list(encoder, arr); /* Array is list for now */"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Encode_object(elm_value_t pairs) {"
        , "    elm_value_t result;"
        , "    result.tag = 906;"
        , "    result.data.c = malloc(sizeof(elm_value_t));"
        , "    *result.data.c = pairs;"
        , "    result.next = NULL;"
        , "    return result;"
        , "}"
        , ""
        , "/* Helper to escape JSON strings */"
        , "static void json_encode_string_to_buf(const char *s, char **bufp, size_t *sizep, size_t *posp) {"
        , "    size_t len = strlen(s);"
        , "    /* Worst case: every char needs escaping (\\uXXXX = 6 chars) plus quotes */"
        , "    while (*posp + len * 6 + 3 > *sizep) {"
        , "        *sizep *= 2;"
        , "        *bufp = realloc(*bufp, *sizep);"
        , "    }"
        , "    char *buf = *bufp;"
        , "    size_t pos = *posp;"
        , "    buf[pos++] = '\"';"
        , "    for (size_t i = 0; i < len; i++) {"
        , "        unsigned char c = s[i];"
        , "        switch (c) {"
        , "            case '\"': buf[pos++] = '\\\\'; buf[pos++] = '\"'; break;"
        , "            case '\\\\': buf[pos++] = '\\\\'; buf[pos++] = '\\\\'; break;"
        , "            case '\\b': buf[pos++] = '\\\\'; buf[pos++] = 'b'; break;"
        , "            case '\\f': buf[pos++] = '\\\\'; buf[pos++] = 'f'; break;"
        , "            case '\\n': buf[pos++] = '\\\\'; buf[pos++] = 'n'; break;"
        , "            case '\\r': buf[pos++] = '\\\\'; buf[pos++] = 'r'; break;"
        , "            case '\\t': buf[pos++] = '\\\\'; buf[pos++] = 't'; break;"
        , "            default:"
        , "                if (c < 32) {"
        , "                    pos += snprintf(buf + pos, *sizep - pos, \"\\\\u%04x\", c);"
        , "                } else {"
        , "                    buf[pos++] = c;"
        , "                }"
        , "        }"
        , "    }"
        , "    buf[pos++] = '\"';"
        , "    *posp = pos;"
        , "}"
        , ""
        , "/* Forward declaration */"
        , "static void json_encode_value_to_buf(elm_value_t v, int indent, int depth, char **bufp, size_t *sizep, size_t *posp);"
        , ""
        , "static void json_add_indent(int indent, int depth, char **bufp, size_t *sizep, size_t *posp) {"
        , "    if (indent <= 0) return;"
        , "    while (*posp + depth * indent + 2 > *sizep) {"
        , "        *sizep *= 2;"
        , "        *bufp = realloc(*bufp, *sizep);"
        , "    }"
        , "    (*bufp)[(*posp)++] = '\\n';"
        , "    for (int i = 0; i < depth * indent; i++) {"
        , "        (*bufp)[(*posp)++] = ' ';"
        , "    }"
        , "}"
        , ""
        , "static void json_encode_value_to_buf(elm_value_t v, int indent, int depth, char **bufp, size_t *sizep, size_t *posp) {"
        , "    while (*posp + 64 > *sizep) { *sizep *= 2; *bufp = realloc(*bufp, *sizep); }"
        , "    char *buf = *bufp;"
        , "    size_t pos = *posp;"
        , "    switch (v.tag) {"
        , "        case 900: /* null */"
        , "            memcpy(buf + pos, \"null\", 4);"
        , "            *posp = pos + 4;"
        , "            break;"
        , "        case 901: /* int */"
        , "            *posp = pos + snprintf(buf + pos, *sizep - pos, \"%ld\", (long)v.data.i);"
        , "            break;"
        , "        case 902: /* float */"
        , "            *posp = pos + snprintf(buf + pos, *sizep - pos, \"%g\", v.data.f);"
        , "            break;"
        , "        case 903: /* string */"
        , "            json_encode_string_to_buf(v.data.s, bufp, sizep, posp);"
        , "            break;"
        , "        case 904: /* bool */"
        , "            if (v.data.i) {"
        , "                memcpy(buf + pos, \"true\", 4);"
        , "                *posp = pos + 4;"
        , "            } else {"
        , "                memcpy(buf + pos, \"false\", 5);"
        , "                *posp = pos + 5;"
        , "            }"
        , "            break;"
        , "        case 905: { /* array */"
        , "            (*bufp)[(*posp)++] = '[';"
        , "            elm_value_t xs = *v.data.c;"
        , "            int first = 1;"
        , "            while (xs.tag == 101) {"
        , "                if (!first) (*bufp)[(*posp)++] = ',';"
        , "                first = 0;"
        , "                if (indent > 0) json_add_indent(indent, depth + 1, bufp, sizep, posp);"
        , "                json_encode_value_to_buf(*xs.data.c, indent, depth + 1, bufp, sizep, posp);"
        , "                xs = *xs.next;"
        , "            }"
        , "            if (indent > 0 && !first) json_add_indent(indent, depth, bufp, sizep, posp);"
        , "            (*bufp)[(*posp)++] = ']';"
        , "            break;"
        , "        }"
        , "        case 906: { /* object */"
        , "            (*bufp)[(*posp)++] = '{';"
        , "            elm_value_t xs = *v.data.c;"
        , "            int first = 1;"
        , "            while (xs.tag == 101) {"
        , "                if (!first) (*bufp)[(*posp)++] = ',';"
        , "                first = 0;"
        , "                if (indent > 0) json_add_indent(indent, depth + 1, bufp, sizep, posp);"
        , "                /* Each element is a tuple (string, value) */"
        , "                elm_value_t pair = *xs.data.c;"
        , "                elm_value_t key = *pair.data.c;"
        , "                elm_value_t val = *pair.next;"
        , "                json_encode_string_to_buf(key.data.s, bufp, sizep, posp);"
        , "                (*bufp)[(*posp)++] = ':';"
        , "                if (indent > 0) (*bufp)[(*posp)++] = ' ';"
        , "                json_encode_value_to_buf(val, indent, depth + 1, bufp, sizep, posp);"
        , "                xs = *xs.next;"
        , "            }"
        , "            if (indent > 0 && !first) json_add_indent(indent, depth, bufp, sizep, posp);"
        , "            (*bufp)[(*posp)++] = '}';"
        , "            break;"
        , "        }"
        , "        default:"
        , "            memcpy(buf + pos, \"null\", 4);"
        , "            *posp = pos + 4;"
        , "    }"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Encode_encode(elm_value_t indent, elm_value_t value) {"
        , "    size_t size = 256;"
        , "    char *buf = malloc(size);"
        , "    size_t pos = 0;"
        , "    json_encode_value_to_buf(value, (int)indent.data.i, 0, &buf, &size, &pos);"
        , "    buf[pos] = '\\0';"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "/* ===== JSON DECODE MODULE ===== */"
        , "/* Decoders are represented as closures (tag 400) that take a Value and return Result */"
        , "/* Tag 910 = Decoder wrapper (for type safety) */"
        , ""
        , "/* JSON Parser - converts string to Value */"
        , "typedef struct {"
        , "    const char *input;"
        , "    size_t pos;"
        , "    size_t len;"
        , "} json_parser_t;"
        , ""
        , "static void json_skip_ws(json_parser_t *p) {"
        , "    while (p->pos < p->len && (p->input[p->pos] == ' ' || p->input[p->pos] == '\\t' ||"
        , "           p->input[p->pos] == '\\n' || p->input[p->pos] == '\\r')) {"
        , "        p->pos++;"
        , "    }"
        , "}"
        , ""
        , "static elm_value_t json_parse_value(json_parser_t *p);"
        , ""
        , "static elm_value_t json_parse_string(json_parser_t *p) {"
        , "    if (p->input[p->pos] != '\"') return (elm_value_t){ .tag = 0, .data.i = -1 }; /* error */"
        , "    p->pos++;"
        , "    size_t start = p->pos;"
        , "    char *buf = malloc(p->len - start + 1);"
        , "    size_t bi = 0;"
        , "    while (p->pos < p->len && p->input[p->pos] != '\"') {"
        , "        if (p->input[p->pos] == '\\\\' && p->pos + 1 < p->len) {"
        , "            p->pos++;"
        , "            switch (p->input[p->pos]) {"
        , "                case '\"': buf[bi++] = '\"'; break;"
        , "                case '\\\\': buf[bi++] = '\\\\'; break;"
        , "                case '/': buf[bi++] = '/'; break;"
        , "                case 'b': buf[bi++] = '\\b'; break;"
        , "                case 'f': buf[bi++] = '\\f'; break;"
        , "                case 'n': buf[bi++] = '\\n'; break;"
        , "                case 'r': buf[bi++] = '\\r'; break;"
        , "                case 't': buf[bi++] = '\\t'; break;"
        , "                default: buf[bi++] = p->input[p->pos];"
        , "            }"
        , "        } else {"
        , "            buf[bi++] = p->input[p->pos];"
        , "        }"
        , "        p->pos++;"
        , "    }"
        , "    buf[bi] = '\\0';"
        , "    if (p->pos < p->len && p->input[p->pos] == '\"') p->pos++;"
        , "    return elm_Json_Encode_string(elm_string(buf));"
        , "}"
        , ""
        , "static elm_value_t json_parse_number(json_parser_t *p) {"
        , "    size_t start = p->pos;"
        , "    int is_float = 0;"
        , "    if (p->input[p->pos] == '-') p->pos++;"
        , "    while (p->pos < p->len && p->input[p->pos] >= '0' && p->input[p->pos] <= '9') p->pos++;"
        , "    if (p->pos < p->len && p->input[p->pos] == '.') { is_float = 1; p->pos++; }"
        , "    while (p->pos < p->len && p->input[p->pos] >= '0' && p->input[p->pos] <= '9') p->pos++;"
        , "    if (p->pos < p->len && (p->input[p->pos] == 'e' || p->input[p->pos] == 'E')) {"
        , "        is_float = 1; p->pos++;"
        , "        if (p->pos < p->len && (p->input[p->pos] == '+' || p->input[p->pos] == '-')) p->pos++;"
        , "        while (p->pos < p->len && p->input[p->pos] >= '0' && p->input[p->pos] <= '9') p->pos++;"
        , "    }"
        , "    char *num = malloc(p->pos - start + 1);"
        , "    memcpy(num, p->input + start, p->pos - start);"
        , "    num[p->pos - start] = '\\0';"
        , "    if (is_float) {"
        , "        double f = strtod(num, NULL);"
        , "        free(num);"
        , "        return elm_Json_Encode_float(elm_float(f));"
        , "    } else {"
        , "        int64_t i = strtoll(num, NULL, 10);"
        , "        free(num);"
        , "        return elm_Json_Encode_int(elm_int(i));"
        , "    }"
        , "}"
        , ""
        , "static elm_value_t json_parse_array(json_parser_t *p) {"
        , "    if (p->input[p->pos] != '[') return (elm_value_t){ .tag = 0, .data.i = -1 };"
        , "    p->pos++;"
        , "    json_skip_ws(p);"
        , "    elm_value_t items = elm_nil();"
        , "    if (p->pos < p->len && p->input[p->pos] != ']') {"
        , "        elm_value_t item = json_parse_value(p);"
        , "        items = elm_cons(item, items);"
        , "        json_skip_ws(p);"
        , "        while (p->pos < p->len && p->input[p->pos] == ',') {"
        , "            p->pos++;"
        , "            json_skip_ws(p);"
        , "            item = json_parse_value(p);"
        , "            items = elm_cons(item, items);"
        , "            json_skip_ws(p);"
        , "        }"
        , "    }"
        , "    if (p->pos < p->len && p->input[p->pos] == ']') p->pos++;"
        , "    elm_value_t result;"
        , "    result.tag = 905;"
        , "    result.data.c = malloc(sizeof(elm_value_t));"
        , "    *result.data.c = elm_List_reverse(items);"
        , "    result.next = NULL;"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t json_parse_object(json_parser_t *p) {"
        , "    if (p->input[p->pos] != '{') return (elm_value_t){ .tag = 0, .data.i = -1 };"
        , "    p->pos++;"
        , "    json_skip_ws(p);"
        , "    elm_value_t pairs = elm_nil();"
        , "    if (p->pos < p->len && p->input[p->pos] != '}') {"
        , "        elm_value_t key = json_parse_string(p);"
        , "        json_skip_ws(p);"
        , "        if (p->pos < p->len && p->input[p->pos] == ':') p->pos++;"
        , "        json_skip_ws(p);"
        , "        elm_value_t val = json_parse_value(p);"
        , "        /* Create tuple (key, value) - key is stored as raw string */"
        , "        elm_value_t pair = elm_tuple2(elm_string(key.data.s), val);"
        , "        pairs = elm_cons(pair, pairs);"
        , "        json_skip_ws(p);"
        , "        while (p->pos < p->len && p->input[p->pos] == ',') {"
        , "            p->pos++;"
        , "            json_skip_ws(p);"
        , "            key = json_parse_string(p);"
        , "            json_skip_ws(p);"
        , "            if (p->pos < p->len && p->input[p->pos] == ':') p->pos++;"
        , "            json_skip_ws(p);"
        , "            val = json_parse_value(p);"
        , "            pair = elm_tuple2(elm_string(key.data.s), val);"
        , "            pairs = elm_cons(pair, pairs);"
        , "            json_skip_ws(p);"
        , "        }"
        , "    }"
        , "    if (p->pos < p->len && p->input[p->pos] == '}') p->pos++;"
        , "    elm_value_t result;"
        , "    result.tag = 906;"
        , "    result.data.c = malloc(sizeof(elm_value_t));"
        , "    *result.data.c = elm_List_reverse(pairs);"
        , "    result.next = NULL;"
        , "    return result;"
        , "}"
        , ""
        , "static elm_value_t json_parse_value(json_parser_t *p) {"
        , "    json_skip_ws(p);"
        , "    if (p->pos >= p->len) return elm_Json_Encode_null();"
        , "    char c = p->input[p->pos];"
        , "    if (c == '\"') return json_parse_string(p);"
        , "    if (c == '[') return json_parse_array(p);"
        , "    if (c == '{') return json_parse_object(p);"
        , "    if (c == 't' && p->pos + 4 <= p->len && strncmp(p->input + p->pos, \"true\", 4) == 0) {"
        , "        p->pos += 4;"
        , "        return elm_Json_Encode_bool(elm_bool(1));"
        , "    }"
        , "    if (c == 'f' && p->pos + 5 <= p->len && strncmp(p->input + p->pos, \"false\", 5) == 0) {"
        , "        p->pos += 5;"
        , "        return elm_Json_Encode_bool(elm_bool(0));"
        , "    }"
        , "    if (c == 'n' && p->pos + 4 <= p->len && strncmp(p->input + p->pos, \"null\", 4) == 0) {"
        , "        p->pos += 4;"
        , "        return elm_Json_Encode_null();"
        , "    }"
        , "    if (c == '-' || (c >= '0' && c <= '9')) return json_parse_number(p);"
        , "    return elm_Json_Encode_null(); /* fallback */"
        , "}"
        , ""
        , "static elm_value_t json_parse(const char *str) {"
        , "    json_parser_t p = { .input = str, .pos = 0, .len = strlen(str) };"
        , "    return json_parse_value(&p);"
        , "}"
        , ""
        , "/* Decoder primitives */"
        , "static elm_value_t elm_Json_Decode_string_impl(elm_value_t val) {"
        , "    if (val.tag == 903) return elm_ok(elm_string(val.data.s));"
        , "    return elm_err(elm_string(\"Expected string\"));"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_int_impl(elm_value_t val) {"
        , "    if (val.tag == 901) return elm_ok(elm_int(val.data.i));"
        , "    return elm_err(elm_string(\"Expected int\"));"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_float_impl(elm_value_t val) {"
        , "    if (val.tag == 902) return elm_ok(elm_float(val.data.f));"
        , "    if (val.tag == 901) return elm_ok(elm_float((double)val.data.i));"
        , "    return elm_err(elm_string(\"Expected float\"));"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_bool_impl(elm_value_t val) {"
        , "    if (val.tag == 904) return elm_ok(elm_bool(val.data.i));"
        , "    return elm_err(elm_string(\"Expected bool\"));"
        , "}"
        , ""
        , "/* Create decoder closures */"
        , "static elm_value_t elm_Json_Decode_string(void) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Json_Decode_string_impl;"
        , "    c->arity = 1;"
        , "    c->applied = 0;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_int(void) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Json_Decode_int_impl;"
        , "    c->arity = 1;"
        , "    c->applied = 0;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_float(void) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Json_Decode_float_impl;"
        , "    c->arity = 1;"
        , "    c->applied = 0;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_bool(void) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Json_Decode_bool_impl;"
        , "    c->arity = 1;"
        , "    c->applied = 0;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_null_impl(elm_value_t defaultVal, elm_value_t val) {"
        , "    if (val.tag == 900) return elm_ok(defaultVal);"
        , "    return elm_err(elm_string(\"Expected null\"));"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_null(elm_value_t defaultVal) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Json_Decode_null_impl;"
        , "    c->arity = 2;"
        , "    c->applied = 1;"
        , "    c->args[0] = defaultVal;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_succeed_impl(elm_value_t val, elm_value_t _unused) {"
        , "    (void)_unused;"
        , "    return elm_ok(val);"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_succeed(elm_value_t val) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Json_Decode_succeed_impl;"
        , "    c->arity = 2;"
        , "    c->applied = 1;"
        , "    c->args[0] = val;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_fail_impl(elm_value_t msg, elm_value_t _unused) {"
        , "    (void)_unused;"
        , "    return elm_err(msg);"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_fail(elm_value_t msg) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Json_Decode_fail_impl;"
        , "    c->arity = 2;"
        , "    c->applied = 1;"
        , "    c->args[0] = msg;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "/* Decode operations */"
        , "static elm_value_t elm_Json_Decode_decodeValue(elm_value_t decoder, elm_value_t val) {"
        , "    return elm_apply1((elm_closure_t *)decoder.data.p, val);"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_decodeString(elm_value_t decoder, elm_value_t str) {"
        , "    elm_value_t val = json_parse(str.data.s);"
        , "    return elm_apply1((elm_closure_t *)decoder.data.p, val);"
        , "}"
        , ""
        , "/* Field decoder */"
        , "static elm_value_t elm_Json_Decode_field_impl(elm_value_t fieldName, elm_value_t innerDecoder, elm_value_t val) {"
        , "    if (val.tag != 906) return elm_err(elm_string(\"Expected object\"));"
        , "    elm_value_t pairs = *val.data.c;"
        , "    while (pairs.tag == 101) {"
        , "        elm_value_t pair = *pairs.data.c;"
        , "        elm_value_t key = *pair.data.c;"
        , "        if (strcmp(key.data.s, fieldName.data.s) == 0) {"
        , "            elm_value_t fieldVal = *pair.next;"
        , "            return elm_apply1((elm_closure_t *)innerDecoder.data.p, fieldVal);"
        , "        }"
        , "        pairs = *pairs.next;"
        , "    }"
        , "    char *msg = malloc(strlen(fieldName.data.s) + 32);"
        , "    sprintf(msg, \"Field '%s' not found\", fieldName.data.s);"
        , "    return elm_err(elm_string(msg));"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_field(elm_value_t fieldName, elm_value_t innerDecoder) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Json_Decode_field_impl;"
        , "    c->arity = 3;"
        , "    c->applied = 2;"
        , "    c->args[0] = fieldName;"
        , "    c->args[1] = innerDecoder;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "/* Map decoder */"
        , "static elm_value_t elm_Json_Decode_map_impl(elm_value_t f, elm_value_t decoder, elm_value_t val) {"
        , "    elm_value_t result = elm_apply1((elm_closure_t *)decoder.data.p, val);"
        , "    if (result.tag == 300) return result; /* Err */"
        , "    return elm_ok(elm_apply1((elm_closure_t *)f.data.p, *result.data.c));"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_map(elm_value_t f, elm_value_t decoder) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Json_Decode_map_impl;"
        , "    c->arity = 3;"
        , "    c->applied = 2;"
        , "    c->args[0] = f;"
        , "    c->args[1] = decoder;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "/* Map2 decoder */"
        , "static elm_value_t elm_Json_Decode_map2_impl(elm_value_t f, elm_value_t d1, elm_value_t d2, elm_value_t val) {"
        , "    elm_value_t r1 = elm_apply1((elm_closure_t *)d1.data.p, val);"
        , "    if (r1.tag == 300) return r1;"
        , "    elm_value_t r2 = elm_apply1((elm_closure_t *)d2.data.p, val);"
        , "    if (r2.tag == 300) return r2;"
        , "    elm_value_t partial = elm_apply1((elm_closure_t *)f.data.p, *r1.data.c);"
        , "    return elm_ok(elm_apply1((elm_closure_t *)partial.data.p, *r2.data.c));"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_map2(elm_value_t f, elm_value_t d1, elm_value_t d2) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Json_Decode_map2_impl;"
        , "    c->arity = 4;"
        , "    c->applied = 3;"
        , "    c->args[0] = f;"
        , "    c->args[1] = d1;"
        , "    c->args[2] = d2;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "/* AndThen decoder */"
        , "static elm_value_t elm_Json_Decode_andThen_impl(elm_value_t callback, elm_value_t decoder, elm_value_t val) {"
        , "    elm_value_t result = elm_apply1((elm_closure_t *)decoder.data.p, val);"
        , "    if (result.tag == 300) return result;"
        , "    elm_value_t nextDecoder = elm_apply1((elm_closure_t *)callback.data.p, *result.data.c);"
        , "    return elm_apply1((elm_closure_t *)nextDecoder.data.p, val);"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_andThen(elm_value_t callback, elm_value_t decoder) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Json_Decode_andThen_impl;"
        , "    c->arity = 3;"
        , "    c->applied = 2;"
        , "    c->args[0] = callback;"
        , "    c->args[1] = decoder;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "/* List decoder */"
        , "static elm_value_t elm_Json_Decode_list_impl(elm_value_t itemDecoder, elm_value_t val) {"
        , "    if (val.tag != 905) return elm_err(elm_string(\"Expected array\"));"
        , "    elm_value_t items = *val.data.c;"
        , "    elm_value_t results = elm_nil();"
        , "    while (items.tag == 101) {"
        , "        elm_value_t item = *items.data.c;"
        , "        elm_value_t decoded = elm_apply1((elm_closure_t *)itemDecoder.data.p, item);"
        , "        if (decoded.tag == 300) return decoded; /* propagate error */"
        , "        results = elm_cons(*decoded.data.c, results);"
        , "        items = *items.next;"
        , "    }"
        , "    return elm_ok(elm_List_reverse(results));"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_list(elm_value_t itemDecoder) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Json_Decode_list_impl;"
        , "    c->arity = 2;"
        , "    c->applied = 1;"
        , "    c->args[0] = itemDecoder;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "/* Nullable decoder */"
        , "static elm_value_t elm_Json_Decode_nullable_impl(elm_value_t decoder, elm_value_t val) {"
        , "    if (val.tag == 900) return elm_ok(elm_nothing());"
        , "    elm_value_t result = elm_apply1((elm_closure_t *)decoder.data.p, val);"
        , "    if (result.tag == 300) return result;"
        , "    return elm_ok(elm_just(*result.data.c));"
        , "}"
        , ""
        , "static elm_value_t elm_Json_Decode_nullable(elm_value_t decoder) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Json_Decode_nullable_impl;"
        , "    c->arity = 2;"
        , "    c->applied = 1;"
        , "    c->args[0] = decoder;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "/* ===== BYTES MODULE ===== */"
        , "/* Tags: 910=Bytes, 911=Encoder, 920=LE, 921=BE */"
        , ""
        , "/* Bytes type - wraps a byte buffer */"
        , "typedef struct {"
        , "    uint8_t *data;"
        , "    size_t len;"
        , "} bytes_t;"
        , ""
        , "static elm_value_t elm_bytes(uint8_t *data, size_t len) {"
        , "    bytes_t *b = malloc(sizeof(bytes_t));"
        , "    b->data = data;"
        , "    b->len = len;"
        , "    return (elm_value_t){ .tag = 910, .data.p = b, .next = NULL };"
        , "}"
        , ""
        , "/* Endianness constructors */"
        , "static elm_value_t elm_Bytes_LE(void) {"
        , "    return (elm_value_t){ .tag = 920, .data.i = 0, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_BE(void) {"
        , "    return (elm_value_t){ .tag = 921, .data.i = 0, .next = NULL };"
        , "}"
        , ""
        , "/* Core Bytes operations */"
        , "static elm_value_t elm_Bytes_width(elm_value_t b) {"
        , "    bytes_t *bytes = (bytes_t *)b.data.p;"
        , "    return elm_int((int64_t)bytes->len);"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_isEmpty(elm_value_t b) {"
        , "    bytes_t *bytes = (bytes_t *)b.data.p;"
        , "    return elm_bool(bytes->len == 0);"
        , "}"
        , ""
        , "/* Encoder type - builds up a sequence of bytes */"
        , "typedef struct encoder_node {"
        , "    uint8_t *data;"
        , "    size_t len;"
        , "    struct encoder_node *next;"
        , "} encoder_node_t;"
        , ""
        , "static elm_value_t elm_encoder(uint8_t *data, size_t len) {"
        , "    encoder_node_t *node = malloc(sizeof(encoder_node_t));"
        , "    node->data = data;"
        , "    node->len = len;"
        , "    node->next = NULL;"
        , "    return (elm_value_t){ .tag = 911, .data.p = node, .next = NULL };"
        , "}"
        , ""
        , "/* Bytes.Encode module */"
        , "static elm_value_t elm_Bytes_Encode_signedInt8(elm_value_t n) {"
        , "    uint8_t *data = malloc(1);"
        , "    data[0] = (int8_t)n.data.i;"
        , "    return elm_encoder(data, 1);"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Encode_unsignedInt8(elm_value_t n) {"
        , "    uint8_t *data = malloc(1);"
        , "    data[0] = (uint8_t)n.data.i;"
        , "    return elm_encoder(data, 1);"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Encode_signedInt16(elm_value_t endian, elm_value_t n) {"
        , "    uint8_t *data = malloc(2);"
        , "    int16_t val = (int16_t)n.data.i;"
        , "    if (endian.tag == 920) { /* LE */"
        , "        data[0] = val & 0xFF;"
        , "        data[1] = (val >> 8) & 0xFF;"
        , "    } else { /* BE */"
        , "        data[0] = (val >> 8) & 0xFF;"
        , "        data[1] = val & 0xFF;"
        , "    }"
        , "    return elm_encoder(data, 2);"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Encode_unsignedInt16(elm_value_t endian, elm_value_t n) {"
        , "    uint8_t *data = malloc(2);"
        , "    uint16_t val = (uint16_t)n.data.i;"
        , "    if (endian.tag == 920) { /* LE */"
        , "        data[0] = val & 0xFF;"
        , "        data[1] = (val >> 8) & 0xFF;"
        , "    } else { /* BE */"
        , "        data[0] = (val >> 8) & 0xFF;"
        , "        data[1] = val & 0xFF;"
        , "    }"
        , "    return elm_encoder(data, 2);"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Encode_signedInt32(elm_value_t endian, elm_value_t n) {"
        , "    uint8_t *data = malloc(4);"
        , "    int32_t val = (int32_t)n.data.i;"
        , "    if (endian.tag == 920) { /* LE */"
        , "        data[0] = val & 0xFF;"
        , "        data[1] = (val >> 8) & 0xFF;"
        , "        data[2] = (val >> 16) & 0xFF;"
        , "        data[3] = (val >> 24) & 0xFF;"
        , "    } else { /* BE */"
        , "        data[0] = (val >> 24) & 0xFF;"
        , "        data[1] = (val >> 16) & 0xFF;"
        , "        data[2] = (val >> 8) & 0xFF;"
        , "        data[3] = val & 0xFF;"
        , "    }"
        , "    return elm_encoder(data, 4);"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Encode_unsignedInt32(elm_value_t endian, elm_value_t n) {"
        , "    uint8_t *data = malloc(4);"
        , "    uint32_t val = (uint32_t)n.data.i;"
        , "    if (endian.tag == 920) { /* LE */"
        , "        data[0] = val & 0xFF;"
        , "        data[1] = (val >> 8) & 0xFF;"
        , "        data[2] = (val >> 16) & 0xFF;"
        , "        data[3] = (val >> 24) & 0xFF;"
        , "    } else { /* BE */"
        , "        data[0] = (val >> 24) & 0xFF;"
        , "        data[1] = (val >> 16) & 0xFF;"
        , "        data[2] = (val >> 8) & 0xFF;"
        , "        data[3] = val & 0xFF;"
        , "    }"
        , "    return elm_encoder(data, 4);"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Encode_float32(elm_value_t endian, elm_value_t f) {"
        , "    uint8_t *data = malloc(4);"
        , "    float val = (float)f.data.f;"
        , "    uint32_t bits;"
        , "    memcpy(&bits, &val, 4);"
        , "    if (endian.tag == 920) { /* LE */"
        , "        data[0] = bits & 0xFF;"
        , "        data[1] = (bits >> 8) & 0xFF;"
        , "        data[2] = (bits >> 16) & 0xFF;"
        , "        data[3] = (bits >> 24) & 0xFF;"
        , "    } else { /* BE */"
        , "        data[0] = (bits >> 24) & 0xFF;"
        , "        data[1] = (bits >> 16) & 0xFF;"
        , "        data[2] = (bits >> 8) & 0xFF;"
        , "        data[3] = bits & 0xFF;"
        , "    }"
        , "    return elm_encoder(data, 4);"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Encode_float64(elm_value_t endian, elm_value_t f) {"
        , "    uint8_t *data = malloc(8);"
        , "    double val = f.data.f;"
        , "    uint64_t bits;"
        , "    memcpy(&bits, &val, 8);"
        , "    if (endian.tag == 920) { /* LE */"
        , "        for (int i = 0; i < 8; i++) data[i] = (bits >> (i * 8)) & 0xFF;"
        , "    } else { /* BE */"
        , "        for (int i = 0; i < 8; i++) data[i] = (bits >> ((7-i) * 8)) & 0xFF;"
        , "    }"
        , "    return elm_encoder(data, 8);"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Encode_bytes(elm_value_t b) {"
        , "    bytes_t *bytes = (bytes_t *)b.data.p;"
        , "    uint8_t *data = malloc(bytes->len);"
        , "    memcpy(data, bytes->data, bytes->len);"
        , "    return elm_encoder(data, bytes->len);"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Encode_string(elm_value_t s) {"
        , "    size_t len = strlen(s.data.s);"
        , "    uint8_t *data = malloc(len);"
        , "    memcpy(data, s.data.s, len);"
        , "    return elm_encoder(data, len);"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Encode_sequence(elm_value_t encoders) {"
        , "    /* Concatenate all encoders in the list */"
        , "    size_t total = 0;"
        , "    elm_value_t curr = encoders;"
        , "    while (curr.tag == 101) {"
        , "        elm_value_t head = *curr.data.c;"
        , "        encoder_node_t *node = (encoder_node_t *)head.data.p;"
        , "        while (node) { total += node->len; node = node->next; }"
        , "        curr = *curr.next;"
        , "    }"
        , "    uint8_t *data = malloc(total > 0 ? total : 1);"
        , "    size_t pos = 0;"
        , "    curr = encoders;"
        , "    while (curr.tag == 101) {"
        , "        elm_value_t head = *curr.data.c;"
        , "        encoder_node_t *node = (encoder_node_t *)head.data.p;"
        , "        while (node) {"
        , "            memcpy(data + pos, node->data, node->len);"
        , "            pos += node->len;"
        , "            node = node->next;"
        , "        }"
        , "        curr = *curr.next;"
        , "    }"
        , "    return elm_encoder(data, total);"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Encode_encode(elm_value_t encoder) {"
        , "    /* Convert encoder chain to Bytes */"
        , "    encoder_node_t *node = (encoder_node_t *)encoder.data.p;"
        , "    size_t total = 0;"
        , "    encoder_node_t *n = node;"
        , "    while (n) { total += n->len; n = n->next; }"
        , "    uint8_t *data = malloc(total > 0 ? total : 1);"
        , "    size_t pos = 0;"
        , "    n = node;"
        , "    while (n) {"
        , "        memcpy(data + pos, n->data, n->len);"
        , "        pos += n->len;"
        , "        n = n->next;"
        , "    }"
        , "    return elm_bytes(data, total);"
        , "}"
        , ""
        , "/* Bytes.Decode module - decoder state */"
        , "typedef struct {"
        , "    const uint8_t *data;"
        , "    size_t len;"
        , "    size_t pos;"
        , "} bytes_decoder_state_t;"
        , ""
        , "/* Decoder primitive implementations */"
        , "static elm_value_t elm_Bytes_Decode_signedInt8_impl(elm_value_t state_val) {"
        , "    bytes_decoder_state_t *st = (bytes_decoder_state_t *)state_val.data.p;"
        , "    if (st->pos + 1 > st->len) return elm_nothing();"
        , "    int8_t val = (int8_t)st->data[st->pos];"
        , "    st->pos += 1;"
        , "    return elm_just(elm_int(val));"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_signedInt8(void) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Bytes_Decode_signedInt8_impl;"
        , "    c->arity = 1;"
        , "    c->applied = 0;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_unsignedInt8_impl(elm_value_t state_val) {"
        , "    bytes_decoder_state_t *st = (bytes_decoder_state_t *)state_val.data.p;"
        , "    if (st->pos + 1 > st->len) return elm_nothing();"
        , "    uint8_t val = st->data[st->pos];"
        , "    st->pos += 1;"
        , "    return elm_just(elm_int(val));"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_unsignedInt8(void) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Bytes_Decode_unsignedInt8_impl;"
        , "    c->arity = 1;"
        , "    c->applied = 0;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_signedInt16_impl(elm_value_t endian, elm_value_t state_val) {"
        , "    bytes_decoder_state_t *st = (bytes_decoder_state_t *)state_val.data.p;"
        , "    if (st->pos + 2 > st->len) return elm_nothing();"
        , "    int16_t val;"
        , "    if (endian.tag == 920) { /* LE */"
        , "        val = st->data[st->pos] | ((int16_t)st->data[st->pos + 1] << 8);"
        , "    } else {"
        , "        val = ((int16_t)st->data[st->pos] << 8) | st->data[st->pos + 1];"
        , "    }"
        , "    st->pos += 2;"
        , "    return elm_just(elm_int(val));"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_signedInt16(elm_value_t endian) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Bytes_Decode_signedInt16_impl;"
        , "    c->arity = 2;"
        , "    c->applied = 1;"
        , "    c->args[0] = endian;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_unsignedInt16_impl(elm_value_t endian, elm_value_t state_val) {"
        , "    bytes_decoder_state_t *st = (bytes_decoder_state_t *)state_val.data.p;"
        , "    if (st->pos + 2 > st->len) return elm_nothing();"
        , "    uint16_t val;"
        , "    if (endian.tag == 920) { /* LE */"
        , "        val = st->data[st->pos] | ((uint16_t)st->data[st->pos + 1] << 8);"
        , "    } else {"
        , "        val = ((uint16_t)st->data[st->pos] << 8) | st->data[st->pos + 1];"
        , "    }"
        , "    st->pos += 2;"
        , "    return elm_just(elm_int(val));"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_unsignedInt16(elm_value_t endian) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Bytes_Decode_unsignedInt16_impl;"
        , "    c->arity = 2;"
        , "    c->applied = 1;"
        , "    c->args[0] = endian;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_signedInt32_impl(elm_value_t endian, elm_value_t state_val) {"
        , "    bytes_decoder_state_t *st = (bytes_decoder_state_t *)state_val.data.p;"
        , "    if (st->pos + 4 > st->len) return elm_nothing();"
        , "    int32_t val;"
        , "    if (endian.tag == 920) { /* LE */"
        , "        val = st->data[st->pos] | ((int32_t)st->data[st->pos+1] << 8) |"
        , "              ((int32_t)st->data[st->pos+2] << 16) | ((int32_t)st->data[st->pos+3] << 24);"
        , "    } else {"
        , "        val = ((int32_t)st->data[st->pos] << 24) | ((int32_t)st->data[st->pos+1] << 16) |"
        , "              ((int32_t)st->data[st->pos+2] << 8) | st->data[st->pos+3];"
        , "    }"
        , "    st->pos += 4;"
        , "    return elm_just(elm_int(val));"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_signedInt32(elm_value_t endian) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Bytes_Decode_signedInt32_impl;"
        , "    c->arity = 2;"
        , "    c->applied = 1;"
        , "    c->args[0] = endian;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_unsignedInt32_impl(elm_value_t endian, elm_value_t state_val) {"
        , "    bytes_decoder_state_t *st = (bytes_decoder_state_t *)state_val.data.p;"
        , "    if (st->pos + 4 > st->len) return elm_nothing();"
        , "    uint32_t val;"
        , "    if (endian.tag == 920) { /* LE */"
        , "        val = st->data[st->pos] | ((uint32_t)st->data[st->pos+1] << 8) |"
        , "              ((uint32_t)st->data[st->pos+2] << 16) | ((uint32_t)st->data[st->pos+3] << 24);"
        , "    } else {"
        , "        val = ((uint32_t)st->data[st->pos] << 24) | ((uint32_t)st->data[st->pos+1] << 16) |"
        , "              ((uint32_t)st->data[st->pos+2] << 8) | st->data[st->pos+3];"
        , "    }"
        , "    st->pos += 4;"
        , "    return elm_just(elm_int(val));"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_unsignedInt32(elm_value_t endian) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Bytes_Decode_unsignedInt32_impl;"
        , "    c->arity = 2;"
        , "    c->applied = 1;"
        , "    c->args[0] = endian;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_float32_impl(elm_value_t endian, elm_value_t state_val) {"
        , "    bytes_decoder_state_t *st = (bytes_decoder_state_t *)state_val.data.p;"
        , "    if (st->pos + 4 > st->len) return elm_nothing();"
        , "    uint32_t bits;"
        , "    if (endian.tag == 920) { /* LE */"
        , "        bits = st->data[st->pos] | ((uint32_t)st->data[st->pos+1] << 8) |"
        , "               ((uint32_t)st->data[st->pos+2] << 16) | ((uint32_t)st->data[st->pos+3] << 24);"
        , "    } else {"
        , "        bits = ((uint32_t)st->data[st->pos] << 24) | ((uint32_t)st->data[st->pos+1] << 16) |"
        , "               ((uint32_t)st->data[st->pos+2] << 8) | st->data[st->pos+3];"
        , "    }"
        , "    float val;"
        , "    memcpy(&val, &bits, 4);"
        , "    st->pos += 4;"
        , "    return elm_just(elm_float(val));"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_float32(elm_value_t endian) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Bytes_Decode_float32_impl;"
        , "    c->arity = 2;"
        , "    c->applied = 1;"
        , "    c->args[0] = endian;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_float64_impl(elm_value_t endian, elm_value_t state_val) {"
        , "    bytes_decoder_state_t *st = (bytes_decoder_state_t *)state_val.data.p;"
        , "    if (st->pos + 8 > st->len) return elm_nothing();"
        , "    uint64_t bits = 0;"
        , "    if (endian.tag == 920) { /* LE */"
        , "        for (int i = 0; i < 8; i++) bits |= ((uint64_t)st->data[st->pos + i] << (i * 8));"
        , "    } else {"
        , "        for (int i = 0; i < 8; i++) bits |= ((uint64_t)st->data[st->pos + i] << ((7-i) * 8));"
        , "    }"
        , "    double val;"
        , "    memcpy(&val, &bits, 8);"
        , "    st->pos += 8;"
        , "    return elm_just(elm_float(val));"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_float64(elm_value_t endian) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Bytes_Decode_float64_impl;"
        , "    c->arity = 2;"
        , "    c->applied = 1;"
        , "    c->args[0] = endian;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_bytes_impl(elm_value_t n, elm_value_t state_val) {"
        , "    bytes_decoder_state_t *st = (bytes_decoder_state_t *)state_val.data.p;"
        , "    size_t len = (size_t)n.data.i;"
        , "    if (st->pos + len > st->len) return elm_nothing();"
        , "    uint8_t *data = malloc(len);"
        , "    memcpy(data, st->data + st->pos, len);"
        , "    st->pos += len;"
        , "    return elm_just(elm_bytes(data, len));"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_bytes(elm_value_t n) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Bytes_Decode_bytes_impl;"
        , "    c->arity = 2;"
        , "    c->applied = 1;"
        , "    c->args[0] = n;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_string_impl(elm_value_t n, elm_value_t state_val) {"
        , "    bytes_decoder_state_t *st = (bytes_decoder_state_t *)state_val.data.p;"
        , "    size_t len = (size_t)n.data.i;"
        , "    if (st->pos + len > st->len) return elm_nothing();"
        , "    char *str = malloc(len + 1);"
        , "    memcpy(str, st->data + st->pos, len);"
        , "    str[len] = '\\0';"
        , "    st->pos += len;"
        , "    return elm_just(elm_string(str));"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_string(elm_value_t n) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Bytes_Decode_string_impl;"
        , "    c->arity = 2;"
        , "    c->applied = 1;"
        , "    c->args[0] = n;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_succeed_impl(elm_value_t val, elm_value_t _state) {"
        , "    (void)_state;"
        , "    return elm_just(val);"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_succeed(elm_value_t val) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Bytes_Decode_succeed_impl;"
        , "    c->arity = 2;"
        , "    c->applied = 1;"
        , "    c->args[0] = val;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_fail_impl(elm_value_t _state) {"
        , "    (void)_state;"
        , "    return elm_nothing();"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_fail(void) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Bytes_Decode_fail_impl;"
        , "    c->arity = 1;"
        , "    c->applied = 0;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_map_impl(elm_value_t f, elm_value_t decoder, elm_value_t state_val) {"
        , "    elm_value_t result = elm_apply1((elm_closure_t *)decoder.data.p, state_val);"
        , "    if (result.tag == 200) return result; /* Nothing */"
        , "    return elm_just(elm_apply1((elm_closure_t *)f.data.p, *result.data.c));"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_map(elm_value_t f, elm_value_t decoder) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Bytes_Decode_map_impl;"
        , "    c->arity = 3;"
        , "    c->applied = 2;"
        , "    c->args[0] = f;"
        , "    c->args[1] = decoder;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_map2_impl(elm_value_t f, elm_value_t d1, elm_value_t d2, elm_value_t state_val) {"
        , "    elm_value_t r1 = elm_apply1((elm_closure_t *)d1.data.p, state_val);"
        , "    if (r1.tag == 200) return r1;"
        , "    elm_value_t r2 = elm_apply1((elm_closure_t *)d2.data.p, state_val);"
        , "    if (r2.tag == 200) return r2;"
        , "    elm_value_t partial = elm_apply1((elm_closure_t *)f.data.p, *r1.data.c);"
        , "    return elm_just(elm_apply1((elm_closure_t *)partial.data.p, *r2.data.c));"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_map2(elm_value_t f, elm_value_t d1, elm_value_t d2) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Bytes_Decode_map2_impl;"
        , "    c->arity = 4;"
        , "    c->applied = 3;"
        , "    c->args[0] = f;"
        , "    c->args[1] = d1;"
        , "    c->args[2] = d2;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_andThen_impl(elm_value_t callback, elm_value_t decoder, elm_value_t state_val) {"
        , "    elm_value_t result = elm_apply1((elm_closure_t *)decoder.data.p, state_val);"
        , "    if (result.tag == 200) return result;"
        , "    elm_value_t nextDecoder = elm_apply1((elm_closure_t *)callback.data.p, *result.data.c);"
        , "    return elm_apply1((elm_closure_t *)nextDecoder.data.p, state_val);"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_andThen(elm_value_t callback, elm_value_t decoder) {"
        , "    elm_closure_t *c = elm_alloc_closure();"
        , "    c->func = (void *)elm_Bytes_Decode_andThen_impl;"
        , "    c->arity = 3;"
        , "    c->applied = 2;"
        , "    c->args[0] = callback;"
        , "    c->args[1] = decoder;"
        , "    return (elm_value_t){ .tag = 400, .data.p = c, .next = NULL };"
        , "}"
        , ""
        , "static elm_value_t elm_Bytes_Decode_decode(elm_value_t decoder, elm_value_t bytesVal) {"
        , "    bytes_t *bytes = (bytes_t *)bytesVal.data.p;"
        , "    bytes_decoder_state_t *st = malloc(sizeof(bytes_decoder_state_t));"
        , "    st->data = bytes->data;"
        , "    st->len = bytes->len;"
        , "    st->pos = 0;"
        , "    elm_value_t state_val = { .tag = 912, .data.p = st, .next = NULL };"
        , "    return elm_apply1((elm_closure_t *)decoder.data.p, state_val);"
        , "}"
        , ""
        , "/* ===== DEBUG MODULE ===== */"
        , ""
        , "/* Forward declaration of print_value */"
        , "static void debug_print_value(elm_value_t v);"
        , ""
        , "static void debug_print_value(elm_value_t v) {"
        , "    switch (v.tag) {"
        , "        case 0: fprintf(stderr, \"%ld\", (long)v.data.i); break;"
        , "        case 1: fprintf(stderr, \"%g\", v.data.f); break;"
        , "        case 2: fprintf(stderr, \"\\\"%s\\\"\", v.data.s); break;"
        , "        case 3: fprintf(stderr, \"'%c'\", (char)v.data.i); break;"
        , "        case 4: fprintf(stderr, \"False\"); break;"
        , "        case 5: fprintf(stderr, \"True\"); break;"
        , "        case 6: fprintf(stderr, \"()\"); break;"
        , "        case 100: fprintf(stderr, \"[]\"); break;"
        , "        case 101: {"
        , "            fprintf(stderr, \"[\");"
        , "            elm_value_t curr = v;"
        , "            int first = 1;"
        , "            while (curr.tag == 101) {"
        , "                if (!first) fprintf(stderr, \", \");"
        , "                first = 0;"
        , "                debug_print_value(*curr.data.c);"
        , "                curr = *curr.next;"
        , "            }"
        , "            fprintf(stderr, \"]\");"
        , "            break;"
        , "        }"
        , "        case 200: fprintf(stderr, \"Nothing\"); break;"
        , "        case 201: {"
        , "            fprintf(stderr, \"Just \");"
        , "            debug_print_value(*v.data.c);"
        , "            break;"
        , "        }"
        , "        case 300: {"
        , "            fprintf(stderr, \"Err \");"
        , "            debug_print_value(*v.data.c);"
        , "            break;"
        , "        }"
        , "        case 301: {"
        , "            fprintf(stderr, \"Ok \");"
        , "            debug_print_value(*v.data.c);"
        , "            break;"
        , "        }"
        , "        case 600: {"
        , "            fprintf(stderr, \"(\");"
        , "            debug_print_value(*v.data.c);"
        , "            fprintf(stderr, \", \");"
        , "            debug_print_value(*v.next);"
        , "            fprintf(stderr, \")\");"
        , "            break;"
        , "        }"
        , "        case 800: {"
        , "            fprintf(stderr, \"Task.fail \");"
        , "            debug_print_value(*v.data.c);"
        , "            break;"
        , "        }"
        , "        case 801: {"
        , "            fprintf(stderr, \"Task.succeed \");"
        , "            debug_print_value(*v.data.c);"
        , "            break;"
        , "        }"
        , "        default: fprintf(stderr, \"<value:%d>\", v.tag); break;"
        , "    }"
        , "}"
        , ""
        , "static elm_value_t elm_Debug_log(elm_value_t msg, elm_value_t value) {"
        , "    fprintf(stderr, \"%s: \", msg.data.s);"
        , "    debug_print_value(value);"
        , "    fprintf(stderr, \"\\n\");"
        , "    return value;"
        , "}"
        , ""
        , "/* Helper to write escaped string */"
        , "static int debug_write_string(char *buf, int pos, int max, const char *s) {"
        , "    buf[pos++] = '\"';"
        , "    while (*s && pos < max - 2) {"
        , "        if (*s == '\"' || *s == '\\\\') buf[pos++] = '\\\\';"
        , "        buf[pos++] = *s++;"
        , "    }"
        , "    buf[pos++] = '\"';"
        , "    buf[pos] = '\\0';"
        , "    return pos;"
        , "}"
        , ""
        , "/* Helper to write value to buffer recursively */"
        , "static int debug_write_value(char *buf, int pos, int max, elm_value_t value) {"
        , "    switch (value.tag) {"
        , "        case 0: return pos + snprintf(buf + pos, max - pos, \"%ld\", (long)value.data.i);"
        , "        case 1: return pos + snprintf(buf + pos, max - pos, \"%g\", value.data.f);"
        , "        case 2: return debug_write_string(buf, pos, max, value.data.s);"
        , "        case 3: return pos + snprintf(buf + pos, max - pos, \"'%c'\", (char)value.data.i);"
        , "        case 4: return pos + snprintf(buf + pos, max - pos, \"False\");"
        , "        case 5: return pos + snprintf(buf + pos, max - pos, \"True\");"
        , "        case 6: return pos + snprintf(buf + pos, max - pos, \"()\");"
        , "        case 100: return pos + snprintf(buf + pos, max - pos, \"[]\");"
        , "        case 101: { /* Cons */"
        , "            pos += snprintf(buf + pos, max - pos, \"[\");"
        , "            elm_value_t *node = (elm_value_t *)value.data.p;"
        , "            int first = 1;"
        , "            while (node && node->tag == 101) {"
        , "                if (!first) pos += snprintf(buf + pos, max - pos, \",\");"
        , "                first = 0;"
        , "                elm_value_t *pair = (elm_value_t *)node->data.p;"
        , "                pos = debug_write_value(buf, pos, max, pair[0]);"
        , "                node = &pair[1];"
        , "                if (node->tag == 100) break;"
        , "            }"
        , "            return pos + snprintf(buf + pos, max - pos, \"]\");"
        , "        }"
        , "        case 200: return pos + snprintf(buf + pos, max - pos, \"Nothing\");"
        , "        case 201: { /* Just */"
        , "            pos += snprintf(buf + pos, max - pos, \"Just \");"
        , "            return debug_write_value(buf, pos, max, *(elm_value_t *)value.data.p);"
        , "        }"
        , "        case 300: { /* Err */"
        , "            pos += snprintf(buf + pos, max - pos, \"Err \");"
        , "            return debug_write_value(buf, pos, max, *(elm_value_t *)value.data.p);"
        , "        }"
        , "        case 301: { /* Ok */"
        , "            pos += snprintf(buf + pos, max - pos, \"Ok \");"
        , "            return debug_write_value(buf, pos, max, *(elm_value_t *)value.data.p);"
        , "        }"
        , "        case 600: { /* Tuple */"
        , "            elm_value_t *fields = (elm_value_t *)value.data.p;"
        , "            int arity = 2;"
        , "            if (value.next) arity = (int)(intptr_t)value.next;"
        , "            pos += snprintf(buf + pos, max - pos, \"(\");"
        , "            for (int i = 0; i < arity; i++) {"
        , "                if (i > 0) pos += snprintf(buf + pos, max - pos, \",\");"
        , "                pos = debug_write_value(buf, pos, max, fields[i]);"
        , "            }"
        , "            return pos + snprintf(buf + pos, max - pos, \")\");"
        , "        }"
        , "        case 700: return pos + snprintf(buf + pos, max - pos, \"LT\");"
        , "        case 701: return pos + snprintf(buf + pos, max - pos, \"EQ\");"
        , "        case 702: return pos + snprintf(buf + pos, max - pos, \"GT\");"
        , "        default: return pos + snprintf(buf + pos, max - pos, \"<value:%d>\", value.tag);"
        , "    }"
        , "}"
        , ""
        , "static elm_value_t elm_Debug_toString(elm_value_t value) {"
        , "    char *buf = malloc(8192);"
        , "    debug_write_value(buf, 0, 8192, value);"
        , "    return elm_string(buf);"
        , "}"
        , ""
        , "/* Debug.todo - crash with message (for incomplete code) */"
        , "static elm_value_t elm_Debug_todo(elm_value_t msg) {"
        , "    fprintf(stderr, \"TODO: %s\\n\", msg.data.s);"
        , "    exit(1);"
        , "    return elm_unit(); /* Never reached */"
        , "}"
        ]



-- TYPE DECLARATIONS


generateTypeDecls : GenCtx -> String
generateTypeDecls ctx =
    let
        dataDecls =
            ctx.dataTypes
                |> Dict.values
                |> List.map (generateDataType ctx)
                |> String.join "\n\n"

        classDecls =
            ctx.classes
                |> Dict.values
                |> List.map (generateClassDecl ctx)
                |> String.join "\n\n"
    in
    if String.isEmpty dataDecls && String.isEmpty classDecls then
        ""
    else
        "/* ===== TYPES ===== */\n\n" ++ dataDecls ++ "\n\n" ++ classDecls


generateDataType : GenCtx -> Core.DataDef -> String
generateDataType ctx dataDef =
    let
        tagDefs =
            dataDef.constructors
                |> List.map (\ctor ->
                    -- Use just constructor name for tag (constructors are unique within module)
                    "#define TAG_" ++ ctor.name ++ " " ++ String.fromInt (1000 + ctor.tag)
                )
                |> String.join "\n"

        ctorFuncs =
            dataDef.constructors
                |> List.map (generateConstructor ctx dataDef.name)
                |> String.join "\n\n"
    in
    "/* " ++ dataDef.name ++ " type tags */\n" ++ tagDefs ++ "\n\n" ++ ctorFuncs


generateConstructor : GenCtx -> String -> Core.DataConDef -> String
generateConstructor _ typeName ctor =
    let
        -- Use just constructor name for tag (constructors are unique within module)
        tag = "TAG_" ++ ctor.name
    in
    if ctor.fields == [] then
        -- Nullary constructor
        "static elm_value_t elm_" ++ ctor.name ++ "(void) {\n"
            ++ "    return (elm_value_t){ .tag = " ++ tag ++ ", .data.p = NULL, .next = NULL };\n"
            ++ "}"
    else if List.length ctor.fields == 1 then
        -- Unary constructor
        "static elm_value_t elm_" ++ ctor.name ++ "(elm_value_t v0) {\n"
            ++ "    elm_value_t *p = elm_alloc();\n"
            ++ "    *p = v0;\n"
            ++ "    return (elm_value_t){ .tag = " ++ tag ++ ", .data.c = p, .next = NULL };\n"
            ++ "}"
    else
        -- Multi-arg constructor
        let
            params =
                List.indexedMap (\i _ -> "elm_value_t v" ++ String.fromInt i) ctor.fields
                    |> String.join ", "

            body =
                List.indexedMap
                    (\i _ ->
                        "    elm_value_t *p" ++ String.fromInt i ++ " = elm_alloc();\n"
                            ++ "    *p" ++ String.fromInt i ++ " = v" ++ String.fromInt i ++ ";"
                    )
                    ctor.fields
                    |> String.join "\n"

            -- Chain them together
            chainSetup =
                List.indexedMap
                    (\i _ ->
                        if i < List.length ctor.fields - 1 then
                            "    p" ++ String.fromInt i ++ "->next = p" ++ String.fromInt (i + 1) ++ ";"
                        else
                            ""
                    )
                    ctor.fields
                    |> List.filter (not << String.isEmpty)
                    |> String.join "\n"
        in
        "static elm_value_t elm_" ++ ctor.name ++ "(" ++ params ++ ") {\n"
            ++ body ++ "\n"
            ++ chainSetup ++ "\n"
            ++ "    return (elm_value_t){ .tag = " ++ tag ++ ", .data.c = p0, .next = NULL };\n"
            ++ "}"


generateClassDecl : GenCtx -> Core.ClassDef -> String
generateClassDecl _ classDef =
    let
        methodSlots =
            classDef.methods
                |> List.indexedMap (\i ( name, _ ) ->
                    "#define SLOT_" ++ classDef.name ++ "_" ++ name ++ " " ++ String.fromInt i
                )
                |> String.join "\n"
    in
    "/* " ++ classDef.name ++ " type class */\n" ++ methodSlots



-- FORWARD DECLARATIONS


generateForwardDecls : GenCtx -> String
generateForwardDecls ctx =
    let
        decls =
            ctx.functions
                |> Dict.values
                |> List.map (generateForwardDecl ctx)
                |> String.join "\n"
    in
    if String.isEmpty decls then
        ""
    else
        "/* ===== FORWARD DECLARATIONS ===== */\n" ++ decls


generateForwardDecl : GenCtx -> Core.FuncDef -> String
generateForwardDecl ctx funcDef =
    let
        params =
            if List.isEmpty funcDef.args then
                "void"
            else
                funcDef.args
                    |> List.map (\tv -> "elm_value_t " ++ mangle tv.name)
                    |> String.join ", "
    in
    "static elm_value_t " ++ mangle (ctx.moduleName ++ "_" ++ funcDef.name) ++ "(" ++ params ++ ");"



-- FOREIGN FUNCTIONS


{-| Generate extern declarations for foreign C functions.
-}
generateForeignExterns : GenCtx -> String
generateForeignExterns ctx =
    if Dict.isEmpty ctx.foreignFunctions then
        ""
    else
        let
            externs =
                ctx.foreignFunctions
                    |> Dict.values
                    |> List.map generateForeignExtern
                    |> String.join "\n"
        in
        "/* ===== FOREIGN FUNCTION EXTERNS ===== */\n" ++ externs


generateForeignExtern : Core.ForeignDef -> String
generateForeignExtern foreignDef =
    -- Generate extern declaration based on the type signature
    let
        ( argTypes, returnType ) = collectForeignArgTypes foreignDef.type_
        cReturnType = typeToCType returnType
        cArgTypes =
            if List.isEmpty argTypes then
                "void"
            else
                argTypes
                    |> List.indexedMap (\i ty -> typeToCType ty ++ " arg" ++ String.fromInt i)
                    |> String.join ", "
    in
    "extern " ++ cReturnType ++ " " ++ foreignDef.cName ++ "(" ++ cArgTypes ++ ");"


{-| Generate wrapper functions that call foreign C functions.
-}
generateForeignWrappers : GenCtx -> String
generateForeignWrappers ctx =
    ctx.foreignFunctions
        |> Dict.values
        |> List.map (generateForeignWrapper ctx)
        |> String.join "\n\n"


generateForeignWrapper : GenCtx -> Core.ForeignDef -> String
generateForeignWrapper ctx foreignDef =
    let
        ( argTypes, returnType ) = collectForeignArgTypes foreignDef.type_
        argCount = List.length argTypes

        -- Generate parameter list for wrapper
        wrapperParams =
            if argCount == 0 then
                "void"
            else
                List.range 0 (argCount - 1)
                    |> List.map (\i -> "elm_value_t arg" ++ String.fromInt i)
                    |> String.join ", "

        -- Generate argument conversions (elm_value_t -> C type)
        argConversions =
            List.indexedMap (\i ty ->
                let
                    cType = typeToCType ty
                    argName = "arg" ++ String.fromInt i
                    cArgName = "c_arg" ++ String.fromInt i
                in
                "    " ++ cType ++ " " ++ cArgName ++ " = " ++ elmValueToC ty argName ++ ";"
            ) argTypes

        -- Call the foreign function
        cArgs =
            List.range 0 (argCount - 1)
                |> List.map (\i -> "c_arg" ++ String.fromInt i)
                |> String.join ", "

        -- Convert return value (C type -> elm_value_t)
        returnConversion =
            if isUnitType returnType then
                [ "    " ++ foreignDef.cName ++ "(" ++ cArgs ++ ");"
                , "    return elm_unit();"
                ]
            else
                [ "    " ++ typeToCType returnType ++ " result = " ++ foreignDef.cName ++ "(" ++ cArgs ++ ");"
                , "    return " ++ cToElmValue returnType "result" ++ ";"
                ]

        wrapperName = mangle (ctx.moduleName ++ "_" ++ foreignDef.name)
    in
    String.join "\n"
        ([ "static elm_value_t " ++ wrapperName ++ "(" ++ wrapperParams ++ ") {" ]
         ++ argConversions
         ++ returnConversion
         ++ [ "}" ]
        )


{-| Collect argument types and return type from a function type scheme.
-}
collectForeignArgTypes : Scheme -> ( List Type, Type )
collectForeignArgTypes (Scheme _ _ ty) =
    collectArrowTypes ty


collectArrowTypes : Type -> ( List Type, Type )
collectArrowTypes ty =
    case ty of
        TArrow arg rest ->
            let
                ( moreArgs, ret ) = collectArrowTypes rest
            in
            ( arg :: moreArgs, ret )

        _ ->
            ( [], ty )


{-| Convert a Type to a C type string.
-}
typeToCType : Type -> String
typeToCType ty =
    case ty of
        TCon "Int" -> "int"
        TCon "Float" -> "double"
        TCon "Bool" -> "int"
        TCon "String" -> "const char*"
        TCon "()" -> "void"
        TCon "Char" -> "char"
        TApp (TCon "Ptr") _ -> "void*"
        TApp (TCon "Task") _ -> "void*"  -- Task becomes opaque pointer
        _ -> "elm_value_t"  -- For complex types, use the generic value type


{-| Convert an elm_value_t to a C value.
-}
elmValueToC : Type -> String -> String
elmValueToC ty varName =
    case ty of
        TCon "Int" -> varName ++ ".data.i"
        TCon "Float" -> varName ++ ".data.d"
        TCon "Bool" -> varName ++ ".data.i"
        TCon "String" -> varName ++ ".data.s"
        TCon "Char" -> "(char)" ++ varName ++ ".data.i"
        TApp (TCon "Ptr") _ -> varName ++ ".data.p"
        _ -> varName  -- For complex types, pass as-is


{-| Convert a C value to an elm_value_t.
-}
cToElmValue : Type -> String -> String
cToElmValue ty varName =
    case ty of
        TCon "Int" -> "elm_int(" ++ varName ++ ")"
        TCon "Float" -> "elm_float(" ++ varName ++ ")"
        TCon "Bool" -> "elm_bool(" ++ varName ++ ")"
        TCon "String" -> "elm_string(" ++ varName ++ ")"
        TCon "Char" -> "elm_int((int)" ++ varName ++ ")"
        TCon "()" -> "elm_unit()"
        TApp (TCon "Ptr") _ -> "elm_ptr(" ++ varName ++ ")"
        _ -> varName  -- For complex types, assume it's already elm_value_t


{-| Check if a type is the unit type.
-}
isUnitType : Type -> Bool
isUnitType ty =
    case ty of
        TCon "()" -> True
        TTuple [] -> True
        _ -> False


-- FUNCTIONS


{-| Generate all functions, accumulating lifted lambdas in the context.
Returns (function code, updated context with lifted lambdas).
-}
generateFunctionsWithLambdas : GenCtx -> ( String, GenCtx )
generateFunctionsWithLambdas ctx =
    let
        funcDefs = Dict.values ctx.functions

        -- Generate each function, threading the context through
        ( funcCodes, finalCtx ) =
            List.foldl
                (\funcDef ( codes, accCtx ) ->
                    let
                        ( code, newCtx ) = generateFunctionWithLambdas accCtx funcDef
                    in
                    ( codes ++ [ code ], newCtx )
                )
                ( [], ctx )
                funcDefs
    in
    ( String.join "\n\n" funcCodes, finalCtx )


{-| Generate a single function, accumulating any lifted lambdas.
-}
generateFunctionWithLambdas : GenCtx -> Core.FuncDef -> ( String, GenCtx )
generateFunctionWithLambdas ctx funcDef =
    let
        fullName = mangle (ctx.moduleName ++ "_" ++ funcDef.name)

        params =
            if List.isEmpty funcDef.args then
                "void"
            else
                funcDef.args
                    |> List.map (\tv -> "elm_value_t " ++ mangle tv.name)
                    |> String.join ", "

        -- Track function parameters as potential closures
        paramNames = List.map .name funcDef.args
        bodyCtx =
            { ctx
            | indent = ctx.indent + 1
            , closureParams = Set.union ctx.closureParams (Set.fromList paramNames)
            }

        ( bodyCode, finalCtx ) = generateExprAccum bodyCtx Dict.empty funcDef.body

        funcCode =
            "static elm_value_t " ++ fullName ++ "(" ++ params ++ ") {\n"
                ++ indentStr bodyCtx ++ "return " ++ bodyCode ++ ";\n"
                ++ "}"
    in
    ( funcCode, finalCtx )


-- Keep the old generateFunctions for backwards compatibility (not used in main path)
generateFunctions : GenCtx -> String
generateFunctions ctx =
    let
        ( code, _ ) = generateFunctionsWithLambdas ctx
    in
    code


generateFunction : GenCtx -> Core.FuncDef -> String
generateFunction ctx funcDef =
    let
        ( code, _ ) = generateFunctionWithLambdas ctx funcDef
    in
    code



-- EXPRESSIONS


{-| Generate expression code, accumulating lifted lambdas in context.
Returns (generated code, updated context).
-}
generateExprAccum : GenCtx -> Dict String String -> Core.Expr -> ( String, GenCtx )
generateExprAccum ctx renames expr =
    case expr of
        Core.EVar tv ->
            let
                name = tv.name
            in
            case Dict.get name renames of
                Just renamed ->
                    ( renamed, ctx )

                Nothing ->
                    let
                        mangledName = mangleWithModule ctx name
                        arity = getFunctionArity ctx name
                        -- Check if this is a local variable (shadows builtins with same name)
                        isLocalVar = Set.member name ctx.localVars
                    in
                    let
                        foreignArity = getForeignArity ctx name
                    in
                    if Set.member name ctx.closureParams then
                        ( mangledName, ctx )
                    else if isLocalVar then
                        -- Local variable, use as-is (it's a closure value)
                        ( mangledName, ctx )
                    else if Dict.member name ctx.functions && arity > 0 then
                        ( generateFunctionClosure ctx mangledName arity, ctx )
                    else if arity == 0 && Dict.member name ctx.functions then
                        ( mangledName ++ "()", ctx )
                    else if isBuiltin name && arity > 0 then
                        -- Builtin function used as a value - wrap in closure
                        ( generateFunctionClosure ctx mangledName arity, ctx )
                    else if isBuiltin name && arity == 0 then
                        -- Zero-arity builtin, call it
                        ( mangledName ++ "()", ctx )
                    else if isForeign ctx name && foreignArity > 0 then
                        -- Foreign function used as a value - wrap in closure
                        ( generateFunctionClosure ctx mangledName foreignArity, ctx )
                    else if isForeign ctx name && foreignArity == 0 then
                        -- Zero-arity foreign function, call it
                        ( mangledName ++ "()", ctx )
                    else
                        ( mangledName, ctx )

        Core.ELit lit _ ->
            ( generateLiteral lit, ctx )

        Core.EApp func arg _ ->
            generateAppAccum ctx renames func arg

        Core.ELam tv body _ ->
            generateLambdaAccum ctx renames tv body

        Core.ELet var value body _ ->
            generateLetAccum ctx renames var value body

        Core.ELetRec bindings body _ ->
            generateLetRecAccum ctx renames bindings body

        Core.ECase scrutinee alts _ ->
            generateCaseAccum ctx renames scrutinee alts

        Core.ECon name args _ ->
            generateConAccum ctx renames name args

        Core.ETuple exprs _ ->
            generateTupleAccum ctx renames exprs

        Core.ERecord fields _ ->
            generateRecordAccum ctx renames fields

        Core.ERecordAccess record field _ ->
            let
                ( recordCode, ctx1 ) = generateExprAccum ctx renames record
            in
            -- Use elm_record_get to lookup field by name in linked list representation
            ( "elm_record_get(\"" ++ field ++ "\", " ++ recordCode ++ ")", ctx1 )

        Core.ERecordUpdate record updates _ ->
            -- Generate record update: { record | field1 = val1, ... }
            -- Build a new record with updated fields prepended
            -- This works with linked-list representation where first match wins
            let
                ( recordCode, ctx1 ) = generateExprAccum ctx renames record

                -- Build the chain of updated fields
                updateChain =
                    updates
                        |> List.foldl
                            (\( fieldName, fieldExpr ) acc ->
                                let
                                    ( fieldCode, _ ) = generateExprAccum ctx renames fieldExpr
                                in
                                "elm_record_field(\"" ++ fieldName ++ "\", " ++ fieldCode ++ ", " ++ acc ++ ")"
                            )
                            recordCode
            in
            ( updateChain, ctx1 )

        Core.ETyApp e _ _ ->
            generateExprAccum ctx renames e

        Core.ETyLam _ e _ ->
            generateExprAccum ctx renames e

        Core.EDictApp func dict _ ->
            let
                ( funcCode, ctx1 ) = generateExprAccum ctx renames func
                ( dictCode, ctx2 ) = generateExprAccum ctx1 renames dict
            in
            ( funcCode ++ "(" ++ dictCode ++ ")", ctx2 )

        Core.EDictLam tv body _ ->
            generateLambdaAccum ctx renames tv body

        Core.EDict className _ _ ->
            ( "(elm_dict_t *)&dict_" ++ className, ctx )


{-| Generate a lambda as a top-level static function.
Adds the function to liftedLambdas and returns the closure creation code.
-}
generateLambdaAccum : GenCtx -> Dict String String -> Core.TypedVar -> Core.Expr -> ( String, GenCtx )
generateLambdaAccum ctx renames tv body =
    let
        ( funcName, ctx1 ) = freshName "__lambda" ctx
        argName = mangle tv.name

        -- Compute free variables
        bodyFreeVars = Core.freeVars body
        freeVarsExcludingParam = Set.remove tv.name bodyFreeVars

        captures =
            freeVarsExcludingParam
                |> Set.filter (\v ->
                    not (Dict.member v ctx1.functions)
                        && not (isBuiltin v)
                )
                |> Set.toList

        numCaptures = List.length captures

        -- Parameters for the lifted function
        captureParams =
            captures
                |> List.indexedMap (\i _ -> "elm_value_t _cap" ++ String.fromInt i)

        allParams = captureParams ++ [ "elm_value_t " ++ argName ]
        paramsStr = String.join ", " allParams

        -- Renames for the lambda body
        innerRenames =
            captures
                |> List.indexedMap (\i name -> ( name, "_cap" ++ String.fromInt i ))
                |> Dict.fromList

        bodyCtx =
            { ctx1
            | closureParams = Set.insert tv.name ctx1.closureParams
            , localVars = Set.union ctx1.localVars (Set.fromList captures)
            }

        -- Generate body with accumulated lambdas
        ( bodyCode, ctx2 ) = generateExprAccum bodyCtx innerRenames body

        -- Create the lifted lambda definition
        lambdaDef =
            "static elm_value_t " ++ funcName ++ "(" ++ paramsStr ++ ") {\n"
                ++ "    return " ++ bodyCode ++ ";\n"
                ++ "}"

        -- Add to lifted lambdas
        ctx3 =
            { ctx2
            | liftedLambdas = { name = funcName, definition = lambdaDef } :: ctx2.liftedLambdas
            }

        -- Generate closure creation code
        captureAssignments =
            captures
                |> List.indexedMap (\i name ->
                    let
                        sourceValue =
                            case Dict.get name renames of
                                Just outerRenamed -> outerRenamed
                                Nothing -> mangle name
                    in
                    "_c->args[" ++ String.fromInt i ++ "] = " ++ sourceValue ++ ";"
                )
                |> String.join " "

        totalArity = numCaptures + 1

        closureCode =
            "({"
                ++ " elm_closure_t *_c = elm_alloc_closure();"
                ++ " _c->func = (void *)" ++ funcName ++ ";"
                ++ " _c->arity = " ++ String.fromInt totalArity ++ ";"
                ++ " _c->applied = " ++ String.fromInt numCaptures ++ ";"
                ++ " " ++ captureAssignments
                ++ " (elm_value_t){ .tag = 400, .data.p = _c, .next = NULL };"
                ++ " })"
    in
    ( closureCode, ctx3 )


generateAppAccum : GenCtx -> Dict String String -> Core.Expr -> Core.Expr -> ( String, GenCtx )
generateAppAccum ctx renames func arg =
    let
        ( baseFunc, allArgs ) = collectArgs func [ arg ]
    in
    case baseFunc of
        Core.EVar tv ->
            let
                funcName = tv.name
            in
            case Dict.get funcName renames of
                Just renamed ->
                    generateClosureApplyAccum ctx renames renamed allArgs

                Nothing ->
                    let
                        mangledName = mangleWithModule ctx funcName
                        -- Check if this is a local variable (shadows builtins with same name)
                        isLocalVar = Set.member funcName ctx.localVars
                        isForeignFunc = isForeign ctx funcName
                        isKnownFunction = not isLocalVar && (Dict.member funcName ctx.functions || isBuiltin funcName || isForeignFunc)
                    in
                    if Set.member funcName ctx.closureParams || not isKnownFunction then
                        generateClosureApplyAccum ctx renames mangledName allArgs
                    else
                        let
                            -- Get arity: for foreign functions use getForeignArity, otherwise getFunctionArity
                            arity =
                                if isForeignFunc then
                                    getForeignArity ctx funcName
                                else
                                    getFunctionArity ctx funcName
                            argCount = List.length allArgs
                        in
                        if arity == 0 then
                            generateClosureApplyAccum ctx renames (mangledName ++ "()") allArgs
                        else if argCount == arity then
                            -- Full application
                            let
                                ( argCodes, finalCtx ) =
                                    List.foldl
                                        (\a ( codes, accCtx ) ->
                                            let
                                                ( code, newCtx ) = generateExprAccum accCtx renames a
                                            in
                                            ( codes ++ [ code ], newCtx )
                                        )
                                        ( [], ctx )
                                        allArgs
                            in
                            ( mangledName ++ "(" ++ String.join ", " argCodes ++ ")", finalCtx )
                        else if argCount < arity then
                            generatePartialAppAccum ctx renames mangledName allArgs arity
                        else
                            let
                                ( directArgs, extraArgs ) = splitAt arity allArgs
                                ( directCodes, ctx1 ) =
                                    List.foldl
                                        (\a ( codes, accCtx ) ->
                                            let
                                                ( code, newCtx ) = generateExprAccum accCtx renames a
                                            in
                                            ( codes ++ [ code ], newCtx )
                                        )
                                        ( [], ctx )
                                        directArgs
                                baseCall = mangledName ++ "(" ++ String.join ", " directCodes ++ ")"
                            in
                            generateClosureApplyAccum ctx1 renames baseCall extraArgs

        Core.ECon name existingArgs _ ->
            generateConAccum ctx renames name (existingArgs ++ allArgs)

        _ ->
            let
                ( baseFuncCode, ctx1 ) = generateExprAccum ctx renames baseFunc
                ( argCode, ctx2 ) = generateExprAccum ctx1 renames arg
            in
            ( "elm_apply1((elm_closure_t *)(" ++ baseFuncCode ++ ").data.p, " ++ argCode ++ ")", ctx2 )


generateClosureApplyAccum : GenCtx -> Dict String String -> String -> List Core.Expr -> ( String, GenCtx )
generateClosureApplyAccum ctx renames closureExpr args =
    case args of
        [] ->
            ( closureExpr, ctx )

        arg :: rest ->
            let
                ( argCode, ctx1 ) = generateExprAccum ctx renames arg
                applied = "elm_apply1((elm_closure_t *)(" ++ closureExpr ++ ").data.p, " ++ argCode ++ ")"
            in
            generateClosureApplyAccum ctx1 renames applied rest


generatePartialAppAccum : GenCtx -> Dict String String -> String -> List Core.Expr -> Int -> ( String, GenCtx )
generatePartialAppAccum ctx renames funcName args arity =
    let
        ( argAssignments, finalCtx ) =
            List.foldl
                (\( i, a ) ( assigns, accCtx ) ->
                    let
                        ( argCode, newCtx ) = generateExprAccum accCtx renames a
                    in
                    ( assigns ++ [ "_c->args[" ++ String.fromInt i ++ "] = " ++ argCode ++ ";" ], newCtx )
                )
                ( [], ctx )
                (List.indexedMap Tuple.pair args)

        resultCode =
            "({"
                ++ " elm_closure_t *_c = elm_alloc_closure();"
                ++ " _c->func = (void *)" ++ funcName ++ ";"
                ++ " _c->arity = " ++ String.fromInt arity ++ ";"
                ++ " _c->applied = " ++ String.fromInt (List.length args) ++ ";"
                ++ " " ++ String.join " " argAssignments
                ++ " (elm_value_t){ .tag = 400, .data.p = _c, .next = NULL };"
                ++ " })"
    in
    ( resultCode, finalCtx )


generateLetAccum : GenCtx -> Dict String String -> String -> Core.Expr -> Core.Expr -> ( String, GenCtx )
generateLetAccum ctx renames var value body =
    let
        ( valueCode, ctx1 ) = generateExprAccum ctx renames value
        bodyRenames = Dict.remove var renames
        -- Add let-bound variable to localVars so it shadows builtins with same name
        bodyCtx = { ctx1 | localVars = Set.insert var ctx1.localVars }
        ( bodyCode, ctx2 ) = generateExprAccum bodyCtx bodyRenames body
    in
    ( "({ elm_value_t " ++ mangle var ++ " = " ++ valueCode ++ "; " ++ bodyCode ++ "; })", ctx2 )


generateLetRecAccum : GenCtx -> Dict String String -> List ( String, Core.Expr ) -> Core.Expr -> ( String, GenCtx )
generateLetRecAccum ctx renames bindings body =
    let
        boundNames = List.map Tuple.first bindings
        innerRenames = List.foldl Dict.remove renames boundNames
        -- Add let-rec bound variables to localVars so they shadow builtins
        innerCtx = { ctx | localVars = Set.union ctx.localVars (Set.fromList boundNames) }

        decls =
            bindings
                |> List.map (\( name, _ ) -> "elm_value_t " ++ mangle name ++ ";")
                |> String.join " "

        ( assigns, ctx1 ) =
            List.foldl
                (\( name, e ) ( strs, accCtx ) ->
                    let
                        ( code, newCtx ) = generateExprAccum accCtx innerRenames e
                    in
                    ( strs ++ [ mangle name ++ " = " ++ code ++ ";" ], newCtx )
                )
                ( [], innerCtx )
                bindings

        ( bodyCode, ctx2 ) = generateExprAccum ctx1 innerRenames body
    in
    ( "({ " ++ decls ++ " " ++ String.join " " assigns ++ " " ++ bodyCode ++ "; })", ctx2 )


generateCaseAccum : GenCtx -> Dict String String -> Core.Expr -> List Core.Alt -> ( String, GenCtx )
generateCaseAccum ctx renames scrutinee alts =
    let
        ( scrutCode, ctx1 ) = generateExprAccum ctx renames scrutinee
        ( scrutVar, ctx2 ) = freshName "scrut" ctx1
        ( resultVar, ctx3 ) = freshName "result" ctx2

        ( branches, ctx4 ) =
            List.foldl
                (\alt ( strs, accCtx ) ->
                    let
                        ( code, newCtx ) = generateAltAccum accCtx renames scrutVar resultVar alt
                    in
                    ( strs ++ [ code ], newCtx )
                )
                ( [], ctx3 )
                alts
    in
    ( "({ elm_value_t " ++ scrutVar ++ " = " ++ scrutCode ++ "; elm_value_t " ++ resultVar ++ "; " ++ String.join " else " branches ++ " " ++ resultVar ++ "; })", ctx4 )


generateAltAccum : GenCtx -> Dict String String -> String -> String -> Core.Alt -> ( String, GenCtx )
generateAltAccum ctx renames scrutVar resultVar (Core.Alt pattern guard body) =
    let
        ( condition, bindings ) = patternCondition ctx scrutVar pattern

        ( guardCode, ctx1 ) =
            case guard of
                Nothing ->
                    ( "", ctx )

                Just g ->
                    let
                        ( code, newCtx ) = generateExprAccum ctx renames g
                    in
                    ( " && (" ++ code ++ ").data.i", newCtx )

        ( bodyCode, ctx2 ) = generateExprAccum ctx1 renames body
    in
    ( "if (" ++ condition ++ guardCode ++ ") { " ++ bindings ++ resultVar ++ " = " ++ bodyCode ++ "; }", ctx2 )


generateConAccum : GenCtx -> Dict String String -> String -> List Core.Expr -> ( String, GenCtx )
generateConAccum ctx renames name args =
    let
        ( argCodes, finalCtx ) =
            List.foldl
                (\a ( codes, accCtx ) ->
                    let
                        ( argCode, newCtx ) = generateExprAccum accCtx renames a
                    in
                    ( codes ++ [ argCode ], newCtx )
                )
                ( [], ctx )
                args

        funcName =
            case name of
                "Nil" -> "elm_nil"
                "Cons" -> "elm_cons"
                "Nothing" -> "elm_nothing"
                "Just" -> "elm_just"
                "Ok" -> "elm_ok"
                "Err" -> "elm_err"
                "True" -> "elm_bool(true)"
                "False" -> "elm_bool(false)"
                "LT" -> "elm_lt"
                "EQ" -> "elm_eq"
                "GT" -> "elm_gt"
                "LE" -> "elm_Bytes_LE"
                "BE" -> "elm_Bytes_BE"
                "Bytes.LE" -> "elm_Bytes_LE"
                "Bytes.BE" -> "elm_Bytes_BE"
                _ -> mangle name

        resultCode =
            if List.isEmpty args && (name == "True" || name == "False") then
                funcName
            else if List.isEmpty args && (name == "LT" || name == "EQ" || name == "GT" || name == "LE" || name == "BE" || name == "Bytes.LE" || name == "Bytes.BE") then
                funcName ++ "()"
            else
                funcName ++ "(" ++ String.join ", " argCodes ++ ")"
    in
    ( resultCode, finalCtx )


generateTupleAccum : GenCtx -> Dict String String -> List Core.Expr -> ( String, GenCtx )
generateTupleAccum ctx renames exprs =
    case exprs of
        [] ->
            ( "elm_unit()", ctx )

        [ single ] ->
            generateExprAccum ctx renames single

        [ first, second ] ->
            -- 2-tuple uses elm_tuple2 for proper tag
            let
                ( firstCode, ctx1 ) = generateExprAccum ctx renames first
                ( secondCode, ctx2 ) = generateExprAccum ctx1 renames second
            in
            ( "elm_tuple2(" ++ firstCode ++ ", " ++ secondCode ++ ")", ctx2 )

        first :: rest ->
            -- 3+ tuple: nest as elm_tuple2(first, tuple(rest))
            let
                ( firstCode, ctx1 ) = generateExprAccum ctx renames first
                ( restCode, ctx2 ) = generateTupleAccum ctx1 renames rest
            in
            ( "elm_tuple2(" ++ firstCode ++ ", " ++ restCode ++ ")", ctx2 )


generateRecordAccum : GenCtx -> Dict String String -> List ( String, Core.Expr ) -> ( String, GenCtx )
generateRecordAccum ctx renames fields =
    -- Records are represented as a linked list of (field name, value) pairs
    -- using the elm_value_t structure where:
    -- - tag = 500 (record node)
    -- - data.s = field name (string)
    -- - data.c = field value
    -- - next = next field (or NULL for last)
    --
    -- We build this as: elm_record_field(name1, value1, elm_record_field(name2, value2, elm_record_end()))
    let
        ( codes, finalCtx ) =
            List.foldr
                (\( name, e ) ( accCode, accCtx ) ->
                    let
                        ( valueCode, newCtx ) = generateExprAccum accCtx renames e
                    in
                    ( "elm_record_field(\"" ++ name ++ "\", " ++ valueCode ++ ", " ++ accCode ++ ")", newCtx )
                )
                ( "elm_record_end()", ctx )
                fields
    in
    ( codes, finalCtx )


-- Old non-accumulating versions (kept for compatibility)
generateExpr : GenCtx -> Core.Expr -> String
generateExpr ctx expr =
    let
        ( code, _ ) = generateExprAccum ctx Dict.empty expr
    in
    code


generateLiteral : Core.Literal -> String
generateLiteral lit =
    case lit of
        Core.LInt n ->
            "elm_int(" ++ String.fromInt n ++ ")"

        Core.LFloat f ->
            "elm_float(" ++ String.fromFloat f ++ ")"

        Core.LString s ->
            "elm_string(\"" ++ escapeString s ++ "\")"

        Core.LChar c ->
            "elm_char('" ++ escapeChar c ++ "')"


generateApp : GenCtx -> Core.Expr -> Core.Expr -> String
generateApp ctx func arg =
    -- Collect all arguments from nested applications
    let
        ( baseFunc, allArgs ) = collectArgs func [ arg ]
    in
    case baseFunc of
        Core.EVar tv ->
            let
                funcName = tv.name
                mangledName = mangleWithModule ctx funcName
                isKnownFunction = Dict.member funcName ctx.functions || isBuiltin funcName
            in
            -- Check if this is a closure parameter or local variable (not a known function)
            if Set.member funcName ctx.closureParams || not isKnownFunction then
                -- Function parameter or local variable - use closure application
                generateClosureApply ctx mangledName allArgs
            else
                -- Known function or builtin
                let
                    arity = getFunctionArity ctx funcName
                    argCount = List.length allArgs
                in
                if arity == 0 then
                    -- Zero-arity function returns a closure - call it and apply args
                    generateClosureApply ctx (mangledName ++ "()") allArgs
                else if argCount == arity then
                    -- Full application - generate direct call
                    mangledName ++ "(" ++ String.join ", " (List.map (generateExpr ctx) allArgs) ++ ")"
                else if argCount < arity then
                    -- Partial application - generate closure
                    generatePartialApp ctx mangledName allArgs arity
                else
                    -- Over-application - call function, then apply extra args to result
                    let
                        ( directArgs, extraArgs ) = splitAt arity allArgs
                        baseCall = mangledName ++ "(" ++ String.join ", " (List.map (generateExpr ctx) directArgs) ++ ")"
                    in
                    generateClosureApply ctx baseCall extraArgs

        Core.ECon name existingArgs _ ->
            -- Constructor application - collect args and generate direct call
            generateCon ctx name (existingArgs ++ allArgs)

        _ ->
            -- Dynamic application (closure)
            "elm_apply1((elm_closure_t *)(" ++ generateExpr ctx baseFunc ++ ").data.p, " ++ generateExpr ctx arg ++ ")"


{-| Apply arguments to a closure expression one at a time.
-}
generateClosureApply : GenCtx -> String -> List Core.Expr -> String
generateClosureApply ctx closureExpr args =
    case args of
        [] ->
            closureExpr

        arg :: rest ->
            let
                applied = "elm_apply1((elm_closure_t *)(" ++ closureExpr ++ ").data.p, " ++ generateExpr ctx arg ++ ")"
            in
            generateClosureApply ctx applied rest


{-| Split a list at index n.
-}
splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )


{-| Collect all arguments from nested EApp nodes.
-}
collectArgs : Core.Expr -> List Core.Expr -> ( Core.Expr, List Core.Expr )
collectArgs expr args =
    case expr of
        Core.EApp innerFunc innerArg _ ->
            collectArgs innerFunc (innerArg :: args)

        _ ->
            ( expr, args )


{-| Get the arity of a known function.
-}
getFunctionArity : GenCtx -> String -> Int
getFunctionArity ctx name =
    case name of
        -- Binary operators (symbolic)
        "+" -> 2
        "-" -> 2
        "*" -> 2
        "/" -> 2
        "//" -> 2
        "^" -> 2
        "==" -> 2
        "/=" -> 2
        "<" -> 2
        ">" -> 2
        "<=" -> 2
        ">=" -> 2
        "&&" -> 2
        "||" -> 2
        "++" -> 2
        "::" -> 2
        "|>" -> 2
        "<|" -> 2
        ">>" -> 2
        "<<" -> 2

        -- Binary operators (desugared names with _op_ prefix from Desugar.elm)
        "_op_add" -> 2
        "_op_sub" -> 2
        "_op_mul" -> 2
        "_op_div" -> 2
        "_op_intDiv" -> 2
        "_op_pow" -> 2
        "_op_mod" -> 2
        "_op_eq" -> 2
        "_op_neq" -> 2
        "_op_lt" -> 2
        "_op_gt" -> 2
        "_op_lte" -> 2
        "_op_gte" -> 2
        "_op_and" -> 2
        "_op_or" -> 2
        "_op_append" -> 2
        "_op_cons" -> 2

        -- Unary functions
        "not" -> 1
        "negate" -> 1

        -- Basics module
        "identity" -> 1
        "always" -> 2
        "flip" -> 3
        "min" -> 2
        "max" -> 2
        "clamp" -> 3
        "abs" -> 1
        "modBy" -> 2
        "remainderBy" -> 2
        "compare" -> 2
        "curry" -> 3
        "uncurry" -> 2
        "xor" -> 2
        "isNaN" -> 1
        "isInfinite" -> 1

        -- Math functions
        "sqrt" -> 1
        "sin" -> 1
        "cos" -> 1
        "tan" -> 1
        "asin" -> 1
        "acos" -> 1
        "atan" -> 1
        "atan2" -> 2
        "logBase" -> 2
        "e" -> 0
        "pi" -> 0
        "degrees" -> 1
        "radians" -> 1
        "turns" -> 1
        "ceiling" -> 1
        "floor" -> 1
        "round" -> 1
        "truncate" -> 1
        "toFloat" -> 1

        -- Debug module
        "Debug.log" -> 2
        "Debug.toString" -> 1
        "Debug.todo" -> 1

        -- Order type (nullary)
        "LT" -> 0
        "EQ" -> 0
        "GT" -> 0

        -- List module (unary)
        "List.isEmpty" -> 1
        "List.length" -> 1
        "List.reverse" -> 1
        "List.head" -> 1
        "List.last" -> 1
        "List.tail" -> 1
        "List.sum" -> 1
        "List.product" -> 1
        "List.maximum" -> 1
        "List.minimum" -> 1
        "List.concat" -> 1
        "List.singleton" -> 1

        -- List module (binary)
        "List.member" -> 2
        "List.take" -> 2
        "List.drop" -> 2
        "List.append" -> 2
        "List.intersperse" -> 2
        "List.range" -> 2
        "List.repeat" -> 2
        "List.map" -> 2
        "List.filter" -> 2
        "List.filterMap" -> 2
        "List.any" -> 2
        "List.all" -> 2
        "List.concatMap" -> 2
        "List.indexedMap" -> 2
        "List.sort" -> 1
        "List.sortBy" -> 2
        "List.sortWith" -> 2
        "List.partition" -> 2
        "List.unzip" -> 1
        "List.map2" -> 3
        "List.map3" -> 4
        "List.map4" -> 5
        "List.map5" -> 6
        "List.getAt" -> 2
        "List.zip" -> 2

        -- List module (ternary)
        "List.foldl" -> 3
        "List.foldr" -> 3

        -- Maybe module
        "Maybe.withDefault" -> 2
        "Maybe.map" -> 2
        "Maybe.andThen" -> 2
        "Maybe.map2" -> 3
        "Maybe.map3" -> 4
        "Maybe.map4" -> 5
        "Maybe.map5" -> 6
        "Maybe.filter" -> 2

        -- Result module
        "Result.withDefault" -> 2
        "Result.map" -> 2
        "Result.mapError" -> 2
        "Result.andThen" -> 2
        "Result.toMaybe" -> 1
        "Result.map2" -> 3
        "Result.map3" -> 4
        "Result.fromMaybe" -> 2

        -- Task module
        "Task.succeed" -> 1
        "Task.fail" -> 1
        "Task.map" -> 2
        "Task.andThen" -> 2
        "Task.mapError" -> 2
        "Task.onError" -> 2
        "Task.map2" -> 3
        "Task.map3" -> 4
        "Task.sequence" -> 1

        -- Time module
        "Time.now" -> 0
        "Time.posixToMillis" -> 1
        "Time.millisToPosix" -> 1

        -- Process module
        "Process.sleep" -> 1

        -- String module (unary)
        "String.length" -> 1
        "String.isEmpty" -> 1
        "String.reverse" -> 1
        "String.concat" -> 1
        "String.fromInt" -> 1
        "String.toInt" -> 1
        "String.fromFloat" -> 1
        "String.toFloat" -> 1

        -- String module (binary)
        "String.append" -> 2
        "String.join" -> 2
        "String.left" -> 2
        "String.right" -> 2
        "String.dropLeft" -> 2
        "String.dropRight" -> 2
        "String.contains" -> 2
        "String.startsWith" -> 2
        "String.endsWith" -> 2
        "String.split" -> 2
        "String.indexes" -> 2
        "String.indices" -> 2
        "String.replace" -> 3
        "String.slice" -> 3
        "String.toUpper" -> 1
        "String.toLower" -> 1
        "String.trim" -> 1
        "String.trimLeft" -> 1
        "String.trimRight" -> 1
        "String.toList" -> 1
        "String.fromList" -> 1
        "String.padLeft" -> 3
        "String.padRight" -> 3
        "String.cons" -> 2
        "String.uncons" -> 1
        "String.fromChar" -> 1
        "String.repeat" -> 2
        "String.words" -> 1
        "String.lines" -> 1
        "String.foldl" -> 3
        "String.foldr" -> 3
        "String.any" -> 2
        "String.all" -> 2
        "String.filter" -> 2
        "String.map" -> 2

        -- Tuple module (unary)
        "Tuple.first" -> 1
        "Tuple.second" -> 1

        -- Tuple module (binary)
        "Tuple.pair" -> 2
        "Tuple.mapFirst" -> 2
        "Tuple.mapSecond" -> 2

        -- Tuple module (ternary)
        "Tuple.mapBoth" -> 3

        -- Dict module (nullary)
        "Dict.empty" -> 0

        -- Dict module (unary)
        "Dict.size" -> 1
        "Dict.isEmpty" -> 1
        "Dict.keys" -> 1
        "Dict.values" -> 1
        "Dict.toList" -> 1
        "Dict.fromList" -> 1

        -- Dict module (binary)
        "Dict.singleton" -> 2
        "Dict.get" -> 2
        "Dict.remove" -> 2
        "Dict.member" -> 2

        -- Dict module (binary)
        "Dict.map" -> 2
        "Dict.filter" -> 2

        -- Dict module (ternary)
        "Dict.insert" -> 3
        "Dict.update" -> 3
        "Dict.foldl" -> 3
        "Dict.foldr" -> 3
        "Dict.partition" -> 2
        "Dict.union" -> 2
        "Dict.diff" -> 2
        "Dict.intersect" -> 2

        -- Set module (zero-arity)
        "Set.empty" -> 0

        -- Set module (unary)
        "Set.singleton" -> 1
        "Set.size" -> 1
        "Set.isEmpty" -> 1
        "Set.toList" -> 1
        "Set.fromList" -> 1

        -- Set module (binary)
        "Set.insert" -> 2
        "Set.remove" -> 2
        "Set.member" -> 2
        "Set.union" -> 2
        "Set.intersect" -> 2
        "Set.diff" -> 2
        "Set.map" -> 2
        "Set.filter" -> 2

        -- Set module (ternary)
        "Set.foldl" -> 3
        "Set.foldr" -> 3

        -- Set module additional
        "Set.partition" -> 2

        -- Char module (all unary)
        "Char.toCode" -> 1
        "Char.fromCode" -> 1
        "Char.isDigit" -> 1
        "Char.isLower" -> 1
        "Char.isUpper" -> 1
        "Char.isAlpha" -> 1
        "Char.isAlphaNum" -> 1
        "Char.isHexDigit" -> 1
        "Char.isOctDigit" -> 1
        "Char.isSpace" -> 1
        "Char.toUpper" -> 1
        "Char.toLower" -> 1

        -- Bitwise module
        "Bitwise.complement" -> 1
        "Bitwise.and" -> 2
        "Bitwise.or" -> 2
        "Bitwise.xor" -> 2
        "Bitwise.shiftLeftBy" -> 2
        "Bitwise.shiftRightBy" -> 2
        "Bitwise.shiftRightZfBy" -> 2

        -- Array module (zero-arity)
        "Array.empty" -> 0

        -- Array module (unary)
        "Array.fromList" -> 1
        "Array.toList" -> 1
        "Array.toIndexedList" -> 1
        "Array.length" -> 1
        "Array.isEmpty" -> 1

        -- Array module (binary)
        "Array.get" -> 2
        "Array.push" -> 2
        "Array.append" -> 2
        "Array.map" -> 2
        "Array.indexedMap" -> 2
        "Array.filter" -> 2

        -- Array module (ternary)
        "Array.set" -> 3
        "Array.slice" -> 3
        "Array.foldl" -> 3
        "Array.foldr" -> 3

        -- Array module additional
        "Array.initialize" -> 2
        "Array.repeat" -> 2

        -- Json.Encode module
        "Json.Encode.string" -> 1
        "Json.Encode.int" -> 1
        "Json.Encode.float" -> 1
        "Json.Encode.bool" -> 1
        "Json.Encode.null" -> 0
        "Json.Encode.list" -> 2
        "Json.Encode.array" -> 2
        "Json.Encode.object" -> 1
        "Json.Encode.encode" -> 2

        -- Json.Decode module
        "Json.Decode.decodeString" -> 2
        "Json.Decode.decodeValue" -> 2
        "Json.Decode.string" -> 0
        "Json.Decode.int" -> 0
        "Json.Decode.float" -> 0
        "Json.Decode.bool" -> 0
        "Json.Decode.null" -> 1
        "Json.Decode.nullable" -> 1
        "Json.Decode.list" -> 1
        "Json.Decode.field" -> 2
        "Json.Decode.map" -> 2
        "Json.Decode.map2" -> 3
        "Json.Decode.andThen" -> 2
        "Json.Decode.succeed" -> 1
        "Json.Decode.fail" -> 1

        -- Bytes module
        "Bytes.width" -> 1
        "Bytes.isEmpty" -> 1
        "Bytes.LE" -> 0
        "Bytes.BE" -> 0

        -- Bytes.Encode module
        "Bytes.Encode.signedInt8" -> 1
        "Bytes.Encode.signedInt16" -> 2
        "Bytes.Encode.signedInt32" -> 2
        "Bytes.Encode.unsignedInt8" -> 1
        "Bytes.Encode.unsignedInt16" -> 2
        "Bytes.Encode.unsignedInt32" -> 2
        "Bytes.Encode.float32" -> 2
        "Bytes.Encode.float64" -> 2
        "Bytes.Encode.bytes" -> 1
        "Bytes.Encode.string" -> 1
        "Bytes.Encode.sequence" -> 1
        "Bytes.Encode.encode" -> 1

        -- Bytes.Decode module
        "Bytes.Decode.decode" -> 2
        "Bytes.Decode.signedInt8" -> 0
        "Bytes.Decode.signedInt16" -> 1
        "Bytes.Decode.signedInt32" -> 1
        "Bytes.Decode.unsignedInt8" -> 0
        "Bytes.Decode.unsignedInt16" -> 1
        "Bytes.Decode.unsignedInt32" -> 1
        "Bytes.Decode.float32" -> 1
        "Bytes.Decode.float64" -> 1
        "Bytes.Decode.bytes" -> 1
        "Bytes.Decode.string" -> 1
        "Bytes.Decode.map" -> 2
        "Bytes.Decode.map2" -> 3
        "Bytes.Decode.andThen" -> 2
        "Bytes.Decode.succeed" -> 1
        "Bytes.Decode.fail" -> 0

        -- User-defined functions - get arity from definition
        _ ->
            case Dict.get name ctx.functions of
                Just funcDef ->
                    List.length funcDef.args

                Nothing ->
                    -- Unknown function, assume unary
                    1


{-| Generate a closure wrapping a known function (for passing as value).
-}
generateFunctionClosure : GenCtx -> String -> Int -> String
generateFunctionClosure _ funcName arity =
    "({"
        ++ " elm_closure_t *_c = elm_alloc_closure();"
        ++ " _c->func = (void *)" ++ funcName ++ ";"
        ++ " _c->arity = " ++ String.fromInt arity ++ ";"
        ++ " _c->applied = 0;"
        ++ " (elm_value_t){ .tag = 400, .data.p = _c, .next = NULL };"
        ++ " })"


{-| Generate partial application as a closure.
-}
generatePartialApp : GenCtx -> String -> List Core.Expr -> Int -> String
generatePartialApp ctx funcName args arity =
    let
        argsCode =
            args
                |> List.indexedMap (\i a -> "_c->args[" ++ String.fromInt i ++ "] = " ++ generateExpr ctx a ++ ";")
                |> String.join " "
    in
    "({"
        ++ " elm_closure_t *_c = elm_alloc_closure();"
        ++ " _c->func = (void *)" ++ funcName ++ ";"
        ++ " _c->arity = " ++ String.fromInt arity ++ ";"
        ++ " _c->applied = " ++ String.fromInt (List.length args) ++ ";"
        ++ " " ++ argsCode
        ++ " (elm_value_t){ .tag = 400, .data.p = _c, .next = NULL };"
        ++ " })"


{-| Mangle a name with the module prefix if it's a local function.
-}
mangleWithModule : GenCtx -> String -> String
mangleWithModule ctx name =
    if isBuiltin name then
        mangle name
    else if Dict.member name ctx.functions then
        mangle (ctx.moduleName ++ "_" ++ name)
    else if Dict.member name ctx.foreignFunctions then
        mangle (ctx.moduleName ++ "_" ++ name)
    else
        mangle name


{-| Check if a name is a builtin operator/function.
    Includes both symbolic operators and their desugared names.
-}
isBuiltin : String -> Bool
isBuiltin name =
    List.member name
        -- Symbolic operators
        [ "+", "-", "*", "/", "//", "^", "%"
        , "==", "/=", "<", ">", "<=", ">="
        , "&&", "||", "not", "negate"
        , "++", "::"
        , "|>", "<|", ">>", "<<"
        , "True", "False"
        -- Desugared operator names with _op_ prefix (from Desugar.elm desugarBinOp)
        , "_op_add", "_op_sub", "_op_mul", "_op_div", "_op_intDiv", "_op_pow", "_op_mod"
        , "_op_eq", "_op_neq", "_op_lt", "_op_gt", "_op_lte", "_op_gte"
        , "_op_and", "_op_or"
        , "_op_append", "_op_cons"
        -- Basics module
        , "identity", "always", "flip", "min", "max", "clamp", "abs"
        , "modBy", "remainderBy", "compare", "curry", "uncurry"
        , "xor", "isNaN", "isInfinite"
        -- Math functions
        , "sqrt", "sin", "cos", "tan", "asin", "acos", "atan", "atan2"
        , "logBase", "e", "pi", "degrees", "radians", "turns"
        , "ceiling", "floor", "round", "truncate", "toFloat"
        -- Debug module
        , "Debug.log", "Debug.toString", "Debug.todo"
        -- Order type
        , "LT", "EQ", "GT"
        -- List module
        , "List.isEmpty", "List.length", "List.reverse", "List.member"
        , "List.head", "List.last", "List.tail", "List.take", "List.drop"
        , "List.sum", "List.product", "List.maximum", "List.minimum"
        , "List.append", "List.concat", "List.intersperse", "List.range", "List.repeat"
        , "List.map", "List.filter", "List.filterMap", "List.foldl", "List.foldr"
        , "List.any", "List.all", "List.concatMap", "List.indexedMap"
        , "List.sort", "List.sortBy", "List.sortWith", "List.partition", "List.singleton"
        , "List.unzip", "List.map2", "List.map3", "List.map4", "List.map5", "List.getAt", "List.zip"
        -- Maybe module
        , "Maybe.withDefault", "Maybe.map", "Maybe.andThen", "Maybe.map2", "Maybe.map3", "Maybe.map4", "Maybe.map5", "Maybe.filter"
        -- Result module
        , "Result.withDefault", "Result.map", "Result.mapError", "Result.andThen", "Result.toMaybe"
        , "Result.map2", "Result.map3", "Result.fromMaybe"
        -- Task module
        , "Task.succeed", "Task.fail", "Task.map", "Task.andThen", "Task.mapError"
        , "Task.onError", "Task.map2", "Task.map3", "Task.sequence"
        -- Time module
        , "Time.now", "Time.posixToMillis", "Time.millisToPosix"
        -- Process module
        , "Process.sleep"
        -- String module
        , "String.length", "String.isEmpty", "String.reverse", "String.concat"
        , "String.append", "String.join", "String.fromInt", "String.toInt", "String.fromFloat", "String.toFloat"
        , "String.left", "String.right", "String.dropLeft", "String.dropRight"
        , "String.contains", "String.startsWith", "String.endsWith"
        , "String.split", "String.indexes", "String.indices", "String.replace", "String.slice", "String.toUpper", "String.toLower"
        , "String.trim", "String.trimLeft", "String.trimRight"
        , "String.toList", "String.fromList", "String.padLeft", "String.padRight"
        , "String.cons", "String.uncons", "String.fromChar"
        , "String.repeat", "String.words", "String.lines"
        , "String.foldl", "String.foldr"
        , "String.any", "String.all", "String.filter", "String.map"
        -- Tuple module
        , "Tuple.pair", "Tuple.first", "Tuple.second", "Tuple.mapFirst", "Tuple.mapSecond", "Tuple.mapBoth"
        -- Dict module
        , "Dict.empty", "Dict.singleton", "Dict.insert", "Dict.get", "Dict.remove"
        , "Dict.member", "Dict.size", "Dict.isEmpty", "Dict.keys", "Dict.values"
        , "Dict.toList", "Dict.fromList", "Dict.update", "Dict.map", "Dict.filter"
        , "Dict.foldl", "Dict.foldr", "Dict.partition", "Dict.union", "Dict.diff", "Dict.intersect"
        -- Set module
        , "Set.empty", "Set.singleton", "Set.insert", "Set.remove", "Set.member"
        , "Set.size", "Set.isEmpty", "Set.toList", "Set.fromList"
        , "Set.union", "Set.intersect", "Set.diff"
        , "Set.map", "Set.filter", "Set.foldl", "Set.foldr", "Set.partition"
        -- Char module
        , "Char.toCode", "Char.fromCode"
        , "Char.isDigit", "Char.isLower", "Char.isUpper", "Char.isAlpha", "Char.isAlphaNum"
        , "Char.isHexDigit", "Char.isOctDigit", "Char.isSpace"
        , "Char.toUpper", "Char.toLower"
        -- Bitwise module
        , "Bitwise.and", "Bitwise.or", "Bitwise.xor", "Bitwise.complement"
        , "Bitwise.shiftLeftBy", "Bitwise.shiftRightBy", "Bitwise.shiftRightZfBy"
        -- Array module
        , "Array.empty", "Array.fromList", "Array.toList", "Array.toIndexedList"
        , "Array.length", "Array.isEmpty", "Array.get", "Array.set"
        , "Array.push", "Array.append", "Array.slice"
        , "Array.map", "Array.indexedMap", "Array.foldl", "Array.foldr", "Array.filter"
        , "Array.initialize", "Array.repeat"
        -- Json.Encode module
        , "Json.Encode.string", "Json.Encode.int", "Json.Encode.float", "Json.Encode.bool"
        , "Json.Encode.null", "Json.Encode.list", "Json.Encode.array", "Json.Encode.object"
        , "Json.Encode.encode"
        -- Json.Decode module
        , "Json.Decode.decodeString", "Json.Decode.decodeValue"
        , "Json.Decode.string", "Json.Decode.int", "Json.Decode.float", "Json.Decode.bool"
        , "Json.Decode.null", "Json.Decode.nullable", "Json.Decode.list", "Json.Decode.field"
        , "Json.Decode.map", "Json.Decode.map2", "Json.Decode.andThen"
        , "Json.Decode.succeed", "Json.Decode.fail"
        -- Bytes module
        , "Bytes.width", "Bytes.isEmpty", "Bytes.LE", "Bytes.BE"
        -- Bytes.Encode module
        , "Bytes.Encode.signedInt8", "Bytes.Encode.signedInt16", "Bytes.Encode.signedInt32"
        , "Bytes.Encode.unsignedInt8", "Bytes.Encode.unsignedInt16", "Bytes.Encode.unsignedInt32"
        , "Bytes.Encode.float32", "Bytes.Encode.float64"
        , "Bytes.Encode.bytes", "Bytes.Encode.string", "Bytes.Encode.sequence", "Bytes.Encode.encode"
        -- Bytes.Decode module
        , "Bytes.Decode.decode", "Bytes.Decode.signedInt8", "Bytes.Decode.signedInt16", "Bytes.Decode.signedInt32"
        , "Bytes.Decode.unsignedInt8", "Bytes.Decode.unsignedInt16", "Bytes.Decode.unsignedInt32"
        , "Bytes.Decode.float32", "Bytes.Decode.float64"
        , "Bytes.Decode.bytes", "Bytes.Decode.string"
        , "Bytes.Decode.map", "Bytes.Decode.map2", "Bytes.Decode.andThen"
        , "Bytes.Decode.succeed", "Bytes.Decode.fail"
        -- Endianness (used as constructors)
        , "LE", "BE"
        ]


{-| Check if a name is a foreign function.
-}
isForeign : GenCtx -> String -> Bool
isForeign ctx name =
    Dict.member name ctx.foreignFunctions


{-| Get the arity of a foreign function.
-}
getForeignArity : GenCtx -> String -> Int
getForeignArity ctx name =
    case Dict.get name ctx.foreignFunctions of
        Just foreignDef ->
            let
                ( argTypes, _ ) = collectForeignArgTypes foreignDef.type_
            in
            List.length argTypes

        Nothing ->
            0


generateLambda : GenCtx -> Core.TypedVar -> Core.Expr -> String
generateLambda ctx tv body =
    -- Generate lambda with proper closure capture.
    -- Captured variables are passed as explicit parameters and stored in the closure's args[].
    -- This works with both GCC and TCC (no reliance on implicit stack capture).
    let
        ( funcName, ctx1 ) = freshName "lambda" ctx
        argName = mangle tv.name

        -- Compute free variables in the body (excluding the lambda param, known functions, and builtins)
        bodyFreeVars = Core.freeVars body
        freeVarsExcludingParam = Set.remove tv.name bodyFreeVars

        -- Filter out known top-level functions and builtins
        captures =
            freeVarsExcludingParam
                |> Set.filter (\v ->
                    not (Dict.member v ctx1.functions)
                        && not (isBuiltin v)
                )
                |> Set.toList

        numCaptures = List.length captures

        -- Generate parameter list for the nested function: (cap0, cap1, ..., arg)
        captureParams =
            captures
                |> List.indexedMap (\i _ -> "elm_value_t _cap" ++ String.fromInt i)

        allParams = captureParams ++ [ "elm_value_t " ++ argName ]
        paramsStr = String.join ", " allParams

        -- In the function body, we need to reference captures by their parameter names
        -- Create a context where capture names map to _cap0, _cap1, etc.
        captureRenames =
            captures
                |> List.indexedMap (\i name -> ( name, "_cap" ++ String.fromInt i ))
                |> Dict.fromList

        -- Add the lambda parameter to closureParams
        bodyCtx =
            { ctx1
            | closureParams = Set.insert tv.name ctx1.closureParams
            , localVars = Set.union ctx1.localVars (Set.fromList captures)
            }

        -- Generate body code with capture renaming
        bodyCode = generateExprWithRenames bodyCtx captureRenames body

        -- Store captured values in args[]
        captureAssignments =
            captures
                |> List.indexedMap (\i name ->
                    "_c->args[" ++ String.fromInt i ++ "] = " ++ mangle name ++ ";"
                )
                |> String.join " "

        totalArity = numCaptures + 1
    in
    "({"
        ++ " elm_value_t " ++ funcName ++ "(" ++ paramsStr ++ ") { return " ++ bodyCode ++ "; }"
        ++ " elm_closure_t *_c = elm_alloc_closure();"
        ++ " _c->func = (void *)" ++ funcName ++ ";"
        ++ " _c->arity = " ++ String.fromInt totalArity ++ ";"
        ++ " _c->applied = " ++ String.fromInt numCaptures ++ ";"
        ++ " " ++ captureAssignments
        ++ " (elm_value_t){ .tag = 400, .data.p = _c, .next = NULL };"
        ++ " })"


{-| Generate expression code with variable renaming for captured variables.
-}
generateExprWithRenames : GenCtx -> Dict String String -> Core.Expr -> String
generateExprWithRenames ctx renames expr =
    case expr of
        Core.EVar tv ->
            let
                name = tv.name
            in
            case Dict.get name renames of
                Just renamed ->
                    -- This is a captured variable, use the renamed parameter directly
                    renamed

                Nothing ->
                    -- Not renamed, use normal generation
                    let
                        mangledName = mangleWithModule ctx name
                        arity = getFunctionArity ctx name
                        -- Check if this is a local variable (shadows builtins with same name)
                        isLocalVar = Set.member name ctx.localVars
                    in
                    if Set.member name ctx.closureParams then
                        mangledName
                    else if isLocalVar then
                        -- Local variable, use as-is (it's a closure value)
                        mangledName
                    else if Dict.member name ctx.functions && arity > 0 then
                        generateFunctionClosure ctx mangledName arity
                    else if arity == 0 && Dict.member name ctx.functions then
                        mangledName ++ "()"
                    else if isBuiltin name && arity > 0 then
                        -- Builtin function used as a value - wrap in closure
                        generateFunctionClosure ctx mangledName arity
                    else if isBuiltin name && arity == 0 then
                        -- Zero-arity builtin, call it
                        mangledName ++ "()"
                    else
                        mangledName

        Core.ELit lit _ ->
            generateLiteral lit

        Core.EApp func arg _ ->
            generateAppWithRenames ctx renames func arg

        Core.ELam innerTv innerBody _ ->
            generateLambdaWithRenames ctx renames innerTv innerBody

        Core.ELet var value body _ ->
            let
                valueCode = generateExprWithRenames ctx renames value
                -- Remove var from renames in case it shadows a captured var
                bodyRenames = Dict.remove var renames
                -- Add let-bound variable to localVars so it shadows builtins with same name
                bodyCtx = { ctx | localVars = Set.insert var ctx.localVars }
                bodyCode = generateExprWithRenames bodyCtx bodyRenames body
            in
            "({ elm_value_t " ++ mangle var ++ " = " ++ valueCode ++ "; " ++ bodyCode ++ "; })"

        Core.ELetRec bindings body _ ->
            let
                boundNames = List.map Tuple.first bindings
                -- Remove all bound names from renames
                innerRenames = List.foldl Dict.remove renames boundNames
                -- Add let-rec bound variables to localVars so they shadow builtins
                innerCtx = { ctx | localVars = Set.union ctx.localVars (Set.fromList boundNames) }

                decls =
                    bindings
                        |> List.map (\( name, _ ) -> "elm_value_t " ++ mangle name ++ ";")
                        |> String.join " "

                assigns =
                    bindings
                        |> List.map (\( name, e ) ->
                            mangle name ++ " = " ++ generateExprWithRenames innerCtx innerRenames e ++ ";"
                        )
                        |> String.join " "

                bodyCode = generateExprWithRenames innerCtx innerRenames body
            in
            "({ " ++ decls ++ " " ++ assigns ++ " " ++ bodyCode ++ "; })"

        Core.ECase scrutinee alts _ ->
            generateCaseWithRenames ctx renames scrutinee alts

        Core.ECon name args _ ->
            generateConWithRenames ctx renames name args

        Core.ETuple exprs _ ->
            generateTupleWithRenames ctx renames exprs

        Core.ERecord fields _ ->
            generateRecordWithRenames ctx renames fields

        Core.ERecordAccess record field _ ->
            let
                recordCode = generateExprWithRenames ctx renames record
            in
            "((" ++ recordCode ++ ").data.p->" ++ field ++ ")"

        Core.ERecordUpdate record updates _ ->
            -- Generate record update: { record | field1 = val1, ... }
            -- Build a new record with updated fields prepended
            let
                recordCode = generateExprWithRenames ctx renames record

                -- Build the chain of updated fields
                updateChain =
                    updates
                        |> List.foldl
                            (\( fieldName, fieldExpr ) acc ->
                                let
                                    fieldCode = generateExprWithRenames ctx renames fieldExpr
                                in
                                "elm_record_field(\"" ++ fieldName ++ "\", " ++ fieldCode ++ ", " ++ acc ++ ")"
                            )
                            recordCode
            in
            updateChain

        Core.ETyApp e _ _ ->
            generateExprWithRenames ctx renames e

        Core.ETyLam _ e _ ->
            generateExprWithRenames ctx renames e

        Core.EDictApp func dict _ ->
            generateExprWithRenames ctx renames func ++ "(" ++ generateExprWithRenames ctx renames dict ++ ")"

        Core.EDictLam innerTv innerBody _ ->
            generateLambdaWithRenames ctx renames innerTv innerBody

        Core.EDict className _ _ ->
            "(elm_dict_t *)&dict_" ++ className


generateAppWithRenames : GenCtx -> Dict String String -> Core.Expr -> Core.Expr -> String
generateAppWithRenames ctx renames func arg =
    let
        ( baseFunc, allArgs ) = collectArgs func [ arg ]
    in
    case baseFunc of
        Core.EVar tv ->
            let
                funcName = tv.name
            in
            -- Check if this is a renamed capture
            case Dict.get funcName renames of
                Just renamed ->
                    -- Captured closure, apply via elm_apply1
                    generateClosureApplyWithRenames ctx renames renamed allArgs

                Nothing ->
                    let
                        mangledName = mangleWithModule ctx funcName
                        -- Check if this is a local variable (shadows builtins with same name)
                        isLocalVar = Set.member funcName ctx.localVars
                        isKnownFunction = not isLocalVar && (Dict.member funcName ctx.functions || isBuiltin funcName)
                    in
                    if Set.member funcName ctx.closureParams || not isKnownFunction then
                        generateClosureApplyWithRenames ctx renames mangledName allArgs
                    else
                        let
                            arity = getFunctionArity ctx funcName
                            argCount = List.length allArgs
                        in
                        if arity == 0 then
                            generateClosureApplyWithRenames ctx renames (mangledName ++ "()") allArgs
                        else if argCount == arity then
                            mangledName ++ "(" ++ String.join ", " (List.map (generateExprWithRenames ctx renames) allArgs) ++ ")"
                        else if argCount < arity then
                            generatePartialAppWithRenames ctx renames mangledName allArgs arity
                        else
                            let
                                ( directArgs, extraArgs ) = splitAt arity allArgs
                                baseCall = mangledName ++ "(" ++ String.join ", " (List.map (generateExprWithRenames ctx renames) directArgs) ++ ")"
                            in
                            generateClosureApplyWithRenames ctx renames baseCall extraArgs

        Core.ECon name existingArgs _ ->
            generateConWithRenames ctx renames name (existingArgs ++ allArgs)

        _ ->
            "elm_apply1((elm_closure_t *)(" ++ generateExprWithRenames ctx renames baseFunc ++ ").data.p, " ++ generateExprWithRenames ctx renames arg ++ ")"


generateClosureApplyWithRenames : GenCtx -> Dict String String -> String -> List Core.Expr -> String
generateClosureApplyWithRenames ctx renames closureExpr args =
    case args of
        [] ->
            closureExpr

        arg :: rest ->
            let
                applied = "elm_apply1((elm_closure_t *)(" ++ closureExpr ++ ").data.p, " ++ generateExprWithRenames ctx renames arg ++ ")"
            in
            generateClosureApplyWithRenames ctx renames applied rest


generatePartialAppWithRenames : GenCtx -> Dict String String -> String -> List Core.Expr -> Int -> String
generatePartialAppWithRenames ctx renames funcName args arity =
    let
        argsCode =
            args
                |> List.indexedMap (\i a -> "_c->args[" ++ String.fromInt i ++ "] = " ++ generateExprWithRenames ctx renames a ++ ";")
                |> String.join " "
    in
    "({"
        ++ " elm_closure_t *_c = elm_alloc_closure();"
        ++ " _c->func = (void *)" ++ funcName ++ ";"
        ++ " _c->arity = " ++ String.fromInt arity ++ ";"
        ++ " _c->applied = " ++ String.fromInt (List.length args) ++ ";"
        ++ " " ++ argsCode
        ++ " (elm_value_t){ .tag = 400, .data.p = _c, .next = NULL };"
        ++ " })"


generateLambdaWithRenames : GenCtx -> Dict String String -> Core.TypedVar -> Core.Expr -> String
generateLambdaWithRenames ctx renames tv body =
    -- For nested lambdas, we need to compute captures relative to current scope
    let
        ( funcName, ctx1 ) = freshName "lambda" ctx
        argName = mangle tv.name

        bodyFreeVars = Core.freeVars body
        freeVarsExcludingParam = Set.remove tv.name bodyFreeVars

        -- Captures include both actual local variables and renamed captures from outer scope
        captures =
            freeVarsExcludingParam
                |> Set.filter (\v ->
                    not (Dict.member v ctx1.functions)
                        && not (isBuiltin v)
                )
                |> Set.toList

        numCaptures = List.length captures

        captureParams =
            captures
                |> List.indexedMap (\i _ -> "elm_value_t _cap" ++ String.fromInt i)

        allParams = captureParams ++ [ "elm_value_t " ++ argName ]
        paramsStr = String.join ", " allParams

        -- New rename map for inner lambda
        innerRenames =
            captures
                |> List.indexedMap (\i name -> ( name, "_cap" ++ String.fromInt i ))
                |> Dict.fromList

        bodyCtx =
            { ctx1
            | closureParams = Set.insert tv.name ctx1.closureParams
            , localVars = Set.union ctx1.localVars (Set.fromList captures)
            }

        bodyCode = generateExprWithRenames bodyCtx innerRenames body

        -- Store captured values - need to look up in outer renames for values
        captureAssignments =
            captures
                |> List.indexedMap (\i name ->
                    let
                        sourceValue =
                            case Dict.get name renames of
                                Just outerRenamed -> outerRenamed
                                Nothing -> mangle name
                    in
                    "_c->args[" ++ String.fromInt i ++ "] = " ++ sourceValue ++ ";"
                )
                |> String.join " "

        totalArity = numCaptures + 1
    in
    "({"
        ++ " elm_value_t " ++ funcName ++ "(" ++ paramsStr ++ ") { return " ++ bodyCode ++ "; }"
        ++ " elm_closure_t *_c = elm_alloc_closure();"
        ++ " _c->func = (void *)" ++ funcName ++ ";"
        ++ " _c->arity = " ++ String.fromInt totalArity ++ ";"
        ++ " _c->applied = " ++ String.fromInt numCaptures ++ ";"
        ++ " " ++ captureAssignments
        ++ " (elm_value_t){ .tag = 400, .data.p = _c, .next = NULL };"
        ++ " })"


generateCaseWithRenames : GenCtx -> Dict String String -> Core.Expr -> List Core.Alt -> String
generateCaseWithRenames ctx renames scrutinee alts =
    let
        scrutCode = generateExprWithRenames ctx renames scrutinee
        ( scrutVar, ctx1 ) = freshName "scrut" ctx
        ( resultVar, ctx2 ) = freshName "result" ctx1

        branches =
            alts
                |> List.map (generateAltWithRenames ctx2 renames scrutVar resultVar)
                |> String.join " else "
    in
    "({ elm_value_t " ++ scrutVar ++ " = " ++ scrutCode ++ "; elm_value_t " ++ resultVar ++ "; " ++ branches ++ " " ++ resultVar ++ "; })"


generateAltWithRenames : GenCtx -> Dict String String -> String -> String -> Core.Alt -> String
generateAltWithRenames ctx renames scrutVar resultVar (Core.Alt pattern guard body) =
    let
        ( condition, bindings ) = patternCondition ctx scrutVar pattern
        -- Remove pattern-bound variables from renames to avoid shadowing issues
        patVars = Core.freeVars body  -- Use body's free vars to find what pattern binds
        bodyRenames = renames  -- Pattern variables are fresh, don't shadow renames

        guardCode =
            case guard of
                Nothing -> ""
                Just g -> " && (" ++ generateExprWithRenames ctx bodyRenames g ++ ").data.i"

        bodyCode = generateExprWithRenames ctx bodyRenames body
    in
    "if (" ++ condition ++ guardCode ++ ") { " ++ bindings ++ resultVar ++ " = " ++ bodyCode ++ "; }"


generateConWithRenames : GenCtx -> Dict String String -> String -> List Core.Expr -> String
generateConWithRenames ctx renames name args =
    let
        argCodes =
            args
                |> List.map (generateExprWithRenames ctx renames)
                |> String.join ", "

        funcName =
            case name of
                "Nil" -> "elm_nil"
                "Cons" -> "elm_cons"
                "Nothing" -> "elm_nothing"
                "Just" -> "elm_just"
                "Ok" -> "elm_ok"
                "Err" -> "elm_err"
                "True" -> "elm_bool(true)"
                "False" -> "elm_bool(false)"
                "LT" -> "elm_lt"
                "EQ" -> "elm_eq"
                "GT" -> "elm_gt"
                "LE" -> "elm_Bytes_LE"
                "BE" -> "elm_Bytes_BE"
                "Bytes.LE" -> "elm_Bytes_LE"
                "Bytes.BE" -> "elm_Bytes_BE"
                _ -> mangle name
    in
    if List.isEmpty args && (name == "True" || name == "False") then
        funcName
    else if List.isEmpty args && (name == "LT" || name == "EQ" || name == "GT" || name == "LE" || name == "BE" || name == "Bytes.LE" || name == "Bytes.BE") then
        funcName ++ "()"
    else
        funcName ++ "(" ++ argCodes ++ ")"


generateTupleWithRenames : GenCtx -> Dict String String -> List Core.Expr -> String
generateTupleWithRenames ctx renames exprs =
    case exprs of
        [] ->
            "elm_unit()"

        [ single ] ->
            generateExprWithRenames ctx renames single

        [ first, second ] ->
            -- 2-tuple uses elm_tuple2 for proper tag
            let
                firstCode = generateExprWithRenames ctx renames first
                secondCode = generateExprWithRenames ctx renames second
            in
            "elm_tuple2(" ++ firstCode ++ ", " ++ secondCode ++ ")"

        first :: rest ->
            -- 3+ tuple: nest as elm_tuple2(first, tuple(rest))
            let
                firstCode = generateExprWithRenames ctx renames first
                restCode = generateTupleWithRenames ctx renames rest
            in
            "elm_tuple2(" ++ firstCode ++ ", " ++ restCode ++ ")"


generateRecordWithRenames : GenCtx -> Dict String String -> List ( String, Core.Expr ) -> String
generateRecordWithRenames ctx renames fields =
    let
        fieldCodes =
            fields
                |> List.map (\( name, e ) ->
                    "." ++ name ++ " = " ++ generateExprWithRenames ctx renames e
                )
                |> String.join ", "
    in
    "((elm_value_t){ .tag = 500, .data.p = &(struct { " ++
        String.join "; " (List.map (\( n, _ ) -> "elm_value_t " ++ n) fields) ++
        "; }){ " ++ fieldCodes ++ " }, .next = NULL })"


generateLet : GenCtx -> String -> Core.Expr -> Core.Expr -> String
generateLet ctx var value body =
    let
        valueCode = generateExpr ctx value
        bodyCode = generateExpr ctx body
    in
    "({ elm_value_t " ++ mangle var ++ " = " ++ valueCode ++ "; " ++ bodyCode ++ "; })"


generateLetRec : GenCtx -> List ( String, Core.Expr ) -> Core.Expr -> String
generateLetRec ctx bindings body =
    let
        decls =
            bindings
                |> List.map (\( name, _ ) ->
                    "elm_value_t " ++ mangle name ++ ";"
                )
                |> String.join " "

        assigns =
            bindings
                |> List.map (\( name, expr ) ->
                    mangle name ++ " = " ++ generateExpr ctx expr ++ ";"
                )
                |> String.join " "

        bodyCode = generateExpr ctx body
    in
    "({ " ++ decls ++ " " ++ assigns ++ " " ++ bodyCode ++ "; })"


generateCase : GenCtx -> Core.Expr -> List Core.Alt -> String
generateCase ctx scrutinee alts =
    let
        scrutCode = generateExpr ctx scrutinee
        ( scrutVar, ctx1 ) = freshName "scrut" ctx
        ( resultVar, ctx2 ) = freshName "result" ctx1

        branches =
            alts
                |> List.map (generateAlt ctx2 scrutVar resultVar)
                |> String.join " else "
    in
    "({ elm_value_t " ++ scrutVar ++ " = " ++ scrutCode ++ "; elm_value_t " ++ resultVar ++ "; " ++ branches ++ " " ++ resultVar ++ "; })"


generateAlt : GenCtx -> String -> String -> Core.Alt -> String
generateAlt ctx scrutVar resultVar (Core.Alt pattern guard body) =
    let
        ( condition, bindings ) = patternCondition ctx scrutVar pattern
        guardCode =
            case guard of
                Nothing -> ""
                Just g -> " && (" ++ generateExpr ctx g ++ ").data.i"
        bodyCode = generateExpr ctx body
    in
    "if (" ++ condition ++ guardCode ++ ") { " ++ bindings ++ resultVar ++ " = " ++ bodyCode ++ "; }"


patternCondition : GenCtx -> String -> Core.Pattern -> ( String, String )
patternCondition ctx scrutVar pattern =
    case pattern of
        Core.PVar tv ->
            ( "1", "elm_value_t " ++ mangle tv.name ++ " = " ++ scrutVar ++ "; " )

        Core.PWildcard _ ->
            ( "1", "" )

        Core.PLit lit _ ->
            case lit of
                Core.LInt n ->
                    ( scrutVar ++ ".data.i == " ++ String.fromInt n, "" )
                Core.LFloat f ->
                    ( scrutVar ++ ".data.f == " ++ String.fromFloat f, "" )
                Core.LString s ->
                    ( "strcmp(" ++ scrutVar ++ ".data.s, \"" ++ escapeString s ++ "\") == 0", "" )
                Core.LChar c ->
                    ( scrutVar ++ ".data.i == '" ++ escapeChar c ++ "'", "" )

        Core.PCon name subPats _ ->
            let
                -- Use actual tag values for known types
                tagValue =
                    case name of
                        "True" -> "5"
                        "False" -> "4"
                        "Nothing" -> "200"
                        "Just" -> "201"
                        "Err" -> "300"
                        "Ok" -> "301"
                        "[]" -> "100"  -- Nil (AST)
                        "::" -> "101"  -- Cons (AST)
                        "Nil" -> "100"  -- Nil (desugared)
                        "Cons" -> "101"  -- Cons (desugared)
                        _ -> "TAG_" ++ name

                tagCheck = scrutVar ++ ".tag == " ++ tagValue

                subConditions =
                    subPats
                        |> List.indexedMap (\i subPat ->
                            let
                                -- For list cons: first arg is .data.c (head), second is .next (tail)
                                -- For other constructors: chain through .data.c and .next
                                actualScrut =
                                    if name == "Cons" || name == "::" then
                                        if i == 0 then
                                            "(*" ++ scrutVar ++ ".data.c)"  -- head
                                        else
                                            "(*" ++ scrutVar ++ ".next)"     -- tail
                                    else
                                        if i == 0 then
                                            "(*" ++ scrutVar ++ ".data.c)"
                                        else
                                            -- Navigate through ->next chain for other constructors
                                            let
                                                nextChain = String.repeat i "->next"
                                            in
                                            "(*" ++ scrutVar ++ ".data.c" ++ nextChain ++ ")"
                            in
                            patternCondition ctx actualScrut subPat
                        )

                allConditions =
                    tagCheck :: List.map Tuple.first subConditions
                        |> String.join " && "

                allBindings =
                    List.map Tuple.second subConditions
                        |> String.join ""
            in
            ( allConditions, allBindings )

        Core.PTuple subPats _ ->
            -- Tuples are encoded as elm_cons(first, second) for 2-tuple
            -- For larger tuples: elm_cons(first, elm_cons(second, elm_cons(third, ...)))
            -- Structure: .data.c = first element, .next = rest (either a value or another cons)
            case subPats of
                [ firstPat, secondPat ] ->
                    -- 2-tuple: (a, b)
                    let
                        ( cond1, bind1 ) = patternCondition ctx ("(*" ++ scrutVar ++ ".data.c)") firstPat
                        ( cond2, bind2 ) = patternCondition ctx ("(*" ++ scrutVar ++ ".next)") secondPat
                    in
                    ( cond1 ++ " && " ++ cond2, bind1 ++ bind2 )

                [ firstPat, secondPat, thirdPat ] ->
                    -- 3-tuple: (a, b, c)
                    -- Structure: elm_cons(first, elm_cons(second, third))
                    let
                        ( cond1, bind1 ) = patternCondition ctx ("(*" ++ scrutVar ++ ".data.c)") firstPat
                        ( cond2, bind2 ) = patternCondition ctx ("(*" ++ scrutVar ++ ".next->data.c)") secondPat
                        ( cond3, bind3 ) = patternCondition ctx ("(*" ++ scrutVar ++ ".next->next)") thirdPat
                    in
                    ( cond1 ++ " && " ++ cond2 ++ " && " ++ cond3, bind1 ++ bind2 ++ bind3 )

                _ ->
                    -- 4+ tuples not supported
                    ( "1", "" )

        Core.PRecord fields _ ->
            -- Record patterns
            let
                bindings =
                    fields
                        |> List.map (\( name, subPat ) ->
                            case subPat of
                                Core.PVar tv ->
                                    "elm_value_t " ++ mangle tv.name ++ " = " ++ scrutVar ++ "." ++ name ++ "; "
                                _ ->
                                    ""
                        )
                        |> String.join ""
            in
            ( "1", bindings )

        Core.PAlias inner aliasName _ ->
            -- As-pattern: pattern as name
            -- Bind the alias to the whole scrutinee, then process inner pattern
            let
                aliasBinding = "elm_value_t " ++ mangle aliasName ++ " = " ++ scrutVar ++ "; "
                ( innerCond, innerBindings ) = patternCondition ctx scrutVar inner
            in
            ( innerCond, aliasBinding ++ innerBindings )


generateCon : GenCtx -> String -> List Core.Expr -> String
generateCon ctx name args =
    let
        argCodes =
            args
                |> List.map (generateExpr ctx)
                |> String.join ", "

        -- Map desugared names to runtime function names
        funcName =
            case name of
                "Nil" -> "elm_nil"
                "Cons" -> "elm_cons"
                "Nothing" -> "elm_nothing"
                "Just" -> "elm_just"
                "Ok" -> "elm_ok"
                "Err" -> "elm_err"
                "True" -> "elm_bool(true)"
                "False" -> "elm_bool(false)"
                "LT" -> "elm_lt"
                "EQ" -> "elm_eq"
                "GT" -> "elm_gt"
                "LE" -> "elm_Bytes_LE"
                "BE" -> "elm_Bytes_BE"
                "Bytes.LE" -> "elm_Bytes_LE"
                "Bytes.BE" -> "elm_Bytes_BE"
                _ -> mangle name
    in
    if List.isEmpty args && (name == "True" || name == "False") then
        funcName
    else if List.isEmpty args && (name == "LT" || name == "EQ" || name == "GT" || name == "LE" || name == "BE" || name == "Bytes.LE" || name == "Bytes.BE") then
        funcName ++ "()"
    else
        funcName ++ "(" ++ argCodes ++ ")"


generateTuple : GenCtx -> List Core.Expr -> String
generateTuple ctx exprs =
    -- Encode tuples as a chain
    case exprs of
        [] ->
            "elm_unit()"

        [ single ] ->
            generateExpr ctx single

        first :: rest ->
            let
                firstCode = generateExpr ctx first
                restCode = generateTuple ctx rest
            in
            "elm_cons(" ++ firstCode ++ ", " ++ restCode ++ ")"


generateRecord : GenCtx -> List ( String, Core.Expr ) -> String
generateRecord ctx fields =
    -- For now, encode records as a simple struct
    -- In a full implementation, we'd generate proper C structs
    let
        fieldCodes =
            fields
                |> List.map (\( name, expr ) ->
                    "." ++ name ++ " = " ++ generateExpr ctx expr
                )
                |> String.join ", "
    in
    "((elm_value_t){ .tag = 500, .data.p = &(struct { " ++
        String.join "; " (List.map (\( n, _ ) -> "elm_value_t " ++ n) fields) ++
        "; }){ " ++ fieldCodes ++ " }, .next = NULL })"


generateRecordAccess : GenCtx -> Core.Expr -> String -> String
generateRecordAccess ctx record field =
    let
        recordCode = generateExpr ctx record
    in
    "((" ++ recordCode ++ ").data.p->" ++ field ++ ")"


generateRecordUpdate : GenCtx -> Core.Expr -> List ( String, Core.Expr ) -> String
generateRecordUpdate ctx record fields =
    -- Record update: copy and modify
    -- This is simplified; full impl needs proper struct handling
    generateExpr ctx record


generateDictApp : GenCtx -> Core.Expr -> Core.Expr -> String
generateDictApp ctx func dict =
    -- Dictionary application - call function with dictionary
    generateExpr ctx func ++ "(" ++ generateExpr ctx dict ++ ")"


generateDictLam : GenCtx -> Core.TypedVar -> Core.Expr -> String
generateDictLam ctx tv body =
    -- Dictionary lambda - becomes a regular function
    generateLambda ctx tv body


generateDict : GenCtx -> String -> List Core.Expr -> String
generateDict ctx className _ =
    -- Dictionary literal
    "(elm_dict_t *)&dict_" ++ className



-- MAIN FUNCTION


generateMain : GenCtx -> String
generateMain ctx =
    let
        mainFunc = mangle (ctx.moduleName ++ "_main")

        hasMain = Dict.member "main" ctx.functions
    in
    if hasMain then
        String.join "\n"
            [ "/* ===== MAIN ===== */"
            , ""
            , "static void print_escaped_string(const char *s) {"
            , "    putchar('\"');"
            , "    while (*s) {"
            , "        if (*s == '\"' || *s == '\\\\') putchar('\\\\');"
            , "        putchar(*s++);"
            , "    }"
            , "    putchar('\"');"
            , "}"
            , ""
            , "static void print_value(elm_value_t v) {"
            , "    switch (v.tag) {"
            , "        case 0: printf(\"%ld\", (long)v.data.i); break;"
            , "        case 1: printf(\"%g\", v.data.f); break;"
            , "        case 2: print_escaped_string(v.data.s); break;"
            , "        case 3: printf(\"'%c'\", (char)v.data.i); break;"
            , "        case 4: printf(\"False\"); break;"
            , "        case 5: printf(\"True\"); break;"
            , "        case 6: printf(\"()\"); break;"
            , "        case 100: printf(\"[]\"); break;"
            , "        case 101: {"
            , "            printf(\"[\");"
            , "            elm_value_t curr = v;"
            , "            int first = 1;"
            , "            while (curr.tag == 101) {"
            , "                if (!first) printf(\", \");"
            , "                first = 0;"
            , "                print_value(*curr.data.c);"
            , "                curr = *curr.next;"
            , "            }"
            , "            printf(\"]\");"
            , "            break;"
            , "        }"
            , "        case 200: printf(\"Nothing\"); break;"
            , "        case 201: {"
            , "            printf(\"Just \");"
            , "            print_value(*v.data.c);"
            , "            break;"
            , "        }"
            , "        case 300: {"
            , "            printf(\"Err \");"
            , "            print_value(*v.data.c);"
            , "            break;"
            , "        }"
            , "        case 301: {"
            , "            printf(\"Ok \");"
            , "            print_value(*v.data.c);"
            , "            break;"
            , "        }"
            , "        case 600: {"
            , "            printf(\"(\");"
            , "            print_value(*v.data.c);"
            , "            printf(\", \");"
            , "            print_value(*v.next);"
            , "            printf(\")\");"
            , "            break;"
            , "        }"
            , "        case 700: printf(\"LT\"); break;"
            , "        case 701: printf(\"EQ\"); break;"
            , "        case 702: printf(\"GT\"); break;"
            , "        case 800: {"
            , "            printf(\"Task.fail \");"
            , "            print_value(*v.data.c);"
            , "            break;"
            , "        }"
            , "        case 801: {"
            , "            printf(\"Task.succeed \");"
            , "            print_value(*v.data.c);"
            , "            break;"
            , "        }"
            , "        default: printf(\"<value:%d>\", v.tag); break;"
            , "    }"
            , "}"
            , ""
            , "int main(void) {"
            , "    elm_value_t result = " ++ mainFunc ++ "();"
            , "    print_value(result);"
            , "    printf(\"\\n\");"
            , "    return 0;"
            , "}"
            ]
    else
        "/* No main function defined */"



-- HELPERS


mangle : String -> String
mangle name =
    -- Keep _op_ prefix so operators don't collide with user variables
    -- _op_gt -> elm__op_gt (different from user var gt -> elm_gt)
    "elm_" ++ String.map (\c -> if c == '.' then '_' else c) name


escapeString : String -> String
escapeString s =
    String.toList s
        |> List.map (\c ->
            case c of
                '"' -> "\\\""
                '\\' -> "\\\\"
                '\n' -> "\\n"
                '\t' -> "\\t"
                '\u{000D}' -> "\\r"
                _ -> String.fromChar c
        )
        |> String.concat


escapeChar : Char -> String
escapeChar c =
    case c of
        '\'' -> "\\'"
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\t' -> "\\t"
        '\u{000D}' -> "\\r"
        _ -> String.fromChar c
