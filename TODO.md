# tcelm TODO - Master Plan

## Target Use Case

**Primary Goal**: Build a production-grade, event-sourced double-entry accounting ledger running on Intel NUC with RTEMS.

**Design Philosophy**: Keep it simple. Elm's existing type system + do-notation + good FFI is sufficient. Avoid exotic Haskell features that add complexity without proportional benefit.

---

## Current Progress

**Last Updated**: 2026-01-25
**Last Session**: Fixed-size integer types (Int8/16/32/64, UInt8/16/32/64) fully implemented
**Next Action**: Continue with ledger work or other advanced features

### Session Log
- [x] 2025-01-24: Created TODO.md with full roadmap
- [x] 2025-01-24: Analyzed Cli.elm structure (6,353 lines, 168 case statements)
- [x] 2025-01-24: Documented Codegen/ module responsibilities
- [x] 2026-01-24: Extracted generateRtemsCode to Target/RTEMS.elm (~1600 lines)
- [x] 2026-01-24: Extracted generateNativeCode to Target/Native.elm (~100 lines)
- [x] 2026-01-24: Expanded Target/TCC.elm with full TCC code generation (~1350 lines)
- [x] 2026-01-24: Reorganized TODO.md for practical NUC+Ledger priorities
- [x] 2026-01-24: Fixed closure capture prefix and output order in TCC target
- [x] 2026-01-24: Implemented fixComplexConstantRefs to transform complex constant references
- [x] 2026-01-24: Fixed module prefix bug - pipeline operators and main now use correct prefixes
- [x] 2026-01-24: Added docs/patterns.md with 30 FP patterns (Python + tcelm examples)
- [x] 2026-01-24: Fixed case expression context - recursive calls now use correct module prefix
- [x] 2026-01-24: **MAJOR** Started fresh architecture in src2/ with:
  - Proper Hindley-Milner type inference (src2/Infer.elm)
  - Type classes infrastructure (src2/Types.elm)
  - Core IR intermediate representation (src2/Core.elm)
  - Modular parser with multi-line syntax support (src2/Parser.elm)
  - New code generator from Core IR (src2/Codegen/C.elm)
- [x] 2026-01-24: tcelm2 compiles and generates C for: functions, let, if, case, lambda
- [x] 2026-01-24: Fixed tcelm2 code generation:
  - Curried function calls now generate proper multi-argument C calls
  - Module prefixes added correctly (elm_Test_double instead of elm_double)
  - Partial application generates closures with correct arity
  - Zero-arity functions (CAFs) called correctly
  - Over-application chains elm_apply1 calls
- [x] 2026-01-24: Fixed parser and type inference:
  - Parser: stop function application at column 1 (new declaration boundary)
  - Type inference: handle cons pattern bindings when scrutinee type is TVar
  - Case expression bodies now correctly return values
  - List cons pattern matching works (h :: t -> h)
- [x] 2026-01-24: **MAJOR** Higher-order functions and closures working:
  - Parser: handle `::` (DoubleColon) as binary operator in expressions
  - Type inference: support recursive function definitions
  - Code generation: functions wrapped in closures when passed as values
  - Code generation: closure parameters called via elm_apply1
  - Pipeline operators (`|>`, `<|`) desugar to direct function application
  - Added `intDiv`, `pow`, `mod` operators
  - Added list printing to runtime
  - Working: `[1,2,3,4,5,6] |> filter isEven |> map double |> sum` = 24
- [x] 2026-01-24: Let-bound functions and inline lambdas:
  - Parser: function definitions in let blocks (`f x = x` -> `f = \x -> x`)
  - Code generation: local variables treated as closures when applied
  - Lambda lifting via GCC nested functions (captures work within scope)
  - Working: `let f = \x -> x + 10 in f 5` = 15
  - Working: `map (\x -> x * 2) [1,2,3]` = [2,4,6]
- [x] 2026-01-24: **MAJOR** Proper closure capture with lifted lambdas:
  - Lambdas lifted to top-level static functions (TCC compatible)
  - Free variables computed and captured explicitly in closure args[]
  - Uses existing partial application mechanism for dispatch
  - Working: `makeAdder 5` correctly captures and returns closure
  - Working: Higher-order functions (map, filter) with captured closures
- [x] 2026-01-24: Standard library in C runtime:
  - Basics: identity, always, flip, min, max, clamp, abs
  - List: isEmpty, length, reverse, member, head, tail, take, drop, sum, product,
          maximum, minimum, append, concat, intersperse, range, repeat,
          map, filter, filterMap, foldl, foldr, any, all, concatMap, indexedMap
  - Maybe: withDefault, map, andThen, map2
  - Result: withDefault, map, mapError, andThen, toMaybe
  - String: length, isEmpty, reverse, concat, append, join, left, right,
            dropLeft, dropRight, contains, startsWith, endsWith, fromInt, toInt
- [x] 2026-01-24: Qualified function names (List.map, Maybe.withDefault, etc.):
  - Parser: handle Module.function and Module.Constructor syntax
  - Infer: look up full qualified names in type environment
  - Infer: type signatures for all stdlib functions
  - Working: `List.length [1,2,3]` = 3, `List.map double [1,2,3]` = [2,4,6]
- [x] 2026-01-24: **MILESTONE 1 COMPLETE** - Multi-line pipelines:
  - Parser: skipNewlinesUnlessAtColumn1 allows operators on new lines
  - Working: `[1,2,3] |> List.map (\x -> x * 2) |> List.filter (\x -> x > 2) |> List.sum` = 10
- [x] 2026-01-24: Records with field access:
  - Parser: record.field syntax (ERecordAccess)
  - Codegen: records as linked lists of (name, value) pairs
  - Working: `{ name = "Alice", age = 30 }.age` = 30
- [x] 2026-01-24: Custom types with constructors and pattern matching:
  - Parser: multi-line type definitions (newlines before =)
  - Infer: pattern binding for constructor arguments
  - Working: `type Shape = Circle Int | Rectangle Int Int` with case matching
  - Working: `area (Rectangle 5 4)` = 20
- [x] 2026-01-24: Tuple support:
  - Tuple pattern matching in case expressions
  - Tuple module: pair, first, second, mapFirst, mapSecond
  - Working: `Tuple.first (1, 2)` = 1, tuple pattern `(a, b) -> a`
- [x] 2026-01-25: **MAJOR** Do-notation for Maybe and Result:
  - Monad detection based on constructor usage (Just/Nothing -> Maybe, Ok/Err -> Result)
  - Desugaring to qualified andThen chains (Maybe.andThen, Result.andThen)
  - Indentation-based parsing to terminate do blocks at column 1
  - Working: `do { x <- Just 10; y <- Just 20; Just (x + y) }` = Just 30
  - Working: `do { x <- validate 10; y <- validate (0 - 5); Ok (x + y) }` = Err "negative"
- [x] 2026-01-25: Parser fixes for case expressions in let bindings:
  - Fixed: case expression in let followed by another binding no longer consumes the binding
  - `parseAppArgs` now detects let bindings via `looksLikeLetBinding` check
  - Multi-line type aliases and record types now parse correctly
  - Working: `let result = case x of ... ; y = 5 in result + y`
- [x] 2026-01-25: As-patterns (PAlias) support:
  - Parser: parse `pattern as name` syntax via KwAs token
  - Core IR: add PAlias pattern type with inner pattern and alias name
  - Desugar: convert AST.PAlias to Core.PAlias
  - Codegen: generate bindings for both alias and inner pattern
  - Working: `(x :: xs) as all -> List.length all`
- [x] 2026-01-25: Additional features:
  - 3-tuple pattern matching in case expressions
  - `modBy` and `remainderBy` functions added to runtime
  - Builtin functions (identity, etc.) wrapped in closures when used as values
  - Function composition operators `>>` and `<<` implemented via inline lambda
  - Record update syntax `{ rec | field = value }` implemented
  - Local variable shadowing of builtins fixed
- [x] 2026-01-25: Operator naming collision fix:
  - Operators desugared with `_op_` prefix (_op_gt, _op_add, etc.)
  - C runtime functions use full prefix (elm__op_gt, elm__op_add)
  - User variables with operator-like names (gt, lt, eq, and, or) no longer conflict
- [x] 2026-01-25: More String module functions:
  - String.split, String.slice, String.toUpper, String.toLower
  - String.trim, String.trimLeft, String.trimRight
- [x] 2026-01-25: More List module functions + tuple fix:
  - List.sort (insertion sort for integers)
  - List.sortBy (sort by key function)
  - List.partition (split by predicate into tuple)
  - Fixed tuple printing: tuples now use dedicated tag 600 and print as (a, b)
  - 84 tests passing
- [x] 2026-01-25: String.fromFloat and String.toFloat added
  - 86 tests passing
- [x] 2026-01-25: Dict module with association list implementation:
  - Dict.empty, Dict.singleton, Dict.insert, Dict.get, Dict.remove
  - Dict.member, Dict.size, Dict.isEmpty, Dict.keys, Dict.values
  - Dict.toList, Dict.fromList
  - Key comparison handles both Int and String keys
  - Maintains insertion order
  - Fixed: zero-arity builtin functions now properly called with ()
  - 94 tests passing
- [x] 2026-01-25: Dict higher-order functions:
  - Dict.update, Dict.map, Dict.filter, Dict.foldl, Dict.foldr
  - Fixed closure extraction in Dict higher-order functions
  - 97 tests passing
- [x] 2026-01-25: Set module with list-based implementation:
  - Set.empty, Set.singleton, Set.insert, Set.remove, Set.member
  - Set.size, Set.isEmpty, Set.toList, Set.fromList
  - Set.union, Set.intersect, Set.diff (set operations)
  - Set.map, Set.filter, Set.foldl, Set.foldr (higher-order functions)
  - 103 tests passing
- [x] 2026-01-25: Char module:
  - Char.toCode / Char.fromCode, Char.toUpper / Char.toLower
  - Char.isDigit, Char.isLower, Char.isUpper, Char.isAlpha, Char.isAlphaNum
  - 109 tests passing
- [x] 2026-01-25: Bitwise module:
  - Bitwise.and, Bitwise.or, Bitwise.xor, Bitwise.complement
  - Bitwise.shiftLeftBy, Bitwise.shiftRightBy, Bitwise.shiftRightZfBy
  - 114 tests passing
- [x] 2026-01-25: Array module (list-based implementation):
  - Array.empty, fromList, toList, toIndexedList, length, isEmpty
  - Array.get, set, push, append, slice
  - Array.map, indexedMap, foldl, foldr, filter
  - 119 tests passing
- [x] 2026-01-25: Maybe.map3/map4/map5 - complete Maybe module
  - 120 tests passing
- [x] 2026-01-25: FFI (foreign import) support:
  - Parse `foreign import name : Type` declarations
  - Convert type annotations to Types (typeAnnotationToType in Infer.elm, Desugar.elm)
  - Generate extern declarations for C functions
  - Generate wrapper functions that convert elm_value_t <-> C types
  - Support for Int, Float, Bool, String, Char, (), Ptr types
  - Foreign functions properly integrated with code generator (mangling, arity, calls)
  - 120 tests passing
- [x] 2026-01-25: Complete String module:
  - String.toList, String.fromList - convert to/from List Char
  - String.padLeft, String.padRight - pad strings with fill character
  - 124 tests passing
- [x] 2026-01-25: More Result functions:
  - Result.map2, Result.map3 - combine multiple Results
  - Result.fromMaybe - convert Maybe to Result with error value
  - 126 tests passing
- [x] 2026-01-25: List.singleton
  - 127 tests passing
- [x] 2026-01-25: Order type, compare, curry, uncurry
  - Order type with LT, EQ, GT constructors
  - compare function for comparable types
  - curry / uncurry for tuple conversion
  - 132 tests passing
- [x] 2026-01-25: Extended List and String modules
  - List.unzip, List.map2/map3/map4/map5
  - String.cons, String.uncons, String.repeat, String.words, String.lines
  - String.foldl, String.foldr, String.any, String.all, String.filter, String.map
  - Basics: xor, isNaN, isInfinite
  - Math: sqrt, sin, cos, tan, asin, acos, atan, atan2, logBase, e, pi
  - Rounding: ceiling, floor, round, truncate, toFloat
  - Debug: log, toString
  - 150 tests passing
- [x] 2026-01-25: More standard library functions
  - Tuple.mapBoth - apply functions to both tuple elements
  - String.fromChar, String.indexes, String.indices - char/search functions
  - String.replace - replace all occurrences of a substring
  - List.sortWith - sort with custom comparator (a -> a -> Order)
  - Char.isHexDigit, Char.isOctDigit, Char.isSpace - more character classifiers
  - Basics: degrees, radians, turns - angle conversion functions
  - Array.initialize, Array.repeat - array creation functions
  - Dict.partition, Set.partition - split by predicate
  - Debug.todo - crash with message for incomplete code
  - Fixed: LT/EQ/GT constructors now in builtinConstructors
  - 163 tests passing, ~245 builtin functions
  - FFI working (foreign import generates wrappers for C functions)
- [x] 2026-01-25: Task module for async computations
  - Task.succeed, Task.fail - create successful/failed tasks
  - Task.map, Task.andThen - transform and chain tasks
  - Task.mapError, Task.onError - error handling
  - Task.map2, Task.map3 - combine multiple tasks
  - Task.sequence - sequence a list of tasks
  - Do-notation support with automatic Task monad detection
  - 175 tests passing
- [x] 2026-01-25: More utility functions
  - List.getAt : Int -> List a -> Maybe a (element at index)
  - List.last : List a -> Maybe a (last element)
  - Maybe.filter : (a -> Bool) -> Maybe a -> Maybe a (filter with predicate)
  - Dict.union, Dict.diff, Dict.intersect (set operations on dicts)
  - 184 tests passing, ~260 builtin functions
- [x] 2026-01-25: Time and Process modules
  - Time.now : Task x Int (get current POSIX time in milliseconds)
  - Time.posixToMillis, Time.millisToPosix (conversions)
  - Process.sleep : Float -> Task x () (sleep for milliseconds)
  - List.zip : List a -> List b -> List (a, b) (combine lists into tuples)
  - 187 tests passing, ~270 builtin functions
- [x] 2026-01-25: Type classes (Elm-style constrained polymorphism)
  - Built-in type classes: comparable, appendable, number
  - Instance resolution for: Int, Float, Char, String, List comparable, Tuple
  - Updated function signatures with constraints:
    - List.sort, List.sortBy, compare: comparable constraint
    - List.maximum, List.minimum: comparable constraint
    - Dict.*: comparable key constraint
    - Set.*: comparable element constraint
  - Instance resolution algorithm in Infer.elm
  - Exported isComparable, isAppendable, isNumber for code generation
  - 188 tests passing
- [x] 2026-01-25: Json.Encode module
  - Json.Encode.string, int, float, bool, null (primitive encoders)
  - Json.Encode.list, array (collection encoders)
  - Json.Encode.object (object encoder from list of pairs)
  - Json.Encode.encode (convert Value to JSON string with optional indentation)
  - Fixed parser to handle nested module paths (Json.Encode.func)
  - 194 tests passing
- [x] 2026-01-25: Json.Decode module
  - Json.Decode.string, int, float, bool (primitive decoders)
  - Json.Decode.null (null decoder with fallback value)
  - Json.Decode.field (object field access)
  - Json.Decode.list, nullable (collection decoders)
  - Json.Decode.map, map2, andThen (decoder combinators)
  - Json.Decode.succeed, fail (create successful/failed decoders)
  - Json.Decode.decodeString, decodeValue (run decoders)
  - Full JSON parser in C runtime with string escape handling
  - Fixed Debug.toString to properly display Result, Maybe, List, Tuple, Order
  - Fixed print_value to escape quotes in string output
  - 198 tests passing
- [x] 2026-01-25: Bytes module (Binary Serialization)
  - Bytes.width, Bytes.isEmpty (core operations)
  - Bytes.Encode: signedInt8/16/32, unsignedInt8/16/32, float32/64
  - Bytes.Encode: bytes, string, sequence, encode
  - Bytes.Decode: signedInt8/16/32, unsignedInt8/16/32, float32/64
  - Bytes.Decode: bytes, string, map, map2, andThen, succeed, fail
  - Bytes.Decode.decode (run decoder on Bytes)
  - Endianness support: Bytes.LE, Bytes.BE
  - Fixed constructor mangling for qualified names (Bytes.LE -> elm_Bytes_LE)
  - 201 tests passing
- [x] 2026-01-25: Extended Json.Decode module
  - Json.Decode.index (array element by index)
  - Json.Decode.at (nested field path)
  - Json.Decode.oneOf (try multiple decoders)
  - Json.Decode.maybe (wrap result in Maybe)
  - Json.Decode.value (return raw JSON value)
  - 204 tests passing
- [x] 2026-01-25: Fixed-Size Integer Types (Phase 5.1)
  - Int8, Int16, Int32, Int64: fromInt, toInt, add/sub/mul/div/mod
  - UInt8, UInt16, UInt32, UInt64: fromInt, toInt, add/sub/mul/div/mod
  - Bitwise operations: and, or, xor, complement, shiftLeftBy, shiftRightBy
  - Proper wrap-around behavior (e.g., Int8.fromInt 128 = -128)
  - 214 tests passing
- [x] 2026-01-25: Safe pointer handling (Phase 4.2)
  - Ptr.null: null pointer constant
  - Ptr.isNull: check if pointer is null
  - Ptr.toMaybe: convert null to Nothing, non-null to Just
  - 216 tests passing
- [ ] **NEXT**: Continue with more advanced features or ledger work

---

## NEW ARCHITECTURE (tcelm2)

The src2/ directory contains a complete rewrite with proper foundations:

### Architecture Overview
```
Source -> Lexer -> Parser -> AST -> Type Inference -> Core IR -> C Code
```

### Key Modules (src2/)

| Module | Purpose |
|--------|---------|
| Types.elm | Core type system: Type, Scheme, Kind, TypeClass, Constraint |
| AST.elm | Source-level AST with Located types |
| Lexer.elm | Tokenizer with indentation tracking |
| Parser.elm | Recursive descent parser, multi-line support |
| Infer.elm | Hindley-Milner type inference with extensions |
| Core.elm | Explicitly-typed Core IR (lambda calculus + dictionary passing) |
| Desugar.elm | Transform AST to Core IR |
| Codegen/C.elm | Generate C from Core IR |
| Compiler.elm | Main entry point |

### tcelm2 Status

| Feature | Status | Notes |
|---------|--------|-------|
| Functions with args | ✅ Working | Type inference handles function parameters |
| Let expressions | ✅ Working | Multi-line let/in, function definitions |
| If expressions | ✅ Working | Multi-line if/then/else supported |
| Case expressions | ✅ Working | Pattern matching compiles |
| Lambdas | ✅ Working | GCC nested functions for captures |
| Binary operators | ✅ Working | +, -, *, /, //, etc. in scope |
| Type inference | ✅ Working | Hindley-Milner with proper scoping |
| Partial application | ✅ Working | Closures generated with correct arity |
| List patterns | ✅ Working | h :: t pattern matching works |
| Recursive functions | ✅ Working | Self-references resolve in type inference |
| Higher-order functions | ✅ Working | map, filter with closures work |
| Pipeline operators | ✅ Working | `|>` and `<|` desugar to function application |
| Qualified names | ✅ Working | `List.map`, `Maybe.withDefault`, etc. |
| Standard library | ✅ Working | List, Maybe, Result, String, Basics in C runtime |
| Records | ✅ Working | Creation and field access (`record.field`) |
| Custom types | ✅ Working | Constructors with args, pattern matching |
| Tuples | ✅ Working | Creation, pattern matching, Tuple module |
| Do-notation | ✅ Working | Maybe and Result, monad auto-detection |
| Type classes | ✅ Working | comparable, appendable, number with instance resolution |

### Known Issues

1. ~~**Curried function calls**: Fixed - generates `elm_add(x, y)` directly~~
2. ~~**Module prefixes**: Fixed - `elm_Test_double` generated correctly~~
3. ~~**Parser boundary**: Fixed - stops at column 1 for new declarations~~
4. ~~**Higher-order functions**: Fixed - closures generated and applied correctly~~
5. ~~**Multi-line expressions**: Fixed - operators on new lines now continue the expression~~
6. ~~**Cross-scope closure capture**: Fixed - lambdas lifted to top-level static functions with explicit capture~~

### Usage
```bash
# Compile the compiler
elm make src2/Compiler.elm --output=bin/tcelm2.js

# Run tcelm2
./bin/tcelm2 source.elm         # Output to stdout
./bin/tcelm2 source.elm -o out.c  # Output to file
```

---

## Current State Summary

| Component | Status | Notes |
|-----------|--------|-------|
| Parser | ~90% | Most Elm syntax parsed |
| Cli.elm | 7,000+ lines | Orchestrates code generators |
| Target/RTEMS.elm | Complete | RTEMS target with CodegenConfig |
| Target/TCC.elm | Complete | TCC target with CodegenConfig |
| Target/Native.elm | Complete | Native target with CodegenConfig |
| Codegen.Pattern | ~60% | Simple patterns only, complex in Cli |
| Codegen.Lambda | Working | Lambda lifting + capture fixed |
| Pipeline operators | Working | |> and <| fully implemented |
| Pattern matching | ~85% | Cons, nested, tuples work; as-patterns partial |
| do-notation | ✅ Working | Maybe and Result monads |
| Standard library | Partial | Many C implementations missing |

---

## Phase 1: Core Language (CRITICAL)

**Goal**: Make basic Elm features work reliably before adding anything new.

### 1.1 Fix Lambda/Closure Capture ✅ COMPLETE
Current: Lambda lifting with closure capture works.
- [x] Implement proper closure struct in C runtime
- [x] Generate closure creation code (allocate, capture vars)
- [x] Generate closure invocation code (extract env, call)
- [x] Test: `List.map (\x -> x + offset) items` where `offset` is captured
- [x] Test: Nested lambdas with multiple capture levels
- Working: `makeAdder 5 |> \add5 -> add5 10` = 15

### 1.2 Pipeline Operators ✅ DONE
- [x] `|>` desugaring: `a |> f` → `f a`
- [x] `<|` desugaring: `f <| a` → `f a`
- [x] `|>` with partial application: `a |> f b` → `f b a`
- [x] `<<` and `>>` composition works
- [x] Fixed module prefix bug in pipeline function calls

### 1.3 Complete Pattern Matching ⚠️ MOSTLY DONE
Current: Most patterns work, module prefix context propagated through case expressions.
- [x] List cons patterns: `x :: xs` - Working with recursive calls
- [x] Nested constructor patterns: `Just (Ok value)` - Working
- [x] Tuple patterns in case: `(a, b, c) ->` - Working (up to 3 elements)
- [x] Record patterns: `{ name, balance } ->` - Working
- [x] As-patterns: `(x :: xs) as list` - Working
- [ ] Tuple patterns > 3 elements - Not supported

### 1.4 Partial Application / Currying
Current: Working.
- [x] Detect partial application sites
- [x] Generate closure for partially applied functions
- [x] Handle curried function definitions
- Working: `let add5 = add 5 in add5 10` = 15
- Working: `List.map (add 1) [1,2,3]` = [2,3,4]

---

## Phase 2: do-notation ✅ COMPLETE

**Goal**: Clean, readable monadic code for Result/Maybe/Task chains.

### 2.1 Parser Support
- [x] Parse `do` keyword as expression starter
- [x] Parse `<-` bind operator
- [x] Parse `let` bindings inside do-blocks
- [ ] Parse `pure` / `return` (alias for identity wrap) - use `Just` / `Ok` directly

### 2.2 Desugaring
```elm
-- This:
do
    validated <- validate txn
    balanced <- checkBalance validated
    Ok (TransactionPosted balanced)

-- Becomes:
Result.andThen (\validated ->
    Result.andThen (\balanced ->
        Ok (TransactionPosted balanced)
    ) (checkBalance validated)
) (validate txn)
```
- [x] Desugar bind (`<-`) to `andThen` calls
- [x] Desugar `let` to regular let bindings
- [x] Detect monad from constructor usage (Just/Nothing -> Maybe, Ok/Err -> Result)
- [x] Generate qualified andThen calls (Maybe.andThen, Result.andThen)

### 2.3 Integration
- [x] Works with Result and Maybe
- [ ] Works with custom types that have `andThen` (needs type classes)
- [ ] Good error messages for type mismatches

---

## Phase 3: Standard Library (HIGH PRIORITY)

**Goal**: Complete C implementations for common functions.

### 3.1 String Module ✅ MOSTLY COMPLETE
- [x] String.fromInt
- [x] String.fromFloat
- [x] String.toInt
- [x] String.toFloat
- [x] String.concat
- [x] String.join
- [x] String.length
- [x] String.isEmpty
- [x] String.reverse
- [x] String.append
- [x] String.left
- [x] String.right
- [x] String.dropLeft
- [x] String.dropRight
- [x] String.contains
- [x] String.startsWith
- [x] String.endsWith
- [x] String.split
- [x] String.slice
- [x] String.toUpper / String.toLower
- [x] String.trim / String.trimLeft / String.trimRight
- [x] String.toList / String.fromList
- [x] String.padLeft / String.padRight

### 3.2 List Module ✅ COMPLETE
- [x] List.map
- [x] List.filter
- [x] List.filterMap
- [x] List.foldl / List.foldr
- [x] List.concat
- [x] List.concatMap
- [x] List.head / List.tail
- [x] List.take / List.drop
- [x] List.length
- [x] List.reverse
- [x] List.member
- [x] List.any / List.all
- [x] List.indexedMap
- [x] List.intersperse
- [x] List.range
- [x] List.repeat
- [x] List.sum / List.product
- [x] List.maximum / List.minimum
- [x] List.sort / List.sortBy
- [x] List.partition

### 3.3 Dict Module ✅ COMPLETE
- [x] Dict.empty
- [x] Dict.singleton
- [x] Dict.insert
- [x] Dict.get
- [x] Dict.remove
- [x] Dict.member
- [x] Dict.size / Dict.isEmpty
- [x] Dict.keys / Dict.values
- [x] Dict.toList / Dict.fromList
- [x] Dict.update
- [x] Dict.map
- [x] Dict.filter
- [x] Dict.foldl / Dict.foldr

### 3.4 Maybe Module ✅ COMPLETE
- [x] Maybe.map
- [x] Maybe.andThen
- [x] Maybe.withDefault
- [x] Maybe.map2
- [x] Maybe.map3 / map4 / map5

### 3.5 Result Module ✅ COMPLETE
- [x] Result.map
- [x] Result.mapError
- [x] Result.andThen
- [x] Result.withDefault
- [x] Result.toMaybe
- [x] Result.map2 / Result.map3
- [x] Result.fromMaybe

### 3.6 Basics ✅ COMPLETE
- [x] identity
- [x] always
- [x] flip
- [x] curry / uncurry
- [x] compare
- [x] min / max
- [x] clamp
- [x] abs
- [x] negate
- [x] modBy / remainderBy

---

## Phase 4: FFI for RTEMS (HIGH PRIORITY)

**Goal**: Call C functions from Elm, integrate with RTEMS APIs.

### 4.1 Basic FFI ✅ COMPLETE
```elm
foreign import rtems_task_wake_after : Int -> Task ()
foreign import serial_write : String -> Task ()
foreign import gpio_read : Int -> Task Int
```
- [x] Parse `foreign import` declarations
- [x] Generate C function call wrappers
- [x] Handle return type conversion (Int, Float, Bool, String, Char, ())
- [ ] Handle Task wrapping for async operations

### 4.2 Pointer Types ✅ COMPLETE
```elm
type alias FileHandle = Ptr
foreign import fopen : String -> String -> Task FileHandle
foreign import fclose : FileHandle -> Task ()
```
- [x] Opaque `Ptr` type (void* as elm_value_t with tag 7)
- [x] Safe handle patterns (Ptr.toMaybe for optional access)
- [x] Null pointer handling (Ptr.null, Ptr.isNull)

### 4.3 Callbacks
```elm
foreign import register_callback : (Int -> ()) -> Task ()
```
- [ ] Generate C function pointers from Elm functions
- [ ] Handle callback invocation

---

## Phase 5: Practical Additions (MEDIUM PRIORITY)

### 5.1 Fixed-Size Integers ✅ COMPLETE
For embedded/protocol work:
- [x] Int8, Int16, Int32, Int64
- [x] UInt8, UInt16, UInt32, UInt64
- [x] Bit operations: and, or, xor, complement, shiftLeftBy, shiftRightBy
- [x] Overflow behavior (wrapping - C standard behavior)

### 5.2 JSON Serialization ✅ COMPLETE
For debugging, config, API communication:
- [x] Json.Encode basics (string, int, float, bool, null, list, object, encode)
- [x] Json.Decode basics (string, int, float, bool, null)
- [x] Decode.field, Decode.map, Decode.map2, Decode.andThen
- [x] Decode.list, Decode.nullable, Decode.succeed, Decode.fail
- [x] Decode.index, Decode.at, Decode.oneOf, Decode.maybe, Decode.value
- [x] decodeString, decodeValue (run decoders)
- [ ] Auto-derive for simple records (optional)

### 5.3 Binary Serialization ✅ COMPLETE
For efficient storage/network:
- [x] Bytes type (wraps byte buffer with length)
- [x] Bytes.Encode: signedInt8/16/32, unsignedInt8/16/32, float32/64
- [x] Bytes.Encode: bytes, string, sequence, encode
- [x] Bytes.Decode: signedInt8/16/32, unsignedInt8/16/32, float32/64
- [x] Bytes.Decode: bytes, string, map, map2, andThen, succeed, fail, decode
- [x] Endianness control via Bytes.LE / Bytes.BE

### 5.4 Simple Deriving
Reduce boilerplate for common cases:
- [ ] deriving Eq (structural equality)
- [ ] deriving Show (debug string)
- [ ] deriving ToJson / FromJson

---

## Phase 6: RTEMS Multi-Core (LOWER PRIORITY)

For 4-core NUC parallel execution.

### 6.1 Core Primitives
- [ ] Core affinity type
- [ ] Core ID query
- [ ] Core count query

### 6.2 Parallelism
- [ ] `Par` type for parallel computations
- [ ] `parMap` - parallel map over array
- [ ] Barrier synchronization

### 6.3 Inter-Core Communication
- [ ] `Channel a` - bounded queues
- [ ] `MVar a` - synchronized mutable variable
- [ ] Atomic variables

---

## Phase 7: Code Cleanup (ONGOING)

### 7.1 Refactoring Cli.elm
- [x] Extract Target/RTEMS.elm
- [x] Extract Target/Native.elm
- [x] Expand Target/TCC.elm
- [ ] Move generateStandaloneCaseFallback to Codegen.Pattern
- [ ] Extract type inference to Codegen/TypeInference.elm
- [ ] Move lambda functions to Codegen.Lambda
- [ ] Remove dead code (old generator functions)

### 7.2 Testing
- [ ] Unit tests for Codegen modules
- [ ] Integration tests (Elm → C → execution)
- [ ] Ledger-specific test cases

---

## Future Advanced Features (tcelm2 Architecture)

The new src2/ architecture was specifically designed to support these features when needed:

| Feature | Status | Notes |
|---------|--------|-------|
| Type Classes | ✅ Working | Elm-style comparable/appendable/number constraints |
| Higher-Kinded Types | 🔧 Infrastructure ready | Kind system in Types.elm (KStar, KArrow) |
| do-notation | ✅ Working | Maybe and Result monads, auto-detection |
| Row Polymorphism | 🔧 Types ready | TRecord has optional row variable |
| Monad Transformers | ⏳ Future | Can be built on type classes |
| GADTs | ⏳ Future | Would require extending Core IR |

The new architecture avoids "boxing in" - advanced features can be added incrementally without rewrites.

### Not Needed (Still True)

| Feature | Why Skip |
|---------|----------|
| Lazy Evaluation | Not needed for embedded real-time |
| Custom Operators | Standard operators are sufficient |
| Full Lenses | Verbose nested update syntax is acceptable |

---

## Implementation Priority

### Now (Blocking Issues)
1. Fix lambda/closure capture
2. Implement pipeline operators
3. Complete pattern matching

### Next (Enable Productivity)
1. do-notation
2. Standard library completion
3. Basic FFI

### Later (Nice to Have)
1. Fixed-size integers
2. JSON/Binary serialization
3. Simple deriving
4. Multi-core support

---

## Success Criteria

**Milestone 1** ✅ COMPLETE: Can compile and run:
```elm
main =
    [ 1, 2, 3 ]
        |> List.map (\x -> x * 2)
        |> List.filter (\x -> x > 2)
        |> List.sum
```
Result: 10 (correct!)

**Milestone 2**: Can compile and run:
```elm
processTransaction : Transaction -> Result Error TransactionId
processTransaction txn = do
    validated <- validate txn
    balanced <- checkBalance validated
    eventId <- recordEvent (TransactionPosted balanced)
    pure eventId
```

**Milestone 3**: Can compile a basic ledger module with:
- Account CRUD
- Transaction posting
- Balance queries
- Event sourcing

**Milestone 4**: Ledger runs on NUC with RTEMS, communicates via serial/network.

---

## Quick Reference: Key Files

| File | Purpose | Lines |
|------|---------|-------|
| src/Cli.elm | Main CLI, orchestrates code generators | 6,353 |
| src/Target/RTEMS.elm | RTEMS target code generation | ~1,600 |
| src/Target/Native.elm | Native target code generation | ~100 |
| src/Target/TCC.elm | TCC target code generation | ~1,350 |
| src/Generate/C.elm | Module-based C generation | ~500 |
| src/Codegen/Shared.elm | Common types and helpers | 343 |
| src/Codegen/Expr.elm | Expression generation | 240 |
| src/Codegen/Builtins.elm | Built-in functions | Large |
| src/Codegen/Pattern.elm | Pattern matching | 468 |
| src/Codegen/Union.elm | Custom types | 105 |
| src/Codegen/Lambda.elm | Lambda lifting | 143 |

---

## Notes

- **Working code beats elegant code** - Ship features that work reliably
- **Elm's type system is already powerful** - Don't need Haskell's complexity
- **Runtime validation is fine** - Types don't need to prove everything
- **Focus on the ledger** - Every feature should serve that goal
