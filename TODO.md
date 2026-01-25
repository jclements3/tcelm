# tcelm TODO - Master Plan

## Target Use Case

**Primary Goal**: Build a production-grade, event-sourced double-entry accounting ledger running on Intel NUC with RTEMS.

**Design Philosophy**: Keep it simple. Elm's existing type system + do-notation + good FFI is sufficient. Avoid exotic Haskell features that add complexity without proportional benefit.

---

## Current Progress

**Last Updated**: 2026-01-24
**Last Session**: Fixed module prefix bug in pipeline operators and main extraction
**Next Action**: Complete pattern matching (cons patterns, nested constructors)

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
- [ ] **NEXT**: Complete pattern matching

---

## Current State Summary

| Component | Status | Notes |
|-----------|--------|-------|
| Parser | ~90% | Most Elm syntax parsed |
| Cli.elm | 6,800+ lines | Orchestrates code generators |
| Target/RTEMS.elm | Complete | RTEMS target with CodegenConfig |
| Target/TCC.elm | Complete | TCC target with CodegenConfig |
| Target/Native.elm | Complete | Native target with CodegenConfig |
| Codegen.Pattern | ~60% | Simple patterns only, complex in Cli |
| Codegen.Lambda | Working | Lambda lifting + capture fixed |
| Pipeline operators | Working | |> and <| fully implemented |
| do-notation | Not done | Needs parser + desugaring |
| Standard library | Partial | Many C implementations missing |

---

## Phase 1: Core Language (CRITICAL)

**Goal**: Make basic Elm features work reliably before adding anything new.

### 1.1 Fix Lambda/Closure Capture (BLOCKING)
Current: Lambda lifting works but variable capture is fragile.
- [ ] Implement proper closure struct in C runtime
- [ ] Generate closure creation code (allocate, capture vars)
- [ ] Generate closure invocation code (extract env, call)
- [ ] Test: `List.map (\x -> x + offset) items` where `offset` is captured
- [ ] Test: Nested lambdas with multiple capture levels
- [ ] Test: Parser combinator patterns (andThen chains)

### 1.2 Pipeline Operators ✅ DONE
- [x] `|>` desugaring: `a |> f` → `f a`
- [x] `<|` desugaring: `f <| a` → `f a`
- [x] `|>` with partial application: `a |> f b` → `f b a`
- [x] `<<` and `>>` composition works
- [x] Fixed module prefix bug in pipeline function calls

### 1.3 Complete Pattern Matching
Current: Simple patterns work, complex ones partial.
- [ ] List cons patterns: `x :: xs`
- [ ] Nested constructor patterns: `Just (Ok value)`
- [ ] As-patterns: `(x :: xs) as list`
- [ ] Tuple patterns in case: `(a, b, c) ->`
- [ ] Record patterns: `{ name, balance } ->`

### 1.4 Partial Application / Currying
Current: Not implemented.
- [ ] Detect partial application sites
- [ ] Generate closure for partially applied functions
- [ ] Handle curried function definitions

---

## Phase 2: do-notation (HIGH PRIORITY)

**Goal**: Clean, readable monadic code for Result/Maybe/Task chains.

### 2.1 Parser Support
- [ ] Parse `do` keyword as expression starter
- [ ] Parse `<-` bind operator
- [ ] Parse `let` bindings inside do-blocks
- [ ] Parse `pure` / `return` (alias for identity wrap)

### 2.2 Desugaring
```elm
-- This:
do
    validated <- validate txn
    balanced <- checkBalance validated
    pure (TransactionPosted balanced)

-- Becomes:
validate txn
    |> Result.andThen (\validated ->
        checkBalance validated
            |> Result.andThen (\balanced ->
                Ok (TransactionPosted balanced)
            )
    )
```
- [ ] Desugar bind (`<-`) to `andThen` calls
- [ ] Desugar `let` to regular let bindings
- [ ] Infer which `andThen` to use based on type (Result.andThen, Maybe.andThen, etc.)
- [ ] Handle `pure` as type-appropriate wrapper

### 2.3 Integration
- [ ] Works with Result, Maybe, Task
- [ ] Works with custom types that have `andThen`
- [ ] Good error messages for type mismatches

---

## Phase 3: Standard Library (HIGH PRIORITY)

**Goal**: Complete C implementations for common functions.

### 3.1 String Module
- [ ] String.fromInt
- [ ] String.fromFloat
- [ ] String.toInt
- [ ] String.toFloat
- [ ] String.concat
- [ ] String.join
- [ ] String.split
- [ ] String.slice
- [ ] String.left / String.right
- [ ] String.dropLeft / String.dropRight
- [ ] String.contains
- [ ] String.startsWith / String.endsWith
- [ ] String.toList / String.fromList
- [ ] String.toUpper / String.toLower
- [ ] String.trim
- [ ] String.padLeft / String.padRight

### 3.2 List Module
- [ ] List.map (needs working closures)
- [ ] List.filter
- [ ] List.filterMap
- [ ] List.foldl / List.foldr
- [ ] List.concat
- [ ] List.concatMap
- [ ] List.head / List.tail
- [ ] List.take / List.drop
- [ ] List.length
- [ ] List.reverse
- [ ] List.member
- [ ] List.any / List.all
- [ ] List.sort / List.sortBy
- [ ] List.indexedMap
- [ ] List.partition
- [ ] List.intersperse
- [ ] List.range

### 3.3 Dict Module
- [ ] Dict.empty
- [ ] Dict.singleton
- [ ] Dict.insert
- [ ] Dict.get
- [ ] Dict.remove
- [ ] Dict.update
- [ ] Dict.member
- [ ] Dict.keys / Dict.values
- [ ] Dict.toList / Dict.fromList
- [ ] Dict.map
- [ ] Dict.filter
- [ ] Dict.foldl

### 3.4 Maybe Module
- [ ] Maybe.map
- [ ] Maybe.andThen
- [ ] Maybe.withDefault
- [ ] Maybe.map2 / Maybe.map3

### 3.5 Result Module
- [ ] Result.map
- [ ] Result.mapError
- [ ] Result.andThen
- [ ] Result.withDefault
- [ ] Result.toMaybe

### 3.6 Basics
- [ ] identity
- [ ] always
- [ ] flip
- [ ] curry / uncurry
- [ ] compare
- [ ] min / max
- [ ] clamp

---

## Phase 4: FFI for RTEMS (HIGH PRIORITY)

**Goal**: Call C functions from Elm, integrate with RTEMS APIs.

### 4.1 Basic FFI
```elm
foreign import rtems_task_wake_after : Int -> Task ()
foreign import serial_write : String -> Task ()
foreign import gpio_read : Int -> Task Int
```
- [ ] Parse `foreign import` declarations
- [ ] Generate C function call wrappers
- [ ] Handle return type conversion
- [ ] Handle Task wrapping for async operations

### 4.2 Pointer Types
```elm
type alias FileHandle = Ptr
foreign import fopen : String -> String -> Task FileHandle
foreign import fclose : FileHandle -> Task ()
```
- [ ] Opaque `Ptr` type
- [ ] Safe handle patterns
- [ ] Null pointer handling

### 4.3 Callbacks
```elm
foreign import register_callback : (Int -> ()) -> Task ()
```
- [ ] Generate C function pointers from Elm functions
- [ ] Handle callback invocation

---

## Phase 5: Practical Additions (MEDIUM PRIORITY)

### 5.1 Fixed-Size Integers
For embedded/protocol work:
- [ ] Int8, Int16, Int32, Int64
- [ ] UInt8, UInt16, UInt32, UInt64
- [ ] Bit operations: and, or, xor, shiftLeft, shiftRight
- [ ] Overflow behavior (wrap vs error)

### 5.2 JSON Serialization
For debugging, config, API communication:
- [ ] Json.Encode basics
- [ ] Json.Decode basics
- [ ] Decode.field, Decode.map, Decode.andThen
- [ ] Auto-derive for simple records (optional)

### 5.3 Binary Serialization
For efficient storage/network:
- [ ] Bytes type
- [ ] Encode.int32, Encode.float64, etc.
- [ ] Decode.int32, Decode.float64, etc.
- [ ] Endianness control

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

## NOT Planned (Exotic Features)

These features are intellectually interesting but add complexity without proportional benefit for the NUC + Ledger use case:

| Feature | Why Not Needed |
|---------|----------------|
| Type Classes | Use module-qualified functions: `Money.add`, `Account.compare` |
| Higher-Kinded Types | Write specific implementations for each type |
| GADTs | Runtime validation works fine for ledger invariants |
| Monad Transformers | Thread state explicitly or use records |
| Refinement Types | Runtime checks: `validatePositive amount` |
| Existential Types | Simple union types for events |
| Full Lenses | Verbose nested update syntax is acceptable |
| Lazy Evaluation | Not needed for embedded real-time |
| Custom Operators | Standard operators are sufficient |

If these become truly necessary later, they can be reconsidered. But the ledger can be built without them.

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

**Milestone 1**: Can compile and run:
```elm
main =
    [ 1, 2, 3 ]
        |> List.map (\x -> x * 2)
        |> List.filter (\x -> x > 2)
        |> List.sum
```

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
