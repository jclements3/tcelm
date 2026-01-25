# tcelm TODO - Master Plan

## Current Progress

**Last Updated**: 2026-01-24
**Last Session**: Completed Phase 1.1 - Extracted all target code generation to Target modules
**Next Action**: Start Phase 1.2 - Expand Codegen.Pattern

### Session Log
- [x] 2025-01-24: Created TODO.md with full roadmap
- [x] 2025-01-24: Analyzed Cli.elm structure (6,353 lines, 168 case statements)
- [x] 2025-01-24: Documented Codegen/ module responsibilities
- [x] 2026-01-24: Extracted generateRtemsCode to Target/RTEMS.elm (~1600 lines)
  - Created CodegenConfig type for callback injection pattern
  - Moved all RTEMS preambles, runtime code, helpers
  - Cli.elm now calls RTEMS.generateCode via config
- [x] 2026-01-24: Extracted generateNativeCode to Target/Native.elm (~100 lines)
  - Uses simpler CodegenConfig with forward decl and function callbacks
  - Cli.elm now calls Native.generateCode via config
- [x] 2026-01-24: Expanded Target/TCC.elm with full TCC code generation (~1350 lines)
  - Added CodegenConfig with 7 callbacks for shared functions
  - Moved generateTccCode, generateTccLibCode, generateTccHeader
  - Cli.elm now calls TCC.generateCode/generateLibCode/generateHeader via config
- [ ] **NEXT**: Continue Phase 1.2 - Expand Codegen.Pattern

---

## Current State Summary

| Component | Status | Notes |
|-----------|--------|-------|
| Parser | ~90% | Most Elm syntax parsed |
| Cli.elm | 6,353 lines | Needs refactoring, contains 5 code generators |
| Codegen.Shared | Complete | Types, name mangling, utilities |
| Codegen.Expr | Complete | Literals, binops, if/record/tuple/list |
| Codegen.Builtins | ~90% | 100+ built-ins, some lambda-using funcs in Cli |
| Codegen.Pattern | ~60% | Simple patterns only, complex in Cli |
| Codegen.Union | Complete | Custom type definitions |
| Codegen.Lambda | Complete | Lambda lifting types & var collection |
| Generate.C | Complete | Module-based generation |

---

## Phase 1: Refactoring (Current Focus)

Goal: Reduce Cli.elm from 6,353 lines to <2,000 lines by extracting to focused modules.

### 1.1 Extract Target Generators ✓
- [x] Create `Target/RTEMS.elm` - move generateRtemsCode (lines 159-770)
- [x] Create `Target/Native.elm` - move generateNativeCode (lines 775-851)
- [x] Create `Target/TCC.elm` - move generateTccCode, generateTccLibCode, generateTccHeader (lines 858-1620)
- [x] Cli.elm now orchestrates via CodegenConfig callbacks to Target modules

### 1.2 Expand Codegen.Pattern
- [ ] Move generateStandaloneCaseFallback (lines 4725-5100) to Codegen.Pattern
- [ ] Add tuple pattern support
- [ ] Add constructor pattern support (union types)
- [ ] Add nested pattern support
- [ ] Add as-pattern support (@)
- [ ] Add list cons pattern support (x :: xs)

### 1.3 Extract Type Inference
- [ ] Create `Codegen/TypeInference.elm`
- [ ] Move parameter type detection from generateUserFunction (lines 3044-3400)
- [ ] Create predicates: isUnionType, isListType, isStringType, isRecordType
- [ ] Replace string-based type detection with proper type tracking

### 1.4 Clean Up Expression Generation
- [ ] Move remaining expression cases from generateStandaloneExprWithCtxImpl to Codegen.Expr
- [ ] Reduce the 168-case statement to dispatcher only

### 1.5 Lambda/Local Function Cleanup
- [ ] Move collectLocalFunctionsWithScope to Codegen.Lambda
- [ ] Move generateLiftedFunction to Codegen.Lambda
- [ ] Move record lambda extraction to Codegen.Lambda

---

## Phase 2: Bootstrap (Self-Hosting Blockers)

Goal: tcelm can compile itself to C for RTEMS.

### 2.1 Lambda/Closure C Generation (HIGH PRIORITY)
Current: Lambda lifting works but closure capture is fragile.
- [ ] Implement proper closure struct in C runtime
- [ ] Generate closure creation code (allocate, capture vars)
- [ ] Generate closure invocation code (extract env, call)
- [ ] Test with parser combinator patterns (P.andThen)

### 2.2 Pipeline Operators (HIGH PRIORITY)
Current: |> and <| not directly supported.
- [ ] Add |> desugaring in parser or early transform: `a |> f` → `f a`
- [ ] Add <| desugaring: `f <| a` → `f a`
- [ ] Verify << and >> composition works (partially implemented)

### 2.3 Partial Application / Currying (HIGH PRIORITY)
Current: Not implemented.
- [ ] Detect partial application sites
- [ ] Generate closure for partially applied functions
- [ ] Handle curried function definitions

### 2.4 Standard Library C Implementations (HIGH PRIORITY)

#### String module
- [ ] String.fromInt
- [ ] String.fromFloat
- [ ] String.fromChar
- [ ] String.concat
- [ ] String.join
- [ ] String.toList
- [ ] String.slice
- [ ] String.dropLeft
- [ ] String.uncons
- [ ] String.toUpper (partial)

#### List module
- [ ] List.map (needs closures)
- [ ] List.concat
- [ ] List.filterMap
- [ ] List.indexedMap
- [ ] List.partition
- [ ] List.isEmpty
- [ ] List.map2
- [ ] List.repeat

#### Maybe module
- [ ] Maybe.map
- [ ] Maybe.withDefault (partial)
- [ ] Maybe.andThen

#### Char module
- [ ] Char.toCode
- [ ] Char.isAlphaNum (partial)

### 2.5 Pattern Matching Gaps
- [ ] List cons patterns (x :: xs) in case expressions
- [ ] Nested constructor patterns
- [ ] As-patterns (@)

---

## Phase 3: RTEMS Multi-Core (4-Core NUC)

Goal: Efficient parallel execution on Intel NUC with RTEMS SMP.

### 3.1 Core Primitives
- [ ] Core affinity type and syntax
- [ ] Core ID query function
- [ ] Core count query function

### 3.2 Parallelism Primitives
- [ ] `Par` type for parallel computations
- [ ] `parMap` - parallel map over array
- [ ] `par2`, `par3`, `par4` - parallel independent operations
- [ ] Barrier synchronization

### 3.3 Inter-Core Communication
- [ ] `Channel a` - bounded MPSC/MPMC queues
- [ ] `send`, `receive`, `tryReceive` operations
- [ ] `MVar a` - synchronized mutable variable
- [ ] Atomic variables (Int, Bool)

### 3.4 Pipeline Parallelism
- [ ] Stage type with queues
- [ ] Pipeline composition
- [ ] Backpressure handling

---

## Phase 4: Language Extensions

Goal: Features needed for embedded systems programming.

### 4.1 FFI (C Interop) - CRITICAL
- [ ] `foreign import` syntax for C functions
- [ ] Pointer types (opaque handles)
- [ ] Memory layout control for structs
- [ ] Inline C code blocks

### 4.2 Numeric Types - CRITICAL
- [ ] Fixed-size integers: Int8, Int16, Int32, Int64
- [ ] Unsigned integers: UInt8, UInt16, UInt32, UInt64
- [ ] Bit operations: and, or, xor, shift, rotate
- [ ] Fixed-point arithmetic

### 4.3 Binary Serialization - CRITICAL
- [ ] ByteArray primitive type
- [ ] Pack/unpack functions
- [ ] Endianness control
- [ ] Deriving Binary (auto-generate encode/decode)

### 4.4 Type Classes - HIGH
- [ ] Type class syntax
- [ ] Instance definitions
- [ ] Eq, Ord, Show basics
- [ ] Device abstraction class

### 4.5 Lazy Evaluation / Streams - HIGH
- [ ] Stream type primitive
- [ ] Lazy evaluation for infinite structures
- [ ] Stream fusion optimization

### 4.6 Custom Operators - MEDIUM
- [ ] Operator definition syntax
- [ ] Precedence/associativity
- [ ] Common operators: |>>, <<<, >>>

### 4.7 Do Notation - MEDIUM
- [ ] do-block syntax
- [ ] Desugar to andThen chains

### 4.8 GADTs - LOW
- [ ] Type-safe protocol state machines

---

## Phase 5: Tooling & Quality

### 5.1 Build System
- [ ] Cross-compilation to Intel NUC
- [ ] RTEMS BSP integration
- [ ] Makefile/build script generation

### 5.2 Testing
- [ ] Unit tests for Codegen modules
- [ ] Integration tests (Elm source → C → execution)
- [ ] Bootstrap test (compile tcelm with tcelm)

### 5.3 Debugging
- [ ] Source maps (Elm line → C line)
- [ ] GDB integration hints
- [ ] Stack usage analysis

---

## Phase 6: Digital Twin Development

Goal: Develop in browser, deploy to RTEMS.

### 6.1 Portable Processing Core
- [ ] Define platform-agnostic IRProcessing module
- [ ] Ensure same code runs in browser (Elm) and RTEMS (C)

### 6.2 Browser Development Environment
- [ ] Synthetic IR data generation
- [ ] WebGL visualization
- [ ] Performance profiling

### 6.3 Hardware Verification
- [ ] Record/playback of real sensor data
- [ ] Bit-exact comparison: browser vs RTEMS output

---

## Implementation Order

### Immediate (This Week)
1. Continue Cli.elm refactoring (Phase 1.1-1.2)
2. Document current lambda/closure implementation

### Short Term (Next 2-4 Weeks)
1. Complete Phase 1 refactoring
2. Implement pipeline operator desugaring (Phase 2.2)
3. Start closure implementation (Phase 2.1)

### Medium Term (1-2 Months)
1. Complete Phase 2 bootstrap blockers
2. Attempt first self-hosting compile
3. Fix issues discovered during bootstrap

### Long Term (3+ Months)
1. Phase 3 multi-core support
2. Phase 4 language extensions
3. Phase 5 tooling
4. Phase 6 digital twin

---

## Notes

- **Bootstrap is the priority** - Extensions are worthless if basic Elm doesn't compile
- **Refactoring enables progress** - Smaller modules are easier to understand and fix
- **Test incrementally** - Each feature should be tested before moving on
- **Document as you go** - Update this file and CLAUDE.md with learnings

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
| BOOTSTRAP_AUDIT.md | Feature gap analysis | 202 |
