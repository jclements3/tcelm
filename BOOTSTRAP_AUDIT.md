# tcelm Bootstrap Audit Report

**Objective**: Self-hosting - tcelm compiles tcelm to C for RTEMS

## 1. Elm Features Used by Compiler Source (src/*.elm)

### Basic Types
| Feature | Used In | Parser | C Gen | Runtime |
|---------|---------|--------|-------|---------|
| Int | Everywhere | ✅ | ✅ | ✅ |
| Float | Number parsing | ✅ | ✅ | ✅ |
| String | Everywhere | ✅ | ✅ | ✅ |
| Bool | Conditions | ✅ | ✅ | ✅ |
| Char | Character parsing | ✅ | ✅ | ✅ |
| Unit () | Empty records/tuples | ✅ | ✅ | ✅ |

### Collections
| Feature | Used In | Parser | C Gen | Runtime |
|---------|---------|--------|-------|---------|
| List | Everywhere | ✅ | ✅ | ✅ |
| 2-Tuple | State, return values | ✅ | ✅ | ✅ |
| 3-Tuple | Position tracking | ✅ | ✅ | ✅ |
| Records | Module, State, etc. | ✅ | ✅ | ✅ |

### Control Flow
| Feature | Used In | Parser | C Gen | Runtime |
|---------|---------|--------|-------|---------|
| case expressions | Pattern matching | ✅ | ⚠️ Partial | ⚠️ |
| if/then/else | Conditions | ✅ | ✅ | ✅ |
| let bindings | Local defs | ✅ | ✅ | N/A |

### Functions
| Feature | Used In | Parser | C Gen | Runtime |
|---------|---------|--------|-------|---------|
| Function definitions | Everywhere | ✅ | ✅ | N/A |
| Lambda (\x -> ...) | Callbacks | ✅ | ❌ Stub | ❌ |
| Multi-arg functions | Everywhere | ✅ | ✅ | ✅ |
| Partial application | Callbacks | ✅ Parse | ❌ | ❌ |

### Custom Types (Union Types)
| Feature | Used In | Parser | C Gen | Runtime |
|---------|---------|--------|-------|---------|
| Simple constructors | Parser, PStep, etc. | ✅ | ✅ | ✅ |
| Constructors with args | Located, Expr_, etc. | ✅ | ✅ | ✅ |
| Maybe | Optional values | ✅ | ✅ | ✅ |
| Result | Parse results | ✅ | ✅ | ✅ |

### Pattern Matching
| Feature | Used In | Parser | C Gen | Runtime |
|---------|---------|--------|-------|---------|
| Variable patterns | Everywhere | ✅ | ✅ | ✅ |
| Wildcard _ | Ignored values | ✅ | ✅ | ✅ |
| Constructor patterns | case on custom types | ✅ | ⚠️ | ⚠️ |
| Tuple patterns | Destructuring | ✅ | ✅ | ✅ |
| List patterns (::) | List processing | ✅ | ❌ | ❌ |
| Record patterns | Destructuring | ✅ | ✅ | ✅ |
| Literal patterns | Int, String matching | ✅ | ✅ | ✅ |
| As patterns | Aliasing | ✅ | ⚠️ | N/A |

### Operators
| Operator | Used In | Parser | C Gen | Runtime |
|----------|---------|--------|-------|---------|
| + - * / // | Arithmetic | ✅ | ✅ | ✅ |
| == /= < > <= >= | Comparison | ✅ | ✅ | ✅ |
| && \|\| | Logic | ✅ | ✅ | ✅ |
| :: | List cons | ✅ | ✅ | ✅ |
| ++ | String/List append | ✅ | ✅ | ✅ |
| \|> | Pipeline | ✅ | ❌ | N/A |
| <\| | Reverse pipeline | ✅ | ❌ | N/A |
| << >> | Composition | ✅ | ❌ | N/A |

### Module System
| Feature | Used In | Parser | C Gen | Runtime |
|---------|---------|--------|-------|---------|
| module declaration | All files | ✅ | N/A | N/A |
| import | All files | ✅ | N/A | N/A |
| import as alias | Some files | ✅ | N/A | N/A |
| exposing | All files | ✅ | N/A | N/A |
| port module | Cli.elm | ✅ | N/A | N/A |
| Qualified names | Module.func | ✅ | ⚠️ | N/A |

### Standard Library Functions Used

#### String module (heavily used)
- `String.fromInt` - ❌ Need C implementation
- `String.fromFloat` - ❌ Need C implementation
- `String.fromChar` - ❌ Need C implementation
- `String.concat` - ❌ Need C implementation
- `String.join` - ❌ Need C implementation
- `String.toList` - ❌ Need C implementation
- `String.slice` - ❌ Need C implementation
- `String.length` - ⚠️ Partial
- `String.dropLeft` - ❌ Need C implementation
- `String.uncons` - ❌ Need C implementation
- `String.toUpper` - ❌ Need C implementation

#### Char module
- `Char.toCode` - ❌ Need C implementation
- `Char.isAlphaNum` - ❌ Need C implementation

#### List module (heavily used)
- `List.map` - ❌ Need C implementation
- `List.reverse` - ✅ In runtime
- `List.concat` - ❌ Need C implementation
- `List.head` - ✅ In runtime
- `List.filterMap` - ❌ Need C implementation
- `List.indexedMap` - ❌ Need C implementation
- `List.partition` - ❌ Need C implementation
- `List.isEmpty` - ❌ Need C implementation
- `List.length` - ✅ In runtime
- `List.map2` - ❌ Need C implementation
- `List.repeat` - ❌ Need C implementation

#### Maybe module
- `Maybe.map` - ❌ Need C implementation
- `Maybe.withDefault` - ❌ Need C implementation
- `Maybe.andThen` - ❌ Need C implementation

## 2. Critical Gaps for Self-Hosting

### HIGH PRIORITY - Blocking Bootstrap

1. **Lambda expressions** - Parser ✅, C Gen ❌
   - Location: `Generate/C.elm:374-378` returns stub
   - Used extensively in parser combinators with `P.andThen`

2. **Pipeline operators |> <|** - Parser ✅, C Gen ❌
   - Used throughout codebase for chaining
   - Need to desugar to function application

3. **Function composition << >>** - Parser ✅, C Gen ❌
   - Used in some utility functions

4. **Partial application / Currying** - Parser ✅, C Gen ❌
   - Fundamental to Elm
   - Need closure support in runtime

5. **Standard library functions** - Need C implementations
   - String.*, List.*, Maybe.*, Char.*
   - These are used extensively

### MEDIUM PRIORITY - May be Avoidable

1. **Debug.todo** in parser (lines 730, 736)
   - Used for lineComment and multiComment errors
   - Need proper error types

2. **Qualified name code generation**
   - Works partially, may need refinement

### LOW PRIORITY - Nice to Have

1. **Type checking** - Not enforced currently
2. **Better error messages**

## 3. Recommended Fix Order

### Phase 1: Core Language Features
1. Fix lambda expression C generation
2. Implement pipeline operators as desugaring
3. Implement function composition as desugaring
4. Add closure/partial application support to runtime

### Phase 2: Standard Library
1. Implement String module in C
2. Implement List module in C
3. Implement Maybe module in C
4. Implement Char module in C

### Phase 3: Bootstrap Test
1. Start with minimal subset (AST.Source.elm)
2. Add parser modules one at a time
3. Test each module compiles and runs
4. Build up to full compiler

## 4. Files to Modify

### Generate/C.elm
- `generateLambda` - implement closure creation
- `generateBinop` - add |>, <|, <<, >>
- Add standard library function generation

### runtime/
- `tcelm_types.h` - add closure type
- `tcelm_types.c` - implement closures
- `tcelm_string.c` - new file for String.*
- `tcelm_list.c` - new file for List.*
- `tcelm_maybe.c` - new file for Maybe.*

### Parse/Primitives.elm
- Lines 730, 736 - fix Debug.todo

## 5. Current State Summary

```
Parser:           ~90% complete for bootstrap
C Code Generator: ~60% complete for bootstrap
Runtime:          ~50% complete for bootstrap
```

**Main blockers**: Lambda/closure support, standard library functions
