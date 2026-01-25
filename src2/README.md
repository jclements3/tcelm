# tcelm2 - Elm to C Compiler (v2 Architecture)

A clean-room rewrite of the tcelm compiler with proper type inference and modular design.

## Quick Start

```bash
# Compile the compiler
elm make src2/Compiler.elm --output=bin/tcelm2.js

# Compile an Elm file
./bin/tcelm2 source.elm              # Output to stdout
./bin/tcelm2 source.elm -o out.c     # Output to file

# Run with TCC
./bin/tcelm2 source.elm -o /tmp/out.c && tcc -run /tmp/out.c
```

## Architecture

```
Source -> Lexer -> Parser -> AST -> Type Inference -> Core IR -> C Code
```

### Modules

| Module | Purpose |
|--------|---------|
| `Lexer.elm` | Tokenizer with indentation tracking |
| `Parser.elm` | Recursive descent parser |
| `AST.elm` | Source-level AST with locations |
| `Types.elm` | Type system (Type, Scheme, Kind) |
| `Infer.elm` | Hindley-Milner type inference |
| `Core.elm` | Core IR (explicitly typed) |
| `Desugar.elm` | AST to Core transformation |
| `Codegen/C.elm` | C code generation |
| `Compiler.elm` | Main entry point |

## Supported Features

### Expressions
- Integer literals and arithmetic (`+`, `-`, `*`, `/`, `//`, `^`)
- Comparison operators (`==`, `/=`, `<`, `>`, `<=`, `>=`)
- Boolean operators (`&&`, `||`, `not`)
- Let expressions (multi-line, multiple bindings)
- If expressions (multi-line)
- Case expressions with pattern matching
- Lambda expressions with closure capture
- Function application and pipelines (`|>`, `<|`)

### Types
- Basic types: `Int`, `Float`, `String`, `Char`, `Bool`
- `Maybe a` with `Just` and `Nothing`
- `Result e a` with `Ok` and `Err`
- Lists with `[]`, `::`, and list literals
- Tuples (2 and 3 elements)
- Records with field access
- Custom types with constructors
- Type aliases

### Patterns
- Variable binding
- Wildcards (`_`)
- Literal matching (Int, String, Char)
- Constructor patterns (`Just x`, `Ok value`)
- List patterns (`[]`, `x :: xs`)
- Tuple patterns (`(a, b)`)
- Record patterns (`{ name, age }`)

### Do-Notation
```elm
result = do
    x <- validate input1
    y <- validate input2
    Ok (x + y)
```

Automatically detects monad type from constructor usage:
- `Just`/`Nothing` → `Maybe.andThen`
- `Ok`/`Err` → `Result.andThen`

### Standard Library

Functions available in the C runtime:

**Basics**: `identity`, `always`, `flip`, `min`, `max`, `clamp`, `abs`

**List**: `isEmpty`, `length`, `reverse`, `member`, `head`, `tail`, `take`, `drop`, `sum`, `product`, `maximum`, `minimum`, `append`, `concat`, `intersperse`, `range`, `repeat`, `map`, `filter`, `filterMap`, `foldl`, `foldr`, `any`, `all`, `concatMap`, `indexedMap`

**Maybe**: `withDefault`, `map`, `andThen`, `map2`

**Result**: `withDefault`, `map`, `mapError`, `andThen`, `toMaybe`

**String**: `length`, `isEmpty`, `reverse`, `concat`, `append`, `join`, `left`, `right`, `dropLeft`, `dropRight`, `contains`, `startsWith`, `endsWith`, `fromInt`, `toInt`

**Tuple**: `pair`, `first`, `second`, `mapFirst`, `mapSecond`

## Example

```elm
module Main exposing (main)

type alias Person =
    { name : String
    , age : Int
    }

validate : Int -> Result String Int
validate n =
    if n > 0 then Ok n
    else Err "must be positive"

main : Int
main =
    let
        numbers = [1, 2, 3, 4, 5]
        doubled = List.map (\x -> x * 2) numbers
        result =
            case do
                x <- validate 10
                y <- validate 20
                Ok (x + y)
            of
                Ok n -> n
                Err _ -> 0
    in
    List.sum doubled + result
```

## Testing

```bash
# Run all tests
./tests/tcelm2/run_all.sh
```

## Known Limitations

- No type class instances (infrastructure ready)
- No as-patterns in some contexts
- Tuples limited to 3 elements
- No module imports (single-file only)
