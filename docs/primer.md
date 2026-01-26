# tcelm Primer

A quick reference for programming in tcelm - an Elm-to-C compiler for embedded systems.

## What is tcelm?

tcelm compiles Elm code to standalone C executables. It's self-hosting (the compiler is written in Elm and compiles itself). Target platforms include native (via TCC) and RTEMS for embedded/real-time systems.

## Quick Start

```bash
# Compile and run
./bin/tcelm myfile.elm
./myfile

# Or specify output
./bin/tcelm myfile.elm -o myprogram
```

## Basic Syntax

### Module Declaration

Every file starts with a module declaration:

```elm
module MyModule exposing (main)
```

### Entry Point

Programs need a `main` value:

```elm
module Test exposing (main)

main = 42
```

### Types

```elm
-- Primitives
x : Int
x = 42

y : Float
y = 3.14

s : String
s = "hello"

c : Char
c = 'a'

b : Bool
b = True
```

### Functions

```elm
-- Named function
add : Int -> Int -> Int
add x y = x + y

-- Anonymous function (lambda)
double = \x -> x * 2

-- Multi-argument lambda
multiply = \x y -> x * y
```

### Let Expressions

```elm
result =
    let
        a = 10
        b = 20
        sum = a + b
    in
    sum * 2
```

### If-Then-Else

```elm
absolute x =
    if x < 0 then
        -x
    else
        x

grade score =
    if score >= 90 then "A"
    else if score >= 80 then "B"
    else if score >= 70 then "C"
    else "F"
```

### Lists

```elm
numbers : List Int
numbers = [1, 2, 3, 4, 5]

-- Cons operator
moreNumbers = 0 :: numbers  -- [0, 1, 2, 3, 4, 5]

-- Common operations
sum = List.sum numbers           -- 15
doubled = List.map (\x -> x * 2) numbers  -- [2, 4, 6, 8, 10]
evens = List.filter (\x -> modBy 2 x == 0) numbers  -- [2, 4]
total = List.foldl (+) 0 numbers  -- 15
```

### Tuples

```elm
pair : (Int, String)
pair = (42, "answer")

-- Destructuring
(num, str) = pair

-- Access
first = Tuple.first pair   -- 42
second = Tuple.second pair -- "answer"

-- Triple
triple : (Int, Int, Int)
triple = (1, 2, 3)
```

### Records

```elm
type alias Person =
    { name : String
    , age : Int
    }

alice : Person
alice = { name = "Alice", age = 30 }

-- Access
aliceName = alice.name

-- Update (immutable - creates new record)
olderAlice = { alice | age = 31 }

-- Function that accesses field
getName : Person -> String
getName person = person.name

-- Or using accessor shorthand
getName2 = .name
```

### Custom Types (Algebraic Data Types)

```elm
-- Enumeration
type Color = Red | Green | Blue

-- With data
type Shape
    = Circle Float
    | Rectangle Float Float

-- Recursive type
type Tree
    = Leaf Int
    | Node Tree Tree
```

### Pattern Matching (case)

```elm
-- On custom types
describe : Shape -> String
describe shape =
    case shape of
        Circle r ->
            "Circle with radius " ++ String.fromFloat r
        Rectangle w h ->
            "Rectangle " ++ String.fromFloat w ++ "x" ++ String.fromFloat h

-- On Maybe
withDefault : a -> Maybe a -> a
withDefault default maybe =
    case maybe of
        Just value -> value
        Nothing -> default

-- On List
head : List a -> Maybe a
head list =
    case list of
        x :: _ -> Just x
        [] -> Nothing

-- Nested patterns
nestedExample : Maybe (Maybe Int) -> Int
nestedExample outer =
    case outer of
        Just (Just n) -> n
        Just Nothing -> -1
        Nothing -> -2

-- Wildcard
isZero : Int -> Bool
isZero n =
    case n of
        0 -> True
        _ -> False
```

### Maybe Type

```elm
-- Maybe a = Just a | Nothing

safeDivide : Int -> Int -> Maybe Int
safeDivide a b =
    if b == 0 then Nothing else Just (a // b)

-- Chain with andThen
result =
    Just 10
        |> Maybe.andThen (\x -> safeDivide x 2)
        |> Maybe.andThen (\x -> safeDivide x 1)

-- Map over Maybe
doubled = Maybe.map (\x -> x * 2) (Just 5)  -- Just 10

-- Combine with map2
sum = Maybe.map2 (+) (Just 3) (Just 4)  -- Just 7

-- Default value
value = Maybe.withDefault 0 (Just 42)  -- 42
value2 = Maybe.withDefault 0 Nothing   -- 0
```

### Result Type

```elm
-- Result e a = Ok a | Err e

parsePositive : String -> Result String Int
parsePositive s =
    case String.toInt s of
        Nothing -> Err "Not a number"
        Just n ->
            if n < 0 then Err "Must be positive"
            else Ok n

-- Chain with andThen
compute : String -> Result String Int
compute input =
    parsePositive input
        |> Result.andThen (\n -> Ok (n * 2))

-- Map
doubled = Result.map (\x -> x * 2) (Ok 5)  -- Ok 10

-- Handle errors
Result.mapError (\e -> "Error: " ++ e) (Err "oops")
```

### Do-Notation

tcelm extends Elm with Haskell-style do-notation for Maybe and Result:

```elm
-- Maybe
computeMaybe : Maybe Int
computeMaybe = do
    x <- Just 10
    y <- Just 20
    z <- safeDivide (x + y) 3
    Just (z * 2)

-- Result
validateUser : String -> Int -> Result String (String, Int)
validateUser name age = do
    validName <- if String.isEmpty name then Err "Name required" else Ok name
    validAge <- if age < 0 then Err "Invalid age" else Ok age
    Ok (validName, validAge)

-- Let in do blocks
withLet = do
    x <- Just 5
    let doubled = x * 2
    y <- Just (doubled + 3)
    Just (x + y)
```

### Pipelines and Composition

```elm
-- Forward pipe: x |> f = f x
result =
    [1, 2, 3, 4, 5]
        |> List.map (\x -> x * 2)
        |> List.filter (\x -> x > 5)
        |> List.sum

-- Backward pipe: f <| x = f x
result2 = List.sum <| List.map (\x -> x * 2) <| [1, 2, 3]

-- Forward composition: f >> g = \x -> g (f x)
addOneThenDouble = (\x -> x + 1) >> (\x -> x * 2)
result3 = addOneThenDouble 5  -- 12

-- Backward composition: f << g = \x -> f (g x)
doubleThenAddOne = (\x -> x + 1) << (\x -> x * 2)
result4 = doubleThenAddOne 5  -- 11
```

### Operators

```elm
-- Arithmetic
(+) (-) (*) (/) (//)  -- // is integer division

-- Comparison
(==) (/=) (<) (>) (<=) (>=)

-- Boolean
(&&) (||) not

-- List/String concatenation
(++)

-- Modulo
modBy 3 10  -- 1

-- Power
2 ^ 10  -- 1024
```

### String Operations

```elm
s = "Hello, World!"

String.length s        -- 13
String.toUpper s       -- "HELLO, WORLD!"
String.toLower s       -- "hello, world!"
String.trim "  hi  "   -- "hi"
String.split "," s     -- ["Hello", " World!"]
String.join "-" ["a","b","c"]  -- "a-b-c"
String.slice 0 5 s     -- "Hello"
String.contains "World" s  -- True
String.fromInt 42      -- "42"
String.toInt "42"      -- Just 42
String.fromFloat 3.14  -- "3.14"
```

### Dict (Dictionary/Map)

```elm
import Dict

-- Create
empty = Dict.empty
single = Dict.singleton "key" 42
fromList = Dict.fromList [("a", 1), ("b", 2)]

-- Operations
Dict.insert "c" 3 fromList
Dict.get "a" fromList      -- Just 1
Dict.get "z" fromList      -- Nothing
Dict.remove "a" fromList
Dict.member "a" fromList   -- True
Dict.size fromList         -- 2
Dict.keys fromList         -- ["a", "b"]
Dict.values fromList       -- [1, 2]
```

### Set

```elm
import Set

-- Create
empty = Set.empty
single = Set.singleton 42
fromList = Set.fromList [1, 2, 3, 2, 1]  -- {1, 2, 3}

-- Operations
Set.insert 4 fromList
Set.remove 1 fromList
Set.member 2 fromList  -- True
Set.size fromList      -- 3
Set.toList fromList    -- [1, 2, 3]
Set.union set1 set2
Set.intersect set1 set2
Set.diff set1 set2
```

## Type Annotations

Type annotations are optional but recommended:

```elm
-- Function type
add : Int -> Int -> Int
add x y = x + y

-- Polymorphic type
identity : a -> a
identity x = x

-- Constrained types
sum : List number -> number
sum = List.foldl (+) 0

sort : List comparable -> List comparable
sort = List.sort
```

## Common Patterns

### The Elm Architecture (TEA)

For RTEMS applications:

```elm
module App exposing (main)

import Rtems

type alias Model = { count : Int }

type Msg = Increment | Decrement

init : Model
init = { count = 0 }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Increment -> ({ model | count = model.count + 1 }, Cmd.none)
        Decrement -> ({ model | count = model.count - 1 }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

main =
    Rtems.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
```

### Recursive Functions

```elm
factorial : Int -> Int
factorial n =
    if n <= 1 then 1
    else n * factorial (n - 1)

-- Tail recursive with accumulator
factorialTail : Int -> Int
factorialTail n =
    let
        go acc n =
            if n <= 1 then acc
            else go (acc * n) (n - 1)
    in
    go 1 n
```

### Higher-Order Functions

```elm
-- Function returning function
makeAdder : Int -> (Int -> Int)
makeAdder n = \x -> x + n

addFive = makeAdder 5
result = addFive 10  -- 15

-- Partial application
add : Int -> Int -> Int
add x y = x + y

addTen = add 10
result = addTen 5  -- 15
```

## Key Differences from Standard Elm

1. **Do-notation** - tcelm adds Haskell-style do-notation
2. **Compiles to C** - Standalone executables, no JavaScript
3. **RTEMS support** - Real-time embedded systems
4. **No ports/JS interop** - Native C FFI instead

## Quick Reference Card

| Concept | Syntax |
|---------|--------|
| Module | `module Name exposing (main)` |
| Function | `add x y = x + y` |
| Lambda | `\x -> x + 1` |
| Let | `let x = 1 in x + 1` |
| If | `if cond then a else b` |
| Case | `case x of Pat -> expr` |
| Record | `{ field = value }` |
| Record update | `{ record \| field = newValue }` |
| Type alias | `type alias Name = ...` |
| Custom type | `type Name = A \| B Int` |
| List | `[1, 2, 3]` or `1 :: [2, 3]` |
| Tuple | `(a, b)` or `(a, b, c)` |
| Pipe forward | `x \|> f \|> g` |
| Compose | `f >> g` or `f << g` |
| Do-notation | `do { x <- ma; ... }` |
