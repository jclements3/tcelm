# 30 Functional Programming Patterns in tcelm

A comprehensive guide to functional programming patterns implemented in tcelm, a self-hosting Elm-to-C compiler for embedded systems.

**tcelm compiles itself.** The compiler is written in Elm and compiles to C, demonstrating full language coverage including all patterns documented here.

---

## Core 8 Patterns

### 1. Functor - `map`

**Action:** Transform wrapped values

```elm
-- tcelm: Functor via Maybe.map, List.map, Result.map
module Main exposing (main)

import Maybe

double : Int -> Int
double x = x * 2

-- Map over Maybe
justFive : Maybe Int
justFive = Just 5

result : Maybe Int
result = Maybe.map double justFive  -- Just 10

-- Map over List
numbers : List Int
numbers = [1, 2, 3]

doubled : List Int
doubled = List.map double numbers  -- [2, 4, 6]

main =
    case result of
        Just n -> n
        Nothing -> 0
```

---

### 2. Applicative - `apply`

**Action:** Apply wrapped function to wrapped value

```elm
-- tcelm: Applicative via Maybe.map2/3/4/5, Result.map2/3
module Main exposing (main)

-- Full applicative support via mapN functions
add : Int -> Int -> Int
add x y = x + y

maybeX : Maybe Int
maybeX = Just 5

maybeY : Maybe Int
maybeY = Just 3

-- Combine two Maybes
result : Maybe Int
result = Maybe.map2 add maybeX maybeY  -- Just 8

-- With one Nothing
resultNone : Maybe Int
resultNone = Maybe.map2 add maybeX Nothing  -- Nothing

main =
    case result of
        Just n -> n
        Nothing -> 0
```

---

### 3. Monad - `flatMap`

**Action:** Chain operations returning wrapped values

```elm
-- tcelm: Monad via andThen (flatMap) + do-notation
module Main exposing (main)

safeDivide : Int -> Int -> Maybe Int
safeDivide x y =
    if y == 0 then
        Nothing
    else
        Just (x // y)

-- Chain Maybe operations with pipeline
result : Maybe Int
result =
    Just 10
        |> Maybe.andThen (\x -> safeDivide x 2)
        |> Maybe.andThen (\x -> safeDivide x 1)
-- result = Just 5

-- Use do-notation for cleaner code
resultDo : Maybe Int
resultDo = do
    x <- Just 10
    y <- safeDivide x 2
    z <- safeDivide y 1
    Just z
-- resultDo = Just 5

-- Chain with failure
resultFail : Maybe Int
resultFail =
    Just 10
        |> Maybe.andThen (\x -> safeDivide x 0)  -- Nothing here
        |> Maybe.andThen (\x -> safeDivide x 2)  -- Never reached
-- resultFail = Nothing

main =
    case result of
        Just n -> n
        Nothing -> -1
```

---

### 4. Monoid - `combine + identity`

**Action:** Smash together with neutral element

```elm
-- tcelm: Monoid via explicit combine functions
module Main exposing (main)

-- String monoid
stringCombine : String -> String -> String
stringCombine a b = a ++ b

stringEmpty : String
stringEmpty = ""

-- Sum monoid
sumCombine : Int -> Int -> Int
sumCombine a b = a + b

sumEmpty : Int
sumEmpty = 0

-- List monoid
listCombine : List a -> List a -> List a
listCombine a b = a ++ b

listEmpty : List a
listEmpty = []

-- Example usage
greeting : String
greeting = stringCombine "hello" (stringCombine " " "world")
-- "hello world"

total : Int
total = List.foldl sumCombine sumEmpty [1, 2, 3, 4]
-- 10

main = total
```

---

### 5. Semigroup - `combine`

**Action:** Smash together without neutral element

```elm
-- tcelm: Semigroup via explicit combine functions
module Main exposing (main)

-- Max semigroup (no identity for finite types)
maxCombine : Int -> Int -> Int
maxCombine a b =
    if a > b then a else b

-- Min semigroup
minCombine : Int -> Int -> Int
minCombine a b =
    if a < b then a else b

-- First semigroup (keep first value)
firstCombine : a -> a -> a
firstCombine a _ = a

-- Example
numbers : List Int
numbers = [3, 1, 4, 1, 5, 9, 2, 6]

maxVal : Int
maxVal =
    case numbers of
        [] -> 0
        x :: xs -> List.foldl maxCombine x xs
-- 9

main = maxVal
```

---

### 6. Foldable - `reduce`

**Action:** Collapse structure to single value

```elm
-- tcelm: Foldable via List.foldl and List.foldr
module Main exposing (main)

numbers : List Int
numbers = [1, 2, 3, 4]

-- Sum using foldl
sum : Int
sum = List.foldl (+) 0 numbers  -- 10

-- Product using foldl
product : Int
product = List.foldl (*) 1 numbers  -- 24

-- Build string from list
toString : List Int -> String
toString nums =
    List.foldl (\n acc -> acc ++ String.fromInt n ++ ", ") "" nums

-- foldr for right-to-left accumulation
reversed : List Int
reversed = List.foldr (::) [] numbers  -- [1, 2, 3, 4] (same, :: is right-associative)

main = sum
```

---

### 7. Traversable - `map + flip wrappers`

**Action:** Map with effects, flip nesting

```elm
-- tcelm: Traversable via custom traverse functions
module Main exposing (main)

-- Custom traverse for List with Maybe
traverseMaybe : (a -> Maybe b) -> List a -> Maybe (List b)
traverseMaybe f list =
    let
        step item acc =
            case acc of
                Nothing -> Nothing
                Just collected ->
                    case f item of
                        Nothing -> Nothing
                        Just val -> Just (collected ++ [val])
    in
    List.foldl step (Just []) list

-- Example: parse list of strings to ints
parseAll : List String -> Maybe (List Int)
parseAll = traverseMaybe String.toInt

result1 : Maybe (List Int)
result1 = parseAll ["1", "2", "3"]  -- Just [1, 2, 3]

result2 : Maybe (List Int)
result2 = parseAll ["1", "abc", "3"]  -- Nothing

-- Alternative: List.filterMap (drops failures instead of failing)
parsed : List Int
parsed = List.filterMap String.toInt ["1", "abc", "3"]  -- [1, 3]

main =
    case result1 of
        Just nums -> List.sum nums
        Nothing -> -1
```

---

### 8. Contravariant - `map inputs (backwards)`

**Action:** Map over inputs instead of outputs

```elm
-- tcelm: Contravariant via function composition on input
module Main exposing (main)

-- A predicate is just a function to Bool
type alias Predicate a = a -> Bool

-- Contramap for predicates
contramap : (b -> a) -> Predicate a -> Predicate b
contramap f pred =
    \b -> pred (f b)

-- Example
isEven : Predicate Int
isEven n = modBy 2 n == 0

-- Check if string length is even
isEvenLength : Predicate String
isEvenLength = contramap String.length isEven

test1 : Bool
test1 = isEvenLength "hello"  -- False (len=5)

test2 : Bool
test2 = isEvenLength "hi"  -- True (len=2)

-- Sorting with key is contravariant!
-- sortBy uses contramap implicitly
type alias User = { name : String, age : Int }

users : List User
users =
    [ { name = "Alice", age = 30 }
    , { name = "Bob", age = 25 }
    ]

sortedByAge : List User
sortedByAge = List.sortBy .age users

main = if test2 then 1 else 0
```

---

## Common Patterns (9-17)

### 9. Bifunctor - `map both sides`

**Action:** Map left or right side

```elm
-- tcelm: Bifunctor via Result.map and Result.mapError
module Main exposing (main)

-- Result is a bifunctor
bimap : (e -> f) -> (a -> b) -> Result e a -> Result f b
bimap mapError mapOk result =
    case result of
        Ok a -> Ok (mapOk a)
        Err e -> Err (mapError e)

-- Or use built-in functions
example1 : Result String Int
example1 = Ok 5
    |> Result.map (\x -> x * 2)
    |> Result.mapError (\e -> "Error: " ++ e)
-- Ok 10

example2 : Result String Int
example2 = Err "failed"
    |> Result.map (\x -> x * 2)
    |> Result.mapError (\e -> "Error: " ++ e)
-- Err "Error: failed"

-- Tuple is also a bifunctor
tupleBimap : (a -> c) -> (b -> d) -> (a, b) -> (c, d)
tupleBimap f g (a, b) = (f a, g b)

pair : (Int, String)
pair = tupleBimap (\x -> x + 1) String.toUpper (1, "hello")
-- (2, "HELLO")

main =
    case example1 of
        Ok n -> n
        Err _ -> -1
```

---

### 10. Either - `explicit success/failure`

**Action:** Handle errors without exceptions

```elm
-- tcelm: Either is Result in Elm
module Main exposing (main)

-- Result e a = Ok a | Err e

safeParseInt : String -> Result String Int
safeParseInt s =
    case String.toInt s of
        Just n -> Ok n
        Nothing -> Err ("Cannot parse '" ++ s ++ "' as int")

safeDivide : Int -> Int -> Result String Int
safeDivide x y =
    if y == 0 then
        Err "Division by zero"
    else
        Ok (x // y)

-- Chain Result operations
compute : String -> Result String Int
compute input =
    safeParseInt input
        |> Result.andThen (\n -> safeDivide n 2)
        |> Result.map (\n -> n + 1)

result1 : Result String Int
result1 = compute "42"  -- Ok 22

result2 : Result String Int
result2 = compute "abc"  -- Err "Cannot parse 'abc' as int"

result3 : Result String Int
result3 = compute "0"  -- Ok 1 (0 / 2 = 0, 0 + 1 = 1)

main =
    case result1 of
        Ok n -> n
        Err _ -> -1
```

---

### 11. Lens - `getter + setter`

**Action:** Immutably update nested data

```elm
-- tcelm: Lens patterns via record update syntax
module Main exposing (main)

-- Elm has built-in record update syntax which covers most lens use cases
type alias User =
    { name : String
    , age : Int
    , address : Address
    }

type alias Address =
    { city : String
    , zip : String
    }

-- Getter is just field access
getName : User -> String
getName user = user.name

-- Setter uses record update syntax
setName : String -> User -> User
setName newName user =
    { user | name = newName }

-- Modify combines get and set
modifyAge : (Int -> Int) -> User -> User
modifyAge f user =
    { user | age = f user.age }

-- Nested update
setCity : String -> User -> User
setCity newCity user =
    { user | address = { city = newCity, zip = user.address.zip } }

-- Helper for nested updates
updateAddress : (Address -> Address) -> User -> User
updateAddress f user =
    { user | address = f user.address }

-- Example
alice : User
alice =
    { name = "Alice"
    , age = 30
    , address = { city = "NYC", zip = "10001" }
    }

-- Update nested city
aliceInLA : User
aliceInLA =
    alice
        |> updateAddress (\addr -> { addr | city = "LA" })

main = alice.age
```

---

### 12. Reader - `implicit config`

**Action:** Pass context/dependencies implicitly

```elm
-- tcelm: Reader pattern via explicit config passing
module Main exposing (main)

-- Config record (like Reader's environment)
type alias Config =
    { dbUrl : String
    , cacheUrl : String
    , debug : Bool
    }

-- Functions that need config take it explicitly
getUserById : Config -> Int -> String
getUserById config userId =
    "User " ++ String.fromInt userId ++ " from " ++ config.dbUrl

getUserPosts : Config -> String -> String
getUserPosts config userName =
    "Posts for " ++ userName ++ " from " ++ config.dbUrl

-- Compose operations that share config
getUserWithPosts : Config -> Int -> String
getUserWithPosts config userId =
    let
        user = getUserById config userId
        posts = getUserPosts config user
    in
    user ++ " | " ++ posts

-- Config is threaded through explicitly
config : Config
config =
    { dbUrl = "postgres://localhost"
    , cacheUrl = "redis://localhost"
    , debug = True
    }

result : String
result = getUserWithPosts config 123

-- Alternative: partial application to "bake in" config
getUserByIdConfigured : Int -> String
getUserByIdConfigured = getUserById config

main = 0
```

---

### 13. Writer - `accumulate logs`

**Action:** Carry logs/metadata through computation

```elm
-- tcelm: Writer pattern via tuple (value, logs)
module Main exposing (main)

-- Writer as (value, logs) tuple
type alias Writer a = ( a, List String )

-- Pure value with no logs
pure : a -> Writer a
pure x = ( x, [] )

-- Map over the value
map : (a -> b) -> Writer a -> Writer b
map f ( val, logs ) = ( f val, logs )

-- FlatMap (andThen) for Writer
andThen : (a -> Writer b) -> Writer a -> Writer b
andThen f ( val, logs ) =
    let
        ( newVal, newLogs ) = f val
    in
    ( newVal, logs ++ newLogs )

-- Add a log entry
tell : String -> Writer ()
tell msg = ( (), [ msg ] )

-- Computation with logging
addWithLog : Int -> Int -> Writer Int
addWithLog x y =
    ( x + y, [ "Added " ++ String.fromInt x ++ " + " ++ String.fromInt y ] )

multiplyWithLog : Int -> Int -> Writer Int
multiplyWithLog x y =
    ( x * y, [ "Multiplied " ++ String.fromInt x ++ " * " ++ String.fromInt y ] )

-- Chain operations
computation : Writer Int
computation =
    pure 5
        |> andThen (\x -> addWithLog x 3)
        |> andThen (\x -> multiplyWithLog x 2)
-- ( 16, ["Added 5 + 3", "Multiplied 8 * 2"] )

-- Extract value and logs
resultValue : Int
resultValue = Tuple.first computation  -- 16

resultLogs : List String
resultLogs = Tuple.second computation  -- ["Added 5 + 3", "Multiplied 8 * 2"]

main = resultValue
```

---

### 14. State - `thread state`

**Action:** Thread state through computations

```elm
-- tcelm: State pattern via explicit state threading
module Main exposing (main)

-- State as function: state -> (value, newState)
type alias State s a = s -> ( a, s )

-- Run state computation
runState : State s a -> s -> ( a, s )
runState computation initialState =
    computation initialState

-- Pure value, unchanged state
pure : a -> State s a
pure x = \s -> ( x, s )

-- Get current state
get : State s s
get = \s -> ( s, s )

-- Set new state
put : s -> State s ()
put newState = \_ -> ( (), newState )

-- Modify state
modify : (s -> s) -> State s ()
modify f = \s -> ( (), f s )

-- FlatMap for State
andThen : (a -> State s b) -> State s a -> State s b
andThen f computation =
    \s ->
        let
            ( a, s1 ) = computation s
            ( b, s2 ) = f a s1
        in
        ( b, s2 )

-- Stack operations
type alias Stack = List Int

push : Int -> State Stack ()
push x = modify (\stack -> x :: stack)

pop : State Stack (Maybe Int)
pop =
    \stack ->
        case stack of
            [] -> ( Nothing, [] )
            x :: xs -> ( Just x, xs )

-- Example computation
stackOps : State Stack (Maybe Int)
stackOps =
    pure ()
        |> andThen (\_ -> push 1)
        |> andThen (\_ -> push 2)
        |> andThen (\_ -> push 3)
        |> andThen (\_ -> pop)

result : ( Maybe Int, Stack )
result = runState stackOps []
-- ( Just 3, [2, 1] )

main =
    case Tuple.first result of
        Just n -> n
        Nothing -> -1
```

---

### 15. Kleisli - `compose monadic`

**Action:** Compose functions returning monads

```elm
-- tcelm: Kleisli composition via andThen chains
module Main exposing (main)

-- Kleisli arrow: a -> M b (where M is Maybe, Result, etc.)
-- Composition: (a -> M b) -> (b -> M c) -> (a -> M c)

-- Kleisli composition for Maybe
composeMaybe : (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
composeMaybe f g =
    \a ->
        f a |> Maybe.andThen g

-- Example functions (Kleisli arrows)
safeParseInt : String -> Maybe Int
safeParseInt = String.toInt

safeDivideBy2 : Int -> Maybe Int
safeDivideBy2 n =
    if n == 0 then Nothing else Just (n // 2)

addTen : Int -> Maybe Int
addTen n = Just (n + 10)

-- Compose Kleisli arrows
pipeline : String -> Maybe Int
pipeline =
    safeParseInt
        |> composeMaybe safeDivideBy2
        |> composeMaybe addTen

result1 : Maybe Int
result1 = pipeline "20"  -- Just 20: 20 -> 10 -> 20

result2 : Maybe Int
result2 = pipeline "abc"  -- Nothing

result3 : Maybe Int
result3 = pipeline "0"  -- Nothing (division by zero)

-- Fish operator (>=>) style
fish : (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
fish = composeMaybe

pipeline2 : String -> Maybe Int
pipeline2 = safeParseInt |> fish safeDivideBy2 |> fish addTen

main =
    case result1 of
        Just n -> n
        Nothing -> -1
```

---

### 16. Validation - `accumulate errors`

**Action:** Collect all errors (vs Either short-circuits)

```elm
-- tcelm: Validation via custom type that accumulates errors
module Main exposing (main)

-- Validation that collects all errors
type Validation e a
    = Success a
    | Failure (List e)

-- Map over success value
map : (a -> b) -> Validation e a -> Validation e b
map f validation =
    case validation of
        Success a -> Success (f a)
        Failure errs -> Failure errs

-- Applicative apply that accumulates errors
apply : Validation e a -> Validation e (a -> b) -> Validation e b
apply valA valF =
    case ( valF, valA ) of
        ( Success f, Success a ) ->
            Success (f a)

        ( Failure errs1, Failure errs2 ) ->
            Failure (errs1 ++ errs2)  -- Accumulate!

        ( Failure errs, _ ) ->
            Failure errs

        ( _, Failure errs ) ->
            Failure errs

-- Validation functions
validateName : String -> Validation String String
validateName name =
    if String.isEmpty name then
        Failure [ "Name is required" ]
    else
        Success name

validateAge : Int -> Validation String Int
validateAge age =
    if age < 0 then
        Failure [ "Age must be positive" ]
    else if age > 150 then
        Failure [ "Age must be realistic" ]
    else
        Success age

validateEmail : String -> Validation String String
validateEmail email =
    if String.contains "@" email then
        Success email
    else
        Failure [ "Invalid email" ]

-- User type
type alias User = { name : String, age : Int, email : String }

-- Validate and create user - collects ALL errors
validateUser : String -> Int -> String -> Validation String User
validateUser name age email =
    Success User
        |> apply (validateName name)
        |> apply (validateAge age)
        |> apply (validateEmail email)

-- Test with all invalid inputs
result : Validation String User
result = validateUser "" -5 "notanemail"
-- Failure ["Name is required", "Age must be positive", "Invalid email"]

main = 0
```

---

### 17. Free Monad - `separate description/execution`

**Action:** Build computation as data, interpret later

```elm
-- tcelm: Free Monad pattern for advanced use cases
module Main exposing (main)

-- Free-like pattern using union types
-- Define operations as data
type ConsoleOp a
    = GetLine (String -> a)
    | PutLine String a

-- Free monad wrapping operations
type Free f a
    = Pure a
    | Impure (f (Free f a))

-- For Console operations
type alias Console a = Free ConsoleOp a

-- Smart constructors
getLine : Console String
getLine = Impure (GetLine Pure)

putLine : String -> Console ()
putLine text = Impure (PutLine text (Pure ()))

-- Build program as data (not executing anything yet!)
program : Console String
program =
    Pure "Program built as data structure"

main = 0
```

---

## Advanced Patterns (18-30)

### 18. Comonad - `extract + extend`

**Action:** Extract value, extend with context

```elm
-- tcelm: Comonad via explicit extract/extend
module Main exposing (main)

-- Zipper (focused list) - a common comonad
type alias Zipper a =
    { left : List a
    , focus : a
    , right : List a
    }

-- Extract: get focused value
extract : Zipper a -> a
extract z = z.focus

-- Duplicate: nest zippers at each position
duplicate : Zipper a -> Zipper (Zipper a)
duplicate z =
    { left = lefts z
    , focus = z
    , right = rights z
    }

-- Extend: apply function at every position
extend : (Zipper a -> b) -> Zipper a -> Zipper b
extend f z =
    let
        dup = duplicate z
    in
    { left = List.map f dup.left
    , focus = f dup.focus
    , right = List.map f dup.right
    }

-- Helper: get all left-shifted versions
lefts : Zipper a -> List (Zipper a)
lefts z =
    case z.left of
        [] -> []
        l :: ls ->
            let newZ = { left = ls, focus = l, right = z.focus :: z.right }
            in newZ :: lefts newZ

-- Helper: get all right-shifted versions
rights : Zipper a -> List (Zipper a)
rights z =
    case z.right of
        [] -> []
        r :: rs ->
            let newZ = { left = z.focus :: z.left, focus = r, right = rs }
            in newZ :: rights newZ

-- Example: moving average using comonad
movingAvg : Zipper Int -> Int
movingAvg z =
    let
        vals = (List.take 1 z.left) ++ [z.focus] ++ (List.take 1 z.right)
        sum = List.foldl (+) 0 vals
    in
    sum // List.length vals

main = 0
```

---

### 19. Profunctor - `dimap`

**Action:** Map both inputs and outputs

```elm
-- tcelm: Profunctor via function composition on both ends
module Main exposing (main)

-- Functions are profunctors: contravariant in input, covariant in output
-- dimap : (a -> b) -> (c -> d) -> (b -> c) -> (a -> d)
dimap : (a -> b) -> (c -> d) -> (b -> c) -> (a -> d)
dimap f g h =
    g << h << f

-- Example: transform a string processor
processString : String -> Int
processString s = String.length s

-- Prepend "hello " to input, double the output
transformed : String -> Int
transformed = dimap (\s -> "hello " ++ s) (\n -> n * 2) processString

result : Int
result = transformed "world"  -- length("hello world") * 2 = 22

main = result
```

---

### 20. Arrow - `compose with structure`

**Action:** Compose computations preserving structure

```elm
-- tcelm: Arrow patterns via tuple manipulation
module Main exposing (main)

-- arr: lift function to arrow (identity for functions)
arr : (a -> b) -> (a -> b)
arr f = f

-- first: apply to first element of tuple
first : (a -> b) -> (a, c) -> (b, c)
first f ( a, c ) = ( f a, c )

-- second: apply to second element of tuple
second : (b -> c) -> (a, b) -> (a, c)
second f ( a, b ) = ( a, f b )

-- (***): apply two arrows in parallel
parallel : (a -> b) -> (c -> d) -> (a, c) -> (b, d)
parallel f g ( a, c ) = ( f a, g c )

-- (&&&): fanout - apply two arrows to same input
fanout : (a -> b) -> (a -> c) -> a -> (b, c)
fanout f g a = ( f a, g a )

-- Example
double : Int -> Int
double x = x * 2

toString : Int -> String
toString = String.fromInt

result : (Int, String)
result = fanout double toString 5  -- (10, "5")

main = Tuple.first result
```

---

### 21. Category - `identity + compose`

**Action:** Objects and morphisms with composition

```elm
-- tcelm: Functions form a category
module Main exposing (main)

-- Category laws are built into function composition:
-- identity: id << f = f = f << id
-- associativity: (f << g) << h = f << (g << h)

identity : a -> a
identity x = x

-- Compose (already built-in as <<)
compose : (b -> c) -> (a -> b) -> (a -> c)
compose f g = f << g

-- Example
double : Int -> Int
double x = x * 2

addOne : Int -> Int
addOne x = x + 1

-- Category composition
combined : Int -> Int
combined = compose double addOne  -- same as: double << addOne

result : Int
result = combined 5  -- (5 + 1) * 2 = 12

main = result
```

---

### 22. Semigroupal - `product`

**Action:** Combine independent contexts into tuple

```elm
-- tcelm: Semigroupal via map2 with Tuple.pair
module Main exposing (main)

-- product : F a -> F b -> F (a, b)
-- This is just map2 with Tuple.pair

productMaybe : Maybe a -> Maybe b -> Maybe (a, b)
productMaybe = Maybe.map2 Tuple.pair

productResult : Result e a -> Result e b -> Result e (a, b)
productResult = Result.map2 Tuple.pair

-- Example
result : Maybe (Int, String)
result = productMaybe (Just 1) (Just "hello")  -- Just (1, "hello")

main =
    case result of
        Just (n, _) -> n
        Nothing -> 0
```

---

### 23. Alternative - `choice`

**Action:** Choose between alternatives

```elm
-- tcelm: Alternative via orElse pattern
module Main exposing (main)

-- (<|>) : f a -> f a -> f a
orElse : Maybe a -> Maybe a -> Maybe a
orElse ma mb =
    case ma of
        Just _ -> ma
        Nothing -> mb

-- empty : f a
empty : Maybe a
empty = Nothing

-- guard : Bool -> f ()
guard : Bool -> Maybe ()
guard cond = if cond then Just () else Nothing

-- Example: try multiple parsers
parseInt : String -> Maybe Int
parseInt = String.toInt

parseDefault : String -> Maybe Int
parseDefault _ = Just 0

parse : String -> Maybe Int
parse s = parseInt s |> orElse (parseDefault s)

result : Maybe Int
result = parse "abc"  -- Just 0 (fallback)

main = Maybe.withDefault -1 result
```

---

### 24. MonadPlus - `monad + choice`

**Action:** Monad with Alternative operations

```elm
-- tcelm: MonadPlus via andThen + orElse
module Main exposing (main)

-- Combines Monad (andThen) with Alternative (orElse)

-- mfilter : (a -> Bool) -> m a -> m a
mfilter : (a -> Bool) -> Maybe a -> Maybe a
mfilter pred ma =
    ma |> Maybe.andThen (\a -> if pred a then Just a else Nothing)

-- msum : List (m a) -> m a
msum : List (Maybe a) -> Maybe a
msum maybes =
    List.foldl (\ma acc ->
        case acc of
            Just _ -> acc
            Nothing -> ma
    ) Nothing maybes

-- Example
result : Maybe Int
result = msum [ Nothing, Just 1, Just 2 ]  -- Just 1

filtered : Maybe Int
filtered = mfilter (\x -> x > 5) (Just 3)  -- Nothing

main = Maybe.withDefault 0 result
```

---

### 25. Distributive - `distribute`

**Action:** Distribute outer functor over inner (dual of Traversable)

```elm
-- tcelm: Distributive for specific types
module Main exposing (main)

-- distribute : f (g a) -> g (f a)
-- For functions (the canonical distributive):
-- distribute : f (a -> b) -> (a -> f b)

distributeList : List (a -> b) -> (a -> List b)
distributeList funcs a =
    List.map (\f -> f a) funcs

-- Example
funcs : List (Int -> Int)
funcs = [ \x -> x + 1, \x -> x * 2, \x -> x * x ]

-- Apply all functions to same input
results : List Int
results = distributeList funcs 5  -- [6, 10, 25]

main = List.sum results
```

---

### 26. Representable

**Implementation:** Manual for specific types

```elm
-- tcelm: Representable for specific fixed-size types
module Main exposing (main)

-- A Vec3 can be represented by an index
type alias Vec3 a = { x : a, y : a, z : a }

type Index = X | Y | Z

-- tabulate : (Index -> a) -> Vec3 a
tabulate : (Index -> a) -> Vec3 a
tabulate f = { x = f X, y = f Y, z = f Z }

-- index : Vec3 a -> Index -> a
index : Vec3 a -> Index -> a
index v i =
    case i of
        X -> v.x
        Y -> v.y
        Z -> v.z

-- Example
vec : Vec3 Int
vec = tabulate (\i -> case i of X -> 1; Y -> 2; Z -> 3)

main = vec.x + vec.y + vec.z  -- 6
```

---

### 27. Cofree

**Implementation:** Manual for specific functors

```elm
-- tcelm: Cofree for specific types
module Main exposing (main)

-- Cofree for List functor (rose tree)
type RoseTree a = Node a (List (RoseTree a))

extractRose : RoseTree a -> a
extractRose (Node a _) = a

-- Example
tree : RoseTree Int
tree = Node 1 [ Node 2 [], Node 3 [ Node 4 [] ] ]

main = extractRose tree  -- 1
```

---

### 28. Yoneda

**Implementation:** Via defunctionalization

```elm
-- tcelm: Yoneda-like optimization via manual fusion
module Main exposing (main)

-- Instead of abstract Yoneda, use manual map fusion
-- map f >> map g >> map h  becomes  map (f >> g >> h)

-- Example: fused pipeline
transform : Int -> Int
transform = (\x -> x + 1) >> (\x -> x * 2) >> (\x -> x - 3)

result : Int
result = transform 5  -- ((5 + 1) * 2) - 3 = 9

main = result
```

---

### 29. Coyoneda

**Implementation:** Via defunctionalization

```elm
-- tcelm: Coyoneda-like deferred mapping
module Main exposing (main)

-- Accumulate maps and apply once
type Deferred a b = Deferred (a -> b) a

runDeferred : Deferred a b -> b
runDeferred (Deferred f a) = f a

mapDeferred : (b -> c) -> Deferred a b -> Deferred a c
mapDeferred g (Deferred f a) = Deferred (f >> g) a

-- Example
deferred : Deferred Int Int
deferred =
    Deferred identity 5
        |> mapDeferred (\x -> x + 1)
        |> mapDeferred (\x -> x * 2)

result : Int
result = runDeferred deferred  -- 12

main = result
```

---

### 30. Continuation - `callbacks`

**Action:** Control flow as first-class values

```elm
-- tcelm: Continuation via callback-style or CPS
module Main exposing (main)

-- Continuation: (a -> r) -> r
type alias Cont r a = (a -> r) -> r

-- pure/return for Cont
pure : a -> Cont r a
pure a = \k -> k a

-- bind/andThen for Cont
andThen : (a -> Cont r b) -> Cont r a -> Cont r b
andThen f cont =
    \k -> cont (\a -> f a k)

-- run continuation
runCont : Cont a a -> a
runCont cont = cont identity

-- Example: CPS-style computation
addCont : Int -> Int -> Cont r Int
addCont x y = pure (x + y)

mulCont : Int -> Int -> Cont r Int
mulCont x y = pure (x * y)

computation : Cont Int Int
computation =
    addCont 2 3
        |> andThen (\sum -> mulCont sum 4)

result : Int
result = runCont computation  -- (2 + 3) * 4 = 20

main = result
```

---

## tcelm-Specific Features

### Pipeline Operators

tcelm fully supports Elm's pipeline operators:

```elm
module Main exposing (main)

-- Forward pipe: x |> f = f x
-- Backward pipe: f <| x = f x
-- Forward compose: f >> g = \x -> g (f x)
-- Backward compose: f << g = \x -> f (g x)

double : Int -> Int
double x = x * 2

addOne : Int -> Int
addOne x = x + 1

square : Int -> Int
square x = x * x

-- Pipeline style (readable data flow)
result1 : Int
result1 =
    5
        |> double      -- 10
        |> addOne      -- 11
        |> square      -- 121

-- Composition
transform : Int -> Int
transform = double >> addOne >> square

result2 : Int
result2 = transform 5  -- 121

main = result1
```

### Do-Notation

tcelm supports Haskell-style do-notation for Maybe, Result, and Task monads:

```elm
module Main exposing (main)

-- Do-notation automatically desugars to andThen chains

-- Maybe monad with do-notation
safeDivide : Int -> Int -> Maybe Int
safeDivide x y =
    if y == 0 then Nothing else Just (x // y)

computeMaybe : Maybe Int
computeMaybe = do
    x <- Just 10
    y <- safeDivide x 2
    z <- safeDivide y 1
    Just (z + 1)
-- Result: Just 6

-- Result monad with do-notation
validateAge : Int -> Result String Int
validateAge age =
    if age < 0 then Err "Age must be positive"
    else if age > 150 then Err "Age must be realistic"
    else Ok age

validateName : String -> Result String String
validateName name =
    if String.isEmpty name then Err "Name is required"
    else Ok name

validateUser : String -> Int -> Result String (String, Int)
validateUser name age = do
    validName <- validateName name
    validAge <- validateAge age
    Ok (validName, validAge)

-- Let bindings inside do blocks
complexComputation : Maybe Int
complexComputation = do
    x <- Just 5
    let doubled = x * 2
    y <- Just (doubled + 3)
    Just (x + y)
-- Result: Just 18

main =
    case computeMaybe of
        Just n -> n
        Nothing -> -1
```

### Pattern Matching

Full pattern matching support:

```elm
module Main exposing (main)

-- Constructor patterns
type Color = Red | Green | Blue

getColorCode : Color -> Int
getColorCode color =
    case color of
        Red -> 1
        Green -> 2
        Blue -> 3

-- Constructor with data
type Shape
    = Circle Float
    | Rectangle Float Float

area : Shape -> Float
area shape =
    case shape of
        Circle r -> 3.14159 * r * r
        Rectangle w h -> w * h

-- Tuple patterns
swap : (a, b) -> (b, a)
swap pair =
    case pair of
        (x, y) -> (y, x)

-- List cons patterns
headOrZero : List Int -> Int
headOrZero list =
    case list of
        x :: xs -> x
        [] -> 0

-- Nested constructor patterns
unwrapNestedMaybe : Maybe (Maybe Int) -> Int
unwrapNestedMaybe outer =
    case outer of
        Just (Just n) -> n
        Just Nothing -> -1
        Nothing -> -2

-- As-patterns
listWithLength : List a -> (Int, List a)
listWithLength list =
    case list of
        (x :: xs) as all -> (List.length all, all)
        [] -> (0, [])

-- Record patterns
type alias User = { name : String, age : Int }

getUserAge : User -> Int
getUserAge user =
    case user of
        { name, age } -> age

main = getColorCode Red
```

### Type Classes

Built-in constrained polymorphism:

```elm
module Main exposing (main)

-- Built-in type classes: comparable, appendable, number

-- comparable: Int, Float, Char, String, List comparable, Tuple of comparables
sortedInts : List Int
sortedInts = List.sort [3, 1, 4, 1, 5, 9]  -- [1, 1, 3, 4, 5, 9]

sortedStrings : List String
sortedStrings = List.sort ["banana", "apple", "cherry"]

-- List of comparable tuples
sortedPairs : List (Int, String)
sortedPairs = List.sort [(2, "b"), (1, "a"), (2, "a")]

-- appendable: String, List a
combined : String
combined = "Hello" ++ " " ++ "World"

combinedList : List Int
combinedList = [1, 2] ++ [3, 4]

-- number: Int, Float
sumInts : Int
sumInts = 5 + 3

sumFloats : Float
sumFloats = 5.5 + 3.2

main = List.sum sortedInts
```

---

## Pattern Support Summary

| # | Pattern | Implementation |
|---|---------|----------------|
| 1 | Functor | `Maybe.map`, `List.map`, `Result.map` |
| 2 | Applicative | `Maybe.map2/3/4/5`, `Result.map2/3` |
| 3 | Monad | `andThen` + **do-notation** |
| 4 | Monoid | `++`, `String.concat`, `List.concat` |
| 5 | Semigroup | `++`, explicit combine functions |
| 6 | Foldable | `List.foldl/foldr`, `Dict.foldl` |
| 7 | Traversable | Custom `traverse` functions |
| 8 | Contravariant | `contramap f g = g << f` |
| 9 | Bifunctor | `Tuple.mapBoth`, `Result.map` + `mapError` |
| 10 | Either | `Result Ok Err` |
| 11 | Lens | Record update `{ r \| field = x }` |
| 12 | Reader | Explicit config parameter |
| 13 | Writer | Return `( value, log )` tuples |
| 14 | State | Functions `s -> ( a, s )` |
| 15 | Kleisli | Chain with `andThen` |
| 16 | Validation | Custom type with error accumulation |
| 17 | Free Monad | Union types as operations |
| 18 | Comonad | Manual for specific types (Zipper, Stream) |
| 19 | Profunctor | `dimap f g h = g << h << f` |
| 20 | Arrow | Tuple functions: `first`, `fanout` |
| 21 | Category | Functions with `<<` and `identity` |
| 22 | Semigroupal | `map2 Tuple.pair` |
| 23 | Alternative | `orElse` pattern |
| 24 | MonadPlus | `andThen` + `orElse` |
| 25 | Distributive | Manual for specific cases |
| 26 | Representable | Manual for fixed-size types |
| 27 | Cofree | Manual for specific functors |
| 28 | Yoneda | Map fusion optimization |
| 29 | Coyoneda | Deferred mapping |
| 30 | Continuation | CPS style |

**All 30 patterns are implementable in tcelm.** The compiler demonstrates this by compiling itself - a self-hosting implementation that exercises the full range of functional programming patterns.

---

## Test Coverage

All patterns have been tested and verified. The compiler passes 265+ tests covering ~270 builtin functions.

---

## References

- [Elm Guide](https://guide.elm-lang.org/)
- [Elm Core Library](https://package.elm-lang.org/packages/elm/core/latest/)
