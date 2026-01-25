# 30 Functional Programming Patterns in Python and tcelm

A comprehensive guide to functional programming patterns with Python and tcelm examples.

**tcelm Support Legend:**
- ✅ Fully supported
- ⚠️ Partially supported (workaround available)
- ❌ Not supported (by design - see TODO.md "NOT Planned" section)

---

## Core 8 Patterns

### 1. Functor → `map` ✅

**Action:** Transform wrapped values

```python
from typing import TypeVar, Generic, Callable

A = TypeVar('A')
B = TypeVar('B')

class Just:
    def __init__(self, value):
        self.value = value

    def map(self, f: Callable[[A], B]) -> 'Just':
        return Just(f(self.value))

    def __repr__(self):
        return f"Just({self.value})"

class Nothing:
    def map(self, f):
        return Nothing()

    def __repr__(self):
        return "Nothing()"

# Example
result = Just(5).map(lambda x: x * 2)
print(result)  # Just(10)

result = Nothing().map(lambda x: x * 2)
print(result)  # Nothing()
```

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

### 2. Applicative → `apply` ⚠️

**Action:** Apply wrapped function to wrapped value

```python
class Just:
    def __init__(self, value):
        self.value = value

    def apply(self, wrapped_func):
        if isinstance(wrapped_func, Just):
            return Just(wrapped_func.value(self.value))
        return Nothing()

    def __repr__(self):
        return f"Just({self.value})"

class Nothing:
    def apply(self, wrapped_func):
        return Nothing()

    def __repr__(self):
        return "Nothing()"

# Example
result = Just(5).apply(Just(lambda x: x + 1))
print(result)  # Just(6)
```

```elm
-- tcelm: Applicative via Maybe.map2, Maybe.map3, etc.
module Main exposing (main)

-- No direct apply, but map2/map3 achieve same result
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

### 3. Monad → `flatMap` ✅

**Action:** Chain operations returning wrapped values

```python
class Just:
    def __init__(self, value):
        self.value = value

    def flat_map(self, f):
        return f(self.value)

    def __repr__(self):
        return f"Just({self.value})"

class Nothing:
    def flat_map(self, f):
        return Nothing()

    def __repr__(self):
        return "Nothing()"

def safe_divide(x, y):
    return Just(x / y) if y != 0 else Nothing()

# Example
result = Just(10).flat_map(lambda x: safe_divide(x, 2))
print(result)  # Just(5.0)

result = Just(10).flat_map(lambda x: safe_divide(x, 0))
print(result)  # Nothing()
```

```elm
-- tcelm: Monad via andThen (flatMap)
module Main exposing (main)

safeDivide : Int -> Int -> Maybe Int
safeDivide x y =
    if y == 0 then
        Nothing
    else
        Just (x // y)

-- Chain Maybe operations
result : Maybe Int
result =
    Just 10
        |> Maybe.andThen (\x -> safeDivide x 2)
        |> Maybe.andThen (\x -> safeDivide x 1)
-- result = Just 5

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

### 4. Monoid → `combine + identity` ✅

**Action:** Smash together with neutral element

```python
class StringMonoid:
    @staticmethod
    def combine(a: str, b: str) -> str:
        return a + b

    @staticmethod
    def empty() -> str:
        return ""

class SumMonoid:
    @staticmethod
    def combine(a: int, b: int) -> int:
        return a + b

    @staticmethod
    def empty() -> int:
        return 0

# Example
result = StringMonoid.combine("hello", " world")
print(result)  # "hello world"

result = SumMonoid.combine(5, 3)
print(result)  # 8
```

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

### 5. Semigroup → `combine` ✅

**Action:** Smash together without neutral element

```python
class MaxSemigroup:
    @staticmethod
    def combine(a: int, b: int) -> int:
        return max(a, b)

class MinSemigroup:
    @staticmethod
    def combine(a: int, b: int) -> int:
        return min(a, b)

# Example
result = MaxSemigroup.combine(5, 3)
print(result)  # 5
```

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

### 6. Foldable → `reduce` ✅

**Action:** Collapse structure to single value

```python
from functools import reduce

class ListFoldable:
    def __init__(self, items):
        self.items = items

    def fold(self, f, initial):
        return reduce(f, self.items, initial)

# Example
foldable = ListFoldable([1, 2, 3, 4])
result = foldable.fold(lambda a, b: a + b, 0)
print(result)  # 10

result = foldable.fold(lambda a, b: a * b, 1)
print(result)  # 24
```

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

### 7. Traversable → `map + flip wrappers` ⚠️

**Action:** Map with effects, flip nesting

```python
class ListTraversable:
    def __init__(self, items):
        self.items = items

    def traverse(self, f):
        """Apply f to each item, flip List[Maybe] to Maybe[List]"""
        results = []
        for item in self.items:
            result = f(item)
            if isinstance(result, Nothing):
                return Nothing()
            results.append(result.value)
        return Just(results)

# Example
traversable = ListTraversable([1, 2, 3])
result = traversable.traverse(lambda x: Just(x * 2))
print(result)  # Just([2, 4, 6])

result = traversable.traverse(lambda x: Nothing() if x == 2 else Just(x))
print(result)  # Nothing()
```

```elm
-- tcelm: Traversable via List.filterMap or custom traverse
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

### 8. Contravariant → `map inputs (backwards)` ⚠️

**Action:** Map over inputs instead of outputs

```python
class Predicate:
    def __init__(self, predicate):
        self.predicate = predicate

    def contramap(self, f):
        """Transform input before applying predicate"""
        return Predicate(lambda x: self.predicate(f(x)))

# Example
is_even = Predicate(lambda x: x % 2 == 0)
is_even_length = is_even.contramap(len)
print(is_even_length.predicate("hello"))  # False (len=5)
print(is_even_length.predicate("hi"))     # True (len=2)
```

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

### 9. Bifunctor → `map both sides` ⚠️

**Action:** Map left or right side

```python
class Right:
    def __init__(self, value):
        self.value = value

    def bimap(self, f_left, f_right):
        return Right(f_right(self.value))

class Left:
    def __init__(self, error):
        self.error = error

    def bimap(self, f_left, f_right):
        return Left(f_left(self.error))

# Example
result = Right(5).bimap(lambda e: f"Error: {e}", lambda x: x * 2)
print(result)  # Right(10)
```

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

### 10. Either → `explicit success/failure` ✅

**Action:** Handle errors without exceptions

```python
class Right:
    def __init__(self, value):
        self.value = value

    def map(self, f):
        return Right(f(self.value))

    def flat_map(self, f):
        return f(self.value)

class Left:
    def __init__(self, error):
        self.error = error

    def map(self, f):
        return self

    def flat_map(self, f):
        return self

def safe_parse_int(s):
    try:
        return Right(int(s))
    except ValueError:
        return Left(f"Cannot parse '{s}' as int")

# Example
result = safe_parse_int("42").map(lambda x: x * 2)
print(result)  # Right(84)
```

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

### 11. Lens → `getter + setter` ⚠️

**Action:** Immutably update nested data

```python
class Lens:
    def __init__(self, getter, setter):
        self.getter = getter
        self.setter = setter

    def get(self, obj):
        return self.getter(obj)

    def set(self, obj, value):
        return self.setter(obj, value)

    def modify(self, obj, f):
        return self.setter(obj, f(self.getter(obj)))

# Example
user = {'name': 'Alice', 'age': 30, 'address': {'city': 'NYC'}}
name_lens = Lens(lambda u: u['name'], lambda u, name: {**u, 'name': name})
print(name_lens.get(user))  # 'Alice'
```

```elm
-- tcelm: Lens-like patterns via record update syntax
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

-- Nested update (more verbose without lens library)
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

-- Or directly
aliceInLA2 : User
aliceInLA2 =
    { alice | address = { city = "LA", zip = alice.address.zip } }

main = alice.age
```

---

### 12. Reader → `implicit config` ⚠️

**Action:** Pass context/dependencies implicitly

```python
class Reader:
    def __init__(self, run):
        self.run_fn = run

    def run(self, config):
        return self.run_fn(config)

    def map(self, f):
        return Reader(lambda config: f(self.run_fn(config)))

    def flat_map(self, f):
        return Reader(lambda config: f(self.run_fn(config)).run(config))

# Example - dependency injection
def get_user_by_id(user_id):
    return Reader(lambda config: f"User {user_id} from {config['db']}")

config = {'db': 'postgres://localhost'}
result = get_user_by_id(123).run(config)
print(result)
```

```elm
-- tcelm: Reader pattern via explicit config passing or records
module Main exposing (main)

-- Config record (like Reader's environment)
type alias Config =
    { dbUrl : String
    , cacheUrl : String
    , debug : Bool
    }

-- Functions that need config take it explicitly
-- (Elm doesn't have implicit parameters)

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
type alias ConfiguredFn a = a
getUserByIdConfigured : Int -> String
getUserByIdConfigured = getUserById config

main = 0  -- String output not directly supported
```

---

### 13. Writer → `accumulate logs` ⚠️

**Action:** Carry logs/metadata through computation

```python
class Writer:
    def __init__(self, value, log):
        self.value = value
        self.log = log

    def map(self, f):
        return Writer(f(self.value), self.log)

    def flat_map(self, f):
        result = f(self.value)
        return Writer(result.value, self.log + result.log)

# Example
def add_with_log(x, y):
    return Writer(x + y, [f"Added {x} + {y}"])

result = Writer(5, ["Started"]).flat_map(lambda x: add_with_log(x, 3))
print(result.value, result.log)  # 8, ['Started', 'Added 5 + 3']
```

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

### 14. State → `thread state` ⚠️

**Action:** Thread state through computations

```python
class State:
    def __init__(self, run):
        self.run_fn = run

    def run(self, initial_state):
        return self.run_fn(initial_state)

    def flat_map(self, f):
        def new_run(state):
            value, new_state = self.run_fn(state)
            return f(value).run(new_state)
        return State(new_run)

# Example - stack operations
def push(value):
    return State(lambda stack: (None, [value] + stack))

def pop():
    return State(lambda stack: (stack[0], stack[1:]) if stack else (None, stack))
```

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

### 15. Kleisli → `compose monadic` ⚠️

**Action:** Compose functions returning monads

```python
class Kleisli:
    def __init__(self, run):
        self.run = run

    def compose(self, other):
        return Kleisli(lambda a: self.run(a).flat_map(other.run))

# Example
parse = Kleisli(safe_parse_int)
divide = Kleisli(safe_divide_by_2)
pipeline = parse.compose(divide)
print(pipeline("20"))  # Just(10.0)
```

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
-- In Haskell: (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
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

### 16. Validation → `accumulate errors` ⚠️

**Action:** Collect all errors (vs Either short-circuits)

```python
class Success:
    def __init__(self, value):
        self.value = value

    def apply(self, wrapped_func):
        if isinstance(wrapped_func, Success):
            return Success(wrapped_func.value(self.value))
        return wrapped_func

class Failure:
    def __init__(self, errors):
        self.errors = errors

    def apply(self, wrapped_func):
        if isinstance(wrapped_func, Failure):
            return Failure(wrapped_func.errors + self.errors)
        return self

# Collect ALL validation errors instead of stopping at first
```

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

-- Compare to Result which short-circuits at first error!
resultShortCircuit : Result String User
resultShortCircuit =
    Result.map3 User
        (if String.isEmpty "" then Err "Name is required" else Ok "")
        (if -5 < 0 then Err "Age must be positive" else Ok -5)
        (if String.contains "@" "bad" then Ok "bad" else Err "Invalid email")
-- Err "Name is required" -- only first error!

main = 0
```

---

### 17. Free Monad → `separate description/execution` ❌

**Action:** Build computation as data, interpret later

```python
# Free Monad allows building a computation as a data structure
# then interpreting it separately - great for testing, optimization

class Pure:
    def __init__(self, value):
        self.value = value

class Impure:
    def __init__(self, operation, continuation):
        self.operation = operation
        self.continuation = continuation

# Build program as data, interpret later with different interpreters
```

```elm
-- tcelm: Free Monad pattern is complex but possible
-- Note: This is an advanced pattern, rarely needed in practice
-- tcelm's design philosophy favors simpler approaches

module Main exposing (main)

-- Simplified Free-like pattern using union types
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
    -- This just builds a data structure!
    -- Actual interpretation happens separately
    -- In practice, tcelm doesn't need this pattern
    -- because we compile to C, not interpret at runtime
    Pure "This pattern is too complex for tcelm"

-- tcelm recommendation: Use simpler patterns
-- Instead of Free Monad, use:
-- 1. Result/Maybe for error handling
-- 2. Explicit state passing
-- 3. Record of functions for dependency injection

main = 0
```

---

## Advanced Patterns (18-30)

### 18-30: Advanced Patterns ❌

The following patterns are intentionally **not supported** in tcelm:

| Pattern | Reason Not Supported |
|---------|---------------------|
| **18. Comonad** | Requires HKT, adds complexity |
| **19. Profunctor** | Requires HKT |
| **20. Arrow** | Type classes required |
| **21. Category** | Type classes required |
| **22. Semigroupal** | Type classes required |
| **23. Alternative** | Type classes required |
| **24. MonadPlus** | Type classes required |
| **25. Distributive** | Requires HKT |
| **26. Representable** | Requires HKT, type families |
| **27. Cofree** | Requires HKT, complex recursion |
| **28. Yoneda** | Requires HKT, rank-2 types |
| **29. Coyoneda** | Requires HKT, existentials |
| **30. Continuation** | Complex control flow |

**tcelm Design Philosophy** (from TODO.md):

> Keep it simple. Elm's existing type system + do-notation + good FFI is sufficient.
> Avoid exotic Haskell features that add complexity without proportional benefit.

For the target use case (embedded accounting ledger on RTEMS), the simpler patterns (1-16) provide all necessary abstractions:

- **Functor/Monad/Applicative**: `Maybe.map`, `Result.andThen`, `Maybe.map2`
- **Error Handling**: `Result` type (Either)
- **Validation**: Custom `Validation` type as shown above
- **State Threading**: Explicit parameter passing
- **Configuration**: Record types passed explicitly
- **Logging**: Writer pattern with tuples

---

## tcelm-Specific Patterns

### Pipeline Operators ✅

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

-- Backward pipe (apply style)
result2 : Int
result2 = square <| addOne <| double <| 5  -- 121

-- Composition
transform : Int -> Int
transform = double >> addOne >> square

result3 : Int
result3 = transform 5  -- 121

-- Compose with partial application
addThenDouble : Int -> Int
addThenDouble = (+) 1 >> (*) 2

result4 : Int
result4 = addThenDouble 5  -- 12

main = result1
```

### Pattern Matching ⚠️

Current tcelm pattern matching support:

```elm
module Main exposing (main)

-- Supported patterns
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

-- Tuple patterns (supported)
swap : (a, b) -> (b, a)
swap pair =
    case pair of
        (x, y) -> (y, x)

-- TODO: These patterns need completion in tcelm
-- List cons: x :: xs
-- Nested constructors: Just (Ok value)
-- As-patterns: (x :: xs) as list

main = getColorCode Red
```

---

## Summary Table for tcelm

| # | Pattern | tcelm Support | Implementation |
|---|---------|---------------|----------------|
| 1 | Functor | ✅ | `Maybe.map`, `List.map`, `Result.map` |
| 2 | Applicative | ⚠️ | `Maybe.map2`, `Maybe.map3` |
| 3 | Monad | ✅ | `Maybe.andThen`, `Result.andThen` |
| 4 | Monoid | ✅ | Explicit combine functions |
| 5 | Semigroup | ✅ | Explicit combine functions |
| 6 | Foldable | ✅ | `List.foldl`, `List.foldr` |
| 7 | Traversable | ⚠️ | Custom `traverse` functions |
| 8 | Contravariant | ⚠️ | Function composition |
| 9 | Bifunctor | ⚠️ | `Result.map` + `Result.mapError` |
| 10 | Either | ✅ | `Result` type |
| 11 | Lens | ⚠️ | Record update syntax |
| 12 | Reader | ⚠️ | Explicit config passing |
| 13 | Writer | ⚠️ | Tuple `(value, logs)` |
| 14 | State | ⚠️ | Explicit state threading |
| 15 | Kleisli | ⚠️ | `andThen` chains |
| 16 | Validation | ⚠️ | Custom `Validation` type |
| 17-30 | Advanced | ❌ | Not needed for target use case |

---

## Recommended Learning Path for tcelm

**Essential (use daily):**
1. Functor (`map`)
2. Monad (`andThen`)
3. Either (`Result`)
4. Foldable (`foldl`)

**Important (use often):**
5. Applicative (`map2`)
6. Monoid (explicit combine)
7. Pipeline operators (`|>`, `>>`)

**As Needed:**
8. Validation (form handling)
9. State (explicit threading)
10. Writer (logging pattern)

---

## References

- [Elm Guide](https://guide.elm-lang.org/)
- [tcelm TODO.md](../TODO.md) - Design philosophy and roadmap
- [Elm Core Library](https://package.elm-lang.org/packages/elm/core/latest/)
