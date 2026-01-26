# tcelm Cookbook

Practical recipes for common programming tasks in tcelm.

---

## Data Transformation

### Transform a List

```elm
-- Double all numbers
doubled = List.map (\x -> x * 2) [1, 2, 3, 4, 5]
-- [2, 4, 6, 8, 10]

-- Using partial application
doubled2 = List.map ((*) 2) [1, 2, 3, 4, 5]
```

### Filter a List

```elm
-- Keep only even numbers
evens = List.filter (\x -> modBy 2 x == 0) [1, 2, 3, 4, 5, 6]
-- [2, 4, 6]

-- Keep non-empty strings
nonEmpty = List.filter (not << String.isEmpty) ["", "hello", "", "world"]
-- ["hello", "world"]
```

### Reduce/Fold a List

```elm
-- Sum
total = List.foldl (+) 0 [1, 2, 3, 4, 5]
-- 15

-- Product
product = List.foldl (*) 1 [1, 2, 3, 4, 5]
-- 120

-- Find maximum
maximum list =
    case list of
        [] -> Nothing
        x :: xs -> Just (List.foldl max x xs)
```

### Chain List Operations

```elm
result =
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        |> List.filter (\x -> modBy 2 x == 0)  -- [2, 4, 6, 8, 10]
        |> List.map (\x -> x * x)              -- [4, 16, 36, 64, 100]
        |> List.sum                             -- 220
```

### Flatten Nested Lists

```elm
-- Flatten one level
flat = List.concat [[1, 2], [3, 4], [5]]
-- [1, 2, 3, 4, 5]

-- Map then flatten
result = List.concatMap (\x -> [x, x * 2]) [1, 2, 3]
-- [1, 2, 2, 4, 3, 6]
```

### Zip Two Lists

```elm
zip : List a -> List b -> List (a, b)
zip xs ys =
    List.map2 Tuple.pair xs ys

zipped = zip [1, 2, 3] ["a", "b", "c"]
-- [(1, "a"), (2, "b"), (3, "c")]
```

### Partition a List

```elm
(evens, odds) = List.partition (\x -> modBy 2 x == 0) [1, 2, 3, 4, 5]
-- evens = [2, 4], odds = [1, 3, 5]
```

### Take and Drop

```elm
firstThree = List.take 3 [1, 2, 3, 4, 5]  -- [1, 2, 3]
afterThree = List.drop 3 [1, 2, 3, 4, 5]  -- [4, 5]
```

### Index Into List

```elm
-- Get element at index (0-based)
getAt : Int -> List a -> Maybe a
getAt idx list =
    list
        |> List.drop idx
        |> List.head

third = getAt 2 [10, 20, 30, 40]  -- Just 30
```

### Map With Index

```elm
indexed = List.indexedMap (\i x -> (i, x)) ["a", "b", "c"]
-- [(0, "a"), (1, "b"), (2, "c")]

-- Add index to value
withIndex = List.indexedMap (\i x -> x + i) [10, 10, 10]
-- [10, 11, 12]
```

---

## Maybe Handling

### Safe Division

```elm
safeDivide : Int -> Int -> Maybe Int
safeDivide a b =
    if b == 0 then Nothing else Just (a // b)
```

### Chain Maybe Operations

```elm
-- With andThen (flatMap)
compute : Int -> Maybe Int
compute x =
    safeDivide x 2
        |> Maybe.andThen (\y -> safeDivide y 2)
        |> Maybe.andThen (\z -> Just (z + 1))

-- With do-notation (cleaner)
computeDo : Int -> Maybe Int
computeDo x = do
    y <- safeDivide x 2
    z <- safeDivide y 2
    Just (z + 1)
```

### Provide Default Value

```elm
value = Maybe.withDefault 0 (String.toInt "abc")  -- 0
value2 = Maybe.withDefault 0 (String.toInt "42")  -- 42
```

### Transform Maybe Value

```elm
doubled = Maybe.map (\x -> x * 2) (Just 5)  -- Just 10
doubled2 = Maybe.map (\x -> x * 2) Nothing  -- Nothing
```

### Combine Multiple Maybes

```elm
-- All must succeed
sum = Maybe.map2 (+) (Just 3) (Just 4)  -- Just 7
sum2 = Maybe.map2 (+) (Just 3) Nothing  -- Nothing

-- Three values
result = Maybe.map3 (\a b c -> a + b + c) (Just 1) (Just 2) (Just 3)
-- Just 6
```

### Convert Maybe to List

```elm
maybeToList : Maybe a -> List a
maybeToList maybe =
    case maybe of
        Just x -> [x]
        Nothing -> []
```

### Filter Maybe from List

```elm
-- Keep only Just values
justs = List.filterMap identity [Just 1, Nothing, Just 2, Nothing, Just 3]
-- [1, 2, 3]

-- Parse strings, keep valid ones
numbers = List.filterMap String.toInt ["1", "abc", "2", "xyz", "3"]
-- [1, 2, 3]
```

---

## Result Handling

### Parse with Validation

```elm
parsePositive : String -> Result String Int
parsePositive s =
    case String.toInt s of
        Nothing -> Err ("Not a number: " ++ s)
        Just n ->
            if n < 0 then
                Err "Must be positive"
            else
                Ok n
```

### Chain Result Operations

```elm
-- Pipeline style
compute : String -> Result String Int
compute input =
    parsePositive input
        |> Result.andThen (\n ->
            if n > 100 then Err "Too large"
            else Ok (n * 2))
        |> Result.map (\n -> n + 1)

-- Do-notation style
computeDo : String -> Result String Int
computeDo input = do
    n <- parsePositive input
    validated <- if n > 100 then Err "Too large" else Ok n
    Ok (validated * 2 + 1)
```

### Handle Both Cases

```elm
handleResult : Result String Int -> String
handleResult result =
    case result of
        Ok value -> "Success: " ++ String.fromInt value
        Err msg -> "Error: " ++ msg
```

### Transform Error

```elm
-- Add context to error
withContext =
    parsePositive "abc"
        |> Result.mapError (\e -> "While parsing input: " ++ e)
```

### Convert Result to Maybe

```elm
Result.toMaybe (Ok 42)     -- Just 42
Result.toMaybe (Err "x")   -- Nothing
```

---

## Record Manipulation

### Create Record

```elm
type alias User =
    { name : String
    , age : Int
    , email : String
    }

alice : User
alice =
    { name = "Alice"
    , age = 30
    , email = "alice@example.com"
    }
```

### Update Record Fields

```elm
-- Update one field
olderAlice = { alice | age = 31 }

-- Update multiple fields
updated = { alice | age = 31, email = "new@example.com" }
```

### Access Fields

```elm
-- Direct access
name = alice.name

-- Using accessor function
getName = .name
name2 = getName alice

-- In pipelines
name3 = alice |> .name
```

### Nested Record Update

```elm
type alias Address = { city : String, zip : String }
type alias Person = { name : String, address : Address }

-- Update nested field
updateCity : String -> Person -> Person
updateCity newCity person =
    { person | address = { city = newCity, zip = person.address.zip } }

-- Or with helper
updateAddress : (Address -> Address) -> Person -> Person
updateAddress f person =
    { person | address = f person.address }

updateCity2 newCity =
    updateAddress (\addr -> { addr | city = newCity })
```

### Map Over Record Field

```elm
incrementAge : User -> User
incrementAge user =
    { user | age = user.age + 1 }
```

---

## Custom Types

### Simple Enum

```elm
type Status = Pending | Active | Completed | Failed

statusToString : Status -> String
statusToString status =
    case status of
        Pending -> "pending"
        Active -> "active"
        Completed -> "completed"
        Failed -> "failed"
```

### Type with Data

```elm
type Shape
    = Circle Float
    | Rectangle Float Float
    | Triangle Float Float Float

area : Shape -> Float
area shape =
    case shape of
        Circle r ->
            3.14159 * r * r
        Rectangle w h ->
            w * h
        Triangle a b c ->
            let s = (a + b + c) / 2
            in sqrt (s * (s - a) * (s - b) * (s - c))
```

### Recursive Type (Tree)

```elm
type Tree a
    = Leaf a
    | Node (Tree a) (Tree a)

sumTree : Tree Int -> Int
sumTree tree =
    case tree of
        Leaf n -> n
        Node left right -> sumTree left + sumTree right

mapTree : (a -> b) -> Tree a -> Tree b
mapTree f tree =
    case tree of
        Leaf a -> Leaf (f a)
        Node left right -> Node (mapTree f left) (mapTree f right)
```

### Linked List

```elm
type MyList a
    = Empty
    | Cons a (MyList a)

myLength : MyList a -> Int
myLength list =
    case list of
        Empty -> 0
        Cons _ rest -> 1 + myLength rest

myMap : (a -> b) -> MyList a -> MyList b
myMap f list =
    case list of
        Empty -> Empty
        Cons x rest -> Cons (f x) (myMap f rest)
```

---

## String Processing

### Parse CSV Line

```elm
parseCSV : String -> List String
parseCSV line =
    String.split "," line
        |> List.map String.trim
```

### Build String from Parts

```elm
buildGreeting : String -> Int -> String
buildGreeting name age =
    "Hello, " ++ name ++ "! You are " ++ String.fromInt age ++ " years old."

-- Or with list join
buildGreeting2 name age =
    String.join ""
        [ "Hello, "
        , name
        , "! You are "
        , String.fromInt age
        , " years old."
        ]
```

### Validate String

```elm
validateEmail : String -> Result String String
validateEmail email =
    if String.isEmpty email then
        Err "Email is required"
    else if not (String.contains "@" email) then
        Err "Invalid email format"
    else
        Ok email
```

### String Contains Check

```elm
containsAny : List String -> String -> Bool
containsAny needles haystack =
    List.any (\needle -> String.contains needle haystack) needles
```

---

## Dictionary Operations

### Group By

```elm
groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy keyFn list =
    List.foldl
        (\item acc ->
            let key = keyFn item
            in Dict.update key
                (\maybe ->
                    case maybe of
                        Nothing -> Just [item]
                        Just existing -> Just (item :: existing)
                )
                acc
        )
        Dict.empty
        list

-- Usage
type alias Person = { name : String, city : String }
people = [{ name = "Alice", city = "NYC" }, { name = "Bob", city = "LA" }, { name = "Carol", city = "NYC" }]
byCity = groupBy .city people
-- Dict.fromList [("NYC", [Carol, Alice]), ("LA", [Bob])]
```

### Frequency Count

```elm
frequencies : List comparable -> Dict comparable Int
frequencies list =
    List.foldl
        (\item acc ->
            Dict.update item
                (\maybe ->
                    case maybe of
                        Nothing -> Just 1
                        Just n -> Just (n + 1)
                )
                acc
        )
        Dict.empty
        list

-- Usage
counts = frequencies ["a", "b", "a", "c", "a", "b"]
-- Dict.fromList [("a", 3), ("b", 2), ("c", 1)]
```

### Merge Dictionaries

```elm
-- Prefer values from second dict
merge : Dict comparable v -> Dict comparable v -> Dict comparable v
merge d1 d2 =
    Dict.foldl Dict.insert d1 d2
```

### Invert Dictionary

```elm
invert : Dict comparable1 comparable2 -> Dict comparable2 comparable1
invert dict =
    Dict.foldl (\k v acc -> Dict.insert v k acc) Dict.empty dict
```

---

## Control Flow

### Early Return Pattern

```elm
-- Using Maybe
processIfValid : Int -> Maybe Int
processIfValid x =
    if x < 0 then Nothing
    else if x > 100 then Nothing
    else Just (x * 2)

-- Using Result
processWithErrors : Int -> Result String Int
processWithErrors x =
    if x < 0 then Err "Negative not allowed"
    else if x > 100 then Err "Too large"
    else Ok (x * 2)
```

### Guard Clauses with Do-Notation

```elm
validateAndProcess : String -> Int -> Result String Int
validateAndProcess name age = do
    _ <- if String.isEmpty name then Err "Name required" else Ok ()
    _ <- if age < 0 then Err "Invalid age" else Ok ()
    _ <- if age > 150 then Err "Unrealistic age" else Ok ()
    Ok (age * 2)
```

### State Machine

```elm
type State = Idle | Loading | Success String | Error String

type Msg = Start | Loaded String | Failed String | Reset

transition : Msg -> State -> State
transition msg state =
    case (state, msg) of
        (Idle, Start) -> Loading
        (Loading, Loaded data) -> Success data
        (Loading, Failed err) -> Error err
        (_, Reset) -> Idle
        _ -> state  -- Invalid transition, stay in current state
```

---

## RTEMS / Embedded

### Basic Worker Application

```elm
module App exposing (main)

import Rtems

type alias Model = { value : Int }

type Msg = Tick Int

init : Model
init = { value = 0 }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick _ ->
            ({ model | value = model.value + 1 }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
    Rtems.everyTick 100 Tick  -- Every 100 ticks

main =
    Rtems.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
```

### PID Controller

```elm
type alias PIDState =
    { setpoint : Float
    , integral : Float
    , lastError : Float
    , kp : Float
    , ki : Float
    , kd : Float
    }

pidUpdate : Float -> Float -> PIDState -> (Float, PIDState)
pidUpdate dt measured state =
    let
        error = state.setpoint - measured
        integral = state.integral + error * dt
        derivative = (error - state.lastError) / dt
        output = state.kp * error + state.ki * integral + state.kd * derivative
    in
    ( output
    , { state | integral = integral, lastError = error }
    )
```

### Sensor Data Aggregation

```elm
type alias SensorData =
    { readings : List Float
    , sum : Float
    , count : Int
    }

initSensor : SensorData
initSensor = { readings = [], sum = 0, count = 0 }

addReading : Float -> SensorData -> SensorData
addReading value data =
    { readings = value :: List.take 99 data.readings  -- Keep last 100
    , sum = data.sum + value
    , count = data.count + 1
    }

average : SensorData -> Float
average data =
    if data.count == 0 then 0
    else data.sum / toFloat data.count
```

---

## Utility Functions

### Clamp Value

```elm
clamp : comparable -> comparable -> comparable -> comparable
clamp low high value =
    if value < low then low
    else if value > high then high
    else value
```

### Safe List Operations

```elm
safeHead : List a -> Maybe a
safeHead list =
    case list of
        x :: _ -> Just x
        [] -> Nothing

safeTail : List a -> Maybe (List a)
safeTail list =
    case list of
        _ :: xs -> Just xs
        [] -> Nothing

safeLast : List a -> Maybe a
safeLast list =
    case list of
        [x] -> Just x
        _ :: xs -> safeLast xs
        [] -> Nothing
```

### Compose Functions

```elm
-- Compose two functions
compose2 : (b -> c) -> (a -> b) -> (a -> c)
compose2 f g x = f (g x)

-- Compose list of functions
composeAll : List (a -> a) -> (a -> a)
composeAll funcs =
    List.foldl (<<) identity funcs
```

### Repeat and Iterate

```elm
-- Repeat value n times
repeat : Int -> a -> List a
repeat n x =
    if n <= 0 then []
    else x :: repeat (n - 1) x

-- Apply function n times
iterate : Int -> (a -> a) -> a -> a
iterate n f x =
    if n <= 0 then x
    else iterate (n - 1) f (f x)
```

### Tuple Helpers

```elm
mapFirst : (a -> c) -> (a, b) -> (c, b)
mapFirst f (a, b) = (f a, b)

mapSecond : (b -> c) -> (a, b) -> (a, c)
mapSecond f (a, b) = (a, f b)

mapBoth : (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapBoth f g (a, b) = (f a, g b)

swap : (a, b) -> (b, a)
swap (a, b) = (b, a)
```
