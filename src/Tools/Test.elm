module Tools.Test exposing
    ( Test, describe, test, todo, skip
    , Expectation, expect, pass, fail
    , equal, notEqual, lessThan, greaterThan, atLeast, atMost
    , true, false, ok, err
    , contains, hasLength, isEmpty
    , run, runWithOptions, TestResult, Summary
    , fuzz, fuzz2, fuzz3, Fuzzer
    , intFuzzer, floatFuzzer, stringFuzzer, boolFuzzer, listFuzzer, maybeFuzzer
    )

{-| Testing Framework - Unit and fuzz testing for Elm.

@docs Test, describe, test, todo, skip
@docs Expectation, expect, pass, fail
@docs equal, notEqual, lessThan, greaterThan, atLeast, atMost
@docs true, false, ok, err
@docs contains, hasLength, isEmpty
@docs run, runWithOptions, TestResult, Summary
@docs fuzz, fuzz2, fuzz3, Fuzzer
@docs intFuzzer, floatFuzzer, stringFuzzer, boolFuzzer, listFuzzer, maybeFuzzer

-}


{-| A test or group of tests.
-}
type Test
    = Single String (() -> Expectation)
    | Batch String (List Test)
    | Todo String
    | Skip Test


{-| The result of an expectation.
-}
type Expectation
    = Pass
    | Fail String


{-| Create a test group.
-}
describe : String -> List Test -> Test
describe =
    Batch


{-| Create a single test.
-}
test : String -> (() -> Expectation) -> Test
test =
    Single


{-| Mark a test as todo (placeholder).
-}
todo : String -> Test
todo =
    Todo


{-| Skip a test.
-}
skip : Test -> Test
skip =
    Skip


{-| Create a passing expectation.
-}
pass : Expectation
pass =
    Pass


{-| Create a failing expectation with a message.
-}
fail : String -> Expectation
fail =
    Fail



-- EXPECTATIONS


{-| Begin an expectation chain.
-}
expect : a -> Subject a
expect value =
    Subject value


type Subject a
    = Subject a


{-| Check equality.
-}
equal : a -> Subject a -> Expectation
equal expected (Subject actual) =
    if actual == expected then
        Pass

    else
        Fail ("Expected: " ++ Debug.toString expected ++ "\nActual: " ++ Debug.toString actual)


{-| Check inequality.
-}
notEqual : a -> Subject a -> Expectation
notEqual unexpected (Subject actual) =
    if actual /= unexpected then
        Pass

    else
        Fail ("Expected not: " ++ Debug.toString unexpected ++ "\nBut got: " ++ Debug.toString actual)


{-| Check less than.
-}
lessThan : comparable -> Subject comparable -> Expectation
lessThan bound (Subject actual) =
    if actual < bound then
        Pass

    else
        Fail (Debug.toString actual ++ " is not less than " ++ Debug.toString bound)


{-| Check greater than.
-}
greaterThan : comparable -> Subject comparable -> Expectation
greaterThan bound (Subject actual) =
    if actual > bound then
        Pass

    else
        Fail (Debug.toString actual ++ " is not greater than " ++ Debug.toString bound)


{-| Check at least (>=).
-}
atLeast : comparable -> Subject comparable -> Expectation
atLeast bound (Subject actual) =
    if actual >= bound then
        Pass

    else
        Fail (Debug.toString actual ++ " is not at least " ++ Debug.toString bound)


{-| Check at most (<=).
-}
atMost : comparable -> Subject comparable -> Expectation
atMost bound (Subject actual) =
    if actual <= bound then
        Pass

    else
        Fail (Debug.toString actual ++ " is not at most " ++ Debug.toString bound)


{-| Check boolean is true.
-}
true : Subject Bool -> Expectation
true (Subject actual) =
    if actual then
        Pass

    else
        Fail "Expected True but got False"


{-| Check boolean is false.
-}
false : Subject Bool -> Expectation
false (Subject actual) =
    if not actual then
        Pass

    else
        Fail "Expected False but got True"


{-| Check Result is Ok.
-}
ok : Subject (Result e a) -> Expectation
ok (Subject actual) =
    case actual of
        Ok _ ->
            Pass

        Err e ->
            Fail ("Expected Ok but got Err: " ++ Debug.toString e)


{-| Check Result is Err.
-}
err : Subject (Result e a) -> Expectation
err (Subject actual) =
    case actual of
        Err _ ->
            Pass

        Ok v ->
            Fail ("Expected Err but got Ok: " ++ Debug.toString v)


{-| Check list contains an element.
-}
contains : a -> Subject (List a) -> Expectation
contains item (Subject actual) =
    if List.member item actual then
        Pass

    else
        Fail (Debug.toString item ++ " not found in list")


{-| Check list has specific length.
-}
hasLength : Int -> Subject (List a) -> Expectation
hasLength expected (Subject actual) =
    let
        actualLength =
            List.length actual
    in
    if actualLength == expected then
        Pass

    else
        Fail ("Expected length " ++ String.fromInt expected ++ " but got " ++ String.fromInt actualLength)


{-| Check list is empty.
-}
isEmpty : Subject (List a) -> Expectation
isEmpty (Subject actual) =
    if List.isEmpty actual then
        Pass

    else
        Fail ("Expected empty list but got " ++ String.fromInt (List.length actual) ++ " items")



-- RUNNING TESTS


{-| Result of a single test.
-}
type alias TestResult =
    { name : String
    , path : List String
    , outcome : TestOutcome
    }


type TestOutcome
    = Passed
    | Failed String
    | TodoTest
    | Skipped


{-| Summary of test run.
-}
type alias Summary =
    { passed : Int
    , failed : Int
    , todos : Int
    , skipped : Int
    , duration : Int
    , results : List TestResult
    }


{-| Run tests with default options.
-}
run : Test -> Summary
run =
    runWithOptions defaultOptions


type alias Options =
    { seed : Int
    , fuzzRuns : Int
    }


defaultOptions : Options
defaultOptions =
    { seed = 12345
    , fuzzRuns = 100
    }


{-| Run tests with specific options.
-}
runWithOptions : Options -> Test -> Summary
runWithOptions options testSuite =
    let
        results =
            runTest options [] testSuite

        passed =
            List.filter (\r -> r.outcome == Passed) results |> List.length

        failed =
            List.filter
                (\r ->
                    case r.outcome of
                        Failed _ ->
                            True

                        _ ->
                            False
                )
                results
                |> List.length

        todos =
            List.filter (\r -> r.outcome == TodoTest) results |> List.length

        skipped =
            List.filter (\r -> r.outcome == Skipped) results |> List.length
    in
    { passed = passed
    , failed = failed
    , todos = todos
    , skipped = skipped
    , duration = 0
    , results = results
    }


runTest : Options -> List String -> Test -> List TestResult
runTest options path testCase =
    case testCase of
        Single name fn ->
            [ { name = name
              , path = path
              , outcome =
                    case fn () of
                        Pass ->
                            Passed

                        Fail msg ->
                            Failed msg
              }
            ]

        Batch name tests ->
            List.concatMap (runTest options (path ++ [ name ])) tests

        Todo name ->
            [ { name = name
              , path = path
              , outcome = TodoTest
              }
            ]

        Skip t ->
            List.map (\r -> { r | outcome = Skipped }) (runTest options path t)



-- FUZZ TESTING


{-| A fuzzer generates random values for property testing.
-}
type Fuzzer a
    = Fuzzer (Int -> a)


{-| Create a fuzz test with one fuzzer.
-}
fuzz : Fuzzer a -> String -> (a -> Expectation) -> Test
fuzz (Fuzzer gen) name fn =
    Single name
        (\() ->
            let
                results =
                    List.range 1 100
                        |> List.map (\seed -> fn (gen seed))

                failures =
                    List.filterMap
                        (\r ->
                            case r of
                                Fail msg ->
                                    Just msg

                                Pass ->
                                    Nothing
                        )
                        results
            in
            case failures of
                [] ->
                    Pass

                first :: _ ->
                    Fail ("Failed after 100 tests: " ++ first)
        )


{-| Create a fuzz test with two fuzzers.
-}
fuzz2 : Fuzzer a -> Fuzzer b -> String -> (a -> b -> Expectation) -> Test
fuzz2 (Fuzzer genA) (Fuzzer genB) name fn =
    Single name
        (\() ->
            let
                results =
                    List.range 1 100
                        |> List.map (\seed -> fn (genA seed) (genB (seed * 31)))

                failures =
                    List.filterMap
                        (\r ->
                            case r of
                                Fail msg ->
                                    Just msg

                                Pass ->
                                    Nothing
                        )
                        results
            in
            case failures of
                [] ->
                    Pass

                first :: _ ->
                    Fail ("Failed after 100 tests: " ++ first)
        )


{-| Create a fuzz test with three fuzzers.
-}
fuzz3 : Fuzzer a -> Fuzzer b -> Fuzzer c -> String -> (a -> b -> c -> Expectation) -> Test
fuzz3 (Fuzzer genA) (Fuzzer genB) (Fuzzer genC) name fn =
    Single name
        (\() ->
            let
                results =
                    List.range 1 100
                        |> List.map (\seed -> fn (genA seed) (genB (seed * 31)) (genC (seed * 37)))

                failures =
                    List.filterMap
                        (\r ->
                            case r of
                                Fail msg ->
                                    Just msg

                                Pass ->
                                    Nothing
                        )
                        results
            in
            case failures of
                [] ->
                    Pass

                first :: _ ->
                    Fail ("Failed after 100 tests: " ++ first)
        )



-- BUILT-IN FUZZERS


{-| Fuzzer for integers.
-}
intFuzzer : Fuzzer Int
intFuzzer =
    Fuzzer
        (\seed ->
            let
                -- Simple LCG
                a =
                    1103515245

                c =
                    12345

                m =
                    2147483648
            in
            modBy 10000 (modBy m (a * seed + c)) - 5000
        )


{-| Fuzzer for floats.
-}
floatFuzzer : Fuzzer Float
floatFuzzer =
    Fuzzer
        (\seed ->
            let
                (Fuzzer intGen) =
                    intFuzzer
            in
            toFloat (intGen seed) / 100.0
        )


{-| Fuzzer for booleans.
-}
boolFuzzer : Fuzzer Bool
boolFuzzer =
    Fuzzer (\seed -> modBy 2 seed == 0)


{-| Fuzzer for strings.
-}
stringFuzzer : Fuzzer String
stringFuzzer =
    Fuzzer
        (\seed ->
            let
                len =
                    modBy 20 seed

                chars =
                    List.range 1 len
                        |> List.map (\i -> Char.fromCode (97 + modBy 26 (seed * i)))
            in
            String.fromList chars
        )


{-| Fuzzer for lists.
-}
listFuzzer : Fuzzer a -> Fuzzer (List a)
listFuzzer (Fuzzer gen) =
    Fuzzer
        (\seed ->
            let
                len =
                    modBy 10 seed
            in
            List.range 1 len
                |> List.map (\i -> gen (seed * i))
        )


{-| Fuzzer for Maybe values.
-}
maybeFuzzer : Fuzzer a -> Fuzzer (Maybe a)
maybeFuzzer (Fuzzer gen) =
    Fuzzer
        (\seed ->
            if modBy 4 seed == 0 then
                Nothing

            else
                Just (gen seed)
        )
