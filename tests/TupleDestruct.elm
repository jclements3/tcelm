module Main exposing (main)

{-| Test tuple destructuring in let bindings -}


{-| Simple 2-tuple destructuring -}
test2Tuple : Int -> Int
test2Tuple n =
    let
        ( a, b ) = makePair n
    in
    a + b


{-| Make a pair from a number -}
makePair : Int -> ( Int, Int )
makePair n =
    ( n, n * 2 )


{-| Chained tuple destructuring -}
testChained : Int -> Int
testChained seed =
    let
        ( x, seed1 ) = step seed
        ( y, seed2 ) = step seed1
        ( z, _ ) = step seed2
    in
    x + y + z


{-| Step function that returns (value, newSeed) -}
step : Int -> ( Int, Int )
step s =
    let
        next = (s * 1103515245 + 12345) |> modBy 1000000
    in
    ( next |> modBy 100, next )


{-| 3-tuple destructuring -}
test3Tuple : Int -> Int
test3Tuple n =
    let
        ( a, b, c ) = makeTriple n
    in
    a + b + c


{-| Make a triple from a number -}
makeTriple : Int -> ( Int, Int, Int )
makeTriple n =
    ( n, n * 2, n * 3 )


{-| Nested tuple destructuring -}
testNested : Int -> Int
testNested n =
    let
        ( ( a, b ), c ) = makeNestedPair n
    in
    a + b + c


{-| Make a nested pair -}
makeNestedPair : Int -> ( ( Int, Int ), Int )
makeNestedPair n =
    ( ( n, n + 1 ), n + 2 )


{-| Main test -}
main : Int
main =
    let
        -- Test 2-tuple: (5, 10) -> 15
        t2 = test2Tuple 5

        -- Test chained: 3 step results
        tc = testChained 42

        -- Test 3-tuple: (3, 6, 9) -> 18
        t3 = test3Tuple 3

        -- Test nested: ((4, 5), 6) -> 15
        tn = testNested 4
    in
    if t2 == 15 && t3 == 18 && tn == 15 then
        1
    else
        0
