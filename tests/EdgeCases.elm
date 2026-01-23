module Main exposing (main)

{-| Test edge cases: lambdas in records, Maybe.andThen with tuple patterns -}


-- Test 1: Lambda in record field
type alias Predicate =
    { name : String
    , check : Int -> Bool
    }


isPositive : Predicate
isPositive =
    { name = "positive"
    , check = \x -> x > 0
    }


-- Test 2: Maybe.andThen with tuple pattern
parsePair : String -> Maybe ( Int, Int )
parsePair s =
    if s == "1,2" then
        Just ( 1, 2 )
    else
        Nothing


sumPair : Maybe ( Int, Int ) -> Maybe Int
sumPair maybePair =
    Maybe.andThen (\( a, b ) -> Just (a + b)) maybePair


-- Test 3: Lambda with record pattern in record field
type alias Processor =
    { name : String
    , process : { x : Int, y : Int } -> Int
    }


adder : Processor
adder =
    { name = "adder"
    , process = \{ x, y } -> x + y
    }


-- Main test
main : Int
main =
    let
        -- Test lambda in record
        t1 = if isPositive.check 5 then 1 else 0

        -- Test Maybe.andThen with tuple
        t2 = case sumPair (Just ( 3, 4 )) of
            Just n -> n
            Nothing -> 0

        -- Test lambda with record pattern in record
        t3 = adder.process { x = 2, y = 3 }

        -- Test chained andThen with pipe (inline)
        t4 = case parsePair "1,2" |> Maybe.andThen (\( a, b ) -> Just (a * b)) of
            Just n -> n
            Nothing -> 0
    in
    t1 + t2 + t3 + t4
