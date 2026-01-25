-- expect: 20
module Test exposing (main)

safeParseInt : String -> Maybe Int
safeParseInt = String.toInt

safeDivideBy2 : Int -> Maybe Int
safeDivideBy2 n =
    if n == 0 then Nothing else Just (n // 2)

addTen : Int -> Maybe Int
addTen n = Just (n + 10)

pipeline : String -> Maybe Int
pipeline s =
    safeParseInt s
        |> Maybe.andThen safeDivideBy2
        |> Maybe.andThen addTen

result1 : Maybe Int
result1 = pipeline "20"

main =
    case result1 of
        Just n -> n
        Nothing -> -1
