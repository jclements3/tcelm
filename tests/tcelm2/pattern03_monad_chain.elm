-- expect: -1
module Test exposing (main)

safeDivide : Int -> Int -> Maybe Int
safeDivide x y =
    if y == 0 then
        Nothing
    else
        Just (x // y)

result : Maybe Int
result =
    Just 10
        |> Maybe.andThen (\x -> safeDivide x 0)
        |> Maybe.andThen (\x -> safeDivide x 2)

main =
    case result of
        Just n -> n
        Nothing -> -1
