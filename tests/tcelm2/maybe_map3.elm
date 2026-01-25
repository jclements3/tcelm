-- expect: 60
module Test exposing (main)

add3 a b c =
    a + b + c

main =
    Maybe.map3 add3 (Just 10) (Just 20) (Just 30)
        |> Maybe.withDefault 0
