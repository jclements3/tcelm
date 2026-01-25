-- expect: 42
module Test exposing (main)

main =
    let
        maybes = [Just 30, Nothing, Just 12]
    in
    maybes
        |> List.filterMap identity
        |> List.sum
