-- expect: [1, 2]
module Test exposing (main)

main =
    Dict.empty
        |> Dict.insert "a" 1
        |> Dict.insert "b" 2
        |> Dict.values
