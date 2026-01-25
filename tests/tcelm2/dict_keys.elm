-- expect: ["a", "b"]
module Test exposing (main)

main =
    Dict.empty
        |> Dict.insert "a" 1
        |> Dict.insert "b" 2
        |> Dict.keys
