-- expect: [("b", 2)]
module Test exposing (main)

main =
    Dict.empty
        |> Dict.insert "a" 1
        |> Dict.insert "b" 2
        |> Dict.insert "c" 3
        |> Dict.filter (\k v -> v == 2)
        |> Dict.toList
