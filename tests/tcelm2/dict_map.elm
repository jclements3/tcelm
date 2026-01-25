-- expect: [("a", 2), ("b", 4)]
module Test exposing (main)

main =
    Dict.empty
        |> Dict.insert "a" 1
        |> Dict.insert "b" 2
        |> Dict.map (\k v -> v * 2)
        |> Dict.toList
