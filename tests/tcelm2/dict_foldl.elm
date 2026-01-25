-- expect: 6
module Test exposing (main)

main =
    Dict.empty
        |> Dict.insert "a" 1
        |> Dict.insert "b" 2
        |> Dict.insert "c" 3
        |> Dict.foldl (\k v acc -> acc + v) 0
