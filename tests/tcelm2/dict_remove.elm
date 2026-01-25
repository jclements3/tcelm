-- expect: Nothing
module Test exposing (main)

main =
    Dict.singleton "key" 42
        |> Dict.remove "key"
        |> Dict.get "key"
