-- expect: Nothing
module Test exposing (main)

main =
    Dict.empty
        |> Dict.insert "foo" 42
        |> Dict.get "bar"
