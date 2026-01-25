-- expect: Just 100
module Test exposing (main)

main =
    Dict.singleton "key" 100
        |> Dict.get "key"
