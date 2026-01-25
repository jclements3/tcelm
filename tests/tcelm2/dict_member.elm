-- expect: True
module Test exposing (main)

main =
    Dict.singleton "hello" 1
        |> Dict.member "hello"
