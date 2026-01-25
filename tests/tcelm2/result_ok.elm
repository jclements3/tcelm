-- expect: 10
module Test exposing (main)
main =
    case Ok 10 of
        Ok n -> n
        Err _ -> 0
