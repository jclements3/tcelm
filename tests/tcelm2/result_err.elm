-- expect: 999
module Test exposing (main)
main =
    case Err "error" of
        Ok n -> n
        Err _ -> 999
