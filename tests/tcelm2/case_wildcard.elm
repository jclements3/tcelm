-- expect: 999
module Test exposing (main)
main =
    case 99 of
        1 -> 100
        2 -> 200
        _ -> 999
