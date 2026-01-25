-- expect: 100
module Test exposing (main)
main =
    case 1 of
        1 -> 100
        2 -> 200
        _ -> 0
