-- expect: 35
module Test exposing (main)
main =
    let
        result =
            case 1 of
                1 -> 30
                _ -> 0
        x = 5
    in
    result + x
