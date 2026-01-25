-- expect: 65
module Test exposing (main)

main =
    let
        c = 'A'
    in
    case c of
        'A' -> 65
        'B' -> 66
        _ -> 0
