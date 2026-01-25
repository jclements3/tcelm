-- expect: 60
module Test exposing (main)
main =
    let
        a = case 1 of
            1 -> 10
            _ -> 0
        b = case 2 of
            2 -> 20
            _ -> 0
        c = case 3 of
            3 -> 30
            _ -> 0
    in
    a + b + c
