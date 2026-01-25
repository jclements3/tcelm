-- expect: 25
module Test exposing (main)
main =
    let
        offset = 10
        f = \x -> x + offset
    in
    f 15
