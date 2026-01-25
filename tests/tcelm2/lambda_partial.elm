-- expect: 15
module Test exposing (main)

main =
    let
        add = \a b -> a + b
        add10 = add 10
    in
    add10 5
