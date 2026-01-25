-- expect: 15
module Test exposing (main)
main =
    let f = \x -> x + 5
    in f 10
