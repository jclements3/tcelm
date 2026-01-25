-- expect: 7
module Test exposing (main)

main =
    let
        { a, b } = { a = 3, b = 4 }
    in
    a + b
