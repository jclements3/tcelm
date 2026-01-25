-- expect: 115
module Test exposing (main)
main =
    let
        outer =
            let
                inner = 100
                y = 10
            in
            inner + y
        z = 5
    in
    outer + z
