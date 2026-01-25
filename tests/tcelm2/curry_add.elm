-- expect: 15
module Test exposing (main)

addTuple t =
    case t of
        (a, b) -> a + b

main =
    curry addTuple 5 10
