-- expect: 30
module Test exposing (main)
main =
    case (10, 20) of
        (a, b) -> a + b
