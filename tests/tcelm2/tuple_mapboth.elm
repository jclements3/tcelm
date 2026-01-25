-- expect: (2, "HELLO")
module Test exposing (main)

main =
    Tuple.mapBoth (\x -> x + 1) String.toUpper (1, "hello")
