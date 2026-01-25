-- expect: 7
module Test exposing (main)

pair : (Int, String)
pair = Tuple.mapBoth (\x -> x + 1) String.toUpper (1, "hello")

main =
    case pair of
        (n, s) -> n + String.length s
