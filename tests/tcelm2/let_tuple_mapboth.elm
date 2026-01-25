-- expect: 10
module Test exposing (main)

main =
    let
        result = Tuple.mapBoth (\x -> x * 2) (\x -> x * 3) (2, 2)
    in
    Tuple.first result + Tuple.second result
