-- expect: 2
module Test exposing (main)

-- Partition dict by value > 5
main =
    let
        dict = Dict.fromList [(1, 10), (2, 3), (3, 8)]
        result = Dict.partition (\k v -> v > 5) dict
        large = Tuple.first result
    in
    Dict.size large
