-- expect: ([4, 2], [5, 3, 1])
module Test exposing (main)

isEven x =
    modBy 2 x == 0

main =
    let
        set = Set.fromList [1, 2, 3, 4, 5]
        result = Set.partition isEven set
        evens = Tuple.first result
        odds = Tuple.second result
    in
    (Set.toList evens, Set.toList odds)
