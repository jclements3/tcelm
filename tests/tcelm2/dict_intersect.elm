-- expect: [(2, 20)]
module Test exposing (main)

main =
    Dict.toList (Dict.intersect (Dict.fromList [(1, 10), (2, 20)]) (Dict.fromList [(2, 200), (3, 300)]))
