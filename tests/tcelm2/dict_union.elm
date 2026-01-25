-- expect: [(2, 200), (1, 10), (3, 30)]
module Test exposing (main)

main =
    Dict.toList (Dict.union (Dict.fromList [(1, 10), (3, 30)]) (Dict.fromList [(2, 200), (3, 300)]))
