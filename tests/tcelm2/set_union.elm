-- expect: 4
module Test exposing (main)

main =
    let
        s1 = Set.fromList [1, 2, 3]
        s2 = Set.fromList [2, 3, 4]
    in
    Set.union s1 s2 |> Set.size
