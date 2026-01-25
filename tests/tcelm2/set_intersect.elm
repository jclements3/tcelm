-- expect: 2
module Test exposing (main)

main =
    let
        s1 = Set.fromList [1, 2, 3]
        s2 = Set.fromList [2, 3, 4]
    in
    Set.intersect s1 s2 |> Set.size
