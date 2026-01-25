-- expect: 3
module Test exposing (main)

main =
    Set.empty
        |> Set.insert 1
        |> Set.insert 2
        |> Set.insert 3
        |> Set.insert 2
        |> Set.size
