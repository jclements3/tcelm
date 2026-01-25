-- expect: False
module Test exposing (main)

main =
    Set.empty
        |> Set.insert 1
        |> Set.insert 2
        |> Set.remove 1
        |> Set.member 1
