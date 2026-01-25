-- expect: 9
module Test exposing (main)

main =
    List.range 1 5
        |> List.filter (\x -> modBy 2 x /= 0)
        |> List.sum
