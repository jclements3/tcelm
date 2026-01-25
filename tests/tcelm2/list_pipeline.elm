-- expect: 28
module Test exposing (main)
main =
    [1, 2, 3, 4, 5]
        |> List.filter (\x -> x > 1)
        |> List.map (\x -> x * 2)
        |> List.sum
