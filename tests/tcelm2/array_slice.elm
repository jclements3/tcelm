-- expect: [20, 30, 40]
module Test exposing (main)

main =
    Array.fromList [10, 20, 30, 40, 50]
        |> Array.slice 1 4
        |> Array.toList
