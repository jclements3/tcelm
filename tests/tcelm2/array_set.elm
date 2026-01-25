-- expect: [10, 99, 30]
module Test exposing (main)

main =
    Array.fromList [10, 20, 30]
        |> Array.set 1 99
        |> Array.toList
