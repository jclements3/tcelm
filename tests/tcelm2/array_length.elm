-- expect: 5
module Test exposing (main)

main =
    Array.fromList [1, 2, 3, 4, 5]
        |> Array.length
