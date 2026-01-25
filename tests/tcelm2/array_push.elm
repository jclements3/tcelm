-- expect: [1, 2, 3, 4]
module Test exposing (main)

main =
    Array.fromList [1, 2, 3]
        |> Array.push 4
        |> Array.toList
