-- expect: Just 30
module Test exposing (main)

main =
    Array.fromList [10, 20, 30, 40]
        |> Array.get 2
