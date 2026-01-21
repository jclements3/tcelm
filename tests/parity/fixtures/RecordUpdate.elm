module Main exposing (main)

main : Int
main =
    let
        point = { x = 10, y = 20 }
        updated = { point | x = 100 }
    in
    updated.x + updated.y
