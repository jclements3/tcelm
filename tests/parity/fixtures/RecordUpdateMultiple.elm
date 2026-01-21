module Main exposing (main)

main : Int
main =
    let
        point = { x = 1, y = 2, z = 3 }
        updated = { point | x = 10, z = 30 }
    in
    updated.x + updated.y + updated.z
