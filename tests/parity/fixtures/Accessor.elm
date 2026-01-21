module Main exposing (main)

type alias Point =
    { x : Int
    , y : Int
    }

main : Int
main =
    let
        point = { x = 10, y = 32 }
    in
    point |> .x |> (\n -> n + point.y)
