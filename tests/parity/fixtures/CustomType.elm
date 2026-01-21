module Main exposing (main)

type Color
    = Red
    | Green
    | Blue

toInt : Color -> Int
toInt color =
    case color of
        Red -> 1
        Green -> 2
        Blue -> 3

main : Int
main =
    toInt Green
