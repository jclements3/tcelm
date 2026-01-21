module Main exposing (main)

main : Int
main =
    Maybe.withDefault 0 (String.toFloat "42.5")
