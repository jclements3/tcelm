module IfElseIfSimple exposing (main)

{-| Test if/else if/else chain without let
-}


main : Int
main =
    if 15 < 10 then
        1

    else if 15 < 20 then
        2

    else if 15 < 30 then
        3

    else
        4
