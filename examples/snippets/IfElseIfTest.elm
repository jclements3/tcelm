module IfElseIfTest exposing (main)

{-| Test if/else if/else chain
-}


main : Int
main =
    let
        x =
            15
    in
    if x < 10 then
        1

    else if x < 20 then
        2

    else if x < 30 then
        3

    else
        4
