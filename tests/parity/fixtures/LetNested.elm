module Main exposing (main)

main : Int
main =
    let
        x = 5
        y = 
            let
                z = 10
            in
            z * 2
    in
    x + y
