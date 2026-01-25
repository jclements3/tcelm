-- expect: 11
module Test exposing (main)

makeCounter : Int -> (Int -> Int)
makeCounter start =
    \n -> start + n

main =
    let
        counter = makeCounter 1
    in
    counter 10
