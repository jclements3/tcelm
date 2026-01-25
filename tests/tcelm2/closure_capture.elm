-- expect: 15
module Test exposing (main)

makeAdder : Int -> (Int -> Int)
makeAdder x =
    \y -> x + y

main =
    let
        add5 = makeAdder 5
    in
    add5 10
