-- expect: 15
module Test exposing (main)

add : Int -> Int -> Int
add x y = x + y

main =
    let
        add5 = add 5
    in
    add5 10
