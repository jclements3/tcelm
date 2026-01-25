-- expect: 10
module Test exposing (main)

add : Int -> Int -> Int
add = (+)

main = add 5 5
