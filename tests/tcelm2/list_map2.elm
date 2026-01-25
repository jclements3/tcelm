-- expect: [5, 7, 9]
module Test exposing (main)

add a b =
    a + b

main =
    List.map2 add [1, 2, 3] [4, 5, 6]
