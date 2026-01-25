-- expect: [12, 15, 18]
module Test exposing (main)

add3 a b c =
    a + b + c

main =
    List.map3 add3 [1, 2, 3] [4, 5, 6] [7, 8, 9]
