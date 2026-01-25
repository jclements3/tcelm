-- expect: ([2, 4, 6], [1, 3, 5])
module Test exposing (main)

isEven n = modBy 2 n == 0

main =
    List.partition isEven [1, 2, 3, 4, 5, 6]
