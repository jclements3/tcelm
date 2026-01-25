-- expect: Just 30
module Test exposing (main)

main =
    List.getAt 2 [10, 20, 30, 40]
