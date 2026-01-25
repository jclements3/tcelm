-- expect: [1, 3, 7, 12]
module Test exposing (main)

main =
    String.indexes "an" "banana and pan"
