-- expect: "[1,2,3]"
module Test exposing (main)

main =
    Json.Encode.encode 0 (Json.Encode.list Json.Encode.int [1, 2, 3])
