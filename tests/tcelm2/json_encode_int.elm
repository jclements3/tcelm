-- expect: "42"
module Test exposing (main)

main =
    Json.Encode.encode 0 (Json.Encode.int 42)
