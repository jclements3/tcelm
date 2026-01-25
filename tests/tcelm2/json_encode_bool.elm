-- expect: "true"
module Test exposing (main)

main =
    Json.Encode.encode 0 (Json.Encode.bool True)
