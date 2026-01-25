-- expect: "null"
module Test exposing (main)

main =
    Json.Encode.encode 0 Json.Encode.null
