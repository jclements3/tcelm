-- expect: "{"name":"test","value":42}"
module Test exposing (main)

main =
    Json.Encode.encode 0 (Json.Encode.object [ ("name", Json.Encode.string "test"), ("value", Json.Encode.int 42) ])
