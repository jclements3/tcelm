-- expect: "Ok 42"
module Test exposing (main)

main =
    Debug.toString (Json.Decode.decodeString Json.Decode.int "42")
