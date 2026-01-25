-- expect: "Ok 123"
module Test exposing (main)

main =
    Debug.toString (Json.Decode.decodeString (Json.Decode.field "x" Json.Decode.int) "{\"x\":123}")
