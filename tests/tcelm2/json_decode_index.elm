-- expect: "Ok 2"
module Test exposing (main)

main =
    Debug.toString (Json.Decode.decodeString (Json.Decode.index 1 Json.Decode.int) "[1,2,3]")
