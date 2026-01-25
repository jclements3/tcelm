-- expect: "Ok Nothing"
module Test exposing (main)

main =
    Debug.toString (Json.Decode.decodeString (Json.Decode.maybe Json.Decode.int) "\"hello\"")
