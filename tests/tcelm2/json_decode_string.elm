-- expect: "Ok \"hello\""
module Test exposing (main)

main =
    Debug.toString (Json.Decode.decodeString Json.Decode.string "\"hello\"")
