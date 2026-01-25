-- expect: "Ok True"
module Test exposing (main)

main =
    Debug.toString (Json.Decode.decodeString Json.Decode.bool "true")
