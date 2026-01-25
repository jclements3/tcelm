-- expect: "Just 42"
module Test exposing (main)

main =
    let
        encoded = Bytes.Encode.encode (Bytes.Encode.signedInt32 Bytes.LE 42)
        decoded = Bytes.Decode.decode (Bytes.Decode.signedInt32 Bytes.LE) encoded
    in
    Debug.toString decoded
