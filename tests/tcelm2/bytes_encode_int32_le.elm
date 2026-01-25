-- expect: 4
module Test exposing (main)

main =
    Bytes.width (Bytes.Encode.encode (Bytes.Encode.signedInt32 Bytes.LE 12345678))
