-- expect: 1
module Test exposing (main)

main =
    Bytes.width (Bytes.Encode.encode (Bytes.Encode.signedInt8 42))
