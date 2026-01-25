-- expect: 3
module Test exposing (main)

main =
    Bytes.width (Bytes.Encode.encode (Bytes.Encode.sequence [Bytes.Encode.unsignedInt8 1, Bytes.Encode.unsignedInt8 2, Bytes.Encode.unsignedInt8 3]))
