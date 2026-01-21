module Main exposing (main)

import Bitwise

main : Int
main =
    Bitwise.and (Bitwise.or 0xFF 0x0F) (Bitwise.complement 0xF0)
