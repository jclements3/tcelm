module Main exposing (main)

import Bitwise

main : Int
main =
    Bitwise.shiftLeftBy 2 (Bitwise.shiftRightBy 1 16)
