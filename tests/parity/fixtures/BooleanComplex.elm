module Main exposing (main)

main : Int
main =
    if (True && False) || (not False && True) then 1 else 0
