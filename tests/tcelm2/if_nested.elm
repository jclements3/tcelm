-- expect: 30
module Test exposing (main)
main =
    if 1 > 2 then 10
    else if 2 > 3 then 20
    else 30
