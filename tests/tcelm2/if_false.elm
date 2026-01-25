-- expect: 20
module Test exposing (main)
main = if 1 < 0 then 10 else 20
