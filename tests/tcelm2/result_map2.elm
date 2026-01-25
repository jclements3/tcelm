-- expect: Ok 30
module Test exposing (main)

add a b =
    a + b

main =
    Result.map2 add (Ok 10) (Ok 20)
