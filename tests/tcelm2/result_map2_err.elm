-- expect: Err "bad"
module Test exposing (main)

add a b =
    a + b

main =
    Result.map2 add (Err "bad") (Ok 20)
