module Main exposing (main)

main : Int
main =
    Maybe.withDefault 0 (Maybe.map2 (\a b -> a + b) (Just 10) (Just 32))
