module Main exposing (main)

main : Int
main =
    Maybe.withDefault 0 (Maybe.map3 (\a b c -> a + b + c) (Just 10) (Just 20) (Just 12))
