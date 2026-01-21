module Main exposing (main)

main : Int
main =
    Maybe.withDefault 0 (Maybe.map5 (\a b c d e -> a + b + c + d + e) (Just 8) (Just 8) (Just 8) (Just 8) (Just 10))
