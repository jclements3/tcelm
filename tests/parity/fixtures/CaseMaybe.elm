module Main exposing (main)

getValue : Maybe Int -> Int
getValue m =
    case m of
        Just x -> x
        Nothing -> 0

main : Int
main =
    getValue (Just 42)
