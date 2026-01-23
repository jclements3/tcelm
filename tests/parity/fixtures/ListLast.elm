module Main exposing (main)

-- Test List.last

main : Int
main =
    let
        items =
            [ 10, 20, 30, 40, 50 ]
    in
    case List.last items of
        Just 50 ->
            50  -- Correct

        Just _ ->
            -1  -- Wrong value

        Nothing ->
            -2  -- Should not be Nothing
