module Main exposing (main)

-- Test List.sort on strings

main : Int
main =
    let
        unsorted =
            [ "banana", "apple", "cherry", "date" ]

        sorted =
            List.sort unsorted
    in
    -- After sorting: ["apple", "banana", "cherry", "date"]
    case sorted of
        "apple" :: "banana" :: "cherry" :: "date" :: [] ->
            1  -- Correct order

        _ ->
            0  -- Wrong order
