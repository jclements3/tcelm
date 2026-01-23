module Main exposing (main)

-- Test List.sort on integers

main : Int
main =
    let
        unsorted =
            [ 5, 2, 8, 1, 9, 3, 7, 4, 6 ]

        sorted =
            List.sort unsorted
    in
    -- Sum should be 45 (1+2+3+4+5+6+7+8+9)
    -- Verify by checking first element is 1 and last element is 9
    case sorted of
        1 :: rest ->
            case List.reverse rest of
                9 :: _ ->
                    45  -- Correct: starts with 1, ends with 9

                _ ->
                    -1  -- Error: doesn't end with 9

        _ ->
            -2  -- Error: doesn't start with 1
