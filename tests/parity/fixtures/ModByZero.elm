module Main exposing (main)

main : Int
main =
    -- modBy 0 should typically cause issues, but let's test with non-zero
    modBy 7 15
