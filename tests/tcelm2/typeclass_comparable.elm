-- expect: [1, 2, 3]
module Test exposing (main)

-- Test that comparable constraint works on List.sort with integers
main =
    List.sort [3, 1, 2]
