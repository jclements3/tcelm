-- expect: [1, 2, 3, 10, 20]
module Test exposing (main)

-- Sort by negation to get descending, then take abs to get original values
-- Actually simpler: sort by identity
main =
    List.sortBy (\x -> x) [10, 3, 1, 20, 2]
