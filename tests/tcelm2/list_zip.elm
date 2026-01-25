-- expect: [(1, "a"), (2, "b"), (3, "c")]
module Test exposing (main)

main =
    List.zip [1, 2, 3] ["a", "b", "c"]
