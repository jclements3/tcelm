-- expect: ([1, 2, 3], ["a", "b", "c"])
module Test exposing (main)

main =
    List.unzip [(1, "a"), (2, "b"), (3, "c")]
