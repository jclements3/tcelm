-- expect: 63
module Test exposing (main)

main = List.sum (List.indexedMap (\i x -> i + x) [10, 20, 30])
