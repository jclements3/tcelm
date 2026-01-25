-- expect: 6
module Test exposing (main)

main = List.sum (List.concatMap (\x -> [x, x]) [1, 2])
