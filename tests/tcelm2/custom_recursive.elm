-- expect: 15
module Test exposing (main)

type Tree
    = Leaf Int
    | Node Tree Tree

sumTree : Tree -> Int
sumTree tree =
    case tree of
        Leaf n -> n
        Node left right -> sumTree left + sumTree right

main =
    let
        tree = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Node (Leaf 4) (Leaf 5)))
    in
    sumTree tree
