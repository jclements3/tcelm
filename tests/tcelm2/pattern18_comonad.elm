-- expect: 2
module Test exposing (main)

type alias Zipper a =
    { left : List a
    , focus : a
    , right : List a
    }

extract : Zipper a -> a
extract z = z.focus

moveRight : Zipper a -> Maybe (Zipper a)
moveRight z =
    case z.right of
        [] -> Nothing
        r :: rs ->
            Just { left = z.focus :: z.left, focus = r, right = rs }

moveLeft : Zipper a -> Maybe (Zipper a)
moveLeft z =
    case z.left of
        [] -> Nothing
        l :: ls ->
            Just { left = ls, focus = l, right = z.focus :: z.right }

zipper : Zipper Int
zipper =
    { left = [1]
    , focus = 2
    , right = [3, 4]
    }

main = extract zipper
