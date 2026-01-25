-- expect: 75
module Test exposing (main)

type Shape = Circle Int | Rectangle Int Int

area : Shape -> Int
area shape =
    case shape of
        Circle r -> r * r * 3
        Rectangle w h -> w * h

main = area (Circle 5)
