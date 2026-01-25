-- expect: Task.succeed 7
module Test exposing (main)

add a b = a + b

main =
    Task.map2 add (Task.succeed 3) (Task.succeed 4)
