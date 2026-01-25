-- expect: Task.succeed 30
module Test exposing (main)

main =
    do
        x <- Task.succeed 10
        y <- Task.succeed 20
        Task.succeed (x + y)
