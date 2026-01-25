-- expect: Task.succeed [1, 2, 3]
module Test exposing (main)

main =
    Task.sequence [ Task.succeed 1, Task.succeed 2, Task.succeed 3 ]
