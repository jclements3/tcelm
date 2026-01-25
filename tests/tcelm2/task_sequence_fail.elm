-- expect: Task.fail "oops"
module Test exposing (main)

main =
    Task.sequence [ Task.succeed 1, Task.fail "oops", Task.succeed 3 ]
