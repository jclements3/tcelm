-- expect: Task.succeed 84
module Test exposing (main)

double x = x * 2

main =
    Task.map double (Task.succeed 42)
