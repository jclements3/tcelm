-- expect: Task.fail "error"
module Test exposing (main)

double x = x * 2

main =
    Task.map double (Task.fail "error")
