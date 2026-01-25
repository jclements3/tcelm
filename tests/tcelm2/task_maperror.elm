-- expect: Task.fail "ERROR"
module Test exposing (main)

main =
    Task.mapError String.toUpper (Task.fail "error")
