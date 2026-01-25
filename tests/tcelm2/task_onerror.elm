-- expect: Task.succeed 0
module Test exposing (main)

recover err = Task.succeed 0

main =
    Task.onError recover (Task.fail "error")
