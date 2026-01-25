-- expect: Task.succeed 52
module Test exposing (main)

addTen x = Task.succeed (x + 10)

main =
    Task.andThen addTen (Task.succeed 42)
