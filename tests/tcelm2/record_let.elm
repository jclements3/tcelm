-- expect: 40
module Test exposing (main)
main =
    let
        person = { name = "Bob", age = 30 }
    in
    person.age + 10
