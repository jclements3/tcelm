-- expect: 31
module Test exposing (main)

type alias Person =
    { name : String
    , age : Int
    }

main =
    let
        alice = { name = "Alice", age = 30 }
        older = { alice | age = 31 }
    in
    older.age
