-- expect: 31
module Test exposing (main)

type alias User =
    { name : String
    , age : Int
    }

alice : User
alice =
    { name = "Alice"
    , age = 30
    }

olderAlice : User
olderAlice = { alice | age = alice.age + 1 }

main = olderAlice.age
