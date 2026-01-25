-- expect: 123
module Test exposing (main)

type alias Config =
    { baseValue : Int
    , multiplier : Int
    }

config : Config
config =
    { baseValue = 100
    , multiplier = 23
    }

result : Int
result = config.baseValue + (1 * config.multiplier)

main = result
