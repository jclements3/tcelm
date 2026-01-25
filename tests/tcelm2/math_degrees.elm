-- expect: 1
module Test exposing (main)

-- sin(90 degrees) should be approximately 1.0
-- We round because of floating point precision
main =
    round (sin (degrees 90.0))
