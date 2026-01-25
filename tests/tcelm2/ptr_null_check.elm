-- expect: 1
module Test exposing (main)

main =
    if Ptr.isNull Ptr.null then
        1
    else
        0
