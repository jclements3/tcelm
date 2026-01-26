module FunctionTest exposing (main)

{-| Test basic function support
-}


double : Int -> Int
double x =
    x * 2


add : Int -> Int -> Int
add a b =
    a + b


main : Int
main =
    add (double 5) 3
