module Main exposing (main)

double : Int -> Int
double x =
    x * 2

triple : Int -> Int
triple x =
    x * 3

main : Int
main =
    double (triple 7)
