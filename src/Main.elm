module Main exposing (main)

{-| A simple test program for the tcelm parser.
-}

import AST.Source as Src
import Parse.Module as Module
import Parse.Primitives
import Platform
import Reporting.Error.Syntax


main : Program () () ()
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


{-| Test parsing a simple Elm module.
-}
testParse : Result (List (Parse.Primitives.DeadEnd Reporting.Error.Syntax.Problem)) Src.Module
testParse =
    Module.parse testCode


testCode : String
testCode =
    """module Example exposing (add, multiply)

import List

add : Int -> Int -> Int
add a b =
    a + b

multiply : Int -> Int -> Int
multiply x y =
    x * y

type alias Point =
    { x : Float
    , y : Float
    }

type Color
    = Red
    | Green
    | Blue

distance : Point -> Point -> Float
distance p1 p2 =
    let
        dx = p1.x - p2.x
        dy = p1.y - p2.y
    in
    sqrt (dx * dx + dy * dy)
"""
