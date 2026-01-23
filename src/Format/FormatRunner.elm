port module Format.FormatRunner exposing (main)

{-| Test runner for the Elm formatter.

Compiles to JS and runs with Node.js to show formatted output.

-}

import Format.Format as Format
import Parse.Module as Module
import Platform


main : Program () () ()
main =
    Platform.worker
        { init = \_ -> ( (), output (runTests ()) )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


port output : String -> Cmd msg


runTests : () -> String
runTests _ =
    [ ( "Short record", shortRecord )
    , ( "Long record", longRecord )
    , ( "Very long record", veryLongRecord )
    , ( "Type alias", typeAlias )
    , ( "Very long type alias", veryLongTypeAlias )
    , ( "Union type", unionType )
    , ( "Function with args", functionWithArgs )
    , ( "Case expression", caseExpr )
    , ( "List", listExpr )
    , ( "Full module", fullModule )
    ]
        |> List.map runTest
        |> String.join "\n\n"


runTest : ( String, String ) -> String
runTest ( name, source ) =
    let
        header =
            "=== " ++ name ++ " ===\n"

        inputHeader =
            "--- Input ---\n" ++ source ++ "\n"

        result =
            case Module.parse source of
                Ok mod ->
                    "--- Output ---\n" ++ Format.format mod

                Err errors ->
                    "--- Parse Error ---\n" ++ Debug.toString errors
    in
    header ++ inputHeader ++ result



-- TEST CASES


shortRecord : String
shortRecord =
    """module Test exposing (..)

person = { firstName = "John", lastName = "Doe", age = 30 }
"""


longRecord : String
longRecord =
    """module Test exposing (..)

config = { host = "localhost", port_ = 8080, debug = True, logLevel = "info", maxConnections = 100 }
"""


veryLongRecord : String
veryLongRecord =
    """module Test exposing (..)

config = { host = "localhost", port_ = 8080, debug = True, logLevel = "info", maxConnections = 100, timeout = 30000, retryAttempts = 3 }
"""


typeAlias : String
typeAlias =
    """module Test exposing (..)

type alias Person = { firstName : String, lastName : String, age : Int, email : String }
"""


veryLongTypeAlias : String
veryLongTypeAlias =
    """module Test exposing (..)

type alias Person = { firstName : String, lastName : String, age : Int, email : String, address : String, phone : String }
"""


unionType : String
unionType =
    """module Test exposing (..)

type Msg = Click | Hover String | KeyPress Char | MouseMove Int Int
"""


functionWithArgs : String
functionWithArgs =
    """module Test exposing (..)

greet first last age =
    "Hello, " ++ first ++ " " ++ last ++ "! You are " ++ String.fromInt age ++ " years old."
"""


caseExpr : String
caseExpr =
    """module Test exposing (..)

handle msg =
    case msg of
        Click -> "clicked"
        Hover s -> "hovering: " ++ s
        KeyPress c -> "key: " ++ String.fromChar c
        MouseMove x y -> "mouse: " ++ String.fromInt x ++ "," ++ String.fromInt y
"""


listExpr : String
listExpr =
    """module Test exposing (..)

items = [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten" ]
"""


fullModule : String
fullModule =
    """module Example exposing (Person, Msg(..), greet, handle)

import Html exposing (Html, div, text)
import String

type alias Person = { firstName : String, lastName : String, age : Int }

type Msg = Click | Hover String | KeyPress Char | MouseMove Int Int

greet person =
    "Hello, " ++ person.firstName ++ " " ++ person.lastName

handle msg model =
    case msg of
        Click -> { model | clicked = True }
        Hover str -> { model | hovered = str }
        KeyPress char -> { model | lastKey = char }
        MouseMove x y -> { model | mouseX = x, mouseY = y }
"""
