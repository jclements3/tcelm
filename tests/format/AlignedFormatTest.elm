module AlignedFormatTest exposing (main)

{-| Test file for verifying the 94-column aligned formatter.

This file contains various Elm constructs that should be formatted
according to the aligned style.

-}


-- Short records should be on one line


person =
    { firstName = "John", lastName = "Doe", age = 30 }



-- Longer records should wrap with aligned fields and K&R closing


config =
    { host = "localhost"
    , port_ = 8080
    , debug = True
    , logLevel = "info"
    }



-- Type aliases should have aligned fields


type alias Person =
    { firstName : String
    , lastName : String
    , age : Int
    }



-- Lists should wrap when needed with K&R closing


items =
    [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven" ]



-- Function applications should align continuation arguments


result =
    someFunction arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12



-- Union types should have aligned variants


type Msg
    = Click
    | Hover String
    | KeyPress Char
    | MouseMove Int Int



-- Case expressions should have aligned patterns when possible


handleMsg : Msg -> Model -> Model
handleMsg msg model =
    case msg of
        Click ->
            handleClick model

        Hover str ->
            handleHover str model

        KeyPress char ->
            handleKeyPress char model

        MouseMove x y ->
            handleMouseMove x y model



-- Helper functions


someFunction a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
    a1


handleClick model =
    model


handleHover str model =
    model


handleKeyPress char model =
    model


handleMouseMove x y model =
    model


main =
    ()
