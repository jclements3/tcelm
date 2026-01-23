port module Tools.Format exposing (main)

{-| tcelm-format - Format Elm source code.

A code formatter for Elm that uses 94-column aligned style.

-}

import Format.Format as Format
import Json.Encode as Encode
import Parse.Module as Module
import Platform


port receiveSource : (String -> msg) -> Sub msg


port sendOutput : Encode.Value -> Cmd msg


type alias Model =
    ()


type Msg
    = GotSource String


main : Program () Model Msg
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = update
        , subscriptions = \_ -> receiveSource GotSource
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSource source ->
            let
                result =
                    format source
            in
            ( model, sendOutput result )


format : String -> Encode.Value
format source =
    case Module.parse source of
        Ok mod ->
            Encode.object
                [ ( "success", Encode.bool True )
                , ( "code", Encode.string (Format.format mod) )
                ]

        Err errors ->
            Encode.object
                [ ( "success", Encode.bool False )
                , ( "error", Encode.string "Parse error" )
                ]
