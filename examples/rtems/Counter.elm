module Counter exposing (main)

{-| A simple counter that runs at 10 Hz on RTEMS.

This demonstrates:
- The Elm Architecture on RTEMS
- Time-based subscriptions
- Console output

-}

import Rtems
import Rtems.Uart as Uart


-- MODEL


type alias Model =
    { count : Int
    , running : Bool
    }


init : Model
init =
    { count = 0
    , running = True
    }



-- MSG


type Msg
    = Tick Int
    | Start
    | Stop
    | Reset



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if model.running then
                ( { model | count = model.count + 1 }
                , Uart.print ("Count: " ++ String.fromInt model.count ++ "\n")
                )

            else
                ( model, Cmd.none )

        Start ->
            ( { model | running = True }, Cmd.none )

        Stop ->
            ( { model | running = False }, Cmd.none )

        Reset ->
            ( { model | count = 0 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Rtems.every 100 Tick  -- 100ms period = 10 Hz



-- MAIN


main : Program () Model Msg
main =
    Rtems.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
