module ControlLoop exposing (main)

{-| A 240 Hz real-time control loop for HWIL simulation.

This demonstrates:
- High-frequency real-time operation (240 Hz)
- Tick-based timing for precise control
- Sensor/actuator model
- State machine pattern

At 1000 ticks/second, 4 ticks = 4ms = 250 Hz (close to 240 Hz)
For exact 240 Hz, configure RTEMS for 240 ticks/second.

-}

import Rtems
import Rtems.Uart as Uart


-- MODEL


type alias Model =
    { -- Simulation state
      position : Float
    , velocity : Float
    , acceleration : Float

    -- Control state
    , setpoint : Float
    , error : Float
    , integralError : Float
    , lastError : Float

    -- PID gains
    , kp : Float
    , ki : Float
    , kd : Float

    -- Timing
    , frameCount : Int
    , dt : Float  -- Time step in seconds
    }


init : Model
init =
    { position = 0.0
    , velocity = 0.0
    , acceleration = 0.0
    , setpoint = 10.0
    , error = 0.0
    , integralError = 0.0
    , lastError = 0.0
    , kp = 2.0
    , ki = 0.1
    , kd = 0.5
    , frameCount = 0
    , dt = 1.0 / 240.0  -- 240 Hz
    }



-- MSG


type Msg
    = Frame Int
    | SetSetpoint Float
    | SetGains Float Float Float



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            let
                -- PID controller
                error =
                    model.setpoint - model.position

                integralError =
                    model.integralError + error * model.dt

                derivativeError =
                    (error - model.lastError) / model.dt

                controlOutput =
                    model.kp * error
                        + model.ki * integralError
                        + model.kd * derivativeError

                -- Apply control as acceleration (simplified dynamics)
                newAcceleration =
                    clamp -100.0 100.0 controlOutput

                -- Integrate dynamics
                newVelocity =
                    model.velocity + newAcceleration * model.dt

                newPosition =
                    model.position + newVelocity * model.dt

                newModel =
                    { model
                        | position = newPosition
                        , velocity = newVelocity
                        , acceleration = newAcceleration
                        , error = error
                        , integralError = integralError
                        , lastError = error
                        , frameCount = model.frameCount + 1
                    }

                -- Log every 240 frames (once per second)
                cmd =
                    if modBy 240 model.frameCount == 0 then
                        Uart.print
                            ("Frame "
                                ++ String.fromInt model.frameCount
                                ++ " | pos="
                                ++ String.fromFloat (roundTo 3 newPosition)
                                ++ " vel="
                                ++ String.fromFloat (roundTo 3 newVelocity)
                                ++ " err="
                                ++ String.fromFloat (roundTo 3 error)
                                ++ "\n"
                            )

                    else
                        Cmd.none
            in
            ( newModel, cmd )

        SetSetpoint sp ->
            ( { model | setpoint = sp }, Cmd.none )

        SetGains p i d ->
            ( { model | kp = p, ki = i, kd = d }, Cmd.none )


clamp : Float -> Float -> Float -> Float
clamp lo hi x =
    if x < lo then
        lo

    else if x > hi then
        hi

    else
        x


roundTo : Int -> Float -> Float
roundTo places x =
    let
        factor =
            toFloat (10 ^ places)
    in
    toFloat (round (x * factor)) / factor



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    -- At 1000 ticks/sec, 4 ticks ≈ 250 Hz
    -- Configure RTEMS ticks appropriately for exact 240 Hz
    Rtems.everyTick 4 Frame



-- MAIN


main : Program () Model Msg
main =
    Rtems.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
