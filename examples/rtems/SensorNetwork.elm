module SensorNetwork exposing (main)

{-| A multi-task sensor network example.

This demonstrates:
- Multiple Elm tasks communicating via channels
- Pub/sub pattern for sensor data
- Data aggregation task

Architecture:
  [Sensor1] --\
  [Sensor2] ----> [sensors channel] ----> [Aggregator] --> Console
  [Sensor3] --/

-}

import Rtems
import Rtems.Uart as Uart


-- SHARED TYPES


type alias SensorReading =
    { sensorId : Int
    , value : Float
    , timestamp : Int
    }


sensorsChannel : Rtems.Channel SensorReading
sensorsChannel =
    Rtems.channel "sensors"



-- ============================================================================
-- SENSOR TASK
-- ============================================================================


type alias SensorModel =
    { sensorId : Int
    , lastValue : Float
    , readingCount : Int
    }


type SensorMsg
    = SensorTick Int


sensorInit : Int -> SensorModel
sensorInit id =
    { sensorId = id
    , lastValue = 0.0
    , readingCount = 0
    }


sensorUpdate : SensorMsg -> SensorModel -> ( SensorModel, Cmd SensorMsg )
sensorUpdate msg model =
    case msg of
        SensorTick time ->
            let
                -- Simulate sensor reading (would be real GPIO/ADC in practice)
                newValue =
                    simulateSensor model.sensorId model.readingCount

                reading =
                    { sensorId = model.sensorId
                    , value = newValue
                    , timestamp = time
                    }
            in
            ( { model
                | lastValue = newValue
                , readingCount = model.readingCount + 1
              }
            , Rtems.publish sensorsChannel reading
            )


{-| Simulate different sensor behaviors based on ID
-}
simulateSensor : Int -> Int -> Float
simulateSensor sensorId count =
    let
        base =
            toFloat sensorId * 10.0

        oscillation =
            sin (toFloat count * 0.1) * 5.0
    in
    base + oscillation


sensorSubscriptions : SensorModel -> Sub SensorMsg
sensorSubscriptions _ =
    Rtems.every 50 SensorTick  -- 20 Hz sensor rate



-- ============================================================================
-- AGGREGATOR TASK
-- ============================================================================


type alias AggregatorModel =
    { readings : List SensorReading
    , windowSize : Int
    , totalReadings : Int
    }


type AggregatorMsg
    = NewReading SensorReading
    | ReportTick Int


aggregatorInit : AggregatorModel
aggregatorInit =
    { readings = []
    , windowSize = 10
    , totalReadings = 0
    }


aggregatorUpdate : AggregatorMsg -> AggregatorModel -> ( AggregatorModel, Cmd AggregatorMsg )
aggregatorUpdate msg model =
    case msg of
        NewReading reading ->
            let
                newReadings =
                    reading :: List.take (model.windowSize - 1) model.readings
            in
            ( { model
                | readings = newReadings
                , totalReadings = model.totalReadings + 1
              }
            , Cmd.none
            )

        ReportTick _ ->
            let
                -- Calculate statistics
                values =
                    List.map .value model.readings

                avg =
                    if List.isEmpty values then
                        0.0

                    else
                        List.sum values / toFloat (List.length values)

                minVal =
                    Maybe.withDefault 0 (List.minimum values)

                maxVal =
                    Maybe.withDefault 0 (List.maximum values)

                report =
                    "Aggregator | readings="
                        ++ String.fromInt model.totalReadings
                        ++ " avg="
                        ++ String.fromFloat (roundTo 2 avg)
                        ++ " min="
                        ++ String.fromFloat (roundTo 2 minVal)
                        ++ " max="
                        ++ String.fromFloat (roundTo 2 maxVal)
                        ++ "\n"
            in
            ( model, Uart.print report )


aggregatorSubscriptions : AggregatorModel -> Sub AggregatorMsg
aggregatorSubscriptions _ =
    Sub.batch
        [ Rtems.subscribe sensorsChannel |> Sub.map NewReading
        , Rtems.every 1000 ReportTick  -- Report every second
        ]


roundTo : Int -> Float -> Float
roundTo places x =
    let
        factor =
            toFloat (10 ^ places)
    in
    toFloat (round (x * factor)) / factor



-- ============================================================================
-- MAIN - Spawn all tasks
-- ============================================================================


main : Program () () ()
main =
    Rtems.worker
        { init = ()
        , update = \_ _ -> ( (), spawnNetwork )
        , subscriptions = \_ -> Sub.none
        }


spawnNetwork : Cmd ()
spawnNetwork =
    Cmd.batch
        [ -- Spawn sensors
          Rtems.spawn
            { init = sensorInit 1
            , update = sensorUpdate
            , subscriptions = sensorSubscriptions
            }
            |> Cmd.map (\_ -> ())
        , Rtems.spawn
            { init = sensorInit 2
            , update = sensorUpdate
            , subscriptions = sensorSubscriptions
            }
            |> Cmd.map (\_ -> ())
        , Rtems.spawn
            { init = sensorInit 3
            , update = sensorUpdate
            , subscriptions = sensorSubscriptions
            }
            |> Cmd.map (\_ -> ())

        -- Spawn aggregator
        , Rtems.spawn
            { init = aggregatorInit
            , update = aggregatorUpdate
            , subscriptions = aggregatorSubscriptions
            }
            |> Cmd.map (\_ -> ())
        ]
