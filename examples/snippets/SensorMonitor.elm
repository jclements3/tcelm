-- SensorMonitor.elm
-- Example tcelm program demonstrating RTEMS integration
--
-- This monitors simulated sensors, aggregates data, and logs to console.
-- Runs identically on localhost (pthreads) and NUC (RTEMS).

module SensorMonitor exposing (main)

import Task exposing (Task)
import Channel exposing (Channel)
import MVar exposing (MVar)
import Timer
import IO


-- Types

type alias SensorReading =
    { sensorId : Int
    , value : Float
    , timestamp : Int
    }

type alias SystemState =
    { totalReadings : Int
    , averageTemp : Float
    , maxTemp : Float
    , minTemp : Float
    }

type Msg
    = SensorData SensorReading
    | Tick Int
    | Shutdown


-- Sensor task: generates readings and sends to channel

sensorTask : Int -> Channel SensorReading -> Task x ()
sensorTask sensorId channel =
    let
        loop count =
            let
                -- Simulate sensor reading (would be real hardware on NUC)
                reading =
                    { sensorId = sensorId
                    , value = 20.0 + (toFloat (modBy 100 (count * 7 + sensorId * 13))) / 10.0
                    , timestamp = count * 100
                    }
            in
            Task.andThen
                (\_ -> Channel.send channel reading)
                (Task.andThen
                    (\_ -> Task.sleep 100)  -- 100ms between readings
                    (if count < 50 then
                        loop (count + 1)
                     else
                        Task.succeed ()
                    )
                )
    in
    loop 0


-- Aggregator task: receives readings, updates shared state

aggregatorTask : Channel SensorReading -> MVar SystemState -> Task x ()
aggregatorTask channel stateMVar =
    let
        loop =
            Task.andThen
                (\maybeReading ->
                    case maybeReading of
                        Just reading ->
                            Task.andThen
                                (\_ ->
                                    MVar.modify stateMVar
                                        (\state ->
                                            let
                                                newTotal = state.totalReadings + 1
                                                newAvg =
                                                    (state.averageTemp * toFloat state.totalReadings + reading.value)
                                                    / toFloat newTotal
                                                newMax = max state.maxTemp reading.value
                                                newMin = min state.minTemp reading.value
                                            in
                                            { totalReadings = newTotal
                                            , averageTemp = newAvg
                                            , maxTemp = newMax
                                            , minTemp = newMin
                                            }
                                        )
                                )
                                loop

                        Nothing ->
                            Task.succeed ()
                )
                (Channel.receiveTimeout channel 500)
    in
    loop


-- Logger task: periodically prints system state

loggerTask : MVar SystemState -> Task x ()
loggerTask stateMVar =
    let
        loop count =
            if count >= 10 then
                Task.succeed ()
            else
                Task.andThen
                    (\state ->
                        Task.andThen
                            (\_ ->
                                IO.print
                                    ("Status: readings=" ++ String.fromInt state.totalReadings
                                     ++ " avg=" ++ String.fromFloat state.averageTemp
                                     ++ " max=" ++ String.fromFloat state.maxTemp
                                     ++ " min=" ++ String.fromFloat state.minTemp
                                    )
                            )
                            (Task.andThen
                                (\_ -> Task.sleep 500)  -- Log every 500ms
                                (loop (count + 1))
                            )
                    )
                    (MVar.read stateMVar)
    in
    loop 0


-- Main: spawn all tasks and wait for completion

main : Task x ()
main =
    let
        initialState =
            { totalReadings = 0
            , averageTemp = 0.0
            , maxTemp = -1000.0
            , minTemp = 1000.0
            }
    in
    Task.andThen
        (\_ -> IO.print "=== Sensor Monitor Starting ===")
        (Task.andThen
            (\channel ->
                Task.andThen
                    (\stateMVar ->
                        Task.andThen
                            (\_ -> IO.print "Spawning sensor tasks...")
                            (Task.andThen
                                (\sensor1 ->
                                    Task.andThen
                                        (\sensor2 ->
                                            Task.andThen
                                                (\sensor3 ->
                                                    Task.andThen
                                                        (\aggregator ->
                                                            Task.andThen
                                                                (\logger ->
                                                                    -- Wait for all tasks
                                                                    Task.andThen
                                                                        (\_ -> Task.await sensor1)
                                                                        (Task.andThen
                                                                            (\_ -> Task.await sensor2)
                                                                            (Task.andThen
                                                                                (\_ -> Task.await sensor3)
                                                                                (Task.andThen
                                                                                    (\_ -> Task.await aggregator)
                                                                                    (Task.andThen
                                                                                        (\_ -> Task.await logger)
                                                                                        (Task.andThen
                                                                                            (\finalState ->
                                                                                                IO.print
                                                                                                    ("=== Final: "
                                                                                                     ++ String.fromInt finalState.totalReadings
                                                                                                     ++ " readings processed ===")
                                                                                            )
                                                                                            (MVar.read stateMVar)
                                                                                        )
                                                                                    )
                                                                                )
                                                                            )
                                                                        )
                                                                )
                                                                (Task.spawn (loggerTask stateMVar))
                                                        )
                                                        (Task.spawn (aggregatorTask channel stateMVar))
                                                )
                                                (Task.spawn (sensorTask 3 channel))
                                        )
                                        (Task.spawn (sensorTask 2 channel))
                                )
                                (Task.spawn (sensorTask 1 channel))
                            )
                    )
                    (MVar.new initialState)
            )
            (Channel.create 32)
        )
