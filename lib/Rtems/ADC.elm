module Rtems.ADC exposing
    ( Device
    , Channel
    , open
    , close
    , Config
    , defaultConfig
    , Reference(..)
    , configure
    , read
    , readVoltage
    , readMultiple
    , startContinuous
    , stopContinuous
    , onSample
    , Sample
    )

{-| ADC (Analog-to-Digital Converter) for RTEMS.

ADCs convert analog voltages to digital values for reading
sensors like thermistors, potentiometers, and current sensors.

# Devices and Channels

@docs Device, Channel, open, close

# Configuration

@docs Config, defaultConfig, Reference, configure

# Single Reads

@docs read, readVoltage, readMultiple

# Continuous Sampling

@docs startContinuous, stopContinuous, onSample, Sample

# Example

    -- Open ADC device
    adc <- ADC.open 0

    -- Configure for 12-bit, internal reference
    ADC.configure adc { ADC.defaultConfig | resolution = 12 }

    -- Read channel 0
    rawValue <- ADC.read adc 0

    -- Read as voltage
    voltage <- ADC.readVoltage adc 0
    -- voltage is in volts (e.g., 1.65)

-}

import Task exposing (Task)


{-| An ADC device handle.
-}
type Device
    = Device Int


{-| An ADC channel (0-based).
-}
type alias Channel =
    Int


{-| Voltage reference source.
-}
type Reference
    = Internal -- Internal reference (e.g., 1.1V, 2.5V)
    | External -- External VREF pin
    | VCC -- Supply voltage as reference


{-| ADC configuration.
-}
type alias Config =
    { resolution : Int -- Bits (8, 10, 12, 16)
    , reference : Reference
    , referenceVoltage : Float -- Reference voltage in volts
    , sampleRate : Int -- Samples per second (for continuous)
    , averaging : Int -- Number of samples to average (1 = no averaging)
    }


{-| Default configuration: 12-bit, 3.3V reference, no averaging.
-}
defaultConfig : Config
defaultConfig =
    { resolution = 12
    , reference = VCC
    , referenceVoltage = 3.3
    , sampleRate = 1000
    , averaging = 1
    }


{-| Open an ADC device.

    adc <- ADC.open 0  -- Open ADC0

-}
open : Int -> Task x Device
open deviceNum =
    -- Kernel: Open ADC device
    Elm.Kernel.ADC.open deviceNum
        |> Task.map (\_ -> Device deviceNum)


{-| Close an ADC device.
-}
close : Device -> Task x ()
close (Device deviceNum) =
    -- Kernel: Close ADC device
    Elm.Kernel.ADC.close deviceNum


{-| Configure ADC device.
-}
configure : Device -> Config -> Task x ()
configure (Device deviceNum) config =
    -- Kernel: Configure ADC
    Elm.Kernel.ADC.configure deviceNum config.resolution (refToInt config.reference)


refToInt : Reference -> Int
refToInt ref =
    case ref of
        Internal ->
            0

        External ->
            1

        VCC ->
            2



-- =============================================================================
-- Single Reads
-- =============================================================================


{-| Read raw ADC value from a channel.

Returns an integer 0 to 2^resolution - 1.

    raw <- ADC.read adc 0
    -- For 12-bit: 0-4095

-}
read : Device -> Channel -> Task x Int
read (Device deviceNum) channel =
    -- Kernel: Read ADC channel
    Elm.Kernel.ADC.read deviceNum channel


{-| Read voltage from a channel.

Returns the voltage based on the configured reference.

    voltage <- ADC.readVoltage adc 0
    -- e.g., 1.65 (volts)

-}
readVoltage : Device -> Channel -> Task x Float
readVoltage device channel =
    -- This would typically be done in Elm using the raw value
    -- But we can also have the kernel do the conversion
    read device channel
        |> Task.map
            (\raw ->
                -- Assuming 12-bit, 3.3V reference
                toFloat raw * 3.3 / 4095.0
            )


{-| Read multiple channels at once.

Useful for sampling related signals simultaneously.

    [ch0, ch1, ch2] <- ADC.readMultiple adc [0, 1, 2]

-}
readMultiple : Device -> List Channel -> Task x (List Int)
readMultiple (Device deviceNum) channels =
    -- Kernel: Read multiple channels
    Elm.Kernel.ADC.readMultiple deviceNum channels



-- =============================================================================
-- Continuous Sampling
-- =============================================================================


{-| An ADC sample with timestamp.
-}
type alias Sample =
    { channel : Channel
    , value : Int
    , timestamp : Int -- Microseconds since start
    }


{-| Start continuous sampling on a channel.

    ADC.startContinuous adc 0 1000  -- 1000 samples/sec

-}
startContinuous : Device -> Channel -> Int -> Task x ()
startContinuous (Device deviceNum) channel sampleRate =
    -- Kernel: Start continuous ADC sampling
    Elm.Kernel.ADC.startContinuous deviceNum channel sampleRate


{-| Stop continuous sampling.
-}
stopContinuous : Device -> Channel -> Task x ()
stopContinuous (Device deviceNum) channel =
    -- Kernel: Stop continuous ADC sampling
    Elm.Kernel.ADC.stopContinuous deviceNum channel


{-| Subscribe to continuous samples.

    subscriptions model =
        ADC.onSample adc 0 GotSample

    update msg model =
        case msg of
            GotSample sample ->
                processReading sample.value

-}
onSample : Device -> Channel -> (Sample -> msg) -> Sub msg
onSample (Device deviceNum) channel toMsg =
    -- Kernel: Subscribe to ADC samples
    Elm.Kernel.ADC.onSample deviceNum channel toMsg
