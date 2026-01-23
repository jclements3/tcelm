module Rtems.DAC exposing
    ( Device
    , Channel
    , open
    , close
    , Config
    , defaultConfig
    , configure
    , write
    , writeVoltage
    , writeMultiple
    , startWaveform
    , stopWaveform
    , Waveform(..)
    )

{-| DAC (Digital-to-Analog Converter) for RTEMS.

DACs convert digital values to analog voltages for driving
actuators, generating audio, and control applications.

# Devices and Channels

@docs Device, Channel, open, close

# Configuration

@docs Config, defaultConfig, configure

# Output

@docs write, writeVoltage, writeMultiple

# Waveform Generation

@docs startWaveform, stopWaveform, Waveform

# Example

    -- Open DAC device
    dac <- DAC.open 0

    -- Configure for 12-bit output
    DAC.configure dac { DAC.defaultConfig | resolution = 12 }

    -- Output half of reference voltage
    DAC.write dac 0 2048  -- 2048/4096 = 50%

    -- Output specific voltage
    DAC.writeVoltage dac 0 1.65  -- 1.65V

-}

import Task exposing (Task)


{-| A DAC device handle.
-}
type Device
    = Device Int


{-| A DAC channel (0-based).
-}
type alias Channel =
    Int


{-| DAC configuration.
-}
type alias Config =
    { resolution : Int -- Bits (8, 10, 12, 16)
    , referenceVoltage : Float -- Output range (e.g., 3.3V)
    , buffered : Bool -- Use output buffer
    }


{-| Default configuration: 12-bit, 3.3V reference, buffered.
-}
defaultConfig : Config
defaultConfig =
    { resolution = 12
    , referenceVoltage = 3.3
    , buffered = True
    }


{-| Open a DAC device.

    dac <- DAC.open 0  -- Open DAC0

-}
open : Int -> Task x Device
open deviceNum =
    -- Kernel: Open DAC device
    Elm.Kernel.DAC.open deviceNum
        |> Task.map (\_ -> Device deviceNum)


{-| Close a DAC device.
-}
close : Device -> Task x ()
close (Device deviceNum) =
    -- Kernel: Close DAC device
    Elm.Kernel.DAC.close deviceNum


{-| Configure DAC device.
-}
configure : Device -> Config -> Task x ()
configure (Device deviceNum) config =
    -- Kernel: Configure DAC
    Elm.Kernel.DAC.configure deviceNum config.resolution config.buffered



-- =============================================================================
-- Output
-- =============================================================================


{-| Write raw value to a channel.

Value should be 0 to 2^resolution - 1.

    -- For 12-bit DAC, output 50%
    DAC.write dac 0 2048

-}
write : Device -> Channel -> Int -> Task x ()
write (Device deviceNum) channel value =
    -- Kernel: Write DAC channel
    Elm.Kernel.DAC.write deviceNum channel value


{-| Write voltage to a channel.

    -- Output 1.65V
    DAC.writeVoltage dac 0 1.65

-}
writeVoltage : Device -> Channel -> Float -> Task x ()
writeVoltage device channel voltage =
    -- Convert voltage to raw value
    -- Assuming 12-bit, 3.3V reference
    let
        raw =
            round (voltage * 4095.0 / 3.3)
    in
    write device channel (clamp 0 4095 raw)


clamp : Int -> Int -> Int -> Int
clamp minVal maxVal val =
    if val < minVal then
        minVal

    else if val > maxVal then
        maxVal

    else
        val


{-| Write to multiple channels at once.

    DAC.writeMultiple dac [(0, 1000), (1, 2000)]

-}
writeMultiple : Device -> List ( Channel, Int ) -> Task x ()
writeMultiple (Device deviceNum) channelValues =
    -- Kernel: Write multiple channels
    Elm.Kernel.DAC.writeMultiple deviceNum channelValues



-- =============================================================================
-- Waveform Generation
-- =============================================================================


{-| Waveform types for automatic generation.
-}
type Waveform
    = Sine Float Float -- frequency, amplitude
    | Square Float Float -- frequency, amplitude
    | Triangle Float Float -- frequency, amplitude
    | Sawtooth Float Float -- frequency, amplitude
    | Noise Float -- amplitude
    | Custom (List Int) Int -- samples, sample rate


{-| Start generating a waveform.

    -- Generate 1kHz sine wave at 50% amplitude
    DAC.startWaveform dac 0 (Sine 1000 0.5)

-}
startWaveform : Device -> Channel -> Waveform -> Task x ()
startWaveform (Device deviceNum) channel waveform =
    let
        ( waveType, freq, amp ) =
            case waveform of
                Sine f a ->
                    ( 0, f, a )

                Square f a ->
                    ( 1, f, a )

                Triangle f a ->
                    ( 2, f, a )

                Sawtooth f a ->
                    ( 3, f, a )

                Noise a ->
                    ( 4, 0, a )

                Custom _ rate ->
                    ( 5, toFloat rate, 1.0 )
    in
    -- Kernel: Start waveform generation
    Elm.Kernel.DAC.startWaveform deviceNum channel waveType freq amp


{-| Stop waveform generation.
-}
stopWaveform : Device -> Channel -> Task x ()
stopWaveform (Device deviceNum) channel =
    -- Kernel: Stop waveform generation
    Elm.Kernel.DAC.stopWaveform deviceNum channel
