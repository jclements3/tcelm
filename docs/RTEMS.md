# tcelm RTEMS Integration

tcelm brings Elm to RTEMS, making Elm a first-class language for real-time systems alongside C and Ada.

## Overview

The tcelm RTEMS integration provides:

- **Arena-allocated runtime** - Deterministic memory management, no GC pauses
- **The Elm Architecture (TEA)** - Model-Update-Subscriptions pattern for RTEMS tasks
- **Inter-task communication** - Channels for pub/sub messaging between Elm tasks
- **RTEMS Shell integration** - Interactive Elm REPL and task management
- **Hardware I/O bindings** - GPIO, UART, and other peripherals (BSP-specific)

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                      Elm Application                             │
│  ┌─────────┐  ┌──────────┐  ┌───────────────┐  ┌──────────┐    │
│  │  Model  │  │  Update  │  │ Subscriptions │  │   View   │    │
│  └─────────┘  └──────────┘  └───────────────┘  └──────────┘    │
└─────────────────────────────────────────────────────────────────┘
                              │
                    ┌─────────┴─────────┐
                    │   tcelm Runtime    │
                    │  (Arena Allocated) │
                    └─────────┬─────────┘
                              │
┌─────────────────────────────┴───────────────────────────────────┐
│                     tcelm RTEMS Layer                            │
│  ┌──────────────┐  ┌────────────┐  ┌────────────┐  ┌─────────┐ │
│  │ Task Manager │  │  Channels  │  │   Timers   │  │  Shell  │ │
│  └──────────────┘  └────────────┘  └────────────┘  └─────────┘ │
└─────────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────┴───────────────────────────────────┐
│                         RTEMS                                    │
│  ┌────────────┐  ┌──────────────┐  ┌───────────┐  ┌──────────┐ │
│  │   Tasks    │  │  Semaphores  │  │  Queues   │  │  Timers  │ │
│  └────────────┘  └──────────────┘  └───────────┘  └──────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

## Quick Start

### 1. Write an Elm Application

```elm
module Counter exposing (main)

import Rtems
import Rtems.Uart as Uart

type alias Model = { count : Int }

type Msg = Tick Int | Reset

init : Model
init = { count = 0 }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick _ ->
            ( { model | count = model.count + 1 }
            , Uart.print ("Count: " ++ String.fromInt model.count)
            )
        Reset ->
            ( { model | count = 0 }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Rtems.every 100 Tick  -- 10 Hz

main =
    Rtems.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
```

### 2. Compile with tcelm

```bash
tcelm compile Counter.elm --target=rtems -o counter.c
```

### 3. Build for RTEMS

```bash
x86_64-rtems7-gcc -o counter.exe counter.c \
    -I$RTEMS_PREFIX/include \
    -L$RTEMS_PREFIX/lib \
    -ltcelm_rtems -lm \
    -specs bsp_specs -qrtems
```

### 4. Run on RTEMS

The application runs as an RTEMS task, receiving timer messages at 10 Hz.

## Elm API

### Rtems Module

```elm
-- Time
now : () -> Int                    -- Current time in ms
ticks : () -> Int                  -- Current tick count
ticksPerSecond : () -> Int         -- System tick rate
every : Int -> (Int -> msg) -> Sub msg      -- Periodic subscription (ms)
everyTick : Int -> (Int -> msg) -> Sub msg  -- Periodic subscription (ticks)

-- Tasks
spawn : TaskDef -> Cmd (TaskId msg)  -- Spawn new Elm task
send : TaskId msg -> msg -> Cmd a    -- Send message to task
self : () -> TaskId msg              -- Get current task ID
kill : TaskId msg -> Cmd a           -- Kill a task

-- Channels
channel : String -> Channel msg      -- Create/get named channel
subscribe : Channel msg -> Sub msg   -- Subscribe to channel
publish : Channel msg -> msg -> Cmd a -- Publish to channel

-- Platform
worker : Config -> Program           -- Create headless Elm program
program : Config -> Program          -- Create Elm program with view
```

### Rtems.Gpio Module

```elm
pin : Int -> Pin                   -- Create pin reference
read : Pin -> Bool                 -- Read pin state
write : Pin -> Bool -> Cmd msg     -- Write pin state
high : Pin -> Cmd msg              -- Set pin high
low : Pin -> Cmd msg               -- Set pin low
toggle : Pin -> Cmd msg            -- Toggle pin
onRising : Pin -> msg -> Sub msg   -- Rising edge subscription
onFalling : Pin -> msg -> Sub msg  -- Falling edge subscription
onChange : Pin -> (Bool -> msg) -> Sub msg  -- Any change subscription
```

### Rtems.Uart Module

```elm
port_ : Int -> Port                -- Create port reference
console : Port                     -- Default console port
write : Port -> String -> Cmd msg  -- Write string
writeLine : Port -> String -> Cmd msg  -- Write with newline
print : String -> Cmd msg          -- Print to console
read : Port -> Int -> String       -- Read bytes (non-blocking)
onReceive : Port -> (String -> msg) -> Sub msg  -- Receive subscription
```

### Additional RTEMS Modules

tcelm provides extensive hardware and system integration:

| Module | Purpose |
|--------|---------|
| `Rtems.ADC` | Analog-to-digital converter input |
| `Rtems.DAC` | Digital-to-analog converter output |
| `Rtems.I2C` | I2C bus communication |
| `Rtems.SPI` | SPI bus communication |
| `Rtems.Clock` | System clock and time management |
| `Rtems.Task` | RTEMS task creation and management |
| `Rtems.Events` | Event-based synchronization |
| `Rtems.Barrier` | Barrier synchronization primitives |
| `Rtems.Sync` | Semaphores, mutexes, condition variables |
| `Rtems.Interrupt` | Interrupt service routine integration |
| `Rtems.Socket` | BSD socket networking |
| `Rtems.Network` | Network configuration and utilities |
| `Rtems.SMP` | Symmetric multi-processing support |
| `Rtems.Shell` | Interactive shell command registration |

See `lib/Rtems/*.elm` for complete API documentation.

## Shell Commands

When RTEMS shell is enabled, tcelm provides interactive commands:

```
elm <expr>          Evaluate an Elm expression
elm-tasks           List all running Elm tasks
elm-stats           Show runtime statistics
elm-send <t> <msg>  Send message to task
elm-spawn <module>  Spawn a new Elm task from module
elm-kill <task>     Kill an Elm task
elm-channels        List all pub/sub channels
elm-arena           Show arena memory statistics
elm-modules         List registered modules
elm-help            Show help
```

Example session:

```
SHLL [/] $ elm 2 + 2
4 : Int

SHLL [/] $ elm-spawn Counter
Spawned Elm task 'Counter' from module 'Counter'

SHLL [/] $ elm-tasks
Elm Tasks:
Name                 Status     Messages        Updates
----                 ------     --------        -------
Counter              running    42              42

SHLL [/] $ elm-stats
=== tcelm RTEMS Runtime Statistics ===
Active tasks: 1
Total tasks created: 1
Total messages sent: 42
...
```

## Real-Time Considerations

### Timing Precision

For precise real-time control, use tick-based subscriptions:

```elm
-- At 1000 ticks/sec: 4 ticks = 4ms = 250 Hz
subscriptions _ =
    Rtems.everyTick 4 Frame
```

Configure RTEMS tick rate to match your requirements:
- 1000 Hz (default) → 1ms resolution
- 10000 Hz → 100μs resolution

### Memory Management

tcelm uses arena allocation for deterministic timing:

- Each task has its own arena
- Arena resets periodically to prevent unbounded growth
- No garbage collection pauses
- Configurable block sizes

```c
tcelm_rtems_config_t config = {
    .arena_block_size = 64 * 1024,  // 64KB per task
    .max_tasks = 32,
    .max_messages = 64,
    .message_size = 4096,
    .default_priority = 100,
    .default_stack_size = 16 * 1024
};
tcelm_rtems_init(&config);
```

### Task Priorities

Set task priority in the task definition:

```elm
main =
    Rtems.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        , priority = 50  -- Higher priority (lower number)
        }
```

## Building

### Host Build (Testing)

```bash
cd runtime
make test
```

### RTEMS Build

```bash
cd runtime
make rtems RTEMS_PREFIX=/path/to/rtems/7
```

### TCC Build (Fast Development)

```bash
cd runtime
make tcc test-tcc
```

## Examples

See `examples/rtems/` for complete examples:

- `Counter.elm` - Simple counter at 10 Hz
- `ControlLoop.elm` - 240 Hz PID control loop
- `SensorNetwork.elm` - Multi-task sensor network with channels
- `ShellScript.elm` - Custom shell command registration

## Files

```
runtime/
├── tcelm_arena.h/c      # Arena allocator
├── tcelm_types.h/c      # Elm value types
├── tcelm_basics.h/c     # Basic operations
├── tcelm_list.h/c       # List operations
├── tcelm_string.h/c     # String operations
├── tcelm_rtems.h/c      # RTEMS integration core
├── tcelm_task.h/c       # Task management
├── tcelm_channel.h/c    # Pub/sub channels
├── tcelm_timer.h/c      # Timer management
├── tcelm_shell.h/c      # Shell integration
├── tcelm_shell_ops.h/c  # Shell command implementations
├── tcelm_gpio.h/c       # GPIO hardware abstraction
├── tcelm_uart.h/c       # UART serial communication
├── tcelm_adc.h/c        # ADC input
├── tcelm_dac.h/c        # DAC output
├── tcelm_i2c.h/c        # I2C bus
├── tcelm_spi.h/c        # SPI bus
├── tcelm_socket.h/c     # BSD sockets
├── tcelm_smp.h/c        # Multi-core support
├── tcelm_semaphore.h/c  # Semaphore primitives
├── tcelm_barrier.h/c    # Barrier synchronization
├── tcelm_mvar.h/c       # Synchronized mutable variables
├── tcelm_events.h/c     # Event flags
├── tcelm_isr.h/c        # Interrupt service routines
├── tcelm_clock.h/c      # Clock and time
├── tcelm_worker.h/c     # Worker task pattern
├── tcelm_protected.h/c  # Protected objects
├── tcelm_rms.h/c        # Rate Monotonic Scheduling
├── tcelm_budget.h/c     # Execution time budgets
├── test_*.c             # Test files
└── Makefile             # Build system

lib/
├── Rtems.elm            # Main RTEMS module
└── Rtems/
    ├── ADC.elm          # Analog-to-digital
    ├── Barrier.elm      # Barrier sync
    ├── Clock.elm        # Clock/time
    ├── DAC.elm          # Digital-to-analog
    ├── Events.elm       # Event flags
    ├── Gpio.elm         # GPIO pins
    ├── I2C.elm          # I2C bus
    ├── Interrupt.elm    # ISR bindings
    ├── Network.elm      # Network config
    ├── Shell.elm        # Shell commands
    ├── SMP.elm          # Multi-core
    ├── Socket.elm       # BSD sockets
    ├── SPI.elm          # SPI bus
    ├── Sync.elm         # Semaphores, mutexes
    ├── Task.elm         # Task management
    └── Uart.elm         # Serial communication

src/Target/
└── RTEMS.elm            # RTEMS code generator (~1600 lines)

examples/rtems/
├── Counter.elm          # Simple counter at 10 Hz
├── ControlLoop.elm      # 240 Hz PID control loop
├── SensorNetwork.elm    # Multi-task sensor network
└── ShellScript.elm      # Shell command example
```

## Comparison with C and Ada

| Feature | C | Ada | Elm (tcelm) |
|---------|---|-----|-------------|
| Memory Safety | Manual | Strong | Strong (immutable) |
| Concurrency | Threads + locks | Tasks + protected objects | Message passing |
| Real-time | Direct hardware | Ravenscar profile | Arena allocation |
| Type System | Weak | Strong | Strong + inference |
| Side Effects | Anywhere | Anywhere | Controlled (Cmd/Sub) |
| Learning Curve | Medium | Steep | Gentle |

tcelm brings functional programming to RTEMS while maintaining real-time guarantees through arena allocation and The Elm Architecture's predictable update cycle.
