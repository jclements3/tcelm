/*
 * tcelm_rtems.h - RTEMS integration for tcelm
 *
 * This provides Elm bindings to RTEMS primitives, making Elm a first-class
 * language for RTEMS real-time applications alongside C and Ada.
 *
 * Design Philosophy:
 * - RTEMS tasks map to Elm "actors" (message-passing concurrency)
 * - Semaphores/mutexes are hidden behind Elm's effect system
 * - Message queues become Elm subscriptions
 * - Timers become time-based subscriptions
 * - All RTEMS operations are arena-allocated (no GC pauses)
 */

#ifndef TCELM_RTEMS_H
#define TCELM_RTEMS_H

#include "tcelm_types.h"
#include "tcelm_arena.h"

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/tasks.h>
#include <rtems/rtems/sem.h>
#include <rtems/rtems/message.h>
#include <rtems/rtems/timer.h>
#include <rtems/rtems/event.h>
#else
/* Stub types for non-RTEMS compilation (testing on host) */
typedef uint32_t rtems_id;
typedef uint32_t rtems_status_code;
typedef uint32_t rtems_interval;
typedef uint32_t rtems_event_set;
typedef uint32_t rtems_task_priority;
typedef uintptr_t rtems_task_argument;
typedef void (*rtems_task_entry)(rtems_task_argument arg);
#define RTEMS_SUCCESSFUL 0
#define RTEMS_NO_TIMEOUT 0
#define RTEMS_WAIT 0
#define RTEMS_EVENT_ALL 0
#endif

/*
 * ============================================================================
 * TCELM RTEMS RUNTIME
 * ============================================================================
 */

/*
 * Runtime configuration
 */
typedef struct tcelm_rtems_config {
    size_t arena_block_size;      /* Default: 64KB */
    size_t max_tasks;             /* Maximum Elm tasks */
    size_t max_messages;          /* Max queued messages per task */
    size_t message_size;          /* Max message size in bytes */
    rtems_task_priority default_priority;
    size_t default_stack_size;
} tcelm_rtems_config_t;

/* Default configuration */
extern const tcelm_rtems_config_t TCELM_RTEMS_DEFAULT_CONFIG;

/*
 * Initialize tcelm RTEMS runtime
 * Call this from Init() before any Elm code runs
 */
int tcelm_rtems_init(const tcelm_rtems_config_t *config);

/*
 * Shutdown tcelm RTEMS runtime
 */
void tcelm_rtems_shutdown(void);

/*
 * ============================================================================
 * ELM TASK MODEL
 * ============================================================================
 *
 * Elm tasks are modeled after The Elm Architecture (TEA):
 *   - Model: The task's state
 *   - Update: (Msg, Model) -> (Model, Cmd Msg)
 *   - Subscriptions: Model -> Sub Msg
 *
 * Each Elm task runs as an RTEMS task with its own arena.
 */

/* Forward declaration */
typedef struct tcelm_task tcelm_task_t;

/* Message handler: update function signature */
typedef tcelm_value_t *(*tcelm_update_fn)(
    tcelm_arena_t *arena,
    tcelm_value_t *msg,
    tcelm_value_t *model
);

/* Subscription function signature */
typedef tcelm_value_t *(*tcelm_subscriptions_fn)(
    tcelm_arena_t *arena,
    tcelm_value_t *model
);

/* View/output function (optional, for tasks that produce output) */
typedef void (*tcelm_view_fn)(
    tcelm_arena_t *arena,
    tcelm_value_t *model
);

/*
 * Elm task definition
 */
typedef struct tcelm_task_def {
    const char *name;                   /* Task name for debugging */
    tcelm_value_t *(*init)(tcelm_arena_t *arena);  /* Initial model */
    tcelm_update_fn update;             /* Update function */
    tcelm_subscriptions_fn subscriptions; /* Subscriptions (can be NULL) */
    tcelm_view_fn view;                 /* View function (can be NULL) */
    rtems_task_priority priority;       /* RTEMS priority (0 = use default) */
    size_t stack_size;                  /* Stack size (0 = use default) */
} tcelm_task_def_t;

/*
 * Spawn an Elm task
 * Returns task handle or NULL on failure
 */
tcelm_task_t *tcelm_elm_task_spawn(const tcelm_task_def_t *def);

/*
 * Send a message to an Elm task
 * The message is copied into the task's message queue
 */
int tcelm_task_send(tcelm_task_t *task, tcelm_value_t *msg);

/*
 * Send a message from C code (creates value in temporary arena)
 */
int tcelm_task_send_int(tcelm_task_t *task, int64_t value);
int tcelm_task_send_float(tcelm_task_t *task, double value);
int tcelm_task_send_string(tcelm_task_t *task, const char *str);

/*
 * Get current task handle (from within an Elm task)
 */
tcelm_task_t *tcelm_task_self(void);

/*
 * Delete/stop an Elm task
 */
void tcelm_elm_task_delete(tcelm_task_t *task);

/*
 * ============================================================================
 * TIMING AND SCHEDULING
 * ============================================================================
 */

/*
 * Timing primitives exposed to Elm
 */

/* Get current tick count */
tcelm_value_t *tcelm_rtems_tick_get(tcelm_arena_t *arena);

/* Get ticks per second */
tcelm_value_t *tcelm_rtems_ticks_per_second(tcelm_arena_t *arena);

/* Get current time in milliseconds since boot */
tcelm_value_t *tcelm_rtems_time_ms(tcelm_arena_t *arena);

/* Get current time in microseconds since boot */
tcelm_value_t *tcelm_rtems_time_us(tcelm_arena_t *arena);

/*
 * Timer subscription types
 */
#define TCELM_SUB_EVERY_MS    1   /* Periodic timer in milliseconds */
#define TCELM_SUB_EVERY_TICKS 2   /* Periodic timer in ticks */
#define TCELM_SUB_DELAY_MS    3   /* One-shot delay in milliseconds */
#define TCELM_SUB_DELAY_TICKS 4   /* One-shot delay in ticks */

/*
 * Create timer subscriptions (used by generated code)
 */
tcelm_value_t *tcelm_sub_every_ms(tcelm_arena_t *arena, tcelm_value_t *interval_ms, tcelm_value_t *msg_fn);
tcelm_value_t *tcelm_sub_every_ticks(tcelm_arena_t *arena, tcelm_value_t *interval_ticks, tcelm_value_t *msg_fn);
tcelm_value_t *tcelm_sub_delay_ms(tcelm_arena_t *arena, tcelm_value_t *delay_ms, tcelm_value_t *msg);
tcelm_value_t *tcelm_sub_delay_ticks(tcelm_arena_t *arena, tcelm_value_t *delay_ticks, tcelm_value_t *msg);

/* No subscriptions */
tcelm_value_t *tcelm_sub_none(tcelm_arena_t *arena);

/* Batch subscriptions */
tcelm_value_t *tcelm_sub_batch(tcelm_arena_t *arena, tcelm_value_t *subs);

/*
 * ============================================================================
 * COMMANDS (EFFECTS)
 * ============================================================================
 */

/*
 * Command types for Elm effects
 */
#define TCELM_CMD_NONE        0
#define TCELM_CMD_SEND        1   /* Send message to another task */
#define TCELM_CMD_BATCH       2   /* Batch of commands */
#define TCELM_CMD_DELAY       3   /* Delayed message to self */
#define TCELM_CMD_RANDOM      4   /* Generate random value */

/* No command */
tcelm_value_t *tcelm_cmd_none(tcelm_arena_t *arena);

/* Batch commands */
tcelm_value_t *tcelm_cmd_batch(tcelm_arena_t *arena, tcelm_value_t *cmds);

/* Send message to task */
tcelm_value_t *tcelm_cmd_send(tcelm_arena_t *arena, tcelm_task_t *target, tcelm_value_t *msg);

/*
 * ============================================================================
 * INTER-TASK COMMUNICATION
 * ============================================================================
 */

/*
 * Named channels for pub/sub communication between Elm tasks
 * (Elm-style pub/sub, distinct from Go-style channels in tcelm_channel.h)
 */
typedef struct tcelm_elm_channel tcelm_elm_channel_t;

/* Create a named channel */
tcelm_elm_channel_t *tcelm_elm_channel_create(const char *name, size_t max_subscribers);

/* Find channel by name */
tcelm_elm_channel_t *tcelm_elm_channel_find(const char *name);

/* Subscribe current task to channel */
int tcelm_elm_channel_subscribe(tcelm_elm_channel_t *channel);

/* Unsubscribe current task from channel */
int tcelm_elm_channel_unsubscribe(tcelm_elm_channel_t *channel);

/* Publish message to all subscribers */
int tcelm_elm_channel_publish(tcelm_elm_channel_t *channel, tcelm_value_t *msg);

/*
 * ============================================================================
 * HARDWARE I/O (Platform-specific)
 * ============================================================================
 * These are wrappers that can be implemented per-BSP
 */

/*
 * GPIO operations (if available)
 */
tcelm_value_t *tcelm_gpio_read(tcelm_arena_t *arena, tcelm_value_t *pin);
tcelm_value_t *tcelm_gpio_write(tcelm_arena_t *arena, tcelm_value_t *pin, tcelm_value_t *value);

/*
 * Serial/UART operations
 */
tcelm_value_t *tcelm_uart_write(tcelm_arena_t *arena, tcelm_value_t *port, tcelm_value_t *data);
tcelm_value_t *tcelm_uart_read(tcelm_arena_t *arena, tcelm_value_t *port, tcelm_value_t *count);

/*
 * ============================================================================
 * DEBUG AND MONITORING
 * ============================================================================
 */

/*
 * Task statistics
 */
typedef struct tcelm_task_stats {
    const char *name;
    uint64_t messages_received;
    uint64_t messages_processed;
    uint64_t update_calls;
    uint64_t arena_resets;
    size_t arena_peak_usage;
    uint64_t max_update_time_us;
    uint64_t avg_update_time_us;
} tcelm_task_stats_t;

/* Get stats for a task */
int tcelm_task_get_stats(tcelm_task_t *task, tcelm_task_stats_t *stats);

/* Print all task stats */
void tcelm_rtems_print_stats(void);

/*
 * Runtime statistics
 */
typedef struct tcelm_rtems_stats {
    size_t active_tasks;
    size_t total_tasks_created;
    size_t total_messages_sent;
    size_t active_channels;
    size_t active_timers;
} tcelm_rtems_stats_t;

void tcelm_rtems_get_stats(tcelm_rtems_stats_t *stats);

#endif /* TCELM_RTEMS_H */
