/*
 * tcelm_rtems.c - RTEMS integration for tcelm
 *
 * Implements the Elm runtime on top of RTEMS primitives.
 */

#include "tcelm_rtems.h"
#include "tcelm_basics.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef __rtems__
#include <rtems/rtems/clock.h>
#include <rtems/score/timecounter.h>
#else
/* Host stubs for testing */
#include <time.h>
static uint32_t stub_tick = 0;
#endif

/*
 * ============================================================================
 * CONSTANTS AND CONFIGURATION
 * ============================================================================
 */

const tcelm_rtems_config_t TCELM_RTEMS_DEFAULT_CONFIG = {
    .arena_block_size = 64 * 1024,      /* 64KB */
    .max_tasks = 32,
    .max_messages = 64,
    .message_size = 4096,
    .default_priority = 100,
    .default_stack_size = 16 * 1024     /* 16KB stack */
};

/*
 * ============================================================================
 * RUNTIME STATE
 * ============================================================================
 */

/* Runtime state */
static struct {
    bool initialized;
    tcelm_rtems_config_t config;

    /* Task registry */
    tcelm_task_t *tasks[32];          /* TODO: make dynamic based on config */
    size_t task_count;

    /* Channel registry */
    tcelm_channel_t *channels[16];
    size_t channel_count;

    /* Statistics */
    tcelm_rtems_stats_t stats;

#ifdef __rtems__
    rtems_id task_registry_sem;        /* Protects task registry */
#endif
} tcelm_runtime;

/*
 * ============================================================================
 * TASK STRUCTURE
 * ============================================================================
 */

struct tcelm_task {
    /* Identity */
    const char *name;
    rtems_id rtems_id;

    /* The Elm Architecture components */
    tcelm_update_fn update;
    tcelm_subscriptions_fn subscriptions;
    tcelm_view_fn view;

    /* State */
    tcelm_arena_t arena;              /* Per-task arena */
    tcelm_value_t *model;             /* Current model */
    bool running;

    /* Message queue */
#ifdef __rtems__
    rtems_id message_queue;
#endif

    /* Subscriptions state */
    tcelm_value_t *current_subs;

    /* Statistics */
    tcelm_task_stats_t stats;
};

/*
 * ============================================================================
 * CHANNEL STRUCTURE
 * ============================================================================
 */

struct tcelm_channel {
    const char *name;
    tcelm_task_t *subscribers[16];    /* TODO: make dynamic */
    size_t subscriber_count;
    size_t max_subscribers;
#ifdef __rtems__
    rtems_id sem;                     /* Protects subscriber list */
#endif
};

/*
 * ============================================================================
 * RUNTIME INITIALIZATION
 * ============================================================================
 */

int tcelm_rtems_init(const tcelm_rtems_config_t *config) {
    if (tcelm_runtime.initialized) {
        return -1;  /* Already initialized */
    }

    /* Use provided config or defaults */
    if (config) {
        tcelm_runtime.config = *config;
    } else {
        tcelm_runtime.config = TCELM_RTEMS_DEFAULT_CONFIG;
    }

    /* Initialize task registry */
    memset(tcelm_runtime.tasks, 0, sizeof(tcelm_runtime.tasks));
    tcelm_runtime.task_count = 0;

    /* Initialize channel registry */
    memset(tcelm_runtime.channels, 0, sizeof(tcelm_runtime.channels));
    tcelm_runtime.channel_count = 0;

    /* Initialize statistics */
    memset(&tcelm_runtime.stats, 0, sizeof(tcelm_runtime.stats));

#ifdef __rtems__
    /* Create semaphore for task registry */
    rtems_status_code sc = rtems_semaphore_create(
        rtems_build_name('T', 'C', 'R', 'G'),
        1,
        RTEMS_BINARY_SEMAPHORE | RTEMS_PRIORITY | RTEMS_INHERIT_PRIORITY,
        0,
        &tcelm_runtime.task_registry_sem
    );
    if (sc != RTEMS_SUCCESSFUL) {
        return -1;
    }
#endif

    tcelm_runtime.initialized = true;
    return 0;
}

void tcelm_rtems_shutdown(void) {
    if (!tcelm_runtime.initialized) return;

    /* Delete all tasks */
    for (size_t i = 0; i < tcelm_runtime.task_count; i++) {
        if (tcelm_runtime.tasks[i]) {
            tcelm_task_delete(tcelm_runtime.tasks[i]);
        }
    }

#ifdef __rtems__
    rtems_semaphore_delete(tcelm_runtime.task_registry_sem);
#endif

    tcelm_runtime.initialized = false;
}

/*
 * ============================================================================
 * TASK IMPLEMENTATION
 * ============================================================================
 */

/* Task-local storage key for current task */
#ifdef __rtems__
static __thread tcelm_task_t *current_task = NULL;
#else
static tcelm_task_t *current_task = NULL;
#endif

tcelm_task_t *tcelm_task_self(void) {
    return current_task;
}

/*
 * Process a single message
 */
static void task_process_message(tcelm_task_t *task, tcelm_value_t *msg) {
    uint64_t start_time = 0;

#ifdef __rtems__
    /* Get high-resolution time for profiling */
    struct timespec ts;
    rtems_clock_get_uptime(&ts);
    start_time = (uint64_t)ts.tv_sec * 1000000 + ts.tv_nsec / 1000;
#endif

    /* Call update function */
    tcelm_value_t *result = task->update(&task->arena, msg, task->model);

    /* Result is a tuple (newModel, cmd) */
    if (result && TCELM_IS_TUPLE2(result)) {
        task->model = tcelm_tuple2_first(result);
        tcelm_value_t *cmd = tcelm_tuple2_second(result);

        /* TODO: Process commands */
        (void)cmd;
    }

    task->stats.update_calls++;

#ifdef __rtems__
    /* Calculate update time */
    rtems_clock_get_uptime(&ts);
    uint64_t end_time = (uint64_t)ts.tv_sec * 1000000 + ts.tv_nsec / 1000;
    uint64_t elapsed = end_time - start_time;

    if (elapsed > task->stats.max_update_time_us) {
        task->stats.max_update_time_us = elapsed;
    }

    /* Update running average */
    task->stats.avg_update_time_us =
        (task->stats.avg_update_time_us * (task->stats.update_calls - 1) + elapsed)
        / task->stats.update_calls;
#endif

    task->stats.messages_processed++;
}

/*
 * Main task loop
 */
#ifdef __rtems__
static rtems_task task_entry(rtems_task_argument arg) {
    tcelm_task_t *task = (tcelm_task_t *)arg;
    current_task = task;

    /* Temporary arena for message deserialization */
    tcelm_arena_t msg_arena;
    tcelm_arena_init(&msg_arena, 4096);

    /* Message buffer */
    uint8_t msg_buffer[4096];
    size_t msg_size;

    while (task->running) {
        /* Wait for message */
        rtems_status_code sc = rtems_message_queue_receive(
            task->message_queue,
            msg_buffer,
            &msg_size,
            RTEMS_WAIT,
            RTEMS_NO_TIMEOUT
        );

        if (sc == RTEMS_SUCCESSFUL) {
            task->stats.messages_received++;

            /* Reset arenas for this update cycle */
            tcelm_arena_reset(&msg_arena);

            /* Deserialize message */
            /* For now, assume message is a pointer to a value */
            tcelm_value_t *msg = *(tcelm_value_t **)msg_buffer;

            /* Process the message */
            task_process_message(task, msg);

            /* Call view if present */
            if (task->view) {
                task->view(&task->arena, task->model);
            }

            /* Reset task arena periodically to prevent unbounded growth */
            /* TODO: smarter arena management - maybe double-buffer */
            tcelm_arena_stats_t arena_stats;
            tcelm_arena_get_stats(&task->arena, &arena_stats);
            if (arena_stats.total_used > task->stats.arena_peak_usage) {
                task->stats.arena_peak_usage = arena_stats.total_used;
            }

            if (arena_stats.total_used > tcelm_runtime.config.arena_block_size / 2) {
                /* Copy model to fresh arena before reset */
                /* TODO: implement value copying */
                tcelm_arena_reset(&task->arena);
                task->stats.arena_resets++;
            }
        }
    }

    tcelm_arena_destroy(&msg_arena);
    rtems_task_delete(RTEMS_SELF);
}
#endif

tcelm_task_t *tcelm_task_spawn(const tcelm_task_def_t *def) {
    if (!tcelm_runtime.initialized) return NULL;
    if (tcelm_runtime.task_count >= 32) return NULL;  /* TODO: use config */

    /* Allocate task structure */
    tcelm_task_t *task = malloc(sizeof(tcelm_task_t));
    if (!task) return NULL;

    memset(task, 0, sizeof(tcelm_task_t));

    /* Initialize task */
    task->name = def->name;
    task->update = def->update;
    task->subscriptions = def->subscriptions;
    task->view = def->view;
    task->running = true;

    /* Initialize task's arena */
    if (tcelm_arena_init(&task->arena, tcelm_runtime.config.arena_block_size) != 0) {
        free(task);
        return NULL;
    }

    /* Create initial model */
    task->model = def->init(&task->arena);

    /* Initialize stats */
    task->stats.name = def->name;

#ifdef __rtems__
    /* Create message queue */
    rtems_status_code sc = rtems_message_queue_create(
        rtems_build_name('E', 'L', 'M', '0' + tcelm_runtime.task_count),
        tcelm_runtime.config.max_messages,
        tcelm_runtime.config.message_size,
        RTEMS_LOCAL,
        &task->message_queue
    );
    if (sc != RTEMS_SUCCESSFUL) {
        tcelm_arena_destroy(&task->arena);
        free(task);
        return NULL;
    }

    /* Create RTEMS task */
    rtems_task_priority priority = def->priority ? def->priority : tcelm_runtime.config.default_priority;
    size_t stack_size = def->stack_size ? def->stack_size : tcelm_runtime.config.default_stack_size;

    sc = rtems_task_create(
        rtems_build_name('E', 'L', 'M', '0' + tcelm_runtime.task_count),
        priority,
        stack_size,
        RTEMS_PREEMPT | RTEMS_TIMESLICE,
        RTEMS_LOCAL,
        &task->rtems_id
    );
    if (sc != RTEMS_SUCCESSFUL) {
        rtems_message_queue_delete(task->message_queue);
        tcelm_arena_destroy(&task->arena);
        free(task);
        return NULL;
    }

    /* Start the task */
    sc = rtems_task_start(task->rtems_id, task_entry, (rtems_task_argument)task);
    if (sc != RTEMS_SUCCESSFUL) {
        rtems_task_delete(task->rtems_id);
        rtems_message_queue_delete(task->message_queue);
        tcelm_arena_destroy(&task->arena);
        free(task);
        return NULL;
    }
#endif

    /* Register task */
    tcelm_runtime.tasks[tcelm_runtime.task_count++] = task;
    tcelm_runtime.stats.active_tasks++;
    tcelm_runtime.stats.total_tasks_created++;

    return task;
}

int tcelm_task_send(tcelm_task_t *task, tcelm_value_t *msg) {
    if (!task || !task->running) return -1;

#ifdef __rtems__
    /* Send pointer to message */
    rtems_status_code sc = rtems_message_queue_send(
        task->message_queue,
        &msg,
        sizeof(tcelm_value_t *)
    );

    if (sc == RTEMS_SUCCESSFUL) {
        tcelm_runtime.stats.total_messages_sent++;
        return 0;
    }
    return -1;
#else
    /* Synchronous execution for testing on host */
    task_process_message(task, msg);
    return 0;
#endif
}

int tcelm_task_send_int(tcelm_task_t *task, int64_t value) {
    tcelm_value_t *msg = tcelm_int(&task->arena, value);
    return tcelm_task_send(task, msg);
}

int tcelm_task_send_float(tcelm_task_t *task, double value) {
    tcelm_value_t *msg = tcelm_float(&task->arena, value);
    return tcelm_task_send(task, msg);
}

int tcelm_task_send_string(tcelm_task_t *task, const char *str) {
    tcelm_value_t *msg = tcelm_string(&task->arena, str);
    return tcelm_task_send(task, msg);
}

void tcelm_task_delete(tcelm_task_t *task) {
    if (!task) return;

    task->running = false;

#ifdef __rtems__
    /* Send a dummy message to wake up the task */
    tcelm_value_t *dummy = tcelm_unit(&task->arena);
    rtems_message_queue_send(task->message_queue, &dummy, sizeof(tcelm_value_t *));

    /* Wait for task to exit and clean up */
    /* Note: rtems_task_delete is called from within task_entry */
    rtems_message_queue_delete(task->message_queue);
#endif

    tcelm_arena_destroy(&task->arena);

    /* Remove from registry */
    for (size_t i = 0; i < tcelm_runtime.task_count; i++) {
        if (tcelm_runtime.tasks[i] == task) {
            /* Shift remaining tasks */
            for (size_t j = i; j < tcelm_runtime.task_count - 1; j++) {
                tcelm_runtime.tasks[j] = tcelm_runtime.tasks[j + 1];
            }
            tcelm_runtime.task_count--;
            break;
        }
    }

    tcelm_runtime.stats.active_tasks--;
    free(task);
}

/*
 * ============================================================================
 * TIMING
 * ============================================================================
 */

tcelm_value_t *tcelm_rtems_tick_get(tcelm_arena_t *arena) {
#ifdef __rtems__
    return tcelm_int(arena, rtems_clock_get_ticks_since_boot());
#else
    return tcelm_int(arena, stub_tick++);
#endif
}

tcelm_value_t *tcelm_rtems_ticks_per_second(tcelm_arena_t *arena) {
#ifdef __rtems__
    return tcelm_int(arena, rtems_clock_get_ticks_per_second());
#else
    return tcelm_int(arena, 1000);  /* Assume 1kHz for testing */
#endif
}

tcelm_value_t *tcelm_rtems_time_ms(tcelm_arena_t *arena) {
#ifdef __rtems__
    struct timespec ts;
    rtems_clock_get_uptime(&ts);
    return tcelm_int(arena, ts.tv_sec * 1000 + ts.tv_nsec / 1000000);
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return tcelm_int(arena, ts.tv_sec * 1000 + ts.tv_nsec / 1000000);
#endif
}

tcelm_value_t *tcelm_rtems_time_us(tcelm_arena_t *arena) {
#ifdef __rtems__
    struct timespec ts;
    rtems_clock_get_uptime(&ts);
    return tcelm_int(arena, ts.tv_sec * 1000000 + ts.tv_nsec / 1000);
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return tcelm_int(arena, ts.tv_sec * 1000000 + ts.tv_nsec / 1000);
#endif
}

/*
 * ============================================================================
 * SUBSCRIPTIONS
 * ============================================================================
 */

/* Subscription tags */
#define SUB_TAG_NONE    0
#define SUB_TAG_BATCH   1
#define SUB_TAG_EVERY   2
#define SUB_TAG_DELAY   3

tcelm_value_t *tcelm_sub_none(tcelm_arena_t *arena) {
    return tcelm_custom(arena, SUB_TAG_NONE, "Sub.none", 0);
}

tcelm_value_t *tcelm_sub_batch(tcelm_arena_t *arena, tcelm_value_t *subs) {
    return tcelm_custom(arena, SUB_TAG_BATCH, "Sub.batch", 1, subs);
}

tcelm_value_t *tcelm_sub_every_ms(tcelm_arena_t *arena, tcelm_value_t *interval_ms, tcelm_value_t *msg_fn) {
    return tcelm_custom(arena, SUB_TAG_EVERY, "Time.every", 2, interval_ms, msg_fn);
}

tcelm_value_t *tcelm_sub_every_ticks(tcelm_arena_t *arena, tcelm_value_t *interval_ticks, tcelm_value_t *msg_fn) {
    return tcelm_custom(arena, SUB_TAG_EVERY, "Rtems.everyTick", 2, interval_ticks, msg_fn);
}

tcelm_value_t *tcelm_sub_delay_ms(tcelm_arena_t *arena, tcelm_value_t *delay_ms, tcelm_value_t *msg) {
    return tcelm_custom(arena, SUB_TAG_DELAY, "Process.sleep", 2, delay_ms, msg);
}

tcelm_value_t *tcelm_sub_delay_ticks(tcelm_arena_t *arena, tcelm_value_t *delay_ticks, tcelm_value_t *msg) {
    return tcelm_custom(arena, SUB_TAG_DELAY, "Rtems.delayTicks", 2, delay_ticks, msg);
}

/*
 * ============================================================================
 * COMMANDS
 * ============================================================================
 */

#define CMD_TAG_NONE    0
#define CMD_TAG_BATCH   1
#define CMD_TAG_SEND    2

tcelm_value_t *tcelm_cmd_none(tcelm_arena_t *arena) {
    return tcelm_custom(arena, CMD_TAG_NONE, "Cmd.none", 0);
}

tcelm_value_t *tcelm_cmd_batch(tcelm_arena_t *arena, tcelm_value_t *cmds) {
    return tcelm_custom(arena, CMD_TAG_BATCH, "Cmd.batch", 1, cmds);
}

tcelm_value_t *tcelm_cmd_send(tcelm_arena_t *arena, tcelm_task_t *target, tcelm_value_t *msg) {
    /* Store task pointer as int for now - TODO: proper handle type */
    tcelm_value_t *handle = tcelm_int(arena, (int64_t)(uintptr_t)target);
    return tcelm_custom(arena, CMD_TAG_SEND, "Task.send", 2, handle, msg);
}

/*
 * ============================================================================
 * CHANNELS (PUB/SUB)
 * ============================================================================
 */

tcelm_channel_t *tcelm_channel_create(const char *name, size_t max_subscribers) {
    if (tcelm_runtime.channel_count >= 16) return NULL;  /* TODO: use config */

    tcelm_channel_t *channel = malloc(sizeof(tcelm_channel_t));
    if (!channel) return NULL;

    memset(channel, 0, sizeof(tcelm_channel_t));
    channel->name = name;
    channel->max_subscribers = max_subscribers < 16 ? max_subscribers : 16;

#ifdef __rtems__
    rtems_status_code sc = rtems_semaphore_create(
        rtems_build_name('C', 'H', 'N', '0' + tcelm_runtime.channel_count),
        1,
        RTEMS_BINARY_SEMAPHORE | RTEMS_PRIORITY,
        0,
        &channel->sem
    );
    if (sc != RTEMS_SUCCESSFUL) {
        free(channel);
        return NULL;
    }
#endif

    tcelm_runtime.channels[tcelm_runtime.channel_count++] = channel;
    tcelm_runtime.stats.active_channels++;

    return channel;
}

tcelm_channel_t *tcelm_channel_find(const char *name) {
    for (size_t i = 0; i < tcelm_runtime.channel_count; i++) {
        if (strcmp(tcelm_runtime.channels[i]->name, name) == 0) {
            return tcelm_runtime.channels[i];
        }
    }
    return NULL;
}

int tcelm_channel_subscribe(tcelm_channel_t *channel) {
    tcelm_task_t *task = tcelm_task_self();
    if (!task || !channel) return -1;
    if (channel->subscriber_count >= channel->max_subscribers) return -1;

#ifdef __rtems__
    rtems_semaphore_obtain(channel->sem, RTEMS_WAIT, RTEMS_NO_TIMEOUT);
#endif

    channel->subscribers[channel->subscriber_count++] = task;

#ifdef __rtems__
    rtems_semaphore_release(channel->sem);
#endif

    return 0;
}

int tcelm_channel_unsubscribe(tcelm_channel_t *channel) {
    tcelm_task_t *task = tcelm_task_self();
    if (!task || !channel) return -1;

#ifdef __rtems__
    rtems_semaphore_obtain(channel->sem, RTEMS_WAIT, RTEMS_NO_TIMEOUT);
#endif

    for (size_t i = 0; i < channel->subscriber_count; i++) {
        if (channel->subscribers[i] == task) {
            /* Shift remaining subscribers */
            for (size_t j = i; j < channel->subscriber_count - 1; j++) {
                channel->subscribers[j] = channel->subscribers[j + 1];
            }
            channel->subscriber_count--;
            break;
        }
    }

#ifdef __rtems__
    rtems_semaphore_release(channel->sem);
#endif

    return 0;
}

int tcelm_channel_publish(tcelm_channel_t *channel, tcelm_value_t *msg) {
    if (!channel) return -1;

#ifdef __rtems__
    rtems_semaphore_obtain(channel->sem, RTEMS_WAIT, RTEMS_NO_TIMEOUT);
#endif

    for (size_t i = 0; i < channel->subscriber_count; i++) {
        tcelm_task_send(channel->subscribers[i], msg);
    }

#ifdef __rtems__
    rtems_semaphore_release(channel->sem);
#endif

    return 0;
}

/*
 * ============================================================================
 * HARDWARE I/O STUBS
 * ============================================================================
 * These should be implemented per-BSP
 */

tcelm_value_t *tcelm_gpio_read(tcelm_arena_t *arena, tcelm_value_t *pin) {
    (void)pin;
    /* TODO: implement per-BSP */
    return tcelm_int(arena, 0);
}

tcelm_value_t *tcelm_gpio_write(tcelm_arena_t *arena, tcelm_value_t *pin, tcelm_value_t *value) {
    (void)pin;
    (void)value;
    /* TODO: implement per-BSP */
    return tcelm_unit(arena);
}

tcelm_value_t *tcelm_uart_write(tcelm_arena_t *arena, tcelm_value_t *port, tcelm_value_t *data) {
    (void)port;
#ifdef __rtems__
    if (TCELM_IS_STRING(data)) {
        printf("%s", TCELM_AS_STRING(data)->data);
    }
#else
    if (TCELM_IS_STRING(data)) {
        printf("%s", TCELM_AS_STRING(data)->data);
    }
#endif
    return tcelm_unit(arena);
}

tcelm_value_t *tcelm_uart_read(tcelm_arena_t *arena, tcelm_value_t *port, tcelm_value_t *count) {
    (void)port;
    (void)count;
    /* TODO: implement per-BSP */
    return tcelm_string(arena, "");
}

/*
 * ============================================================================
 * STATISTICS AND DEBUGGING
 * ============================================================================
 */

int tcelm_task_get_stats(tcelm_task_t *task, tcelm_task_stats_t *stats) {
    if (!task || !stats) return -1;
    *stats = task->stats;
    return 0;
}

void tcelm_rtems_print_stats(void) {
    printf("\n=== tcelm RTEMS Runtime Statistics ===\n\n");
    printf("Active tasks: %zu\n", tcelm_runtime.stats.active_tasks);
    printf("Total tasks created: %zu\n", tcelm_runtime.stats.total_tasks_created);
    printf("Total messages sent: %zu\n", tcelm_runtime.stats.total_messages_sent);
    printf("Active channels: %zu\n", tcelm_runtime.stats.active_channels);

    printf("\nPer-task statistics:\n");
    for (size_t i = 0; i < tcelm_runtime.task_count; i++) {
        tcelm_task_t *task = tcelm_runtime.tasks[i];
        if (!task) continue;

        printf("\n  Task: %s\n", task->stats.name);
        printf("    Messages received:  %lu\n", (unsigned long)task->stats.messages_received);
        printf("    Messages processed: %lu\n", (unsigned long)task->stats.messages_processed);
        printf("    Update calls:       %lu\n", (unsigned long)task->stats.update_calls);
        printf("    Arena resets:       %lu\n", (unsigned long)task->stats.arena_resets);
        printf("    Arena peak usage:   %zu bytes\n", task->stats.arena_peak_usage);
        printf("    Max update time:    %lu us\n", (unsigned long)task->stats.max_update_time_us);
        printf("    Avg update time:    %lu us\n", (unsigned long)task->stats.avg_update_time_us);
    }
}

void tcelm_rtems_get_stats(tcelm_rtems_stats_t *stats) {
    if (stats) {
        *stats = tcelm_runtime.stats;
    }
}
