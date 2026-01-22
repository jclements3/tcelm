/*
 * tcelm_task.h - Task monad runtime for RTEMS
 *
 * Wraps RTEMS task primitives to provide Elm's Task abstraction.
 * Tasks are spawned as RTEMS tasks with proper priority inheritance.
 */

#ifndef TCELM_TASK_H
#define TCELM_TASK_H

#include "tcelm_types.h"
#include "tcelm_arena.h"

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/tasks.h>
#include <rtems/rtems/event.h>
#else
/* Native stubs for development/testing */
typedef uint32_t rtems_id;
typedef uint32_t rtems_status_code;
typedef uint32_t rtems_task_priority;
typedef uintptr_t rtems_task_argument;
typedef void (*rtems_task_entry)(rtems_task_argument);
#define RTEMS_SUCCESSFUL 0
#define RTEMS_SELF 0
#define RTEMS_NO_TIMEOUT 0
#define RTEMS_WAIT 0
#define RTEMS_EVENT_0 0x00000001
#define RTEMS_EVENT_ALL 0xFFFFFFFF
#endif

/*
 * Task handle - wraps rtems_id with result storage
 */
typedef struct tcelm_task_handle {
    rtems_id task_id;           /* RTEMS task ID */
    rtems_id completion_event;  /* Event for signaling completion */
    void *native_data;          /* Native implementation data (64-bit safe) */
    tcelm_value_t *result;      /* Task result (set on completion) */
    tcelm_value_t *error;       /* Task error (set on failure) */
    bool completed;             /* Has the task finished? */
    tcelm_arena_t *arena;       /* Arena for task-local allocations */
} tcelm_task_handle_t;

/*
 * Task function signature
 * Takes arena and argument, returns result value
 */
typedef tcelm_value_t *(*tcelm_task_fn)(tcelm_arena_t *arena, tcelm_value_t *arg);

/*
 * Configuration for spawned tasks
 */
typedef struct tcelm_task_config {
    const char *name;           /* Task name (4 chars for RTEMS) */
    rtems_task_priority priority; /* Task priority (1-255, lower = higher) */
    size_t stack_size;          /* Stack size in bytes */
    uint32_t cpu_affinity;      /* CPU affinity mask (0 = any) */
} tcelm_task_config_t;

/* Default configuration */
extern const tcelm_task_config_t TCELM_TASK_DEFAULT_CONFIG;

/*
 * Initialize task subsystem
 * Call once at startup
 */
int tcelm_task_init(void);

/*
 * Shutdown task subsystem
 */
void tcelm_task_shutdown(void);

/*
 * Create a successful task result
 */
tcelm_value_t *tcelm_task_succeed(tcelm_arena_t *arena, tcelm_value_t *value);

/*
 * Create a failed task result
 */
tcelm_value_t *tcelm_task_fail(tcelm_arena_t *arena, tcelm_value_t *error);

/*
 * Spawn a new task
 * Returns task handle that can be used with await
 */
tcelm_task_handle_t *tcelm_task_spawn(
    tcelm_arena_t *arena,
    tcelm_task_fn fn,
    tcelm_value_t *arg,
    const tcelm_task_config_t *config
);

/*
 * Spawn with default configuration
 */
tcelm_task_handle_t *tcelm_task_spawn_default(
    tcelm_arena_t *arena,
    tcelm_task_fn fn,
    tcelm_value_t *arg
);

/*
 * Wait for a task to complete and get result
 * Blocks until task finishes
 */
tcelm_value_t *tcelm_task_await(tcelm_arena_t *arena, tcelm_task_handle_t *handle);

/*
 * Wait for a task with timeout (milliseconds)
 * Returns Nothing on timeout, Just result on completion
 */
tcelm_value_t *tcelm_task_await_timeout(
    tcelm_arena_t *arena,
    tcelm_task_handle_t *handle,
    uint32_t timeout_ms
);

/*
 * Run multiple tasks in parallel, wait for all
 * Returns list of results in order
 */
tcelm_value_t *tcelm_task_parallel(
    tcelm_arena_t *arena,
    tcelm_value_t *tasks  /* List of task handles */
);

/*
 * Run multiple tasks, return first to complete
 */
tcelm_value_t *tcelm_task_race(
    tcelm_arena_t *arena,
    tcelm_value_t *tasks  /* List of task handles */
);

/*
 * Sleep for specified milliseconds
 */
void tcelm_task_sleep(uint32_t ms);

/*
 * Get current task's priority
 */
int tcelm_task_get_priority(void);

/*
 * Set current task's priority
 */
int tcelm_task_set_priority(int priority);

/*
 * Set CPU affinity for current task
 * core: 0-3 for NUC's 4 cores
 */
int tcelm_task_set_affinity(int core);

/*
 * Get current task handle (for self-reference)
 */
tcelm_task_handle_t *tcelm_task_self(void);

/*
 * Yield to other tasks of same priority
 */
void tcelm_task_yield(void);

/*
 * Delete/cleanup a task handle
 */
void tcelm_task_delete(tcelm_task_handle_t *handle);

#endif /* TCELM_TASK_H */
