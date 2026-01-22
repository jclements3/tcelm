/*
 * tcelm_task.c - Task monad runtime for RTEMS
 *
 * Implementation of Elm's Task abstraction using RTEMS tasks.
 */

#include "tcelm_task.h"
#include <string.h>
#include <stdio.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/tasks.h>
#include <rtems/rtems/event.h>
#include <sys/cpuset.h>
#else
/* Native implementation using pthreads */
#include <pthread.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>
#endif

/* Default task configuration */
const tcelm_task_config_t TCELM_TASK_DEFAULT_CONFIG = {
    .name = "TELM",
    .priority = 100,        /* Middle priority */
    .stack_size = 8192,     /* 8KB stack */
    .cpu_affinity = 0       /* Any CPU */
};

/* Task counter for generating unique names */
static uint32_t task_counter = 0;

/* Task wrapper data - passed to RTEMS task entry */
typedef struct task_wrapper_data {
    tcelm_task_fn fn;
    tcelm_value_t *arg;
    tcelm_task_handle_t *handle;
} task_wrapper_data_t;

/*
 * Initialize task subsystem
 */
int tcelm_task_init(void) {
    task_counter = 0;
    return 0;
}

/*
 * Shutdown task subsystem
 */
void tcelm_task_shutdown(void) {
    /* Nothing to do - RTEMS handles cleanup */
}

/*
 * Create a successful task result (Ok value)
 */
tcelm_value_t *tcelm_task_succeed(tcelm_arena_t *arena, tcelm_value_t *value) {
    return tcelm_custom(arena, TCELM_CTOR_OK, "Ok", 1, value);
}

/*
 * Create a failed task result (Err value)
 */
tcelm_value_t *tcelm_task_fail(tcelm_arena_t *arena, tcelm_value_t *error) {
    return tcelm_custom(arena, TCELM_CTOR_ERR, "Err", 1, error);
}

#ifdef __rtems__

/*
 * RTEMS task entry point wrapper
 */
static void task_entry_wrapper(rtems_task_argument arg) {
    task_wrapper_data_t *data = (task_wrapper_data_t *)arg;

    /* Run the Elm task function */
    tcelm_value_t *result = data->fn(data->handle->arena, data->arg);

    /* Store result and mark complete */
    data->handle->result = result;
    data->handle->completed = true;

    /* Signal completion via event */
    rtems_event_send(RTEMS_SELF, RTEMS_EVENT_0);

    /* Task exits - RTEMS will clean up */
    rtems_task_delete(RTEMS_SELF);
}

/*
 * Spawn a new RTEMS task
 */
tcelm_task_handle_t *tcelm_task_spawn(
    tcelm_arena_t *arena,
    tcelm_task_fn fn,
    tcelm_value_t *arg,
    const tcelm_task_config_t *config
) {
    rtems_status_code status;
    rtems_id task_id;

    /* Allocate handle */
    tcelm_task_handle_t *handle = tcelm_arena_alloc(arena, sizeof(tcelm_task_handle_t));
    if (!handle) return NULL;

    /* Create task-local arena */
    handle->arena = tcelm_arena_create(64 * 1024);  /* 64KB arena */
    if (!handle->arena) return NULL;

    handle->result = NULL;
    handle->error = NULL;
    handle->completed = false;

    /* Build unique task name */
    char name[5];
    uint32_t num = __sync_fetch_and_add(&task_counter, 1);
    snprintf(name, 5, "T%03u", num % 1000);
    rtems_name rtems_task_name = rtems_build_name(name[0], name[1], name[2], name[3]);

    /* Create the RTEMS task */
    status = rtems_task_create(
        rtems_task_name,
        config->priority,
        config->stack_size,
        RTEMS_DEFAULT_MODES,
        RTEMS_DEFAULT_ATTRIBUTES,
        &task_id
    );

    if (status != RTEMS_SUCCESSFUL) {
        tcelm_arena_free(handle->arena);
        return NULL;
    }

    handle->task_id = task_id;

    /* Set CPU affinity if specified */
    if (config->cpu_affinity != 0) {
        cpu_set_t cpuset;
        CPU_ZERO(&cpuset);
        for (int i = 0; i < 4; i++) {
            if (config->cpu_affinity & (1 << i)) {
                CPU_SET(i, &cpuset);
            }
        }
        rtems_task_set_affinity(task_id, sizeof(cpuset), &cpuset);
    }

    /* Prepare wrapper data */
    task_wrapper_data_t *wrapper = tcelm_arena_alloc(handle->arena, sizeof(task_wrapper_data_t));
    wrapper->fn = fn;
    wrapper->arg = arg;
    wrapper->handle = handle;

    /* Start the task */
    status = rtems_task_start(task_id, task_entry_wrapper, (rtems_task_argument)wrapper);

    if (status != RTEMS_SUCCESSFUL) {
        rtems_task_delete(task_id);
        tcelm_arena_free(handle->arena);
        return NULL;
    }

    return handle;
}

/*
 * Wait for task completion
 */
tcelm_value_t *tcelm_task_await(tcelm_arena_t *arena, tcelm_task_handle_t *handle) {
    if (handle->completed) {
        return handle->result;
    }

    /* Wait for completion event */
    rtems_event_set events;
    rtems_event_receive(
        RTEMS_EVENT_0,
        RTEMS_WAIT | RTEMS_EVENT_ALL,
        RTEMS_NO_TIMEOUT,
        &events
    );

    return handle->result;
}

/*
 * Wait with timeout
 */
tcelm_value_t *tcelm_task_await_timeout(
    tcelm_arena_t *arena,
    tcelm_task_handle_t *handle,
    uint32_t timeout_ms
) {
    if (handle->completed) {
        return tcelm_custom(arena, TCELM_CTOR_JUST, "Just", 1, handle->result);
    }

    /* Convert ms to ticks */
    rtems_interval ticks = (timeout_ms * rtems_clock_get_ticks_per_second()) / 1000;

    rtems_event_set events;
    rtems_status_code status = rtems_event_receive(
        RTEMS_EVENT_0,
        RTEMS_WAIT | RTEMS_EVENT_ALL,
        ticks,
        &events
    );

    if (status == RTEMS_TIMEOUT) {
        return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
    }

    return tcelm_custom(arena, TCELM_CTOR_JUST, "Just", 1, handle->result);
}

/*
 * Sleep for milliseconds
 */
void tcelm_task_sleep(uint32_t ms) {
    rtems_interval ticks = (ms * rtems_clock_get_ticks_per_second()) / 1000;
    if (ticks == 0) ticks = 1;
    rtems_task_wake_after(ticks);
}

/*
 * Get current task priority
 */
int tcelm_task_get_priority(void) {
    rtems_task_priority old_priority;
    rtems_task_set_priority(RTEMS_SELF, RTEMS_CURRENT_PRIORITY, &old_priority);
    return (int)old_priority;
}

/*
 * Set current task priority
 */
int tcelm_task_set_priority(int priority) {
    rtems_task_priority old_priority;
    rtems_status_code status = rtems_task_set_priority(
        RTEMS_SELF,
        (rtems_task_priority)priority,
        &old_priority
    );
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

/*
 * Set CPU affinity
 */
int tcelm_task_set_affinity(int core) {
    cpu_set_t cpuset;
    CPU_ZERO(&cpuset);
    CPU_SET(core, &cpuset);
    rtems_status_code status = rtems_task_set_affinity(
        RTEMS_SELF,
        sizeof(cpuset),
        &cpuset
    );
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

/*
 * Yield to other tasks
 */
void tcelm_task_yield(void) {
    rtems_task_wake_after(RTEMS_YIELD_PROCESSOR);
}

/*
 * Delete task
 */
void tcelm_task_delete(tcelm_task_handle_t *handle) {
    if (handle && handle->task_id) {
        rtems_task_delete(handle->task_id);
        if (handle->arena) {
            tcelm_arena_free(handle->arena);
        }
    }
}

#else /* Native implementation */

/*
 * pthread wrapper for native builds
 */
typedef struct pthread_wrapper_data {
    tcelm_task_fn fn;
    tcelm_value_t *arg;
    tcelm_task_handle_t *handle;
} pthread_wrapper_data_t;

static void *pthread_entry_wrapper(void *arg) {
    pthread_wrapper_data_t *data = (pthread_wrapper_data_t *)arg;

    /* Run the Elm task function */
    tcelm_value_t *result = data->fn(data->handle->arena, data->arg);

    /* Store result */
    data->handle->result = result;
    data->handle->completed = true;

    return NULL;
}

tcelm_task_handle_t *tcelm_task_spawn(
    tcelm_arena_t *arena,
    tcelm_task_fn fn,
    tcelm_value_t *arg,
    const tcelm_task_config_t *config
) {
    (void)config;  /* Unused in native build */

    tcelm_task_handle_t *handle = tcelm_arena_alloc(arena, sizeof(tcelm_task_handle_t));
    if (!handle) return NULL;

    handle->arena = tcelm_arena_create(64 * 1024);
    if (!handle->arena) return NULL;

    handle->result = NULL;
    handle->error = NULL;
    handle->completed = false;

    pthread_wrapper_data_t *wrapper = tcelm_arena_alloc(handle->arena, sizeof(pthread_wrapper_data_t));
    wrapper->fn = fn;
    wrapper->arg = arg;
    wrapper->handle = handle;

    pthread_t *thread = malloc(sizeof(pthread_t));
    if (!thread) {
        tcelm_arena_free(handle->arena);
        return NULL;
    }

    if (pthread_create(thread, NULL, pthread_entry_wrapper, wrapper) != 0) {
        free(thread);
        tcelm_arena_free(handle->arena);
        return NULL;
    }

    handle->task_id = 0;
    handle->native_data = thread;
    return handle;
}

tcelm_value_t *tcelm_task_await(tcelm_arena_t *arena, tcelm_task_handle_t *handle) {
    (void)arena;
    pthread_t *thread = (pthread_t *)handle->native_data;
    pthread_join(*thread, NULL);
    free(thread);
    handle->native_data = NULL;
    return handle->result;
}

tcelm_value_t *tcelm_task_await_timeout(
    tcelm_arena_t *arena,
    tcelm_task_handle_t *handle,
    uint32_t timeout_ms
) {
    /* Simple implementation: just wait (no timeout in basic pthreads) */
    return tcelm_custom(arena, TCELM_CTOR_JUST, "Just", 1,
                       tcelm_task_await(arena, handle));
}

void tcelm_task_sleep(uint32_t ms) {
    struct timespec ts = {
        .tv_sec = ms / 1000,
        .tv_nsec = (ms % 1000) * 1000000
    };
    nanosleep(&ts, NULL);
}

int tcelm_task_get_priority(void) {
    return 100;  /* Default priority */
}

int tcelm_task_set_priority(int priority) {
    (void)priority;
    return 0;  /* No-op on native */
}

int tcelm_task_set_affinity(int core) {
    (void)core;
    return 0;  /* No-op on native */
}

void tcelm_task_yield(void) {
    sched_yield();
}

void tcelm_task_delete(tcelm_task_handle_t *handle) {
    if (handle && handle->arena) {
        tcelm_arena_free(handle->arena);
    }
}

#endif /* __rtems__ */

/*
 * Spawn with default config
 */
tcelm_task_handle_t *tcelm_task_spawn_default(
    tcelm_arena_t *arena,
    tcelm_task_fn fn,
    tcelm_value_t *arg
) {
    return tcelm_task_spawn(arena, fn, arg, &TCELM_TASK_DEFAULT_CONFIG);
}

/*
 * Run tasks in parallel
 */
tcelm_value_t *tcelm_task_parallel(
    tcelm_arena_t *arena,
    tcelm_value_t *tasks
) {
    /* Count tasks */
    int count = 0;
    tcelm_value_t *curr = tasks;
    while (!tcelm_is_nil(curr)) {
        count++;
        curr = tcelm_list_tail(curr);
    }

    if (count == 0) {
        return tcelm_nil(arena);
    }

    /* Collect handles */
    tcelm_task_handle_t **handles = tcelm_arena_alloc(arena, count * sizeof(tcelm_task_handle_t *));
    curr = tasks;
    for (int i = 0; i < count; i++) {
        tcelm_value_t *head = tcelm_list_head(curr);
        handles[i] = (tcelm_task_handle_t *)TCELM_AS_INT(head);
        curr = tcelm_list_tail(curr);
    }

    /* Wait for all and collect results */
    tcelm_value_t *results = tcelm_nil(arena);
    for (int i = count - 1; i >= 0; i--) {
        tcelm_value_t *result = tcelm_task_await(arena, handles[i]);
        results = tcelm_cons(arena, result, results);
    }

    return results;
}

/*
 * Race tasks - return first to complete
 */
tcelm_value_t *tcelm_task_race(
    tcelm_arena_t *arena,
    tcelm_value_t *tasks
) {
    /* Simple implementation: poll until one completes */
    while (true) {
        tcelm_value_t *curr = tasks;
        while (!tcelm_is_nil(curr)) {
            tcelm_value_t *head = tcelm_list_head(curr);
            tcelm_task_handle_t *handle = (tcelm_task_handle_t *)TCELM_AS_INT(head);
            if (handle->completed) {
                return handle->result;
            }
            curr = tcelm_list_tail(curr);
        }
        tcelm_task_yield();
    }
}
