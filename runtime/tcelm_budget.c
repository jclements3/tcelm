/*
 * tcelm_budget.c - Execution time budgets for RTEMS
 *
 * Implementation of CPU time budgets using RTEMS thread CPU time
 * or POSIX clock_gettime for native builds.
 */

#include "tcelm_budget.h"
#include <string.h>
#include <stdio.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/tasks.h>
#include <sys/time.h>
#else
#include <pthread.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>
#endif

/* Budget counter for unique names */
static uint32_t budget_counter = 0;

/* Default configuration */
const tcelm_budget_config_t TCELM_BUDGET_DEFAULT_CONFIG = {
    .budget_us = 1000000,  /* 1 second */
    .action = TCELM_BUDGET_ACTION_NONE,
    .handler = NULL,
    .handler_arg = NULL,
    .name = NULL
};

/*
 * Initialize budget subsystem
 */
int tcelm_budget_init(void) {
    budget_counter = 0;
    return 0;
}

/*
 * Shutdown budget subsystem
 */
void tcelm_budget_shutdown(void) {
    /* Nothing to do */
}

/*
 * Get current time in microseconds (monotonic)
 */
static uint64_t get_time_us(void) {
#ifdef __rtems__
    struct timespec ts;
    rtems_clock_get_uptime(&ts);
    return (uint64_t)ts.tv_sec * 1000000ULL + (uint64_t)ts.tv_nsec / 1000ULL;
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000ULL + (uint64_t)ts.tv_nsec / 1000ULL;
#endif
}

#ifdef __rtems__

/*
 * Get CPU time for current thread (RTEMS)
 * Note: RTEMS doesn't have per-thread CPU time in the classic API.
 * We use wall clock time as an approximation.
 */
static uint64_t get_thread_cpu_time_us(void) {
    /* RTEMS classic API doesn't provide per-task CPU time directly.
     * Use wall clock time as approximation for budget tracking.
     * For accurate CPU time, would need POSIX threads API or
     * rate monotonic statistics. */
    return get_time_us();
}

/*
 * Create budget (RTEMS implementation)
 */
tcelm_budget_t *tcelm_budget_create(
    tcelm_arena_t *arena,
    const tcelm_budget_config_t *config
) {
    if (!config) config = &TCELM_BUDGET_DEFAULT_CONFIG;

    tcelm_budget_t *budget = tcelm_arena_alloc(arena, sizeof(tcelm_budget_t));
    if (!budget) return NULL;

    memset(budget, 0, sizeof(tcelm_budget_t));

    budget->budget_us = config->budget_us;
    budget->action = config->action;
    budget->handler = config->handler;
    budget->handler_arg = config->handler_arg;
    budget->name = config->name;
    budget->active = false;
    budget->exhausted = false;
    budget->task_id = 0;

    __sync_fetch_and_add(&budget_counter, 1);

    return budget;
}

/*
 * Attach budget to current task (RTEMS)
 */
int tcelm_budget_attach(tcelm_budget_t *budget) {
    if (!budget) return -1;
    budget->task_id = rtems_task_self();
    return 0;
}

/*
 * Detach budget (RTEMS)
 */
int tcelm_budget_detach(tcelm_budget_t *budget) {
    if (!budget) return -1;
    tcelm_budget_stop(budget);
    budget->task_id = 0;
    return 0;
}

/*
 * Start budget tracking (RTEMS)
 */
int tcelm_budget_start(tcelm_budget_t *budget) {
    if (!budget) return -1;

    budget->start_time_us = get_thread_cpu_time_us();
    budget->active = true;

    return 0;
}

/*
 * Stop budget tracking (RTEMS)
 */
int tcelm_budget_stop(tcelm_budget_t *budget) {
    if (!budget || !budget->active) return -1;

    uint64_t now = get_thread_cpu_time_us();
    budget->used_us += (now - budget->start_time_us);
    budget->active = false;

    return 0;
}

/*
 * Get CPU time for specific task (RTEMS)
 * Note: Not directly supported in RTEMS classic API without POSIX.
 */
uint64_t tcelm_cpu_time_task_us(void *task_handle) {
    (void)task_handle;
    /* RTEMS classic API doesn't provide per-task CPU time.
     * Would need POSIX threads or rate monotonic statistics
     * for accurate per-task CPU time tracking. */
    return 0;
}

#else /* Native implementation */

/*
 * Native budget data
 */
typedef struct native_budget_data {
    pthread_t thread_id;
    clockid_t clock_id;
    bool has_thread_clock;
} native_budget_data_t;

/*
 * Get CPU time for current thread (native)
 */
static uint64_t get_thread_cpu_time_us(void) {
#ifdef CLOCK_THREAD_CPUTIME_ID
    struct timespec ts;
    if (clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts) == 0) {
        return (uint64_t)ts.tv_sec * 1000000ULL + (uint64_t)ts.tv_nsec / 1000ULL;
    }
#endif
    /* Fallback to wall clock */
    return get_time_us();
}

/*
 * Create budget (native implementation)
 */
tcelm_budget_t *tcelm_budget_create(
    tcelm_arena_t *arena,
    const tcelm_budget_config_t *config
) {
    if (!config) config = &TCELM_BUDGET_DEFAULT_CONFIG;

    tcelm_budget_t *budget = tcelm_arena_alloc(arena, sizeof(tcelm_budget_t));
    if (!budget) return NULL;

    native_budget_data_t *data = malloc(sizeof(native_budget_data_t));
    if (!data) return NULL;

    memset(budget, 0, sizeof(tcelm_budget_t));
    memset(data, 0, sizeof(native_budget_data_t));

    budget->native_data = data;
    budget->budget_us = config->budget_us;
    budget->action = config->action;
    budget->handler = config->handler;
    budget->handler_arg = config->handler_arg;
    budget->name = config->name;
    budget->active = false;
    budget->exhausted = false;

    __sync_fetch_and_add(&budget_counter, 1);

    return budget;
}

/*
 * Attach budget to current task (native)
 */
int tcelm_budget_attach(tcelm_budget_t *budget) {
    if (!budget || !budget->native_data) return -1;

    native_budget_data_t *data = (native_budget_data_t *)budget->native_data;
    data->thread_id = pthread_self();

#ifdef CLOCK_THREAD_CPUTIME_ID
    if (pthread_getcpuclockid(data->thread_id, &data->clock_id) == 0) {
        data->has_thread_clock = true;
    } else {
        data->has_thread_clock = false;
    }
#else
    data->has_thread_clock = false;
#endif

    return 0;
}

/*
 * Detach budget (native)
 */
int tcelm_budget_detach(tcelm_budget_t *budget) {
    if (!budget) return -1;
    tcelm_budget_stop(budget);
    return 0;
}

/*
 * Start budget tracking (native)
 */
int tcelm_budget_start(tcelm_budget_t *budget) {
    if (!budget) return -1;

    budget->start_time_us = get_thread_cpu_time_us();
    budget->active = true;

    return 0;
}

/*
 * Stop budget tracking (native)
 */
int tcelm_budget_stop(tcelm_budget_t *budget) {
    if (!budget || !budget->active) return -1;

    uint64_t now = get_thread_cpu_time_us();
    budget->used_us += (now - budget->start_time_us);
    budget->active = false;

    return 0;
}

/*
 * Get CPU time for specific task (native)
 * Note: Requires the task handle to contain pthread_t
 */
uint64_t tcelm_cpu_time_task_us(void *task_handle) {
    if (!task_handle) return 0;

#ifdef CLOCK_THREAD_CPUTIME_ID
    pthread_t thread = *(pthread_t *)task_handle;
    clockid_t clock_id;

    if (pthread_getcpuclockid(thread, &clock_id) == 0) {
        struct timespec ts;
        if (clock_gettime(clock_id, &ts) == 0) {
            return (uint64_t)ts.tv_sec * 1000000ULL + (uint64_t)ts.tv_nsec / 1000ULL;
        }
    }
#endif

    return 0;
}

/*
 * Delete budget (native-specific cleanup)
 */
static void budget_delete_native(tcelm_budget_t *budget) {
    if (budget && budget->native_data) {
        free(budget->native_data);
        budget->native_data = NULL;
    }
}

#endif /* __rtems__ */

/*
 * ============================================================================
 * COMMON IMPLEMENTATIONS
 * ============================================================================
 */

/*
 * Create budget with microsecond limit
 */
tcelm_budget_t *tcelm_budget_create_us(
    tcelm_arena_t *arena,
    uint64_t budget_us
) {
    tcelm_budget_config_t config = TCELM_BUDGET_DEFAULT_CONFIG;
    config.budget_us = budget_us;
    return tcelm_budget_create(arena, &config);
}

/*
 * Create budget with millisecond limit
 */
tcelm_budget_t *tcelm_budget_create_ms(
    tcelm_arena_t *arena,
    uint32_t budget_ms
) {
    return tcelm_budget_create_us(arena, (uint64_t)budget_ms * 1000ULL);
}

/*
 * Reset budget for new period
 */
int tcelm_budget_reset(tcelm_budget_t *budget) {
    if (!budget) return -1;

    budget->used_us = 0;
    budget->exhausted = false;

    if (budget->active) {
        budget->start_time_us = get_thread_cpu_time_us();
    }

    return 0;
}

/*
 * Get CPU time used
 */
uint64_t tcelm_budget_get_used(tcelm_budget_t *budget) {
    if (!budget) return 0;

    uint64_t used = budget->used_us;

    if (budget->active) {
        uint64_t now = get_thread_cpu_time_us();
        used += (now - budget->start_time_us);
    }

    return used;
}

/*
 * Get remaining budget
 */
uint64_t tcelm_budget_get_remaining(tcelm_budget_t *budget) {
    if (!budget) return 0;

    uint64_t used = tcelm_budget_get_used(budget);

    if (used >= budget->budget_us) {
        return 0;
    }

    return budget->budget_us - used;
}

/*
 * Check if budget exhausted
 */
bool tcelm_budget_is_exhausted(tcelm_budget_t *budget) {
    if (!budget) return false;

    uint64_t used = tcelm_budget_get_used(budget);

    if (used >= budget->budget_us && !budget->exhausted) {
        budget->exhausted = true;
        budget->overrun_count++;

        /* Execute action */
        switch (budget->action) {
            case TCELM_BUDGET_ACTION_CALLBACK:
                if (budget->handler) {
                    budget->handler(
                        budget->handler_arg,
                        used,
                        budget->budget_us
                    );
                }
                break;

            case TCELM_BUDGET_ACTION_SIGNAL:
#ifdef __rtems__
                if (budget->task_id) {
                    rtems_event_send(budget->task_id, RTEMS_EVENT_0);
                }
#endif
                break;

            case TCELM_BUDGET_ACTION_SUSPEND:
#ifdef __rtems__
                if (budget->task_id) {
                    rtems_task_suspend(budget->task_id);
                }
#endif
                break;

            case TCELM_BUDGET_ACTION_DELETE:
#ifdef __rtems__
                if (budget->task_id) {
                    rtems_task_delete(budget->task_id);
                }
#endif
                break;

            case TCELM_BUDGET_ACTION_NONE:
            default:
                /* Just record */
                break;
        }
    }

    return budget->exhausted;
}

/*
 * Get overrun count
 */
uint32_t tcelm_budget_get_overrun_count(tcelm_budget_t *budget) {
    if (!budget) return 0;
    return budget->overrun_count;
}

/*
 * Set new budget limit
 */
int tcelm_budget_set_limit(tcelm_budget_t *budget, uint64_t budget_us) {
    if (!budget) return -1;
    budget->budget_us = budget_us;
    return 0;
}

/*
 * Set exhaustion action
 */
int tcelm_budget_set_action(
    tcelm_budget_t *budget,
    tcelm_budget_action_t action,
    tcelm_budget_handler_t handler,
    void *handler_arg
) {
    if (!budget) return -1;

    budget->action = action;
    budget->handler = handler;
    budget->handler_arg = handler_arg;

    return 0;
}

/*
 * Delete budget tracker
 */
void tcelm_budget_delete(tcelm_budget_t *budget) {
    if (!budget) return;

#ifndef __rtems__
    budget_delete_native(budget);
#endif

    /* Arena will clean up the budget struct itself */
}

/*
 * Get current task's CPU time in microseconds
 */
uint64_t tcelm_cpu_time_self_us(void) {
    return get_thread_cpu_time_us();
}

/*
 * Get current task's CPU time in milliseconds
 */
uint64_t tcelm_cpu_time_self_ms(void) {
    return get_thread_cpu_time_us() / 1000ULL;
}
