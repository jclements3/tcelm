/*
 * tcelm_budget.h - Execution time budgets for RTEMS
 *
 * Provides per-task CPU time budgets to prevent runaway tasks.
 * Similar to Ada's execution time clocks and budget handlers.
 *
 * Use cases:
 * - Hard real-time systems where task overruns must be detected
 * - Preventing single tasks from monopolizing CPU
 * - Debugging and profiling task execution times
 */

#ifndef TCELM_BUDGET_H
#define TCELM_BUDGET_H

#include "tcelm_types.h"
#include "tcelm_arena.h"

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/ratemon.h>
#else
#include <pthread.h>
#include <time.h>
#include <stdint.h>
#include <stdbool.h>
typedef uint32_t rtems_id;
#endif

/*
 * Budget exhaustion action
 */
typedef enum {
    TCELM_BUDGET_ACTION_NONE,       /* Just record, don't act */
    TCELM_BUDGET_ACTION_SIGNAL,     /* Signal the task (let it handle) */
    TCELM_BUDGET_ACTION_SUSPEND,    /* Suspend the task */
    TCELM_BUDGET_ACTION_DELETE,     /* Delete the task (drastic) */
    TCELM_BUDGET_ACTION_CALLBACK    /* Call user-defined handler */
} tcelm_budget_action_t;

/*
 * Budget exhaustion callback type
 */
typedef void (*tcelm_budget_handler_t)(void *task_handle, uint64_t used_us, uint64_t budget_us);

/*
 * Budget handle
 */
typedef struct tcelm_budget {
    rtems_id task_id;               /* Associated task ID */
    void *native_data;              /* Native implementation data */
    uint64_t budget_us;             /* Budget in microseconds */
    uint64_t used_us;               /* CPU time used in microseconds */
    uint64_t start_time_us;         /* Start time for current period */
    uint32_t overrun_count;         /* Number of budget overruns */
    tcelm_budget_action_t action;   /* Action on exhaustion */
    tcelm_budget_handler_t handler; /* Optional callback handler */
    void *handler_arg;              /* Argument for callback */
    bool active;                    /* Is budget tracking active? */
    bool exhausted;                 /* Has budget been exhausted this period? */
    const char *name;               /* Name for debugging */
} tcelm_budget_t;

/*
 * Budget configuration
 */
typedef struct tcelm_budget_config {
    uint64_t budget_us;             /* Budget in microseconds */
    tcelm_budget_action_t action;   /* Action on exhaustion */
    tcelm_budget_handler_t handler; /* Optional callback (if action is CALLBACK) */
    void *handler_arg;              /* Argument for callback */
    const char *name;               /* Optional name */
} tcelm_budget_config_t;

/* Default configuration (1 second budget, no action) */
extern const tcelm_budget_config_t TCELM_BUDGET_DEFAULT_CONFIG;

/*
 * Initialize budget subsystem
 */
int tcelm_budget_init(void);

/*
 * Shutdown budget subsystem
 */
void tcelm_budget_shutdown(void);

/*
 * Create a budget tracker for a task
 * Returns budget handle or NULL on failure
 */
tcelm_budget_t *tcelm_budget_create(
    tcelm_arena_t *arena,
    const tcelm_budget_config_t *config
);

/*
 * Create budget with microsecond limit (convenience function)
 */
tcelm_budget_t *tcelm_budget_create_us(
    tcelm_arena_t *arena,
    uint64_t budget_us
);

/*
 * Create budget with millisecond limit (convenience function)
 */
tcelm_budget_t *tcelm_budget_create_ms(
    tcelm_arena_t *arena,
    uint32_t budget_ms
);

/*
 * Attach budget to current task
 * Must be called from the task being tracked
 */
int tcelm_budget_attach(tcelm_budget_t *budget);

/*
 * Detach budget from current task
 */
int tcelm_budget_detach(tcelm_budget_t *budget);

/*
 * Start/resume budget tracking
 */
int tcelm_budget_start(tcelm_budget_t *budget);

/*
 * Stop/pause budget tracking
 */
int tcelm_budget_stop(tcelm_budget_t *budget);

/*
 * Reset budget for new period
 * Clears used time, resets exhausted flag
 */
int tcelm_budget_reset(tcelm_budget_t *budget);

/*
 * Check current CPU time used
 * Returns time in microseconds
 */
uint64_t tcelm_budget_get_used(tcelm_budget_t *budget);

/*
 * Check remaining budget
 * Returns remaining time in microseconds (0 if exhausted)
 */
uint64_t tcelm_budget_get_remaining(tcelm_budget_t *budget);

/*
 * Check if budget is exhausted
 */
bool tcelm_budget_is_exhausted(tcelm_budget_t *budget);

/*
 * Get overrun count
 */
uint32_t tcelm_budget_get_overrun_count(tcelm_budget_t *budget);

/*
 * Set new budget limit (can be called while running)
 */
int tcelm_budget_set_limit(tcelm_budget_t *budget, uint64_t budget_us);

/*
 * Set exhaustion action
 */
int tcelm_budget_set_action(
    tcelm_budget_t *budget,
    tcelm_budget_action_t action,
    tcelm_budget_handler_t handler,
    void *handler_arg
);

/*
 * Delete budget tracker
 */
void tcelm_budget_delete(tcelm_budget_t *budget);

/*
 * ============================================================================
 * EXECUTION TIME CLOCK (per-task CPU time queries)
 * ============================================================================
 */

/*
 * Get current task's CPU time in microseconds
 */
uint64_t tcelm_cpu_time_self_us(void);

/*
 * Get current task's CPU time in milliseconds
 */
uint64_t tcelm_cpu_time_self_ms(void);

/*
 * Get CPU time for a specific task (by handle)
 * Returns 0 if task not found or not supported
 */
uint64_t tcelm_cpu_time_task_us(void *task_handle);

#endif /* TCELM_BUDGET_H */
