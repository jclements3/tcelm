/*
 * tcelm_barrier.h - Barrier synchronization for RTEMS
 *
 * Barriers allow multiple tasks to synchronize at a common point.
 * All tasks must reach the barrier before any can proceed.
 *
 * Use cases:
 * - Phase-based computation (all tasks complete phase N before starting N+1)
 * - Parallel algorithms requiring synchronization points
 * - Initialization barriers (all tasks ready before starting work)
 */

#ifndef TCELM_BARRIER_H
#define TCELM_BARRIER_H

#include "tcelm_types.h"
#include "tcelm_arena.h"

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/barrier.h>
#else
#include <pthread.h>
typedef uint32_t rtems_id;
typedef uint32_t rtems_status_code;
#define RTEMS_SUCCESSFUL 0
#endif

/*
 * Barrier handle
 */
typedef struct tcelm_barrier {
    rtems_id barrier_id;        /* RTEMS barrier ID */
    void *native_data;          /* Native implementation data (pthread barrier) */
    uint32_t count;             /* Number of tasks that must wait */
    const char *name;           /* Name for debugging */
} tcelm_barrier_t;

/*
 * Barrier configuration
 */
typedef struct tcelm_barrier_config {
    uint32_t count;             /* Number of tasks to synchronize */
    bool auto_release;          /* Auto-release when count reached (default: true) */
    const char *name;           /* Optional name */
} tcelm_barrier_config_t;

/* Default configuration (must set count) */
extern const tcelm_barrier_config_t TCELM_BARRIER_DEFAULT_CONFIG;

/*
 * Barrier wait result
 */
typedef enum {
    TCELM_BARRIER_WAIT_SUCCESS,       /* Successfully synchronized */
    TCELM_BARRIER_WAIT_RELEASED,      /* This task was the one that released the barrier */
    TCELM_BARRIER_WAIT_TIMEOUT,       /* Timed out waiting */
    TCELM_BARRIER_WAIT_ERROR          /* Error occurred */
} tcelm_barrier_wait_result_t;

/*
 * Initialize barrier subsystem
 */
int tcelm_barrier_init(void);

/*
 * Shutdown barrier subsystem
 */
void tcelm_barrier_shutdown(void);

/*
 * Create a new barrier
 * count: number of tasks that must call wait() before any can proceed
 * Returns barrier handle or NULL on failure
 */
tcelm_barrier_t *tcelm_barrier_create(
    tcelm_arena_t *arena,
    const tcelm_barrier_config_t *config
);

/*
 * Create barrier with count (convenience function)
 */
tcelm_barrier_t *tcelm_barrier_create_with_count(
    tcelm_arena_t *arena,
    uint32_t count
);

/*
 * Wait at the barrier
 * Blocks until 'count' tasks have called wait
 * Returns TCELM_BARRIER_WAIT_RELEASED for exactly one task (the "serial" task)
 * Returns TCELM_BARRIER_WAIT_SUCCESS for all other tasks
 */
tcelm_barrier_wait_result_t tcelm_barrier_wait(tcelm_barrier_t *barrier);

/*
 * Wait with timeout (milliseconds)
 * Returns TCELM_BARRIER_WAIT_TIMEOUT if timeout expires before all tasks arrive
 */
tcelm_barrier_wait_result_t tcelm_barrier_wait_timeout(
    tcelm_barrier_t *barrier,
    uint32_t timeout_ms
);

/*
 * Release all waiting tasks manually
 * Used when auto_release is false, or to abort waiting tasks
 * Returns number of tasks released
 */
uint32_t tcelm_barrier_release(tcelm_barrier_t *barrier);

/*
 * Get the number of tasks currently waiting at the barrier
 * Note: This value may be stale by the time it's returned
 */
uint32_t tcelm_barrier_get_waiting(tcelm_barrier_t *barrier);

/*
 * Reset the barrier for reuse
 * Only safe to call when no tasks are waiting
 */
int tcelm_barrier_reset(tcelm_barrier_t *barrier);

/*
 * Delete a barrier
 */
void tcelm_barrier_delete(tcelm_barrier_t *barrier);

#endif /* TCELM_BARRIER_H */
