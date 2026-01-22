/*
 * tcelm_semaphore.h - Counting semaphore runtime for RTEMS
 *
 * Counting semaphores provide resource counting and event signaling.
 * On RTEMS, wraps counting semaphores with optional priority inheritance.
 *
 * Use cases:
 * - Resource pools (e.g., limit concurrent access to N resources)
 * - Event notification (signal N waiting tasks)
 * - Producer/consumer coordination
 */

#ifndef TCELM_SEMAPHORE_H
#define TCELM_SEMAPHORE_H

#include "tcelm_types.h"
#include "tcelm_arena.h"

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/sem.h>
#else
/* Native stubs */
#include <pthread.h>
#include <semaphore.h>
typedef uint32_t rtems_id;
typedef uint32_t rtems_status_code;
#define RTEMS_SUCCESSFUL 0
#endif

/*
 * Semaphore handle
 */
typedef struct tcelm_semaphore {
    rtems_id sem_id;            /* RTEMS semaphore ID */
    void *native_data;          /* Native implementation data (pthread sem) */
    uint32_t initial_count;     /* Initial count for reference */
    uint32_t max_count;         /* Maximum count (0 = unlimited) */
    const char *name;           /* Name for debugging */
} tcelm_semaphore_t;

/*
 * Semaphore configuration
 */
typedef struct tcelm_semaphore_config {
    uint32_t initial_count;     /* Starting count (default: 0) */
    uint32_t max_count;         /* Max count, 0 = unlimited (default: 0) */
    bool priority_ceiling;      /* Use priority ceiling protocol */
    uint8_t ceiling_priority;   /* Priority ceiling if enabled */
    const char *name;           /* Optional name */
} tcelm_semaphore_config_t;

/* Default configuration */
extern const tcelm_semaphore_config_t TCELM_SEMAPHORE_DEFAULT_CONFIG;

/*
 * Initialize semaphore subsystem
 */
int tcelm_semaphore_init(void);

/*
 * Shutdown semaphore subsystem
 */
void tcelm_semaphore_shutdown(void);

/*
 * Create a new counting semaphore
 * Returns semaphore handle or NULL on failure
 */
tcelm_semaphore_t *tcelm_semaphore_create(
    tcelm_arena_t *arena,
    const tcelm_semaphore_config_t *config
);

/*
 * Create semaphore with initial count (convenience function)
 */
tcelm_semaphore_t *tcelm_semaphore_create_with_count(
    tcelm_arena_t *arena,
    uint32_t initial_count
);

/*
 * Acquire (decrement) the semaphore
 * Blocks if count is 0
 * Returns 0 on success, -1 on error
 */
int tcelm_semaphore_acquire(tcelm_semaphore_t *sem);

/*
 * Acquire with timeout (milliseconds)
 * Returns 0 on success, -1 on timeout or error
 */
int tcelm_semaphore_acquire_timeout(
    tcelm_semaphore_t *sem,
    uint32_t timeout_ms
);

/*
 * Try to acquire without blocking
 * Returns 0 on success, -1 if count is 0
 */
int tcelm_semaphore_try_acquire(tcelm_semaphore_t *sem);

/*
 * Release (increment) the semaphore
 * Wakes one waiting task if any
 * Returns 0 on success, -1 on error (e.g., max count exceeded)
 */
int tcelm_semaphore_release(tcelm_semaphore_t *sem);

/*
 * Release multiple permits at once
 * Wakes up to 'count' waiting tasks
 * Returns 0 on success, -1 on error
 */
int tcelm_semaphore_release_multiple(tcelm_semaphore_t *sem, uint32_t count);

/*
 * Get current count (non-blocking, for diagnostics)
 * Note: Value may be stale by the time it's read
 */
uint32_t tcelm_semaphore_get_count(tcelm_semaphore_t *sem);

/*
 * Flush the semaphore - release all waiting tasks
 * Each waiting task returns with error status
 * Returns number of tasks released
 */
uint32_t tcelm_semaphore_flush(tcelm_semaphore_t *sem);

/*
 * Delete a semaphore
 */
void tcelm_semaphore_delete(tcelm_semaphore_t *sem);

#endif /* TCELM_SEMAPHORE_H */
