/*
 * tcelm_rms.h - Rate Monotonic Scheduler runtime for RTEMS
 *
 * Provides hard real-time periodic task execution with deadline
 * violation detection. Implements RTEMS rate_monotonic period
 * management with Elm-friendly statistics and notifications.
 */

#ifndef TCELM_RMS_H
#define TCELM_RMS_H

#include "tcelm_types.h"
#include "tcelm_arena.h"

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/ratemon.h>
#else
/* Native stubs for development/testing */
typedef uint32_t rtems_id;
typedef uint32_t rtems_status_code;
typedef uint32_t rtems_interval;
#define RTEMS_SUCCESSFUL 0
#define RTEMS_TIMEOUT 1
#endif

/*
 * Deadline status codes (matches Elm DeadlineStatus type)
 */
typedef enum tcelm_deadline_status {
    TCELM_DEADLINE_ON_TIME = 0,    /* Period completed within deadline */
    TCELM_DEADLINE_MISSED = 1,     /* Deadline was violated */
    TCELM_DEADLINE_NOT_STARTED = 2 /* Period not yet started */
} tcelm_deadline_status_t;

/*
 * Period statistics (matches Elm PeriodStats type)
 */
typedef struct tcelm_rms_stats {
    uint32_t count;              /* Number of periods completed */
    uint32_t missed_count;       /* Number of deadline violations */
    uint32_t min_cpu_time_us;    /* Minimum CPU time per period (us) */
    uint32_t max_cpu_time_us;    /* Maximum CPU time per period (us) */
    uint32_t avg_cpu_time_us;    /* Average CPU time per period (us) */
    uint32_t period_ms;          /* Configured period (ms) */
    tcelm_deadline_status_t last_status; /* Status of last period */
} tcelm_rms_stats_t;

/*
 * Deadline miss callback function type
 */
typedef void (*tcelm_rms_deadline_callback)(void *user_data, tcelm_rms_stats_t *stats);

/*
 * RMS Period handle
 */
typedef struct tcelm_rms_period {
    rtems_id period_id;              /* RTEMS rate monotonic period ID */
    uint32_t period_ms;              /* Period in milliseconds */
    uint32_t period_ticks;           /* Period in ticks */
    bool started;                    /* Has period been started? */
    tcelm_deadline_status_t last_status; /* Last period status */

    /* Deadline miss notification */
    tcelm_rms_deadline_callback on_miss_callback;
    void *on_miss_user_data;

    /* Local statistics (in addition to RTEMS stats) */
    uint32_t local_missed_count;     /* Missed count since last reset */
    uint64_t last_period_start_us;   /* Start time of last period */

    /* Debug name */
    char name[8];
} tcelm_rms_period_t;

/*
 * Initialize RMS subsystem
 */
int tcelm_rms_init(void);

/*
 * Shutdown RMS subsystem
 */
void tcelm_rms_shutdown(void);

/*
 * Create a new rate monotonic period
 *
 * Returns NULL on failure, valid handle on success.
 * The period is not started until tcelm_rms_start() is called.
 */
tcelm_rms_period_t *tcelm_rms_create(
    tcelm_arena_t *arena,
    uint32_t period_ms
);

/*
 * Create a rate monotonic period with ticks (more precise)
 */
tcelm_rms_period_t *tcelm_rms_create_ticks(
    tcelm_arena_t *arena,
    uint32_t period_ticks
);

/*
 * Delete a rate monotonic period
 */
void tcelm_rms_delete(tcelm_rms_period_t *period);

/*
 * Start/initiate a period
 *
 * First call initiates the period. Subsequent calls should use
 * tcelm_rms_wait_period() instead.
 *
 * Returns: TCELM_DEADLINE_NOT_STARTED on first call
 */
tcelm_deadline_status_t tcelm_rms_start(tcelm_rms_period_t *period);

/*
 * Cancel (stop) a period
 */
int tcelm_rms_cancel(tcelm_rms_period_t *period);

/*
 * Wait for the next period boundary
 *
 * This is the core RMS operation. Call at the START of each periodic
 * iteration. Blocks until the next period begins.
 *
 * Returns:
 *   TCELM_DEADLINE_ON_TIME  - Previous period completed within deadline
 *   TCELM_DEADLINE_MISSED   - Previous period overran its deadline
 *   TCELM_DEADLINE_NOT_STARTED - First call (period just started)
 *
 * IMPORTANT: If MISSED is returned, the deadline miss callback is
 * invoked automatically (if registered).
 */
tcelm_deadline_status_t tcelm_rms_wait_period(tcelm_rms_period_t *period);

/*
 * Check deadline status without blocking
 *
 * Returns the status of the previous period without waiting for
 * the next period boundary.
 */
tcelm_deadline_status_t tcelm_rms_check_status(tcelm_rms_period_t *period);

/*
 * Get comprehensive period statistics
 */
int tcelm_rms_get_stats(tcelm_rms_period_t *period, tcelm_rms_stats_t *stats);

/*
 * Get only the missed deadline count
 */
uint32_t tcelm_rms_get_missed_count(tcelm_rms_period_t *period);

/*
 * Reset statistics for a period
 */
int tcelm_rms_reset_stats(tcelm_rms_period_t *period);

/*
 * Register deadline miss callback
 *
 * The callback is invoked immediately when a deadline violation
 * is detected during tcelm_rms_wait_period().
 */
void tcelm_rms_set_deadline_callback(
    tcelm_rms_period_t *period,
    tcelm_rms_deadline_callback callback,
    void *user_data
);

/*
 * Assign RTEMS priority based on RMS policy
 *
 * Sets current task's priority to match its period (shorter period
 * = higher priority = lower RTEMS priority number).
 *
 * Returns: 0 on success, -1 on failure
 */
int tcelm_rms_assign_priority(uint32_t period_ms);

/*
 * Calculate priority from period
 *
 * Returns the RTEMS priority value for a given period.
 * Uses simple mapping: priority = period_ms (capped 1-255)
 */
uint8_t tcelm_rms_priority_from_period(uint32_t period_ms);

/*
 * Get current time in microseconds (for internal timing)
 */
uint64_t tcelm_rms_now_us(void);

/*
 * Convert milliseconds to ticks
 */
uint32_t tcelm_rms_ms_to_ticks(uint32_t ms);

/*
 * Convert ticks to milliseconds
 */
uint32_t tcelm_rms_ticks_to_ms(uint32_t ticks);

/*
 * Global deadline miss counter (for system-wide monitoring)
 */
uint32_t tcelm_rms_global_missed_count(void);

/*
 * Reset global missed count
 */
void tcelm_rms_reset_global_missed_count(void);

#endif /* TCELM_RMS_H */
