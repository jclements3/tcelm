/*
 * tcelm_timer.h - Timer runtime for RTEMS
 *
 * Wraps RTEMS timer and rate monotonic primitives for time-based
 * subscriptions and periodic execution.
 */

#ifndef TCELM_TIMER_H
#define TCELM_TIMER_H

#include "tcelm_types.h"
#include "tcelm_arena.h"

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/timer.h>
#include <rtems/rtems/ratemon.h>
#else
/* Native stubs */
typedef uint32_t rtems_id;
typedef uint32_t rtems_status_code;
typedef uint32_t rtems_interval;
#define RTEMS_SUCCESSFUL 0
#endif

/*
 * Timer types
 */
typedef enum tcelm_timer_type {
    TCELM_TIMER_ONESHOT,    /* Fire once after delay */
    TCELM_TIMER_PERIODIC,   /* Fire repeatedly at interval */
    TCELM_TIMER_RATE_MONO   /* Rate monotonic (hard real-time) */
} tcelm_timer_type_t;

/*
 * Timer callback function
 */
typedef void (*tcelm_timer_callback)(void *user_data);

/*
 * Timer handle
 */
typedef struct tcelm_timer {
    rtems_id timer_id;              /* RTEMS timer ID */
    rtems_id period_id;             /* Rate monotonic period ID (if applicable) */
    tcelm_timer_type_t type;        /* Timer type */
    uint32_t interval_ms;           /* Interval in milliseconds */
    tcelm_timer_callback callback;  /* User callback */
    void *user_data;                /* Callback argument */
    bool active;                    /* Is timer running? */
    const char *name;               /* Name for debugging */
} tcelm_timer_t;

/*
 * Initialize timer subsystem
 */
int tcelm_timer_init(void);

/*
 * Shutdown timer subsystem
 */
void tcelm_timer_shutdown(void);

/*
 * Create a one-shot timer (fires once)
 */
tcelm_timer_t *tcelm_timer_create_oneshot(
    tcelm_arena_t *arena,
    uint32_t delay_ms,
    tcelm_timer_callback callback,
    void *user_data
);

/*
 * Create a periodic timer (fires repeatedly)
 */
tcelm_timer_t *tcelm_timer_create_periodic(
    tcelm_arena_t *arena,
    uint32_t interval_ms,
    tcelm_timer_callback callback,
    void *user_data
);

/*
 * Create a rate monotonic timer (hard real-time)
 * Uses rtems_rate_monotonic_period for deterministic timing
 */
tcelm_timer_t *tcelm_timer_create_rate_monotonic(
    tcelm_arena_t *arena,
    uint32_t period_ms,
    tcelm_timer_callback callback,
    void *user_data
);

/*
 * Start a timer
 */
int tcelm_timer_start(tcelm_timer_t *timer);

/*
 * Stop/cancel a timer
 */
int tcelm_timer_cancel(tcelm_timer_t *timer);

/*
 * Reset a timer (restart from beginning)
 */
int tcelm_timer_reset(tcelm_timer_t *timer);

/*
 * Delete a timer
 */
void tcelm_timer_delete(tcelm_timer_t *timer);

/*
 * Get current time in milliseconds since boot
 */
uint64_t tcelm_timer_now_ms(void);

/*
 * Get current time in microseconds since boot
 */
uint64_t tcelm_timer_now_us(void);

/*
 * Get current tick count
 */
uint32_t tcelm_timer_ticks(void);

/*
 * Get ticks per second
 */
uint32_t tcelm_timer_ticks_per_second(void);

/*
 * Convert milliseconds to ticks
 */
uint32_t tcelm_timer_ms_to_ticks(uint32_t ms);

/*
 * Convert ticks to milliseconds
 */
uint32_t tcelm_timer_ticks_to_ms(uint32_t ticks);

/*
 * Rate monotonic period execution
 * Call this at the start of each period in a rate monotonic task
 * Returns 0 on success, -1 if deadline was missed
 */
int tcelm_timer_period_wait(tcelm_timer_t *timer);

/*
 * Get rate monotonic statistics
 */
typedef struct tcelm_timer_stats {
    uint32_t count;             /* Number of periods executed */
    uint32_t missed_count;      /* Number of missed deadlines */
    uint32_t min_cpu_time_us;   /* Minimum CPU time per period */
    uint32_t max_cpu_time_us;   /* Maximum CPU time per period */
    uint32_t avg_cpu_time_us;   /* Average CPU time per period */
} tcelm_timer_stats_t;

int tcelm_timer_get_stats(tcelm_timer_t *timer, tcelm_timer_stats_t *stats);

#endif /* TCELM_TIMER_H */
