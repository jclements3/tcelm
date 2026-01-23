/*
 * tcelm_rms.c - Rate Monotonic Scheduler runtime implementation
 *
 * Implements RMS period management using RTEMS rate_monotonic API
 * with deadline violation detection and statistics tracking.
 */

#include "tcelm_rms.h"
#include "tcelm_atomic.h"
#include <string.h>
#include <stdio.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/ratemon.h>
#include <rtems/rtems/clock.h>
#include <rtems/rtems/tasks.h>
#else
#include <time.h>
#include <unistd.h>
#endif

/* Period counter for unique names */
static uint32_t period_counter = 0;

/* Global deadline miss counter */
static uint32_t global_missed_count = 0;

/*
 * Initialize RMS subsystem
 */
int tcelm_rms_init(void) {
    period_counter = 0;
    global_missed_count = 0;
    return 0;
}

/*
 * Shutdown RMS subsystem
 */
void tcelm_rms_shutdown(void) {
    /* Nothing to do - periods should be deleted by owner */
}

#ifdef __rtems__

/*
 * Get current time in microseconds (RTEMS)
 */
uint64_t tcelm_rms_now_us(void) {
    struct timespec ts;
    rtems_clock_get_uptime(&ts);
    return (uint64_t)ts.tv_sec * 1000000ULL + (uint64_t)ts.tv_nsec / 1000ULL;
}

/*
 * Convert milliseconds to ticks (RTEMS)
 */
uint32_t tcelm_rms_ms_to_ticks(uint32_t ms) {
    uint32_t tps = rtems_clock_get_ticks_per_second();
    uint32_t ticks = (ms * tps) / 1000;
    return ticks > 0 ? ticks : 1;
}

/*
 * Convert ticks to milliseconds (RTEMS)
 */
uint32_t tcelm_rms_ticks_to_ms(uint32_t ticks) {
    uint32_t tps = rtems_clock_get_ticks_per_second();
    return (ticks * 1000) / tps;
}

/*
 * Create rate monotonic period (RTEMS)
 */
tcelm_rms_period_t *tcelm_rms_create(
    tcelm_arena_t *arena,
    uint32_t period_ms
) {
    rtems_status_code status;
    rtems_id period_id;

    tcelm_rms_period_t *period = tcelm_arena_alloc(arena, sizeof(tcelm_rms_period_t));
    if (!period) return NULL;

    /* Build unique name (R000-R999) */
    uint32_t num = tcelm_atomic_fetch_add_u32(&period_counter, 1);
    snprintf(period->name, sizeof(period->name), "R%03u", num % 1000);
    rtems_name rname = rtems_build_name(
        period->name[0], period->name[1],
        period->name[2], period->name[3]
    );

    status = rtems_rate_monotonic_create(rname, &period_id);
    if (status != RTEMS_SUCCESSFUL) {
        return NULL;
    }

    period->period_id = period_id;
    period->period_ms = period_ms;
    period->period_ticks = tcelm_rms_ms_to_ticks(period_ms);
    period->started = false;
    period->last_status = TCELM_DEADLINE_NOT_STARTED;
    period->on_miss_callback = NULL;
    period->on_miss_user_data = NULL;
    period->local_missed_count = 0;
    period->last_period_start_us = 0;

    return period;
}

/*
 * Create rate monotonic period with ticks (RTEMS)
 */
tcelm_rms_period_t *tcelm_rms_create_ticks(
    tcelm_arena_t *arena,
    uint32_t period_ticks
) {
    rtems_status_code status;
    rtems_id period_id;

    tcelm_rms_period_t *period = tcelm_arena_alloc(arena, sizeof(tcelm_rms_period_t));
    if (!period) return NULL;

    uint32_t num = tcelm_atomic_fetch_add_u32(&period_counter, 1);
    snprintf(period->name, sizeof(period->name), "R%03u", num % 1000);
    rtems_name rname = rtems_build_name(
        period->name[0], period->name[1],
        period->name[2], period->name[3]
    );

    status = rtems_rate_monotonic_create(rname, &period_id);
    if (status != RTEMS_SUCCESSFUL) {
        return NULL;
    }

    period->period_id = period_id;
    period->period_ticks = period_ticks;
    period->period_ms = tcelm_rms_ticks_to_ms(period_ticks);
    period->started = false;
    period->last_status = TCELM_DEADLINE_NOT_STARTED;
    period->on_miss_callback = NULL;
    period->on_miss_user_data = NULL;
    period->local_missed_count = 0;
    period->last_period_start_us = 0;

    return period;
}

/*
 * Delete rate monotonic period (RTEMS)
 */
void tcelm_rms_delete(tcelm_rms_period_t *period) {
    if (!period) return;

    if (period->period_id) {
        rtems_rate_monotonic_delete(period->period_id);
        period->period_id = 0;
    }

    period->started = false;
}

/*
 * Start/initiate a period (RTEMS)
 */
tcelm_deadline_status_t tcelm_rms_start(tcelm_rms_period_t *period) {
    if (!period) return TCELM_DEADLINE_NOT_STARTED;

    rtems_status_code status = rtems_rate_monotonic_period(
        period->period_id,
        period->period_ticks
    );

    period->started = true;
    period->last_period_start_us = tcelm_rms_now_us();

    if (status == RTEMS_SUCCESSFUL) {
        period->last_status = TCELM_DEADLINE_NOT_STARTED;
        return TCELM_DEADLINE_NOT_STARTED;
    }

    return TCELM_DEADLINE_NOT_STARTED;
}

/*
 * Cancel a period (RTEMS)
 */
int tcelm_rms_cancel(tcelm_rms_period_t *period) {
    if (!period) return -1;

    rtems_status_code status = rtems_rate_monotonic_cancel(period->period_id);
    period->started = false;

    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

/*
 * Wait for next period boundary (RTEMS)
 *
 * This is the core RMS operation.
 */
tcelm_deadline_status_t tcelm_rms_wait_period(tcelm_rms_period_t *period) {
    if (!period) return TCELM_DEADLINE_NOT_STARTED;

    /* If not started, start now */
    if (!period->started) {
        return tcelm_rms_start(period);
    }

    /* Call rtems_rate_monotonic_period to wait for next period */
    rtems_status_code status = rtems_rate_monotonic_period(
        period->period_id,
        period->period_ticks
    );

    period->last_period_start_us = tcelm_rms_now_us();

    if (status == RTEMS_TIMEOUT) {
        /* Deadline was missed! */
        period->last_status = TCELM_DEADLINE_MISSED;
        period->local_missed_count++;
        tcelm_atomic_fetch_add_u32(&global_missed_count, 1);

        /* Invoke callback if registered */
        if (period->on_miss_callback) {
            tcelm_rms_stats_t stats;
            tcelm_rms_get_stats(period, &stats);
            period->on_miss_callback(period->on_miss_user_data, &stats);
        }

        return TCELM_DEADLINE_MISSED;
    }

    if (status == RTEMS_SUCCESSFUL) {
        period->last_status = TCELM_DEADLINE_ON_TIME;
        return TCELM_DEADLINE_ON_TIME;
    }

    /* Some other error */
    return TCELM_DEADLINE_NOT_STARTED;
}

/*
 * Check deadline status without blocking (RTEMS)
 */
tcelm_deadline_status_t tcelm_rms_check_status(tcelm_rms_period_t *period) {
    if (!period) return TCELM_DEADLINE_NOT_STARTED;
    if (!period->started) return TCELM_DEADLINE_NOT_STARTED;

    rtems_rate_monotonic_period_status status;
    rtems_status_code sc = rtems_rate_monotonic_get_status(
        period->period_id,
        &status
    );

    if (sc != RTEMS_SUCCESSFUL) {
        return TCELM_DEADLINE_NOT_STARTED;
    }

    /* Check if we're past the deadline */
    /* RTEMS 7: state is RATE_MONOTONIC_EXPIRED if overrun */
    if (status.state == RATE_MONOTONIC_EXPIRED) {
        return TCELM_DEADLINE_MISSED;
    }

    return period->last_status;
}

/*
 * Get comprehensive statistics (RTEMS)
 */
int tcelm_rms_get_stats(tcelm_rms_period_t *period, tcelm_rms_stats_t *stats) {
    if (!period || !stats) return -1;

    rtems_rate_monotonic_period_statistics rtems_stats;
    rtems_status_code status = rtems_rate_monotonic_get_statistics(
        period->period_id,
        &rtems_stats
    );

    if (status != RTEMS_SUCCESSFUL) {
        /* Return what we know locally */
        memset(stats, 0, sizeof(*stats));
        stats->missed_count = period->local_missed_count;
        stats->period_ms = period->period_ms;
        stats->last_status = period->last_status;
        return -1;
    }

    stats->count = rtems_stats.count;
    stats->missed_count = rtems_stats.missed_count;

    /* Convert timespec to microseconds */
    stats->min_cpu_time_us = (uint32_t)(
        rtems_stats.min_cpu_time.tv_sec * 1000000 +
        rtems_stats.min_cpu_time.tv_nsec / 1000
    );
    stats->max_cpu_time_us = (uint32_t)(
        rtems_stats.max_cpu_time.tv_sec * 1000000 +
        rtems_stats.max_cpu_time.tv_nsec / 1000
    );

    if (rtems_stats.count > 0) {
        uint64_t total_us = (uint64_t)rtems_stats.total_cpu_time.tv_sec * 1000000 +
                           rtems_stats.total_cpu_time.tv_nsec / 1000;
        stats->avg_cpu_time_us = (uint32_t)(total_us / rtems_stats.count);
    } else {
        stats->avg_cpu_time_us = 0;
    }

    stats->period_ms = period->period_ms;
    stats->last_status = period->last_status;

    return 0;
}

/*
 * Get missed deadline count (RTEMS)
 */
uint32_t tcelm_rms_get_missed_count(tcelm_rms_period_t *period) {
    if (!period) return 0;

    rtems_rate_monotonic_period_statistics rtems_stats;
    rtems_status_code status = rtems_rate_monotonic_get_statistics(
        period->period_id,
        &rtems_stats
    );

    if (status == RTEMS_SUCCESSFUL) {
        return rtems_stats.missed_count;
    }

    return period->local_missed_count;
}

/*
 * Reset statistics (RTEMS)
 */
int tcelm_rms_reset_stats(tcelm_rms_period_t *period) {
    if (!period) return -1;

    rtems_status_code status = rtems_rate_monotonic_reset_statistics(
        period->period_id
    );

    period->local_missed_count = 0;

    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

/*
 * Register deadline miss callback (RTEMS)
 */
void tcelm_rms_set_deadline_callback(
    tcelm_rms_period_t *period,
    tcelm_rms_deadline_callback callback,
    void *user_data
) {
    if (!period) return;

    period->on_miss_callback = callback;
    period->on_miss_user_data = user_data;
}

/*
 * Assign RTEMS priority based on RMS policy (RTEMS)
 */
int tcelm_rms_assign_priority(uint32_t period_ms) {
    rtems_task_priority priority = tcelm_rms_priority_from_period(period_ms);
    rtems_task_priority old_priority;

    rtems_status_code status = rtems_task_set_priority(
        RTEMS_SELF,
        priority,
        &old_priority
    );

    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

/*
 * Calculate priority from period (RTEMS)
 */
uint8_t tcelm_rms_priority_from_period(uint32_t period_ms) {
    /* RMS: shorter period = higher priority (lower number in RTEMS) */
    /* Map period directly to priority, capped at 1-255 */
    if (period_ms < 1) return 1;
    if (period_ms > 255) return 255;
    return (uint8_t)period_ms;
}

#else /* Native (POSIX) implementation for development/testing */

uint64_t tcelm_rms_now_us(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000ULL + (uint64_t)ts.tv_nsec / 1000ULL;
}

uint32_t tcelm_rms_ms_to_ticks(uint32_t ms) {
    return ms;  /* 1ms = 1 tick on native */
}

uint32_t tcelm_rms_ticks_to_ms(uint32_t ticks) {
    return ticks;
}

tcelm_rms_period_t *tcelm_rms_create(
    tcelm_arena_t *arena,
    uint32_t period_ms
) {
    tcelm_rms_period_t *period = tcelm_arena_alloc(arena, sizeof(tcelm_rms_period_t));
    if (!period) return NULL;

    uint32_t num = tcelm_atomic_fetch_add_u32(&period_counter, 1);
    snprintf(period->name, sizeof(period->name), "R%03u", num % 1000);

    period->period_id = num;
    period->period_ms = period_ms;
    period->period_ticks = period_ms;
    period->started = false;
    period->last_status = TCELM_DEADLINE_NOT_STARTED;
    period->on_miss_callback = NULL;
    period->on_miss_user_data = NULL;
    period->local_missed_count = 0;
    period->last_period_start_us = 0;

    return period;
}

tcelm_rms_period_t *tcelm_rms_create_ticks(
    tcelm_arena_t *arena,
    uint32_t period_ticks
) {
    return tcelm_rms_create(arena, period_ticks);
}

void tcelm_rms_delete(tcelm_rms_period_t *period) {
    if (period) {
        period->started = false;
        period->period_id = 0;
    }
}

tcelm_deadline_status_t tcelm_rms_start(tcelm_rms_period_t *period) {
    if (!period) return TCELM_DEADLINE_NOT_STARTED;

    period->started = true;
    period->last_period_start_us = tcelm_rms_now_us();
    period->last_status = TCELM_DEADLINE_NOT_STARTED;

    return TCELM_DEADLINE_NOT_STARTED;
}

int tcelm_rms_cancel(tcelm_rms_period_t *period) {
    if (period) {
        period->started = false;
    }
    return 0;
}

tcelm_deadline_status_t tcelm_rms_wait_period(tcelm_rms_period_t *period) {
    if (!period) return TCELM_DEADLINE_NOT_STARTED;

    if (!period->started) {
        return tcelm_rms_start(period);
    }

    uint64_t now = tcelm_rms_now_us();
    uint64_t elapsed = now - period->last_period_start_us;
    uint64_t period_us = (uint64_t)period->period_ms * 1000;

    if (elapsed < period_us) {
        /* Sleep for remaining time */
        uint64_t remaining_us = period_us - elapsed;
        struct timespec ts = {
            .tv_sec = remaining_us / 1000000,
            .tv_nsec = (remaining_us % 1000000) * 1000
        };
        nanosleep(&ts, NULL);
        period->last_status = TCELM_DEADLINE_ON_TIME;
    } else {
        /* Overrun - deadline missed */
        period->last_status = TCELM_DEADLINE_MISSED;
        period->local_missed_count++;
        tcelm_atomic_fetch_add_u32(&global_missed_count, 1);

        if (period->on_miss_callback) {
            tcelm_rms_stats_t stats;
            tcelm_rms_get_stats(period, &stats);
            period->on_miss_callback(period->on_miss_user_data, &stats);
        }
    }

    period->last_period_start_us = tcelm_rms_now_us();
    return period->last_status;
}

tcelm_deadline_status_t tcelm_rms_check_status(tcelm_rms_period_t *period) {
    if (!period) return TCELM_DEADLINE_NOT_STARTED;
    if (!period->started) return TCELM_DEADLINE_NOT_STARTED;

    uint64_t now = tcelm_rms_now_us();
    uint64_t elapsed = now - period->last_period_start_us;
    uint64_t period_us = (uint64_t)period->period_ms * 1000;

    if (elapsed > period_us) {
        return TCELM_DEADLINE_MISSED;
    }

    return period->last_status;
}

int tcelm_rms_get_stats(tcelm_rms_period_t *period, tcelm_rms_stats_t *stats) {
    if (!period || !stats) return -1;

    memset(stats, 0, sizeof(*stats));
    stats->missed_count = period->local_missed_count;
    stats->period_ms = period->period_ms;
    stats->last_status = period->last_status;

    return 0;
}

uint32_t tcelm_rms_get_missed_count(tcelm_rms_period_t *period) {
    return period ? period->local_missed_count : 0;
}

int tcelm_rms_reset_stats(tcelm_rms_period_t *period) {
    if (period) {
        period->local_missed_count = 0;
    }
    return 0;
}

void tcelm_rms_set_deadline_callback(
    tcelm_rms_period_t *period,
    tcelm_rms_deadline_callback callback,
    void *user_data
) {
    if (period) {
        period->on_miss_callback = callback;
        period->on_miss_user_data = user_data;
    }
}

int tcelm_rms_assign_priority(uint32_t period_ms) {
    (void)period_ms;
    /* Native: no-op (no RTEMS task priorities) */
    return 0;
}

uint8_t tcelm_rms_priority_from_period(uint32_t period_ms) {
    if (period_ms < 1) return 1;
    if (period_ms > 255) return 255;
    return (uint8_t)period_ms;
}

#endif /* __rtems__ */

/*
 * Global deadline miss counter
 */
uint32_t tcelm_rms_global_missed_count(void) {
    return global_missed_count;
}

/*
 * Reset global missed count
 */
void tcelm_rms_reset_global_missed_count(void) {
    global_missed_count = 0;
}
