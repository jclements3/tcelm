/*
 * tcelm_timer.c - Timer runtime for RTEMS
 *
 * Implementation of timers using RTEMS timer and rate monotonic services.
 */

#include "tcelm_timer.h"
#include <string.h>
#include <stdio.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/timer.h>
#include <rtems/rtems/ratemon.h>
#include <rtems/rtems/clock.h>
#else
#include <pthread.h>
#include <time.h>
#include <signal.h>
#include <stdlib.h>
#include <sys/time.h>
#endif

/* Timer counter for unique names */
static uint32_t timer_counter = 0;

/*
 * Initialize timer subsystem
 */
int tcelm_timer_init(void) {
    timer_counter = 0;
    return 0;
}

/*
 * Shutdown timer subsystem
 */
void tcelm_timer_shutdown(void) {
    /* Nothing to do */
}

#ifdef __rtems__

/*
 * Timer service routine - called by RTEMS timer ISR
 */
static void timer_service_routine(rtems_id timer_id, void *user_data) {
    (void)timer_id;
    tcelm_timer_t *timer = (tcelm_timer_t *)user_data;

    if (timer->callback) {
        timer->callback(timer->user_data);
    }

    /* Restart periodic timers */
    if (timer->type == TCELM_TIMER_PERIODIC && timer->active) {
        rtems_interval ticks = tcelm_timer_ms_to_ticks(timer->interval_ms);
        rtems_timer_fire_after(timer->timer_id, ticks, timer_service_routine, timer);
    }
}

/*
 * Create one-shot timer (RTEMS implementation)
 */
tcelm_timer_t *tcelm_timer_create_oneshot(
    tcelm_arena_t *arena,
    uint32_t delay_ms,
    tcelm_timer_callback callback,
    void *user_data
) {
    rtems_status_code status;
    rtems_id timer_id;

    tcelm_timer_t *timer = tcelm_arena_alloc(arena, sizeof(tcelm_timer_t));
    if (!timer) return NULL;

    /* Build unique timer name */
    char name[5];
    uint32_t num = __sync_fetch_and_add(&timer_counter, 1);
    snprintf(name, 5, "T%03u", num % 1000);
    rtems_name timer_name = rtems_build_name(name[0], name[1], name[2], name[3]);

    status = rtems_timer_create(timer_name, &timer_id);
    if (status != RTEMS_SUCCESSFUL) {
        return NULL;
    }

    timer->timer_id = timer_id;
    timer->period_id = 0;
    timer->type = TCELM_TIMER_ONESHOT;
    timer->interval_ms = delay_ms;
    timer->callback = callback;
    timer->user_data = user_data;
    timer->active = false;
    timer->name = name;

    return timer;
}

/*
 * Create periodic timer (RTEMS implementation)
 */
tcelm_timer_t *tcelm_timer_create_periodic(
    tcelm_arena_t *arena,
    uint32_t interval_ms,
    tcelm_timer_callback callback,
    void *user_data
) {
    tcelm_timer_t *timer = tcelm_timer_create_oneshot(arena, interval_ms, callback, user_data);
    if (timer) {
        timer->type = TCELM_TIMER_PERIODIC;
    }
    return timer;
}

/*
 * Create rate monotonic timer (RTEMS implementation)
 */
tcelm_timer_t *tcelm_timer_create_rate_monotonic(
    tcelm_arena_t *arena,
    uint32_t period_ms,
    tcelm_timer_callback callback,
    void *user_data
) {
    rtems_status_code status;
    rtems_id period_id;

    tcelm_timer_t *timer = tcelm_arena_alloc(arena, sizeof(tcelm_timer_t));
    if (!timer) return NULL;

    char name[5];
    uint32_t num = __sync_fetch_and_add(&timer_counter, 1);
    snprintf(name, 5, "R%03u", num % 1000);
    rtems_name period_name = rtems_build_name(name[0], name[1], name[2], name[3]);

    status = rtems_rate_monotonic_create(period_name, &period_id);
    if (status != RTEMS_SUCCESSFUL) {
        return NULL;
    }

    timer->timer_id = 0;
    timer->period_id = period_id;
    timer->type = TCELM_TIMER_RATE_MONO;
    timer->interval_ms = period_ms;
    timer->callback = callback;
    timer->user_data = user_data;
    timer->active = false;
    timer->name = name;

    return timer;
}

/*
 * Start timer (RTEMS implementation)
 */
int tcelm_timer_start(tcelm_timer_t *timer) {
    rtems_status_code status;

    if (timer->type == TCELM_TIMER_RATE_MONO) {
        /* Rate monotonic - first call initiates the period */
        rtems_interval ticks = tcelm_timer_ms_to_ticks(timer->interval_ms);
        status = rtems_rate_monotonic_period(timer->period_id, ticks);
    } else {
        /* Regular timer */
        rtems_interval ticks = tcelm_timer_ms_to_ticks(timer->interval_ms);
        status = rtems_timer_fire_after(
            timer->timer_id,
            ticks,
            timer_service_routine,
            timer
        );
    }

    if (status == RTEMS_SUCCESSFUL) {
        timer->active = true;
        return 0;
    }

    return -1;
}

/*
 * Cancel timer (RTEMS implementation)
 */
int tcelm_timer_cancel(tcelm_timer_t *timer) {
    rtems_status_code status;

    timer->active = false;

    if (timer->type == TCELM_TIMER_RATE_MONO) {
        status = rtems_rate_monotonic_cancel(timer->period_id);
    } else {
        status = rtems_timer_cancel(timer->timer_id);
    }

    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

/*
 * Reset timer (RTEMS implementation)
 */
int tcelm_timer_reset(tcelm_timer_t *timer) {
    if (timer->type == TCELM_TIMER_RATE_MONO) {
        /* Rate monotonic doesn't have reset - just restart period */
        return tcelm_timer_start(timer);
    }

    rtems_status_code status = rtems_timer_reset(timer->timer_id);
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

/*
 * Delete timer (RTEMS implementation)
 */
void tcelm_timer_delete(tcelm_timer_t *timer) {
    if (!timer) return;

    timer->active = false;

    if (timer->type == TCELM_TIMER_RATE_MONO) {
        if (timer->period_id) {
            rtems_rate_monotonic_delete(timer->period_id);
            timer->period_id = 0;
        }
    } else {
        if (timer->timer_id) {
            rtems_timer_delete(timer->timer_id);
            timer->timer_id = 0;
        }
    }
}

/*
 * Get current time in ms (RTEMS implementation)
 */
uint64_t tcelm_timer_now_ms(void) {
    struct timespec ts;
    rtems_clock_get_uptime(&ts);
    return (uint64_t)ts.tv_sec * 1000 + (uint64_t)ts.tv_nsec / 1000000;
}

/*
 * Get current time in us (RTEMS implementation)
 */
uint64_t tcelm_timer_now_us(void) {
    struct timespec ts;
    rtems_clock_get_uptime(&ts);
    return (uint64_t)ts.tv_sec * 1000000 + (uint64_t)ts.tv_nsec / 1000;
}

/*
 * Get tick count (RTEMS implementation)
 */
uint32_t tcelm_timer_ticks(void) {
    return rtems_clock_get_ticks_since_boot();
}

/*
 * Get ticks per second (RTEMS implementation)
 */
uint32_t tcelm_timer_ticks_per_second(void) {
    return rtems_clock_get_ticks_per_second();
}

/*
 * Convert ms to ticks (RTEMS implementation)
 */
uint32_t tcelm_timer_ms_to_ticks(uint32_t ms) {
    uint32_t tps = rtems_clock_get_ticks_per_second();
    uint32_t ticks = (ms * tps) / 1000;
    return ticks > 0 ? ticks : 1;
}

/*
 * Convert ticks to ms (RTEMS implementation)
 */
uint32_t tcelm_timer_ticks_to_ms(uint32_t ticks) {
    uint32_t tps = rtems_clock_get_ticks_per_second();
    return (ticks * 1000) / tps;
}

/*
 * Rate monotonic period wait (RTEMS implementation)
 */
int tcelm_timer_period_wait(tcelm_timer_t *timer) {
    if (timer->type != TCELM_TIMER_RATE_MONO) {
        return -1;
    }

    rtems_interval ticks = tcelm_timer_ms_to_ticks(timer->interval_ms);
    rtems_status_code status = rtems_rate_monotonic_period(timer->period_id, ticks);

    if (status == RTEMS_TIMEOUT) {
        return -1;  /* Missed deadline */
    }

    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

/*
 * Get timer statistics (RTEMS implementation)
 */
int tcelm_timer_get_stats(tcelm_timer_t *timer, tcelm_timer_stats_t *stats) {
    if (timer->type != TCELM_TIMER_RATE_MONO) {
        return -1;
    }

    rtems_rate_monotonic_period_statistics rtems_stats;
    rtems_status_code status = rtems_rate_monotonic_get_statistics(
        timer->period_id,
        &rtems_stats
    );

    if (status != RTEMS_SUCCESSFUL) {
        return -1;
    }

    stats->count = rtems_stats.count;
    stats->missed_count = rtems_stats.missed_count;
    /* RTEMS 7: cpu_time fields are struct timespec, convert to microseconds */
    stats->min_cpu_time_us = (uint32_t)(rtems_stats.min_cpu_time.tv_sec * 1000000 +
                                        rtems_stats.min_cpu_time.tv_nsec / 1000);
    stats->max_cpu_time_us = (uint32_t)(rtems_stats.max_cpu_time.tv_sec * 1000000 +
                                        rtems_stats.max_cpu_time.tv_nsec / 1000);
    if (rtems_stats.count > 0) {
        uint64_t total_us = rtems_stats.total_cpu_time.tv_sec * 1000000 +
                           rtems_stats.total_cpu_time.tv_nsec / 1000;
        stats->avg_cpu_time_us = (uint32_t)(total_us / rtems_stats.count);
    } else {
        stats->avg_cpu_time_us = 0;
    }

    return 0;
}

#else /* Native implementation */

/*
 * Native timer using POSIX timer_create
 */
typedef struct native_timer_data {
    timer_t posix_timer;
    tcelm_timer_callback callback;
    void *user_data;
    bool active;
} native_timer_data_t;

static void native_timer_handler(union sigval sv) {
    native_timer_data_t *data = (native_timer_data_t *)sv.sival_ptr;
    if (data->callback && data->active) {
        data->callback(data->user_data);
    }
}

tcelm_timer_t *tcelm_timer_create_oneshot(
    tcelm_arena_t *arena,
    uint32_t delay_ms,
    tcelm_timer_callback callback,
    void *user_data
) {
    tcelm_timer_t *timer = tcelm_arena_alloc(arena, sizeof(tcelm_timer_t));
    if (!timer) return NULL;

    native_timer_data_t *data = malloc(sizeof(native_timer_data_t));
    if (!data) return NULL;

    struct sigevent sev;
    sev.sigev_notify = SIGEV_THREAD;
    sev.sigev_notify_function = native_timer_handler;
    sev.sigev_notify_attributes = NULL;
    sev.sigev_value.sival_ptr = data;

    if (timer_create(CLOCK_MONOTONIC, &sev, &data->posix_timer) != 0) {
        free(data);
        return NULL;
    }

    data->callback = callback;
    data->user_data = user_data;
    data->active = false;

    timer->timer_id = (rtems_id)(uintptr_t)data;
    timer->period_id = 0;
    timer->type = TCELM_TIMER_ONESHOT;
    timer->interval_ms = delay_ms;
    timer->callback = callback;
    timer->user_data = user_data;
    timer->active = false;
    timer->name = "TIMER";

    return timer;
}

tcelm_timer_t *tcelm_timer_create_periodic(
    tcelm_arena_t *arena,
    uint32_t interval_ms,
    tcelm_timer_callback callback,
    void *user_data
) {
    tcelm_timer_t *timer = tcelm_timer_create_oneshot(arena, interval_ms, callback, user_data);
    if (timer) {
        timer->type = TCELM_TIMER_PERIODIC;
    }
    return timer;
}

tcelm_timer_t *tcelm_timer_create_rate_monotonic(
    tcelm_arena_t *arena,
    uint32_t period_ms,
    tcelm_timer_callback callback,
    void *user_data
) {
    /* Native: same as periodic */
    tcelm_timer_t *timer = tcelm_timer_create_periodic(arena, period_ms, callback, user_data);
    if (timer) {
        timer->type = TCELM_TIMER_RATE_MONO;
    }
    return timer;
}

int tcelm_timer_start(tcelm_timer_t *timer) {
    native_timer_data_t *data = (native_timer_data_t *)(uintptr_t)timer->timer_id;

    struct itimerspec its;
    its.it_value.tv_sec = timer->interval_ms / 1000;
    its.it_value.tv_nsec = (timer->interval_ms % 1000) * 1000000;

    if (timer->type == TCELM_TIMER_PERIODIC || timer->type == TCELM_TIMER_RATE_MONO) {
        its.it_interval = its.it_value;
    } else {
        its.it_interval.tv_sec = 0;
        its.it_interval.tv_nsec = 0;
    }

    data->active = true;
    timer->active = true;

    if (timer_settime(data->posix_timer, 0, &its, NULL) != 0) {
        return -1;
    }

    return 0;
}

int tcelm_timer_cancel(tcelm_timer_t *timer) {
    native_timer_data_t *data = (native_timer_data_t *)(uintptr_t)timer->timer_id;

    struct itimerspec its = {0};
    timer_settime(data->posix_timer, 0, &its, NULL);

    data->active = false;
    timer->active = false;

    return 0;
}

int tcelm_timer_reset(tcelm_timer_t *timer) {
    tcelm_timer_cancel(timer);
    return tcelm_timer_start(timer);
}

void tcelm_timer_delete(tcelm_timer_t *timer) {
    if (timer && timer->timer_id) {
        native_timer_data_t *data = (native_timer_data_t *)(uintptr_t)timer->timer_id;
        timer_delete(data->posix_timer);
        free(data);
        timer->timer_id = 0;
    }
}

uint64_t tcelm_timer_now_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000 + (uint64_t)ts.tv_nsec / 1000000;
}

uint64_t tcelm_timer_now_us(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000 + (uint64_t)ts.tv_nsec / 1000;
}

uint32_t tcelm_timer_ticks(void) {
    return (uint32_t)(tcelm_timer_now_ms());
}

uint32_t tcelm_timer_ticks_per_second(void) {
    return 1000;  /* Assume 1ms tick */
}

uint32_t tcelm_timer_ms_to_ticks(uint32_t ms) {
    return ms;
}

uint32_t tcelm_timer_ticks_to_ms(uint32_t ticks) {
    return ticks;
}

int tcelm_timer_period_wait(tcelm_timer_t *timer) {
    /* Native: just sleep for the period */
    struct timespec ts = {
        .tv_sec = timer->interval_ms / 1000,
        .tv_nsec = (timer->interval_ms % 1000) * 1000000
    };
    nanosleep(&ts, NULL);
    return 0;
}

int tcelm_timer_get_stats(tcelm_timer_t *timer, tcelm_timer_stats_t *stats) {
    (void)timer;
    memset(stats, 0, sizeof(*stats));
    return 0;
}

#endif /* __rtems__ */
