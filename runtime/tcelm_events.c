/*
 * tcelm_events.c - Event flags for RTEMS
 *
 * Lightweight task-to-task signaling using 32-bit event flags.
 */

#include "tcelm_events.h"
#include <string.h>
#include <stdio.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/event.h>
#elif defined(__linux__) || defined(__APPLE__)
#include <pthread.h>
#include <time.h>
#include <errno.h>
#endif

#ifndef __rtems__
/*
 * Native implementation using condition variables
 * This simulates RTEMS events for development/testing
 */

#define MAX_TASKS 64

typedef struct {
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    tcelm_event_set_t pending;
    bool in_use;
} task_events_t;

static struct {
    bool initialized;
    pthread_mutex_t global_mutex;
    task_events_t tasks[MAX_TASKS];
    pthread_key_t task_key;  /* Thread-local task ID */
} events_state;

static uint32_t get_current_task_id(void) {
    void *val = pthread_getspecific(events_state.task_key);
    if (!val) {
        /* Assign new task ID */
        pthread_mutex_lock(&events_state.global_mutex);
        for (uint32_t i = 0; i < MAX_TASKS; i++) {
            if (!events_state.tasks[i].in_use) {
                events_state.tasks[i].in_use = true;
                pthread_mutex_init(&events_state.tasks[i].mutex, NULL);
                pthread_cond_init(&events_state.tasks[i].cond, NULL);
                events_state.tasks[i].pending = 0;
                pthread_setspecific(events_state.task_key, (void *)(uintptr_t)(i + 1));
                pthread_mutex_unlock(&events_state.global_mutex);
                return i;
            }
        }
        pthread_mutex_unlock(&events_state.global_mutex);
        return 0;  /* Fallback */
    }
    return (uint32_t)(uintptr_t)val - 1;
}

static task_events_t *get_task_events(uint32_t task_id) {
    if (task_id >= MAX_TASKS) return NULL;
    if (!events_state.tasks[task_id].in_use) return NULL;
    return &events_state.tasks[task_id];
}

#endif /* !__rtems__ */

/*
 * Initialize event subsystem
 */
int tcelm_events_init(void) {
#ifdef __rtems__
    /* RTEMS events are built-in, no init needed */
    return 0;
#else
    if (events_state.initialized) {
        return 0;
    }

    memset(&events_state, 0, sizeof(events_state));
    pthread_mutex_init(&events_state.global_mutex, NULL);
    pthread_key_create(&events_state.task_key, NULL);
    events_state.initialized = true;

    return 0;
#endif
}

/*
 * Shutdown event subsystem
 */
void tcelm_events_shutdown(void) {
#ifdef __rtems__
    /* Nothing to do */
#else
    if (!events_state.initialized) {
        return;
    }

    for (int i = 0; i < MAX_TASKS; i++) {
        if (events_state.tasks[i].in_use) {
            pthread_mutex_destroy(&events_state.tasks[i].mutex);
            pthread_cond_destroy(&events_state.tasks[i].cond);
        }
    }

    pthread_mutex_destroy(&events_state.global_mutex);
    pthread_key_delete(events_state.task_key);
    events_state.initialized = false;
#endif
}

/*
 * Send events to a task
 */
int tcelm_event_send(uint32_t task_id, tcelm_event_set_t events) {
#ifdef __rtems__
    rtems_id id = (rtems_id)task_id;
    if (id == 0) {
        id = RTEMS_SELF;
    }

    rtems_status_code status = rtems_event_send(id, events);
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
#else
    task_events_t *te = get_task_events(task_id);
    if (!te) {
        return -1;
    }

    pthread_mutex_lock(&te->mutex);
    te->pending |= events;
    pthread_cond_broadcast(&te->cond);
    pthread_mutex_unlock(&te->mutex);

    return 0;
#endif
}

/*
 * Receive events (blocking)
 */
tcelm_event_result_t tcelm_event_receive(
    tcelm_event_set_t requested,
    tcelm_event_wait_mode_t mode,
    tcelm_event_set_t *received
) {
#ifdef __rtems__
    rtems_option option = (mode == TCELM_EVENT_WAIT_ALL)
        ? RTEMS_EVENT_ALL
        : RTEMS_EVENT_ANY;

    rtems_event_set out = 0;
    rtems_status_code status = rtems_event_receive(
        requested,
        option | RTEMS_WAIT,
        RTEMS_NO_TIMEOUT,
        &out
    );

    if (received) *received = out;

    if (status == RTEMS_SUCCESSFUL) {
        return TCELM_EVENT_SUCCESS;
    }
    return TCELM_EVENT_ERROR;
#else
    uint32_t task_id = get_current_task_id();
    task_events_t *te = get_task_events(task_id);
    if (!te) {
        return TCELM_EVENT_ERROR;
    }

    pthread_mutex_lock(&te->mutex);

    while (1) {
        tcelm_event_set_t matched = te->pending & requested;

        if (mode == TCELM_EVENT_WAIT_ANY) {
            if (matched != 0) {
                te->pending &= ~matched;
                if (received) *received = matched;
                pthread_mutex_unlock(&te->mutex);
                return TCELM_EVENT_SUCCESS;
            }
        } else {  /* WAIT_ALL */
            if ((matched & requested) == requested) {
                te->pending &= ~requested;
                if (received) *received = requested;
                pthread_mutex_unlock(&te->mutex);
                return TCELM_EVENT_SUCCESS;
            }
        }

        pthread_cond_wait(&te->cond, &te->mutex);
    }
#endif
}

/*
 * Receive events with timeout
 */
tcelm_event_result_t tcelm_event_receive_timeout(
    tcelm_event_set_t requested,
    tcelm_event_wait_mode_t mode,
    uint32_t timeout_ms,
    tcelm_event_set_t *received
) {
#ifdef __rtems__
    rtems_option option = (mode == TCELM_EVENT_WAIT_ALL)
        ? RTEMS_EVENT_ALL
        : RTEMS_EVENT_ANY;

    rtems_interval ticks = (timeout_ms * rtems_clock_get_ticks_per_second()) / 1000;
    if (ticks == 0 && timeout_ms > 0) ticks = 1;

    rtems_event_set out = 0;
    rtems_status_code status = rtems_event_receive(
        requested,
        option | RTEMS_WAIT,
        ticks,
        &out
    );

    if (received) *received = out;

    if (status == RTEMS_SUCCESSFUL) {
        return TCELM_EVENT_SUCCESS;
    } else if (status == RTEMS_TIMEOUT) {
        return TCELM_EVENT_TIMEOUT;
    }
    return TCELM_EVENT_ERROR;
#else
    uint32_t task_id = get_current_task_id();
    task_events_t *te = get_task_events(task_id);
    if (!te) {
        return TCELM_EVENT_ERROR;
    }

    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    ts.tv_sec += timeout_ms / 1000;
    ts.tv_nsec += (timeout_ms % 1000) * 1000000;
    if (ts.tv_nsec >= 1000000000) {
        ts.tv_sec++;
        ts.tv_nsec -= 1000000000;
    }

    pthread_mutex_lock(&te->mutex);

    while (1) {
        tcelm_event_set_t matched = te->pending & requested;

        if (mode == TCELM_EVENT_WAIT_ANY) {
            if (matched != 0) {
                te->pending &= ~matched;
                if (received) *received = matched;
                pthread_mutex_unlock(&te->mutex);
                return TCELM_EVENT_SUCCESS;
            }
        } else {
            if ((matched & requested) == requested) {
                te->pending &= ~requested;
                if (received) *received = requested;
                pthread_mutex_unlock(&te->mutex);
                return TCELM_EVENT_SUCCESS;
            }
        }

        int rc = pthread_cond_timedwait(&te->cond, &te->mutex, &ts);
        if (rc == ETIMEDOUT) {
            if (received) *received = 0;
            pthread_mutex_unlock(&te->mutex);
            return TCELM_EVENT_TIMEOUT;
        }
    }
#endif
}

/*
 * Try to receive events (non-blocking)
 */
tcelm_event_result_t tcelm_event_try_receive(
    tcelm_event_set_t requested,
    tcelm_event_wait_mode_t mode,
    tcelm_event_set_t *received
) {
#ifdef __rtems__
    rtems_option option = (mode == TCELM_EVENT_WAIT_ALL)
        ? RTEMS_EVENT_ALL
        : RTEMS_EVENT_ANY;

    rtems_event_set out = 0;
    rtems_status_code status = rtems_event_receive(
        requested,
        option | RTEMS_NO_WAIT,
        0,
        &out
    );

    if (received) *received = out;

    if (status == RTEMS_SUCCESSFUL) {
        return TCELM_EVENT_SUCCESS;
    } else if (status == RTEMS_UNSATISFIED) {
        return TCELM_EVENT_TIMEOUT;
    }
    return TCELM_EVENT_ERROR;
#else
    uint32_t task_id = get_current_task_id();
    task_events_t *te = get_task_events(task_id);
    if (!te) {
        return TCELM_EVENT_ERROR;
    }

    pthread_mutex_lock(&te->mutex);

    tcelm_event_set_t matched = te->pending & requested;

    if (mode == TCELM_EVENT_WAIT_ANY) {
        if (matched != 0) {
            te->pending &= ~matched;
            if (received) *received = matched;
            pthread_mutex_unlock(&te->mutex);
            return TCELM_EVENT_SUCCESS;
        }
    } else {
        if ((matched & requested) == requested) {
            te->pending &= ~requested;
            if (received) *received = requested;
            pthread_mutex_unlock(&te->mutex);
            return TCELM_EVENT_SUCCESS;
        }
    }

    if (received) *received = 0;
    pthread_mutex_unlock(&te->mutex);
    return TCELM_EVENT_TIMEOUT;
#endif
}

/*
 * Get pending events without consuming them
 */
tcelm_event_set_t tcelm_event_pending(void) {
#ifdef __rtems__
    rtems_event_set pending = 0;
    /* RTEMS doesn't have a direct way to peek at events without consuming.
     * We'd need to do a no-wait receive and then re-send if we got any.
     * For now, return 0 as this is mainly for debugging. */
    (void)pending;
    return 0;
#else
    uint32_t task_id = get_current_task_id();
    task_events_t *te = get_task_events(task_id);
    if (!te) {
        return 0;
    }

    pthread_mutex_lock(&te->mutex);
    tcelm_event_set_t pending = te->pending;
    pthread_mutex_unlock(&te->mutex);

    return pending;
#endif
}

/*
 * Clear specific events
 */
int tcelm_event_clear(tcelm_event_set_t events) {
#ifdef __rtems__
    /* Clear by receiving with no-wait */
    rtems_event_set out;
    rtems_event_receive(
        events,
        RTEMS_EVENT_ANY | RTEMS_NO_WAIT,
        0,
        &out
    );
    return 0;
#else
    uint32_t task_id = get_current_task_id();
    task_events_t *te = get_task_events(task_id);
    if (!te) {
        return -1;
    }

    pthread_mutex_lock(&te->mutex);
    te->pending &= ~events;
    pthread_mutex_unlock(&te->mutex);

    return 0;
#endif
}
