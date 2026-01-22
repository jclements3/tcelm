/*
 * tcelm_barrier.c - Barrier synchronization for RTEMS
 *
 * Implementation of barriers using RTEMS barrier API.
 */

#include "tcelm_barrier.h"
#include <string.h>
#include <stdio.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/barrier.h>
#else
#include <pthread.h>
#include <stdlib.h>
#include <errno.h>
#include <time.h>
#endif

/* Barrier counter for unique names */
static uint32_t barrier_counter = 0;

/* Default configuration */
const tcelm_barrier_config_t TCELM_BARRIER_DEFAULT_CONFIG = {
    .count = 2,
    .auto_release = true,
    .name = NULL
};

/*
 * Initialize barrier subsystem
 */
int tcelm_barrier_init(void) {
    barrier_counter = 0;
    return 0;
}

/*
 * Shutdown barrier subsystem
 */
void tcelm_barrier_shutdown(void) {
    /* Nothing to do */
}

#ifdef __rtems__

/*
 * Create barrier (RTEMS implementation)
 */
tcelm_barrier_t *tcelm_barrier_create(
    tcelm_arena_t *arena,
    const tcelm_barrier_config_t *config
) {
    if (!config) config = &TCELM_BARRIER_DEFAULT_CONFIG;
    if (config->count == 0) return NULL;

    tcelm_barrier_t *barrier = tcelm_arena_alloc(arena, sizeof(tcelm_barrier_t));
    if (!barrier) return NULL;

    /* Build unique barrier name */
    char name[5];
    uint32_t num = __sync_fetch_and_add(&barrier_counter, 1);
    snprintf(name, 5, "B%03u", num % 1000);
    rtems_name bar_name = rtems_build_name(name[0], name[1], name[2], name[3]);

    /* Set barrier attributes */
    rtems_attribute attr = RTEMS_BARRIER_AUTOMATIC_RELEASE;
    if (!config->auto_release) {
        attr = RTEMS_BARRIER_MANUAL_RELEASE;
    }

    rtems_id barrier_id;
    rtems_status_code status = rtems_barrier_create(
        bar_name,
        attr,
        config->count,
        &barrier_id
    );

    if (status != RTEMS_SUCCESSFUL) {
        return NULL;
    }

    barrier->barrier_id = barrier_id;
    barrier->native_data = NULL;
    barrier->count = config->count;
    barrier->name = config->name;

    return barrier;
}

/*
 * Create with count (RTEMS implementation)
 */
tcelm_barrier_t *tcelm_barrier_create_with_count(
    tcelm_arena_t *arena,
    uint32_t count
) {
    tcelm_barrier_config_t config = TCELM_BARRIER_DEFAULT_CONFIG;
    config.count = count;
    return tcelm_barrier_create(arena, &config);
}

/*
 * Wait at barrier (RTEMS implementation)
 * Note: RTEMS doesn't distinguish which task was the "releasing" one,
 * so we just return SUCCESS for all.
 */
tcelm_barrier_wait_result_t tcelm_barrier_wait(tcelm_barrier_t *barrier) {
    rtems_status_code status = rtems_barrier_wait(
        barrier->barrier_id,
        RTEMS_NO_TIMEOUT
    );

    if (status == RTEMS_SUCCESSFUL) {
        return TCELM_BARRIER_WAIT_SUCCESS;
    }

    return TCELM_BARRIER_WAIT_ERROR;
}

/*
 * Wait with timeout (RTEMS implementation)
 */
tcelm_barrier_wait_result_t tcelm_barrier_wait_timeout(
    tcelm_barrier_t *barrier,
    uint32_t timeout_ms
) {
    rtems_interval ticks = (timeout_ms * rtems_clock_get_ticks_per_second()) / 1000;
    if (ticks == 0) ticks = 1;

    rtems_status_code status = rtems_barrier_wait(
        barrier->barrier_id,
        ticks
    );

    if (status == RTEMS_TIMEOUT) {
        return TCELM_BARRIER_WAIT_TIMEOUT;
    }

    if (status == RTEMS_SUCCESSFUL) {
        return TCELM_BARRIER_WAIT_SUCCESS;
    }

    return TCELM_BARRIER_WAIT_ERROR;
}

/*
 * Release all waiting tasks (RTEMS implementation)
 */
uint32_t tcelm_barrier_release(tcelm_barrier_t *barrier) {
    uint32_t released = 0;
    rtems_status_code status = rtems_barrier_release(
        barrier->barrier_id,
        &released
    );

    return (status == RTEMS_SUCCESSFUL) ? released : 0;
}

/*
 * Get number of waiting tasks (RTEMS implementation)
 * Note: RTEMS doesn't provide direct access to this, return 0
 */
uint32_t tcelm_barrier_get_waiting(tcelm_barrier_t *barrier) {
    (void)barrier;
    /* RTEMS doesn't provide API to query waiting count */
    return 0;
}

/*
 * Reset barrier (RTEMS implementation)
 */
int tcelm_barrier_reset(tcelm_barrier_t *barrier) {
    /* RTEMS barriers auto-reset after release */
    (void)barrier;
    return 0;
}

/*
 * Delete barrier (RTEMS implementation)
 */
void tcelm_barrier_delete(tcelm_barrier_t *barrier) {
    if (barrier && barrier->barrier_id) {
        rtems_barrier_delete(barrier->barrier_id);
        barrier->barrier_id = 0;
    }
}

#else /* Native implementation using pthread barrier */

/*
 * Native barrier data
 */
typedef struct native_barrier_data {
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    uint32_t count;             /* Number of tasks required */
    uint32_t waiting;           /* Number currently waiting */
    uint32_t generation;        /* Barrier generation for reuse */
    bool auto_release;
} native_barrier_data_t;

/*
 * Create barrier (native implementation)
 */
tcelm_barrier_t *tcelm_barrier_create(
    tcelm_arena_t *arena,
    const tcelm_barrier_config_t *config
) {
    if (!config) config = &TCELM_BARRIER_DEFAULT_CONFIG;
    if (config->count == 0) return NULL;

    tcelm_barrier_t *barrier = tcelm_arena_alloc(arena, sizeof(tcelm_barrier_t));
    if (!barrier) return NULL;

    native_barrier_data_t *data = malloc(sizeof(native_barrier_data_t));
    if (!data) return NULL;

    pthread_mutex_init(&data->mutex, NULL);
    pthread_cond_init(&data->cond, NULL);
    data->count = config->count;
    data->waiting = 0;
    data->generation = 0;
    data->auto_release = config->auto_release;

    barrier->barrier_id = 0;
    barrier->native_data = data;
    barrier->count = config->count;
    barrier->name = config->name;

    return barrier;
}

/*
 * Create with count (native implementation)
 */
tcelm_barrier_t *tcelm_barrier_create_with_count(
    tcelm_arena_t *arena,
    uint32_t count
) {
    tcelm_barrier_config_t config = TCELM_BARRIER_DEFAULT_CONFIG;
    config.count = count;
    return tcelm_barrier_create(arena, &config);
}

/*
 * Wait at barrier (native implementation)
 */
tcelm_barrier_wait_result_t tcelm_barrier_wait(tcelm_barrier_t *barrier) {
    native_barrier_data_t *data = (native_barrier_data_t *)barrier->native_data;

    pthread_mutex_lock(&data->mutex);

    uint32_t my_generation = data->generation;
    data->waiting++;

    if (data->waiting >= data->count && data->auto_release) {
        /* We're the last task - release everyone */
        data->waiting = 0;
        data->generation++;
        pthread_cond_broadcast(&data->cond);
        pthread_mutex_unlock(&data->mutex);
        return TCELM_BARRIER_WAIT_RELEASED;
    }

    /* Wait for barrier release */
    while (my_generation == data->generation) {
        pthread_cond_wait(&data->cond, &data->mutex);
    }

    pthread_mutex_unlock(&data->mutex);
    return TCELM_BARRIER_WAIT_SUCCESS;
}

/*
 * Wait with timeout (native implementation)
 */
tcelm_barrier_wait_result_t tcelm_barrier_wait_timeout(
    tcelm_barrier_t *barrier,
    uint32_t timeout_ms
) {
    native_barrier_data_t *data = (native_barrier_data_t *)barrier->native_data;

    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    ts.tv_sec += timeout_ms / 1000;
    ts.tv_nsec += (timeout_ms % 1000) * 1000000;
    if (ts.tv_nsec >= 1000000000) {
        ts.tv_sec++;
        ts.tv_nsec -= 1000000000;
    }

    pthread_mutex_lock(&data->mutex);

    uint32_t my_generation = data->generation;
    data->waiting++;

    if (data->waiting >= data->count && data->auto_release) {
        /* We're the last task - release everyone */
        data->waiting = 0;
        data->generation++;
        pthread_cond_broadcast(&data->cond);
        pthread_mutex_unlock(&data->mutex);
        return TCELM_BARRIER_WAIT_RELEASED;
    }

    /* Wait for barrier release or timeout */
    while (my_generation == data->generation) {
        int rc = pthread_cond_timedwait(&data->cond, &data->mutex, &ts);
        if (rc == ETIMEDOUT) {
            data->waiting--;  /* Remove ourselves from waiting count */
            pthread_mutex_unlock(&data->mutex);
            return TCELM_BARRIER_WAIT_TIMEOUT;
        }
    }

    pthread_mutex_unlock(&data->mutex);
    return TCELM_BARRIER_WAIT_SUCCESS;
}

/*
 * Release all waiting tasks (native implementation)
 */
uint32_t tcelm_barrier_release(tcelm_barrier_t *barrier) {
    native_barrier_data_t *data = (native_barrier_data_t *)barrier->native_data;

    pthread_mutex_lock(&data->mutex);

    uint32_t released = data->waiting;
    data->waiting = 0;
    data->generation++;
    pthread_cond_broadcast(&data->cond);

    pthread_mutex_unlock(&data->mutex);
    return released;
}

/*
 * Get number of waiting tasks (native implementation)
 */
uint32_t tcelm_barrier_get_waiting(tcelm_barrier_t *barrier) {
    native_barrier_data_t *data = (native_barrier_data_t *)barrier->native_data;

    pthread_mutex_lock(&data->mutex);
    uint32_t waiting = data->waiting;
    pthread_mutex_unlock(&data->mutex);

    return waiting;
}

/*
 * Reset barrier (native implementation)
 */
int tcelm_barrier_reset(tcelm_barrier_t *barrier) {
    native_barrier_data_t *data = (native_barrier_data_t *)barrier->native_data;

    pthread_mutex_lock(&data->mutex);

    if (data->waiting > 0) {
        /* Can't reset while tasks are waiting */
        pthread_mutex_unlock(&data->mutex);
        return -1;
    }

    data->generation++;

    pthread_mutex_unlock(&data->mutex);
    return 0;
}

/*
 * Delete barrier (native implementation)
 */
void tcelm_barrier_delete(tcelm_barrier_t *barrier) {
    if (barrier && barrier->native_data) {
        native_barrier_data_t *data = (native_barrier_data_t *)barrier->native_data;
        pthread_mutex_destroy(&data->mutex);
        pthread_cond_destroy(&data->cond);
        free(data);
        barrier->native_data = NULL;
    }
}

#endif /* __rtems__ */
