/*
 * tcelm_semaphore.c - Counting semaphore runtime for RTEMS
 *
 * Implementation of counting semaphores using RTEMS semaphore API.
 */

#include "tcelm_semaphore.h"
#include <string.h>
#include <stdio.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/sem.h>
#else
#include <pthread.h>
#include <stdlib.h>
#include <errno.h>
#include <time.h>
#endif

/* Semaphore counter for unique names */
static uint32_t sem_counter = 0;

/* Default configuration */
const tcelm_semaphore_config_t TCELM_SEMAPHORE_DEFAULT_CONFIG = {
    .initial_count = 0,
    .max_count = 0,  /* Unlimited */
    .priority_ceiling = false,
    .ceiling_priority = 0,
    .name = NULL
};

/*
 * Initialize semaphore subsystem
 */
int tcelm_semaphore_init(void) {
    sem_counter = 0;
    return 0;
}

/*
 * Shutdown semaphore subsystem
 */
void tcelm_semaphore_shutdown(void) {
    /* Nothing to do */
}

#ifdef __rtems__

/*
 * Create counting semaphore (RTEMS implementation)
 */
tcelm_semaphore_t *tcelm_semaphore_create(
    tcelm_arena_t *arena,
    const tcelm_semaphore_config_t *config
) {
    if (!config) config = &TCELM_SEMAPHORE_DEFAULT_CONFIG;

    tcelm_semaphore_t *sem = tcelm_arena_alloc(arena, sizeof(tcelm_semaphore_t));
    if (!sem) return NULL;

    /* Build unique semaphore name */
    char name[5];
    uint32_t num = __sync_fetch_and_add(&sem_counter, 1);
    snprintf(name, 5, "S%03u", num % 1000);
    rtems_name sem_name = rtems_build_name(name[0], name[1], name[2], name[3]);

    /* Set semaphore attributes */
    rtems_attribute attr = RTEMS_COUNTING_SEMAPHORE | RTEMS_PRIORITY;

    if (config->priority_ceiling) {
        attr |= RTEMS_PRIORITY_CEILING;
    }

    rtems_id sem_id;
    rtems_status_code status = rtems_semaphore_create(
        sem_name,
        config->initial_count,
        attr,
        config->ceiling_priority,
        &sem_id
    );

    if (status != RTEMS_SUCCESSFUL) {
        return NULL;
    }

    sem->sem_id = sem_id;
    sem->native_data = NULL;
    sem->initial_count = config->initial_count;
    sem->max_count = config->max_count;
    sem->name = config->name;

    return sem;
}

/*
 * Create with initial count (RTEMS implementation)
 */
tcelm_semaphore_t *tcelm_semaphore_create_with_count(
    tcelm_arena_t *arena,
    uint32_t initial_count
) {
    tcelm_semaphore_config_t config = TCELM_SEMAPHORE_DEFAULT_CONFIG;
    config.initial_count = initial_count;
    return tcelm_semaphore_create(arena, &config);
}

/*
 * Acquire semaphore (RTEMS implementation)
 */
int tcelm_semaphore_acquire(tcelm_semaphore_t *sem) {
    rtems_status_code status = rtems_semaphore_obtain(
        sem->sem_id,
        RTEMS_WAIT,
        RTEMS_NO_TIMEOUT
    );
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

/*
 * Acquire with timeout (RTEMS implementation)
 */
int tcelm_semaphore_acquire_timeout(
    tcelm_semaphore_t *sem,
    uint32_t timeout_ms
) {
    rtems_interval ticks = (timeout_ms * rtems_clock_get_ticks_per_second()) / 1000;
    if (ticks == 0) ticks = 1;

    rtems_status_code status = rtems_semaphore_obtain(
        sem->sem_id,
        RTEMS_WAIT,
        ticks
    );

    if (status == RTEMS_TIMEOUT) {
        return -1;
    }
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

/*
 * Try acquire without blocking (RTEMS implementation)
 */
int tcelm_semaphore_try_acquire(tcelm_semaphore_t *sem) {
    rtems_status_code status = rtems_semaphore_obtain(
        sem->sem_id,
        RTEMS_NO_WAIT,
        0
    );
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

/*
 * Release semaphore (RTEMS implementation)
 */
int tcelm_semaphore_release(tcelm_semaphore_t *sem) {
    rtems_status_code status = rtems_semaphore_release(sem->sem_id);
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

/*
 * Release multiple (RTEMS implementation)
 */
int tcelm_semaphore_release_multiple(tcelm_semaphore_t *sem, uint32_t count) {
    for (uint32_t i = 0; i < count; i++) {
        rtems_status_code status = rtems_semaphore_release(sem->sem_id);
        if (status != RTEMS_SUCCESSFUL) {
            return -1;
        }
    }
    return 0;
}

/*
 * Get current count (RTEMS implementation)
 * Note: RTEMS doesn't have a direct API for this, so we estimate
 */
uint32_t tcelm_semaphore_get_count(tcelm_semaphore_t *sem) {
    (void)sem;
    /* RTEMS doesn't provide a way to query semaphore count directly.
     * Would need to track internally, which adds overhead.
     * Return 0 as placeholder - callers should not rely on this. */
    return 0;
}

/*
 * Flush semaphore (RTEMS implementation)
 */
uint32_t tcelm_semaphore_flush(tcelm_semaphore_t *sem) {
    rtems_status_code status = rtems_semaphore_flush(sem->sem_id);
    /* RTEMS doesn't return the count of flushed waiters */
    return (status == RTEMS_SUCCESSFUL) ? 1 : 0;
}

/*
 * Delete semaphore (RTEMS implementation)
 */
void tcelm_semaphore_delete(tcelm_semaphore_t *sem) {
    if (sem && sem->sem_id) {
        rtems_semaphore_delete(sem->sem_id);
        sem->sem_id = 0;
    }
}

#else /* Native implementation using POSIX */

/*
 * Native semaphore data using pthread mutex + condition variable
 * (More portable than sem_t which has issues on macOS)
 */
typedef struct native_sem_data {
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    uint32_t count;
    uint32_t max_count;
} native_sem_data_t;

/*
 * Create counting semaphore (native implementation)
 */
tcelm_semaphore_t *tcelm_semaphore_create(
    tcelm_arena_t *arena,
    const tcelm_semaphore_config_t *config
) {
    if (!config) config = &TCELM_SEMAPHORE_DEFAULT_CONFIG;

    tcelm_semaphore_t *sem = tcelm_arena_alloc(arena, sizeof(tcelm_semaphore_t));
    if (!sem) return NULL;

    native_sem_data_t *data = malloc(sizeof(native_sem_data_t));
    if (!data) return NULL;

    pthread_mutex_init(&data->mutex, NULL);
    pthread_cond_init(&data->cond, NULL);
    data->count = config->initial_count;
    data->max_count = config->max_count;

    sem->sem_id = 0;
    sem->native_data = data;
    sem->initial_count = config->initial_count;
    sem->max_count = config->max_count;
    sem->name = config->name;

    return sem;
}

/*
 * Create with initial count (native implementation)
 */
tcelm_semaphore_t *tcelm_semaphore_create_with_count(
    tcelm_arena_t *arena,
    uint32_t initial_count
) {
    tcelm_semaphore_config_t config = TCELM_SEMAPHORE_DEFAULT_CONFIG;
    config.initial_count = initial_count;
    return tcelm_semaphore_create(arena, &config);
}

/*
 * Acquire semaphore (native implementation)
 */
int tcelm_semaphore_acquire(tcelm_semaphore_t *sem) {
    native_sem_data_t *data = (native_sem_data_t *)sem->native_data;

    pthread_mutex_lock(&data->mutex);

    while (data->count == 0) {
        pthread_cond_wait(&data->cond, &data->mutex);
    }

    data->count--;

    pthread_mutex_unlock(&data->mutex);
    return 0;
}

/*
 * Acquire with timeout (native implementation)
 */
int tcelm_semaphore_acquire_timeout(
    tcelm_semaphore_t *sem,
    uint32_t timeout_ms
) {
    native_sem_data_t *data = (native_sem_data_t *)sem->native_data;

    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    ts.tv_sec += timeout_ms / 1000;
    ts.tv_nsec += (timeout_ms % 1000) * 1000000;
    if (ts.tv_nsec >= 1000000000) {
        ts.tv_sec++;
        ts.tv_nsec -= 1000000000;
    }

    pthread_mutex_lock(&data->mutex);

    while (data->count == 0) {
        int rc = pthread_cond_timedwait(&data->cond, &data->mutex, &ts);
        if (rc == ETIMEDOUT) {
            pthread_mutex_unlock(&data->mutex);
            return -1;
        }
    }

    data->count--;

    pthread_mutex_unlock(&data->mutex);
    return 0;
}

/*
 * Try acquire without blocking (native implementation)
 */
int tcelm_semaphore_try_acquire(tcelm_semaphore_t *sem) {
    native_sem_data_t *data = (native_sem_data_t *)sem->native_data;

    pthread_mutex_lock(&data->mutex);

    if (data->count == 0) {
        pthread_mutex_unlock(&data->mutex);
        return -1;
    }

    data->count--;

    pthread_mutex_unlock(&data->mutex);
    return 0;
}

/*
 * Release semaphore (native implementation)
 */
int tcelm_semaphore_release(tcelm_semaphore_t *sem) {
    native_sem_data_t *data = (native_sem_data_t *)sem->native_data;

    pthread_mutex_lock(&data->mutex);

    /* Check max count if set */
    if (data->max_count > 0 && data->count >= data->max_count) {
        pthread_mutex_unlock(&data->mutex);
        return -1;
    }

    data->count++;
    pthread_cond_signal(&data->cond);

    pthread_mutex_unlock(&data->mutex);
    return 0;
}

/*
 * Release multiple (native implementation)
 */
int tcelm_semaphore_release_multiple(tcelm_semaphore_t *sem, uint32_t count) {
    native_sem_data_t *data = (native_sem_data_t *)sem->native_data;

    pthread_mutex_lock(&data->mutex);

    /* Check max count if set */
    if (data->max_count > 0 && data->count + count > data->max_count) {
        pthread_mutex_unlock(&data->mutex);
        return -1;
    }

    data->count += count;
    pthread_cond_broadcast(&data->cond);

    pthread_mutex_unlock(&data->mutex);
    return 0;
}

/*
 * Get current count (native implementation)
 */
uint32_t tcelm_semaphore_get_count(tcelm_semaphore_t *sem) {
    native_sem_data_t *data = (native_sem_data_t *)sem->native_data;

    pthread_mutex_lock(&data->mutex);
    uint32_t count = data->count;
    pthread_mutex_unlock(&data->mutex);

    return count;
}

/*
 * Flush semaphore (native implementation)
 */
uint32_t tcelm_semaphore_flush(tcelm_semaphore_t *sem) {
    native_sem_data_t *data = (native_sem_data_t *)sem->native_data;

    pthread_mutex_lock(&data->mutex);
    /* Wake all waiters - they will return with error (count still 0) */
    pthread_cond_broadcast(&data->cond);
    pthread_mutex_unlock(&data->mutex);

    return 1;  /* We don't track waiter count */
}

/*
 * Delete semaphore (native implementation)
 */
void tcelm_semaphore_delete(tcelm_semaphore_t *sem) {
    if (sem && sem->native_data) {
        native_sem_data_t *data = (native_sem_data_t *)sem->native_data;
        pthread_mutex_destroy(&data->mutex);
        pthread_cond_destroy(&data->cond);
        free(data);
        sem->native_data = NULL;
    }
}

#endif /* __rtems__ */
