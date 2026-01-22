/*
 * tcelm_mvar.c - MVar runtime for RTEMS
 *
 * Implementation of MVars using RTEMS binary semaphores with priority inheritance.
 */

#include "tcelm_mvar.h"
#include <string.h>
#include <stdio.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/sem.h>
#else
#include <pthread.h>
#include <stdlib.h>
#endif

/* MVar counter for unique names */
static uint32_t mvar_counter = 0;

/*
 * Initialize MVar subsystem
 */
int tcelm_mvar_init(void) {
    mvar_counter = 0;
    return 0;
}

/*
 * Shutdown MVar subsystem
 */
void tcelm_mvar_shutdown(void) {
    /* Nothing to do */
}

#ifdef __rtems__

/*
 * Create new MVar with value (RTEMS implementation)
 */
tcelm_mvar_t *tcelm_mvar_new(tcelm_arena_t *arena, tcelm_value_t *value) {
    rtems_status_code status;
    rtems_id sem_id;

    tcelm_mvar_t *mvar = tcelm_arena_alloc(arena, sizeof(tcelm_mvar_t));
    if (!mvar) return NULL;

    /* Build unique semaphore name */
    char name[5];
    uint32_t num = __sync_fetch_and_add(&mvar_counter, 1);
    snprintf(name, 5, "M%03u", num % 1000);
    rtems_name sem_name = rtems_build_name(name[0], name[1], name[2], name[3]);

    /* Create binary semaphore with priority inheritance */
    status = rtems_semaphore_create(
        sem_name,
        1,  /* Initially available (MVar is full) */
        RTEMS_BINARY_SEMAPHORE | RTEMS_PRIORITY | RTEMS_INHERIT_PRIORITY,
        0,
        &sem_id
    );

    if (status != RTEMS_SUCCESSFUL) {
        return NULL;
    }

    mvar->sem_id = sem_id;
    mvar->value = value;
    mvar->is_empty = false;
    mvar->name = name;

    return mvar;
}

/*
 * Create new empty MVar (RTEMS implementation)
 */
tcelm_mvar_t *tcelm_mvar_new_empty(tcelm_arena_t *arena) {
    rtems_status_code status;
    rtems_id sem_id;

    tcelm_mvar_t *mvar = tcelm_arena_alloc(arena, sizeof(tcelm_mvar_t));
    if (!mvar) return NULL;

    char name[5];
    uint32_t num = __sync_fetch_and_add(&mvar_counter, 1);
    snprintf(name, 5, "M%03u", num % 1000);
    rtems_name sem_name = rtems_build_name(name[0], name[1], name[2], name[3]);

    /* Create semaphore initially unavailable (MVar is empty) */
    status = rtems_semaphore_create(
        sem_name,
        0,  /* Initially unavailable */
        RTEMS_BINARY_SEMAPHORE | RTEMS_PRIORITY | RTEMS_INHERIT_PRIORITY,
        0,
        &sem_id
    );

    if (status != RTEMS_SUCCESSFUL) {
        return NULL;
    }

    mvar->sem_id = sem_id;
    mvar->value = NULL;
    mvar->is_empty = true;
    mvar->name = name;

    return mvar;
}

/*
 * Take value from MVar (RTEMS implementation)
 */
tcelm_value_t *tcelm_mvar_take(tcelm_arena_t *arena, tcelm_mvar_t *mvar) {
    (void)arena;

    /* Wait for semaphore (blocks if empty) */
    rtems_status_code status = rtems_semaphore_obtain(
        mvar->sem_id,
        RTEMS_WAIT,
        RTEMS_NO_TIMEOUT
    );

    if (status != RTEMS_SUCCESSFUL) {
        return NULL;
    }

    tcelm_value_t *value = mvar->value;
    mvar->value = NULL;
    mvar->is_empty = true;

    /* Don't release - MVar is now empty */
    return value;
}

/*
 * Take with timeout (RTEMS implementation)
 */
tcelm_value_t *tcelm_mvar_take_timeout(
    tcelm_arena_t *arena,
    tcelm_mvar_t *mvar,
    uint32_t timeout_ms
) {
    rtems_interval ticks = (timeout_ms * rtems_clock_get_ticks_per_second()) / 1000;
    if (ticks == 0) ticks = 1;

    rtems_status_code status = rtems_semaphore_obtain(
        mvar->sem_id,
        RTEMS_WAIT,
        ticks
    );

    if (status == RTEMS_TIMEOUT) {
        return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
    }

    if (status != RTEMS_SUCCESSFUL) {
        return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
    }

    tcelm_value_t *value = mvar->value;
    mvar->value = NULL;
    mvar->is_empty = true;

    return tcelm_custom(arena, TCELM_CTOR_JUST, "Just", 1, value);
}

/*
 * Try take without blocking (RTEMS implementation)
 */
tcelm_value_t *tcelm_mvar_try_take(tcelm_arena_t *arena, tcelm_mvar_t *mvar) {
    rtems_status_code status = rtems_semaphore_obtain(
        mvar->sem_id,
        RTEMS_NO_WAIT,
        0
    );

    if (status == RTEMS_UNSATISFIED) {
        return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
    }

    if (status != RTEMS_SUCCESSFUL) {
        return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
    }

    tcelm_value_t *value = mvar->value;
    mvar->value = NULL;
    mvar->is_empty = true;

    return tcelm_custom(arena, TCELM_CTOR_JUST, "Just", 1, value);
}

/*
 * Put value into MVar (RTEMS implementation)
 */
int tcelm_mvar_put(tcelm_mvar_t *mvar, tcelm_value_t *value) {
    /* Store value and release semaphore */
    mvar->value = value;
    mvar->is_empty = false;

    rtems_status_code status = rtems_semaphore_release(mvar->sem_id);
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

/*
 * Put with timeout (RTEMS implementation)
 */
int tcelm_mvar_put_timeout(
    tcelm_mvar_t *mvar,
    tcelm_value_t *value,
    uint32_t timeout_ms
) {
    (void)timeout_ms;
    /* RTEMS semaphore_release doesn't block, so no timeout needed */
    return tcelm_mvar_put(mvar, value);
}

/*
 * Try put without blocking (RTEMS implementation)
 */
int tcelm_mvar_try_put(tcelm_mvar_t *mvar, tcelm_value_t *value) {
    if (!mvar->is_empty) {
        return -1;  /* MVar is full */
    }
    return tcelm_mvar_put(mvar, value);
}

/*
 * Read without removing (RTEMS implementation)
 */
tcelm_value_t *tcelm_mvar_read(tcelm_arena_t *arena, tcelm_mvar_t *mvar) {
    /* Take and immediately put back */
    tcelm_value_t *value = tcelm_mvar_take(arena, mvar);
    tcelm_mvar_put(mvar, value);
    return value;
}

/*
 * Check if empty (RTEMS implementation)
 */
bool tcelm_mvar_is_empty(tcelm_mvar_t *mvar) {
    return mvar->is_empty;
}

/*
 * Delete MVar (RTEMS implementation)
 */
void tcelm_mvar_delete(tcelm_mvar_t *mvar) {
    if (mvar && mvar->sem_id) {
        rtems_semaphore_delete(mvar->sem_id);
        mvar->sem_id = 0;
    }
}

#else /* Native implementation */

/*
 * Native MVar using pthread mutex + condition variable
 */
typedef struct native_mvar_data {
    pthread_mutex_t mutex;
    pthread_cond_t not_empty;
    pthread_cond_t not_full;
    tcelm_value_t *value;
    bool is_empty;
} native_mvar_data_t;

tcelm_mvar_t *tcelm_mvar_new(tcelm_arena_t *arena, tcelm_value_t *value) {
    tcelm_mvar_t *mvar = tcelm_arena_alloc(arena, sizeof(tcelm_mvar_t));
    if (!mvar) return NULL;

    native_mvar_data_t *data = malloc(sizeof(native_mvar_data_t));
    if (!data) return NULL;

    pthread_mutex_init(&data->mutex, NULL);
    pthread_cond_init(&data->not_empty, NULL);
    pthread_cond_init(&data->not_full, NULL);
    data->value = value;
    data->is_empty = false;

    mvar->sem_id = 0;
    mvar->native_data = data;
    mvar->value = value;
    mvar->is_empty = false;
    mvar->name = "MVAR";

    return mvar;
}

tcelm_mvar_t *tcelm_mvar_new_empty(tcelm_arena_t *arena) {
    tcelm_mvar_t *mvar = tcelm_arena_alloc(arena, sizeof(tcelm_mvar_t));
    if (!mvar) return NULL;

    native_mvar_data_t *data = malloc(sizeof(native_mvar_data_t));
    if (!data) return NULL;

    pthread_mutex_init(&data->mutex, NULL);
    pthread_cond_init(&data->not_empty, NULL);
    pthread_cond_init(&data->not_full, NULL);
    data->value = NULL;
    data->is_empty = true;

    mvar->sem_id = 0;
    mvar->native_data = data;
    mvar->value = NULL;
    mvar->is_empty = true;
    mvar->name = "MVAR";

    return mvar;
}

tcelm_value_t *tcelm_mvar_take(tcelm_arena_t *arena, tcelm_mvar_t *mvar) {
    (void)arena;
    native_mvar_data_t *data = (native_mvar_data_t *)mvar->native_data;

    pthread_mutex_lock(&data->mutex);

    while (data->is_empty) {
        pthread_cond_wait(&data->not_empty, &data->mutex);
    }

    tcelm_value_t *value = data->value;
    data->value = NULL;
    data->is_empty = true;
    mvar->value = NULL;
    mvar->is_empty = true;

    pthread_cond_signal(&data->not_full);
    pthread_mutex_unlock(&data->mutex);

    return value;
}

tcelm_value_t *tcelm_mvar_take_timeout(
    tcelm_arena_t *arena,
    tcelm_mvar_t *mvar,
    uint32_t timeout_ms
) {
    (void)timeout_ms;
    tcelm_value_t *value = tcelm_mvar_take(arena, mvar);
    return tcelm_custom(arena, TCELM_CTOR_JUST, "Just", 1, value);
}

tcelm_value_t *tcelm_mvar_try_take(tcelm_arena_t *arena, tcelm_mvar_t *mvar) {
    native_mvar_data_t *data = (native_mvar_data_t *)mvar->native_data;

    pthread_mutex_lock(&data->mutex);

    if (data->is_empty) {
        pthread_mutex_unlock(&data->mutex);
        return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
    }

    tcelm_value_t *value = data->value;
    data->value = NULL;
    data->is_empty = true;
    mvar->value = NULL;
    mvar->is_empty = true;

    pthread_cond_signal(&data->not_full);
    pthread_mutex_unlock(&data->mutex);

    return tcelm_custom(arena, TCELM_CTOR_JUST, "Just", 1, value);
}

int tcelm_mvar_put(tcelm_mvar_t *mvar, tcelm_value_t *value) {
    native_mvar_data_t *data = (native_mvar_data_t *)mvar->native_data;

    pthread_mutex_lock(&data->mutex);

    while (!data->is_empty) {
        pthread_cond_wait(&data->not_full, &data->mutex);
    }

    data->value = value;
    data->is_empty = false;
    mvar->value = value;
    mvar->is_empty = false;

    pthread_cond_signal(&data->not_empty);
    pthread_mutex_unlock(&data->mutex);

    return 0;
}

int tcelm_mvar_put_timeout(
    tcelm_mvar_t *mvar,
    tcelm_value_t *value,
    uint32_t timeout_ms
) {
    (void)timeout_ms;
    return tcelm_mvar_put(mvar, value);
}

int tcelm_mvar_try_put(tcelm_mvar_t *mvar, tcelm_value_t *value) {
    native_mvar_data_t *data = (native_mvar_data_t *)mvar->native_data;

    pthread_mutex_lock(&data->mutex);

    if (!data->is_empty) {
        pthread_mutex_unlock(&data->mutex);
        return -1;
    }

    data->value = value;
    data->is_empty = false;
    mvar->value = value;
    mvar->is_empty = false;

    pthread_cond_signal(&data->not_empty);
    pthread_mutex_unlock(&data->mutex);

    return 0;
}

tcelm_value_t *tcelm_mvar_read(tcelm_arena_t *arena, tcelm_mvar_t *mvar) {
    tcelm_value_t *value = tcelm_mvar_take(arena, mvar);
    tcelm_mvar_put(mvar, value);
    return value;
}

bool tcelm_mvar_is_empty(tcelm_mvar_t *mvar) {
    native_mvar_data_t *data = (native_mvar_data_t *)mvar->native_data;
    pthread_mutex_lock(&data->mutex);
    bool empty = data->is_empty;
    pthread_mutex_unlock(&data->mutex);
    return empty;
}

void tcelm_mvar_delete(tcelm_mvar_t *mvar) {
    if (mvar && mvar->native_data) {
        native_mvar_data_t *data = (native_mvar_data_t *)mvar->native_data;
        pthread_mutex_destroy(&data->mutex);
        pthread_cond_destroy(&data->not_empty);
        pthread_cond_destroy(&data->not_full);
        free(data);
        mvar->native_data = NULL;
    }
}

#endif /* __rtems__ */

/*
 * Atomically modify MVar contents
 */
tcelm_value_t *tcelm_mvar_modify(
    tcelm_arena_t *arena,
    tcelm_mvar_t *mvar,
    tcelm_value_t *(*fn)(tcelm_arena_t *, tcelm_value_t *)
) {
    tcelm_value_t *old_value = tcelm_mvar_take(arena, mvar);
    tcelm_value_t *result = fn(arena, old_value);

    /* result is expected to be a tuple (new_value, return_value) */
    tcelm_value_t *new_value = tcelm_tuple2_first(result);
    tcelm_value_t *ret_value = tcelm_tuple2_second(result);

    tcelm_mvar_put(mvar, new_value);
    return ret_value;
}

/*
 * Swap MVar contents
 */
tcelm_value_t *tcelm_mvar_swap(
    tcelm_arena_t *arena,
    tcelm_mvar_t *mvar,
    tcelm_value_t *new_value
) {
    tcelm_value_t *old_value = tcelm_mvar_take(arena, mvar);
    tcelm_mvar_put(mvar, new_value);
    return old_value;
}
