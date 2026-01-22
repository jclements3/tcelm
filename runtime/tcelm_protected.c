/*
 * tcelm_protected.c - Protected types for RTEMS
 *
 * Implementation using RTEMS semaphores with priority inheritance,
 * or pthread rwlock + condition variables for native builds.
 */

#include "tcelm_protected.h"
#include "tcelm_atomic.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/sem.h>
#else
#include <pthread.h>
#include <errno.h>
#include <time.h>
#endif

/* Protected counter for unique names */
static uint32_t protected_counter = 0;

/*
 * Initialize protected subsystem
 */
int tcelm_protected_init(void) {
    protected_counter = 0;
    return 0;
}

/*
 * Shutdown protected subsystem
 */
void tcelm_protected_shutdown(void) {
    /* Nothing to do */
}

#ifdef __rtems__

/*
 * Create protected object (RTEMS implementation)
 */
tcelm_protected_t *tcelm_protected_create(
    tcelm_arena_t *arena,
    const tcelm_protected_config_t *config
) {
    if (!config) return NULL;

    tcelm_protected_t *prot = tcelm_arena_alloc(arena, sizeof(tcelm_protected_t));
    if (!prot) return NULL;

    memset(prot, 0, sizeof(tcelm_protected_t));

    /* Allocate and copy data */
    if (config->data_size > 0) {
        prot->data = tcelm_arena_alloc(arena, config->data_size);
        if (!prot->data) return NULL;

        if (config->initial_data) {
            memcpy(prot->data, config->initial_data, config->data_size);
        } else {
            memset(prot->data, 0, config->data_size);
        }
        prot->data_size = config->data_size;
    }

    /* Build unique mutex name */
    char name[5];
    uint32_t num = tcelm_atomic_fetch_add_u32(&protected_counter, 1);
    snprintf(name, 5, "P%03u", num % 1000);
    rtems_name mutex_name = rtems_build_name(name[0], name[1], name[2], name[3]);

    /* Create mutex with priority inheritance */
    rtems_status_code status = rtems_semaphore_create(
        mutex_name,
        1,
        RTEMS_BINARY_SEMAPHORE | RTEMS_PRIORITY | RTEMS_INHERIT_PRIORITY,
        0,
        &prot->mutex_id
    );

    if (status != RTEMS_SUCCESSFUL) {
        return NULL;
    }

    prot->name = config->name;
    prot->entry_count = 0;
    prot->reader_count = 0;
    prot->writer_waiting = false;

    return prot;
}

/*
 * Lock for write (RTEMS)
 */
void tcelm_protected_lock_write(tcelm_protected_t *prot) {
    if (!prot) return;
    rtems_semaphore_obtain(prot->mutex_id, RTEMS_WAIT, RTEMS_NO_TIMEOUT);
}

/*
 * Unlock after write (RTEMS)
 */
void tcelm_protected_unlock_write(tcelm_protected_t *prot) {
    if (!prot) return;
    rtems_semaphore_release(prot->mutex_id);
}

/*
 * Lock for read (RTEMS)
 * Note: RTEMS binary semaphore doesn't support multiple readers,
 * so we use exclusive access for simplicity (correct but less concurrent)
 */
void tcelm_protected_lock_read(tcelm_protected_t *prot) {
    tcelm_protected_lock_write(prot);
    prot->reader_count++;
}

/*
 * Unlock after read (RTEMS)
 */
void tcelm_protected_unlock_read(tcelm_protected_t *prot) {
    if (!prot) return;
    prot->reader_count--;
    tcelm_protected_unlock_write(prot);
}

/*
 * Signal state change (RTEMS)
 */
void tcelm_protected_signal(tcelm_protected_t *prot) {
    (void)prot;
    /* RTEMS doesn't have condition variables in the classic API.
     * Tasks waiting on entries will re-check guards when they acquire the mutex. */
}

/*
 * Delete protected object (RTEMS)
 */
void tcelm_protected_delete(tcelm_protected_t *prot) {
    if (!prot) return;

    if (prot->mutex_id) {
        rtems_semaphore_delete(prot->mutex_id);
        prot->mutex_id = 0;
    }
}

/*
 * Call entry with timeout (RTEMS)
 */
int tcelm_protected_call_entry_timeout(
    tcelm_protected_t *prot,
    int entry_index,
    void *arg,
    uint32_t timeout_ms
) {
    if (!prot || entry_index < 0 || entry_index >= (int)prot->entry_count) {
        return -1;
    }

    tcelm_entry_t *entry = &prot->entries[entry_index];
    if (!entry->active || !entry->proc) {
        return -1;
    }

    rtems_interval ticks = (timeout_ms * rtems_clock_get_ticks_per_second()) / 1000;
    if (ticks == 0) ticks = 1;

    rtems_interval start_ticks = rtems_clock_get_ticks_since_boot();
    rtems_interval elapsed = 0;

    while (elapsed < ticks) {
        rtems_interval remaining = ticks - elapsed;

        rtems_status_code status = rtems_semaphore_obtain(
            prot->mutex_id,
            RTEMS_WAIT,
            remaining
        );

        if (status == RTEMS_TIMEOUT) {
            return -1;
        }

        /* Check guard */
        if (!entry->guard || entry->guard(prot->data)) {
            /* Guard satisfied, execute procedure */
            entry->proc(prot->data, arg);
            rtems_semaphore_release(prot->mutex_id);
            return 0;
        }

        /* Guard not satisfied, release and retry */
        rtems_semaphore_release(prot->mutex_id);

        /* Small delay before retry */
        rtems_task_wake_after(1);

        elapsed = rtems_clock_get_ticks_since_boot() - start_ticks;
    }

    return -1;  /* Timeout */
}

#else /* Native implementation */

/*
 * Native protected data
 */
typedef struct native_protected_data {
    pthread_rwlock_t rwlock;
    pthread_mutex_t entry_mutex;
    pthread_cond_t entry_cond;
} native_protected_data_t;

/*
 * Create protected object (native implementation)
 */
tcelm_protected_t *tcelm_protected_create(
    tcelm_arena_t *arena,
    const tcelm_protected_config_t *config
) {
    if (!config) return NULL;

    tcelm_protected_t *prot = tcelm_arena_alloc(arena, sizeof(tcelm_protected_t));
    if (!prot) return NULL;

    native_protected_data_t *native = malloc(sizeof(native_protected_data_t));
    if (!native) return NULL;

    memset(prot, 0, sizeof(tcelm_protected_t));

    /* Allocate and copy data */
    if (config->data_size > 0) {
        prot->data = tcelm_arena_alloc(arena, config->data_size);
        if (!prot->data) {
            free(native);
            return NULL;
        }

        if (config->initial_data) {
            memcpy(prot->data, config->initial_data, config->data_size);
        } else {
            memset(prot->data, 0, config->data_size);
        }
        prot->data_size = config->data_size;
    }

    /* Initialize synchronization primitives */
    pthread_rwlock_init(&native->rwlock, NULL);
    pthread_mutex_init(&native->entry_mutex, NULL);
    pthread_cond_init(&native->entry_cond, NULL);

    prot->native_data = native;
    prot->name = config->name;
    prot->entry_count = 0;
    prot->reader_count = 0;
    prot->writer_waiting = false;

    tcelm_atomic_fetch_add_u32(&protected_counter, 1);

    return prot;
}

/*
 * Lock for write (native)
 */
void tcelm_protected_lock_write(tcelm_protected_t *prot) {
    if (!prot || !prot->native_data) return;
    native_protected_data_t *native = (native_protected_data_t *)prot->native_data;
    pthread_rwlock_wrlock(&native->rwlock);
}

/*
 * Unlock after write (native)
 */
void tcelm_protected_unlock_write(tcelm_protected_t *prot) {
    if (!prot || !prot->native_data) return;
    native_protected_data_t *native = (native_protected_data_t *)prot->native_data;
    pthread_rwlock_unlock(&native->rwlock);
}

/*
 * Lock for read (native)
 */
void tcelm_protected_lock_read(tcelm_protected_t *prot) {
    if (!prot || !prot->native_data) return;
    native_protected_data_t *native = (native_protected_data_t *)prot->native_data;
    pthread_rwlock_rdlock(&native->rwlock);
    tcelm_atomic_fetch_add_u32(&prot->reader_count, 1);
}

/*
 * Unlock after read (native)
 */
void tcelm_protected_unlock_read(tcelm_protected_t *prot) {
    if (!prot || !prot->native_data) return;
    native_protected_data_t *native = (native_protected_data_t *)prot->native_data;
    tcelm_atomic_fetch_sub_u32(&prot->reader_count, 1);
    pthread_rwlock_unlock(&native->rwlock);
}

/*
 * Signal state change (native)
 */
void tcelm_protected_signal(tcelm_protected_t *prot) {
    if (!prot || !prot->native_data) return;
    native_protected_data_t *native = (native_protected_data_t *)prot->native_data;
    pthread_cond_broadcast(&native->entry_cond);
}

/*
 * Delete protected object (native)
 */
void tcelm_protected_delete(tcelm_protected_t *prot) {
    if (!prot) return;

    if (prot->native_data) {
        native_protected_data_t *native = (native_protected_data_t *)prot->native_data;
        pthread_rwlock_destroy(&native->rwlock);
        pthread_mutex_destroy(&native->entry_mutex);
        pthread_cond_destroy(&native->entry_cond);
        free(native);
        prot->native_data = NULL;
    }
}

/*
 * Call entry with timeout (native)
 */
int tcelm_protected_call_entry_timeout(
    tcelm_protected_t *prot,
    int entry_index,
    void *arg,
    uint32_t timeout_ms
) {
    if (!prot || !prot->native_data) return -1;
    if (entry_index < 0 || entry_index >= (int)prot->entry_count) return -1;

    tcelm_entry_t *entry = &prot->entries[entry_index];
    if (!entry->active || !entry->proc) return -1;

    native_protected_data_t *native = (native_protected_data_t *)prot->native_data;

    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    ts.tv_sec += timeout_ms / 1000;
    ts.tv_nsec += (timeout_ms % 1000) * 1000000;
    if (ts.tv_nsec >= 1000000000) {
        ts.tv_sec++;
        ts.tv_nsec -= 1000000000;
    }

    pthread_mutex_lock(&native->entry_mutex);

    while (1) {
        /* Try to acquire write lock */
        pthread_rwlock_wrlock(&native->rwlock);

        /* Check guard */
        if (!entry->guard || entry->guard(prot->data)) {
            /* Guard satisfied, execute procedure */
            entry->proc(prot->data, arg);
            pthread_rwlock_unlock(&native->rwlock);
            pthread_mutex_unlock(&native->entry_mutex);
            return 0;
        }

        /* Guard not satisfied, release and wait */
        pthread_rwlock_unlock(&native->rwlock);

        int rc = pthread_cond_timedwait(&native->entry_cond, &native->entry_mutex, &ts);
        if (rc == ETIMEDOUT) {
            pthread_mutex_unlock(&native->entry_mutex);
            return -1;
        }
    }
}

#endif /* __rtems__ */

/*
 * ============================================================================
 * COMMON IMPLEMENTATIONS
 * ============================================================================
 */

/*
 * Create with data pointer
 */
tcelm_protected_t *tcelm_protected_create_with_data(
    tcelm_arena_t *arena,
    void *data,
    size_t data_size
) {
    tcelm_protected_config_t config = {
        .initial_data = data,
        .data_size = data_size,
        .name = NULL
    };
    return tcelm_protected_create(arena, &config);
}

/*
 * Call procedure with exclusive access
 */
void tcelm_protected_call_proc(
    tcelm_protected_t *prot,
    tcelm_protected_proc_t proc,
    void *arg
) {
    if (!prot || !proc) return;

    tcelm_protected_lock_write(prot);
    proc(prot->data, arg);
    tcelm_protected_unlock_write(prot);
}

/*
 * Call function with shared read access
 */
void *tcelm_protected_call_func(
    tcelm_protected_t *prot,
    tcelm_protected_func_t func,
    void *arg
) {
    if (!prot || !func) return NULL;

    tcelm_protected_lock_read(prot);
    void *result = func(prot->data, arg);
    tcelm_protected_unlock_read(prot);

    return result;
}

/*
 * Add entry
 */
int tcelm_protected_add_entry(
    tcelm_protected_t *prot,
    const char *name,
    tcelm_entry_guard_t guard,
    tcelm_entry_proc_t proc
) {
    if (!prot || !proc) return -1;
    if (prot->entry_count >= TCELM_PROTECTED_MAX_ENTRIES) return -1;

    int index = (int)prot->entry_count;

    prot->entries[index].name = name;
    prot->entries[index].guard = guard;
    prot->entries[index].proc = proc;
    prot->entries[index].active = true;

    prot->entry_count++;

    return index;
}

/*
 * Call entry (blocking)
 */
int tcelm_protected_call_entry(
    tcelm_protected_t *prot,
    int entry_index,
    void *arg
) {
    /* Use a very long timeout (essentially infinite) */
    return tcelm_protected_call_entry_timeout(prot, entry_index, arg, 3600000);  /* 1 hour */
}

/*
 * Call entry by name
 */
int tcelm_protected_call_entry_by_name(
    tcelm_protected_t *prot,
    const char *name,
    void *arg
) {
    if (!prot || !name) return -1;

    for (uint32_t i = 0; i < prot->entry_count; i++) {
        if (prot->entries[i].active && prot->entries[i].name &&
            strcmp(prot->entries[i].name, name) == 0) {
            return tcelm_protected_call_entry(prot, (int)i, arg);
        }
    }

    return -1;  /* Entry not found */
}

/*
 * Try to call entry without blocking
 */
int tcelm_protected_try_call_entry(
    tcelm_protected_t *prot,
    int entry_index,
    void *arg
) {
    if (!prot || entry_index < 0 || entry_index >= (int)prot->entry_count) {
        return -1;
    }

    tcelm_entry_t *entry = &prot->entries[entry_index];
    if (!entry->active || !entry->proc) {
        return -1;
    }

    tcelm_protected_lock_write(prot);

    /* Check guard */
    if (!entry->guard || entry->guard(prot->data)) {
        /* Guard satisfied, execute procedure */
        entry->proc(prot->data, arg);
        tcelm_protected_unlock_write(prot);
        return 0;
    }

    /* Guard not satisfied */
    tcelm_protected_unlock_write(prot);
    return -1;
}

/*
 * Get data pointer
 */
void *tcelm_protected_get_data(tcelm_protected_t *prot) {
    if (!prot) return NULL;
    return prot->data;
}
