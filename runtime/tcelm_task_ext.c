/*
 * tcelm_task_ext.c - Extended task management for RTEMS
 *
 * Provides suspend/resume, priority control, and task information.
 */

#include "tcelm_task.h"
#include <string.h>
#include <stdio.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/score/threadimpl.h>
#else
#include <pthread.h>
#include <unistd.h>
#include <time.h>
#endif

#ifdef __rtems__

uint32_t tcelm_task_self(void) {
    rtems_id id;
    rtems_task_ident(RTEMS_SELF, RTEMS_SEARCH_LOCAL_NODE, &id);
    return (uint32_t)id;
}

int tcelm_task_suspend(uint32_t task_id) {
    rtems_id id = (task_id == 0) ? RTEMS_SELF : (rtems_id)task_id;
    rtems_status_code status = rtems_task_suspend(id);
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

int tcelm_task_resume(uint32_t task_id) {
    rtems_id id = (rtems_id)task_id;
    rtems_status_code status = rtems_task_resume(id);
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

int tcelm_task_restart(uint32_t task_id) {
    rtems_id id = (task_id == 0) ? RTEMS_SELF : (rtems_id)task_id;
    rtems_status_code status = rtems_task_restart(id, 0);
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

int tcelm_task_delete(uint32_t task_id) {
    rtems_id id = (task_id == 0) ? RTEMS_SELF : (rtems_id)task_id;
    rtems_status_code status = rtems_task_delete(id);
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

int tcelm_task_set_priority(uint32_t task_id, uint32_t priority) {
    rtems_id id = (task_id == 0) ? RTEMS_SELF : (rtems_id)task_id;
    rtems_task_priority old_priority;

    rtems_status_code status = rtems_task_set_priority(
        id,
        (rtems_task_priority)priority,
        &old_priority
    );

    if (status != RTEMS_SUCCESSFUL) {
        return -1;
    }
    return (int)old_priority;
}

int tcelm_task_get_priority(uint32_t task_id) {
    rtems_id id = (task_id == 0) ? RTEMS_SELF : (rtems_id)task_id;
    rtems_task_priority current_priority;

    rtems_status_code status = rtems_task_set_priority(
        id,
        RTEMS_CURRENT_PRIORITY,
        &current_priority
    );

    if (status != RTEMS_SUCCESSFUL) {
        return -1;
    }
    return (int)current_priority;
}

int tcelm_task_set_name(uint32_t task_id, const char *name) {
    (void)task_id;
    (void)name;
    /* RTEMS doesn't support changing task name after creation */
    return -1;
}

int tcelm_task_get_name(uint32_t task_id, char *name, size_t name_len) {
    if (!name || name_len < 5) return -1;

    /* Task name is encoded in the ID for RTEMS */
    rtems_id id = (task_id == 0) ? tcelm_task_self() : task_id;

    /* Names are 4 chars in RTEMS */
    snprintf(name, name_len, "%c%c%c%c",
        (char)((id >> 24) & 0xFF),
        (char)((id >> 16) & 0xFF),
        (char)((id >> 8) & 0xFF),
        (char)(id & 0xFF)
    );

    return 0;
}

tcelm_task_status_t tcelm_task_get_status(uint32_t task_id) {
    (void)task_id;
    /* Would need to use internal RTEMS APIs to get state */
    return TCELM_TASK_UNKNOWN;
}

int tcelm_task_get_info(uint32_t task_id, tcelm_task_info_t *info) {
    if (!info) return -1;

    info->id = task_id;
    tcelm_task_get_name(task_id, info->name, sizeof(info->name));
    info->status = tcelm_task_get_status(task_id);
    info->priority = tcelm_task_get_priority(task_id);
    info->stack_size = 0;  /* Would need internal APIs */
    info->stack_used = 0;

    return 0;
}

int tcelm_task_yield(void) {
    rtems_status_code status = rtems_task_wake_after(RTEMS_YIELD_PROCESSOR);
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

int tcelm_task_wake_after(uint32_t ms) {
    rtems_interval ticks = RTEMS_MILLISECONDS_TO_TICKS(ms);
    if (ticks == 0 && ms > 0) ticks = 1;

    rtems_status_code status = rtems_task_wake_after(ticks);
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

int tcelm_task_wake_after_ticks(uint32_t ticks) {
    rtems_status_code status = rtems_task_wake_after((rtems_interval)ticks);
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

#else /* Native implementation */

static pthread_key_t task_id_key;
static uint32_t next_task_id = 1;
static pthread_once_t task_init_once = PTHREAD_ONCE_INIT;

static void init_task_key(void) {
    pthread_key_create(&task_id_key, NULL);
}

uint32_t tcelm_task_self(void) {
    pthread_once(&task_init_once, init_task_key);

    void *val = pthread_getspecific(task_id_key);
    if (!val) {
        uint32_t id = __sync_fetch_and_add(&next_task_id, 1);
        pthread_setspecific(task_id_key, (void *)(uintptr_t)id);
        return id;
    }
    return (uint32_t)(uintptr_t)val;
}

int tcelm_task_suspend(uint32_t task_id) {
    (void)task_id;
    /* Native pthreads don't support suspend/resume directly */
    fprintf(stderr, "tcelm_task_suspend: Not supported on this platform\n");
    return -1;
}

int tcelm_task_resume(uint32_t task_id) {
    (void)task_id;
    fprintf(stderr, "tcelm_task_resume: Not supported on this platform\n");
    return -1;
}

int tcelm_task_restart(uint32_t task_id) {
    (void)task_id;
    fprintf(stderr, "tcelm_task_restart: Not supported on this platform\n");
    return -1;
}

int tcelm_task_delete(uint32_t task_id) {
    (void)task_id;
    /* Could use pthread_cancel but that's generally unsafe */
    fprintf(stderr, "tcelm_task_delete: Not supported on this platform\n");
    return -1;
}

int tcelm_task_set_priority(uint32_t task_id, uint32_t priority) {
    (void)task_id;
    (void)priority;
    /* Could use pthread_setschedparam but requires root */
    return 0;
}

int tcelm_task_get_priority(uint32_t task_id) {
    (void)task_id;
    return 128;  /* Default middle priority */
}

int tcelm_task_set_name(uint32_t task_id, const char *name) {
    (void)task_id;
    (void)name;
#ifdef __linux__
    /* Linux supports thread names */
    pthread_setname_np(pthread_self(), name);
    return 0;
#else
    return -1;
#endif
}

int tcelm_task_get_name(uint32_t task_id, char *name, size_t name_len) {
    (void)task_id;
    if (!name || name_len < 8) return -1;

#ifdef __linux__
    pthread_getname_np(pthread_self(), name, name_len);
    return 0;
#else
    snprintf(name, name_len, "Task%u", task_id);
    return 0;
#endif
}

tcelm_task_status_t tcelm_task_get_status(uint32_t task_id) {
    (void)task_id;
    return TCELM_TASK_RUNNING;  /* Assume running */
}

int tcelm_task_get_info(uint32_t task_id, tcelm_task_info_t *info) {
    if (!info) return -1;

    info->id = task_id;
    tcelm_task_get_name(task_id, info->name, sizeof(info->name));
    info->status = TCELM_TASK_RUNNING;
    info->priority = 128;
    info->stack_size = 0;
    info->stack_used = 0;

    return 0;
}

int tcelm_task_yield(void) {
    sched_yield();
    return 0;
}

int tcelm_task_wake_after(uint32_t ms) {
    struct timespec ts;
    ts.tv_sec = ms / 1000;
    ts.tv_nsec = (ms % 1000) * 1000000;
    nanosleep(&ts, NULL);
    return 0;
}

int tcelm_task_wake_after_ticks(uint32_t ticks) {
    /* Assume 1ms ticks */
    return tcelm_task_wake_after(ticks);
}

#endif /* __rtems__ */
