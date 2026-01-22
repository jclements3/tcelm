/*
 * rtems_sem.h - RTEMS Semaphore API for tcelm/NUC toolchain
 */

#ifndef _RTEMS_SEM_H
#define _RTEMS_SEM_H

#include <rtems/rtems_types.h>

/*
 * Semaphore Management Functions
 */

/* Create a semaphore */
rtems_status_code rtems_semaphore_create(
    rtems_name       name,
    uint32_t         count,
    rtems_attribute  attribute_set,
    rtems_task_priority priority_ceiling,
    rtems_id        *id
);

/* Delete a semaphore */
rtems_status_code rtems_semaphore_delete(
    rtems_id id
);

/* Obtain (acquire) a semaphore */
rtems_status_code rtems_semaphore_obtain(
    rtems_id       id,
    rtems_option   option_set,
    rtems_interval timeout
);

/* Release a semaphore */
rtems_status_code rtems_semaphore_release(
    rtems_id id
);

/* Flush (release all waiters) */
rtems_status_code rtems_semaphore_flush(
    rtems_id id
);

/* Get semaphore by name */
rtems_status_code rtems_semaphore_ident(
    rtems_name  name,
    uint32_t    node,
    rtems_id   *id
);

/* Set priority ceiling */
rtems_status_code rtems_semaphore_set_priority(
    rtems_id             semaphore_id,
    rtems_id             scheduler_id,
    rtems_task_priority  new_priority,
    rtems_task_priority *old_priority
);

#endif /* _RTEMS_SEM_H */
