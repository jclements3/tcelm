/*
 * rtems_tasks.h - RTEMS Task Management API for tcelm/NUC toolchain
 */

#ifndef _RTEMS_TASKS_H
#define _RTEMS_TASKS_H

#include <rtems/rtems_types.h>

/* Task entry point */
typedef void (*rtems_task_entry)(rtems_task_argument);
typedef uintptr_t rtems_task_argument;

/* Task configuration */
typedef struct {
    rtems_name          name;
    size_t              initial_stack_size;
    rtems_task_priority initial_priority;
    rtems_mode          initial_modes;
    rtems_attribute     attributes;
} rtems_task_config;

/* Default stack size */
#define RTEMS_MINIMUM_STACK_SIZE      4096
#define RTEMS_CONFIGURED_MINIMUM_STACK_SIZE RTEMS_MINIMUM_STACK_SIZE

/*
 * Task Management Functions
 */

/* Create a task */
rtems_status_code rtems_task_create(
    rtems_name           name,
    rtems_task_priority  initial_priority,
    size_t               stack_size,
    rtems_mode           initial_modes,
    rtems_attribute      attribute_set,
    rtems_id            *id
);

/* Start a task */
rtems_status_code rtems_task_start(
    rtems_id            id,
    rtems_task_entry    entry_point,
    rtems_task_argument argument
);

/* Delete a task */
rtems_status_code rtems_task_delete(
    rtems_id id
);

/* Suspend a task */
rtems_status_code rtems_task_suspend(
    rtems_id id
);

/* Resume a task */
rtems_status_code rtems_task_resume(
    rtems_id id
);

/* Set task priority */
rtems_status_code rtems_task_set_priority(
    rtems_id             id,
    rtems_task_priority  new_priority,
    rtems_task_priority *old_priority
);

/* Get current task ID */
rtems_status_code rtems_task_ident(
    rtems_name  name,
    uint32_t    node,
    rtems_id   *id
);

/* Wake after interval */
rtems_status_code rtems_task_wake_after(
    rtems_interval ticks
);

/* Wake at specific time */
rtems_status_code rtems_task_wake_when(
    const rtems_time_of_day *time_buffer
);

/* Yield processor */
rtems_status_code rtems_task_wake_after_yielding(void);

/* Get self ID */
rtems_id rtems_task_self(void);

/* Task mode control */
rtems_status_code rtems_task_mode(
    rtems_mode  mode_set,
    rtems_mode  mask,
    rtems_mode *previous_mode_set
);

/* Restart a task */
rtems_status_code rtems_task_restart(
    rtems_id            id,
    rtems_task_argument argument
);

/* Exit current task */
void rtems_task_exit(void);

/*
 * CPU Affinity (SMP support)
 */

/* CPU set type for affinity */
#include <sys/cpuset.h>

/* Set task CPU affinity */
rtems_status_code rtems_task_set_affinity(
    rtems_id        id,
    size_t          cpusetsize,
    const cpu_set_t *cpuset
);

/* Get task CPU affinity */
rtems_status_code rtems_task_get_affinity(
    rtems_id   id,
    size_t     cpusetsize,
    cpu_set_t *cpuset
);

#endif /* _RTEMS_TASKS_H */
