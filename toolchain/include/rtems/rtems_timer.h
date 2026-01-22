/*
 * rtems_timer.h - RTEMS Timer API for tcelm/NUC toolchain
 */

#ifndef _RTEMS_TIMER_H
#define _RTEMS_TIMER_H

#include <rtems/rtems_types.h>

/*
 * Timer Management Functions
 */

/* Create a timer */
rtems_status_code rtems_timer_create(
    rtems_name  name,
    rtems_id   *id
);

/* Delete a timer */
rtems_status_code rtems_timer_delete(
    rtems_id id
);

/* Fire timer after interval (ticks) */
rtems_status_code rtems_timer_fire_after(
    rtems_id                     id,
    rtems_interval               ticks,
    rtems_timer_service_routine  routine,
    void                        *user_data
);

/* Fire timer at specific time */
rtems_status_code rtems_timer_fire_when(
    rtems_id                     id,
    const rtems_time_of_day     *wall_time,
    rtems_timer_service_routine  routine,
    void                        *user_data
);

/* Reset timer (restart with same parameters) */
rtems_status_code rtems_timer_reset(
    rtems_id id
);

/* Cancel a timer */
rtems_status_code rtems_timer_cancel(
    rtems_id id
);

/* Get timer by name */
rtems_status_code rtems_timer_ident(
    rtems_name  name,
    rtems_id   *id
);

/* Server-based timer fire after */
rtems_status_code rtems_timer_server_fire_after(
    rtems_id                     id,
    rtems_interval               ticks,
    rtems_timer_service_routine  routine,
    void                        *user_data
);

/* Server-based timer fire when */
rtems_status_code rtems_timer_server_fire_when(
    rtems_id                     id,
    const rtems_time_of_day     *wall_time,
    rtems_timer_service_routine  routine,
    void                        *user_data
);

/* Initialize timer server */
rtems_status_code rtems_timer_initiate_server(
    rtems_task_priority priority,
    size_t              stack_size,
    rtems_attribute     attribute_set
);

/*
 * Rate Monotonic API
 */

/* Create rate monotonic period */
rtems_status_code rtems_rate_monotonic_create(
    rtems_name  name,
    rtems_id   *id
);

/* Delete rate monotonic period */
rtems_status_code rtems_rate_monotonic_delete(
    rtems_id id
);

/* Period (blocking wait for next period) */
rtems_status_code rtems_rate_monotonic_period(
    rtems_id       id,
    rtems_interval length
);

/* Cancel rate monotonic period */
rtems_status_code rtems_rate_monotonic_cancel(
    rtems_id id
);

/* Get rate monotonic by name */
rtems_status_code rtems_rate_monotonic_ident(
    rtems_name  name,
    rtems_id   *id
);

/* Period statistics */
typedef struct {
    uint32_t count;
    uint32_t missed_count;
    uint32_t min_cpu_time;
    uint32_t max_cpu_time;
    uint32_t total_cpu_time;
    uint32_t min_wall_time;
    uint32_t max_wall_time;
    uint32_t total_wall_time;
} rtems_rate_monotonic_period_statistics;

rtems_status_code rtems_rate_monotonic_get_statistics(
    rtems_id                                id,
    rtems_rate_monotonic_period_statistics *statistics
);

rtems_status_code rtems_rate_monotonic_reset_statistics(
    rtems_id id
);

#endif /* _RTEMS_TIMER_H */
