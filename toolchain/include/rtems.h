/*
 * rtems.h - Main RTEMS header for tcelm/NUC toolchain
 *
 * This provides a TCC-compatible RTEMS API for the Intel NUC.
 * Include this header to access all RTEMS functionality.
 */

#ifndef _RTEMS_H
#define _RTEMS_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/* RTEMS Core Types */
#include <rtems/rtems_types.h>

/* RTEMS APIs */
#include <rtems/rtems_tasks.h>
#include <rtems/rtems_sem.h>
#include <rtems/rtems_message.h>
#include <rtems/rtems_timer.h>
#include <rtems/rtems_event.h>

/*
 * Clock and Time Functions
 */

/* Get current tick count */
rtems_interval rtems_clock_get_ticks_since_boot(void);

/* Get ticks per second */
rtems_interval rtems_clock_get_ticks_per_second(void);

/* Get current time of day */
rtems_status_code rtems_clock_get_tod(
    rtems_time_of_day *time_buffer
);

/* Set current time of day */
rtems_status_code rtems_clock_set(
    const rtems_time_of_day *time_buffer
);

/* Get uptime as timespec (time since boot) */
rtems_status_code rtems_clock_get_uptime(
    struct timespec *uptime
);

/* Get uptime in seconds (convenience) */
static inline time_t rtems_clock_get_uptime_seconds(void) {
    struct timespec ts;
    rtems_clock_get_uptime(&ts);
    return ts.tv_sec;
}

/* Get uptime in nanoseconds (convenience) */
static inline uint64_t rtems_clock_get_uptime_nanoseconds(void) {
    struct timespec ts;
    rtems_clock_get_uptime(&ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}

/* Convert seconds to ticks */
#define RTEMS_MILLISECONDS_TO_TICKS(ms) \
    ((ms) * rtems_clock_get_ticks_per_second() / 1000)

#define RTEMS_MICROSECONDS_TO_TICKS(us) \
    ((us) * rtems_clock_get_ticks_per_second() / 1000000)

/*
 * Interrupt Management
 */

typedef uint32_t rtems_interrupt_level;

#define rtems_interrupt_disable(level) \
    do { (level) = _rtems_interrupt_disable(); } while(0)

#define rtems_interrupt_enable(level) \
    _rtems_interrupt_enable(level)

#define rtems_interrupt_flash(level) \
    do { _rtems_interrupt_enable(level); (level) = _rtems_interrupt_disable(); } while(0)

rtems_interrupt_level _rtems_interrupt_disable(void);
void _rtems_interrupt_enable(rtems_interrupt_level level);

bool rtems_interrupt_is_in_progress(void);

/*
 * Workspace and Memory
 */

void *rtems_workspace_allocate(size_t size);
bool  rtems_workspace_free(void *ptr);

/*
 * Fatal Error Handling
 */

#define RTEMS_FATAL_SOURCE_APPLICATION 0

void rtems_fatal(uint32_t fatal_source, uint32_t error_code);
void rtems_fatal_error_occurred(uint32_t error_code);

/*
 * Object Services
 */

rtems_status_code rtems_object_get_classic_name(
    rtems_id   id,
    rtems_name *name
);

/*
 * Scheduler
 */

rtems_status_code rtems_scheduler_ident(
    rtems_name  name,
    rtems_id   *id
);

rtems_status_code rtems_scheduler_ident_by_processor(
    uint32_t   cpu_index,
    rtems_id  *id
);

/*
 * SMP Support
 */

/* Get number of configured/available processors */
uint32_t rtems_get_processor_count(void);

/* Get current processor index */
uint32_t rtems_get_current_processor(void);

/* Run the scheduler (call from main, each CPU runs this) */
void rtems_scheduler_run(void);

/*
 * Initialization
 */

/* Called by startup code */
void rtems_initialize_executive(void);

/* Application-provided configuration */
extern const char *bsp_boot_cmdline;

/*
 * Debug/Status helpers
 */

const char *rtems_status_text(rtems_status_code code);

#endif /* _RTEMS_H */
