/*
 * rtems_types.h - RTEMS basic types for tcelm/NUC toolchain
 *
 * This provides RTEMS-compatible type definitions that work with TCC.
 */

#ifndef _RTEMS_TYPES_H
#define _RTEMS_TYPES_H

#include <stdint.h>
#include <stdbool.h>

/*
 * RTEMS Object ID
 * Format: CCCCC-NNNNN-IIIII-NNNNN (class-node-API-index)
 */
typedef uint32_t rtems_id;

/* Status codes */
typedef uint32_t rtems_status_code;

#define RTEMS_SUCCESSFUL              0
#define RTEMS_TASK_EXITTED            1
#define RTEMS_MP_NOT_CONFIGURED       2
#define RTEMS_INVALID_NAME            3
#define RTEMS_INVALID_ID              4
#define RTEMS_TOO_MANY                5
#define RTEMS_TIMEOUT                 6
#define RTEMS_OBJECT_WAS_DELETED      7
#define RTEMS_INVALID_SIZE            8
#define RTEMS_INVALID_ADDRESS         9
#define RTEMS_INVALID_NUMBER          10
#define RTEMS_NOT_DEFINED             11
#define RTEMS_RESOURCE_IN_USE         12
#define RTEMS_UNSATISFIED             13
#define RTEMS_INCORRECT_STATE         14
#define RTEMS_ALREADY_SUSPENDED       15
#define RTEMS_ILLEGAL_ON_SELF         16
#define RTEMS_ILLEGAL_ON_REMOTE_OBJECT 17
#define RTEMS_CALLED_FROM_ISR         18
#define RTEMS_INVALID_PRIORITY        19
#define RTEMS_INVALID_CLOCK           20
#define RTEMS_INVALID_NODE            21
#define RTEMS_NOT_CONFIGURED          22
#define RTEMS_NOT_OWNER_OF_RESOURCE   23
#define RTEMS_NOT_IMPLEMENTED         24
#define RTEMS_INTERNAL_ERROR          25
#define RTEMS_NO_MEMORY               26
#define RTEMS_IO_ERROR                27
#define RTEMS_PROXY_BLOCKING          28

/* Name type (4-character code) */
typedef uint32_t rtems_name;

/* Build name from 4 characters */
#define rtems_build_name(c1, c2, c3, c4) \
    ((uint32_t)(c1) << 24 | (uint32_t)(c2) << 16 | (uint32_t)(c3) << 8 | (uint32_t)(c4))

/* Time types */
typedef uint32_t rtems_interval;

typedef struct {
    uint32_t year;
    uint32_t month;
    uint32_t day;
    uint32_t hour;
    uint32_t minute;
    uint32_t second;
    uint32_t ticks;
} rtems_time_of_day;

/* Task priority */
typedef uint32_t rtems_task_priority;

#define RTEMS_NO_PRIORITY             0
#define RTEMS_MINIMUM_PRIORITY        1
#define RTEMS_MAXIMUM_PRIORITY        255
#define RTEMS_CURRENT_PRIORITY        0

/* Mode flags */
typedef uint32_t rtems_mode;

#define RTEMS_DEFAULT_MODES           0x00000000
#define RTEMS_PREEMPT                 0x00000000
#define RTEMS_NO_PREEMPT              0x00000100
#define RTEMS_TIMESLICE               0x00000200
#define RTEMS_NO_TIMESLICE            0x00000000
#define RTEMS_ASR                     0x00000000
#define RTEMS_NO_ASR                  0x00000400
#define RTEMS_INTERRUPT_LEVEL(n)      ((n) & 0x0f)

/* Attribute flags */
typedef uint32_t rtems_attribute;

#define RTEMS_DEFAULT_ATTRIBUTES      0x00000000
#define RTEMS_LOCAL                   0x00000000
#define RTEMS_GLOBAL                  0x00000002
#define RTEMS_FIFO                    0x00000000
#define RTEMS_PRIORITY                0x00000004
#define RTEMS_BINARY_SEMAPHORE        0x00000010
#define RTEMS_COUNTING_SEMAPHORE      0x00000000
#define RTEMS_SIMPLE_BINARY_SEMAPHORE 0x00000020
#define RTEMS_INHERIT_PRIORITY        0x00000040
#define RTEMS_PRIORITY_CEILING        0x00000080
#define RTEMS_NO_INHERIT_PRIORITY     0x00000000
#define RTEMS_NO_PRIORITY_CEILING     0x00000000

/* Option flags */
typedef uint32_t rtems_option;

#define RTEMS_DEFAULT_OPTIONS         0x00000000
#define RTEMS_WAIT                    0x00000000
#define RTEMS_NO_WAIT                 0x00000001
#define RTEMS_EVENT_ALL               0x00000000
#define RTEMS_EVENT_ANY               0x00000002

/* Event set */
typedef uint32_t rtems_event_set;

#define RTEMS_EVENT_0                 0x00000001
#define RTEMS_EVENT_1                 0x00000002
#define RTEMS_EVENT_2                 0x00000004
#define RTEMS_EVENT_3                 0x00000008
#define RTEMS_EVENT_4                 0x00000010
#define RTEMS_EVENT_5                 0x00000020
#define RTEMS_EVENT_6                 0x00000040
#define RTEMS_EVENT_7                 0x00000080
#define RTEMS_EVENT_8                 0x00000100
#define RTEMS_EVENT_9                 0x00000200
#define RTEMS_EVENT_10                0x00000400
#define RTEMS_EVENT_11                0x00000800
#define RTEMS_EVENT_12                0x00001000
#define RTEMS_EVENT_13                0x00002000
#define RTEMS_EVENT_14                0x00004000
#define RTEMS_EVENT_15                0x00008000
#define RTEMS_PENDING_EVENTS          0x00000000
#define RTEMS_ALL_EVENTS              0xFFFFFFFF

/* Timer */
typedef void (*rtems_timer_service_routine)(rtems_id, void *);

/* ISR */
typedef uint32_t rtems_isr_entry;

/* Misc */
#define RTEMS_SELF                    0
#define RTEMS_NO_TIMEOUT              0
#define RTEMS_SEARCH_ALL_NODES        0
#define RTEMS_SEARCH_LOCAL_NODE       0x7FFFFFFF

/* Yield processor (special value for rtems_task_wake_after) */
#define RTEMS_YIELD_PROCESSOR         0

/* Time types */
typedef long time_t;

/* Timespec for high-resolution time */
struct timespec {
    time_t tv_sec;  /* Seconds */
    long   tv_nsec; /* Nanoseconds */
};

#endif /* _RTEMS_TYPES_H */
