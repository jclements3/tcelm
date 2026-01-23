/*
 * tcelm_events.h - Event flags for RTEMS
 *
 * Lightweight task-to-task signaling using 32-bit event flags.
 */

#ifndef TCELM_EVENTS_H
#define TCELM_EVENTS_H

#include "tcelm_types.h"
#include <stdint.h>
#include <stdbool.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/event.h>
#endif

/*
 * Event set (32 bits)
 */
typedef uint32_t tcelm_event_set_t;

/*
 * Event wait mode
 */
typedef enum {
    TCELM_EVENT_WAIT_ANY,   /* Return when ANY requested event is received */
    TCELM_EVENT_WAIT_ALL    /* Return when ALL requested events are received */
} tcelm_event_wait_mode_t;

/*
 * Event receive result
 */
typedef enum {
    TCELM_EVENT_SUCCESS,
    TCELM_EVENT_TIMEOUT,
    TCELM_EVENT_INVALID_TASK,
    TCELM_EVENT_ERROR
} tcelm_event_result_t;

/*
 * Initialize event subsystem
 */
int tcelm_events_init(void);

/*
 * Shutdown event subsystem
 */
void tcelm_events_shutdown(void);

/*
 * Send events to a task
 * task_id: Target task ID (0 = self for RTEMS, or use tcelm_task_self())
 * events: Bit mask of events to send
 * Returns 0 on success, -1 on error
 */
int tcelm_event_send(uint32_t task_id, tcelm_event_set_t events);

/*
 * Receive events (blocking)
 * requested: Events to wait for
 * mode: WAIT_ANY or WAIT_ALL
 * received: Output - events that were received
 * Returns result code
 */
tcelm_event_result_t tcelm_event_receive(
    tcelm_event_set_t requested,
    tcelm_event_wait_mode_t mode,
    tcelm_event_set_t *received
);

/*
 * Receive events with timeout
 * timeout_ms: Timeout in milliseconds (0 = no wait)
 */
tcelm_event_result_t tcelm_event_receive_timeout(
    tcelm_event_set_t requested,
    tcelm_event_wait_mode_t mode,
    uint32_t timeout_ms,
    tcelm_event_set_t *received
);

/*
 * Try to receive events (non-blocking)
 */
tcelm_event_result_t tcelm_event_try_receive(
    tcelm_event_set_t requested,
    tcelm_event_wait_mode_t mode,
    tcelm_event_set_t *received
);

/*
 * Get pending events without consuming them
 */
tcelm_event_set_t tcelm_event_pending(void);

/*
 * Clear specific events
 */
int tcelm_event_clear(tcelm_event_set_t events);

/*
 * Event helper macros
 */
#define TCELM_EVENT(n) (1u << (n))
#define TCELM_EVENT_ALL 0xFFFFFFFFu

/*
 * Standard event bits (by convention)
 */
#define TCELM_EVENT_DATA_READY  TCELM_EVENT(0)
#define TCELM_EVENT_TIMEOUT     TCELM_EVENT(1)
#define TCELM_EVENT_SHUTDOWN    TCELM_EVENT(2)
#define TCELM_EVENT_ERROR       TCELM_EVENT(3)
#define TCELM_EVENT_COMPLETE    TCELM_EVENT(4)
#define TCELM_EVENT_USER_BASE   TCELM_EVENT(8)

#endif /* TCELM_EVENTS_H */
