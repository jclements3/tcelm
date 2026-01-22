/*
 * rtems_event.h - RTEMS Event API for tcelm/NUC toolchain
 */

#ifndef _RTEMS_EVENT_H
#define _RTEMS_EVENT_H

#include <rtems/rtems_types.h>

/*
 * Event Management Functions
 */

/* Send events to a task */
rtems_status_code rtems_event_send(
    rtems_id        id,
    rtems_event_set event_in
);

/* Receive events */
rtems_status_code rtems_event_receive(
    rtems_event_set  event_in,
    rtems_option     option_set,
    rtems_interval   ticks,
    rtems_event_set *event_out
);

/*
 * System Events (for internal use)
 */
rtems_status_code rtems_event_system_send(
    rtems_id        id,
    rtems_event_set event_in
);

rtems_status_code rtems_event_system_receive(
    rtems_event_set  event_in,
    rtems_option     option_set,
    rtems_interval   ticks,
    rtems_event_set *event_out
);

#endif /* _RTEMS_EVENT_H */
