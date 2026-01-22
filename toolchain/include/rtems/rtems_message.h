/*
 * rtems_message.h - RTEMS Message Queue API for tcelm/NUC toolchain
 */

#ifndef _RTEMS_MESSAGE_H
#define _RTEMS_MESSAGE_H

#include <rtems/rtems_types.h>

/*
 * Message Queue Management Functions
 */

/* Create a message queue */
rtems_status_code rtems_message_queue_create(
    rtems_name       name,
    uint32_t         count,
    size_t           max_message_size,
    rtems_attribute  attribute_set,
    rtems_id        *id
);

/* Delete a message queue */
rtems_status_code rtems_message_queue_delete(
    rtems_id id
);

/* Send a message to a queue */
rtems_status_code rtems_message_queue_send(
    rtems_id    id,
    const void *buffer,
    size_t      size
);

/* Send urgent message (to front of queue) */
rtems_status_code rtems_message_queue_urgent(
    rtems_id    id,
    const void *buffer,
    size_t      size
);

/* Broadcast message (to all waiters) */
rtems_status_code rtems_message_queue_broadcast(
    rtems_id    id,
    const void *buffer,
    size_t      size,
    uint32_t   *count
);

/* Receive a message from a queue */
rtems_status_code rtems_message_queue_receive(
    rtems_id       id,
    void          *buffer,
    size_t        *size,
    rtems_option   option_set,
    rtems_interval timeout
);

/* Flush all messages */
rtems_status_code rtems_message_queue_flush(
    rtems_id  id,
    uint32_t *count
);

/* Get number of pending messages */
rtems_status_code rtems_message_queue_get_number_pending(
    rtems_id  id,
    uint32_t *count
);

/* Get message queue by name */
rtems_status_code rtems_message_queue_ident(
    rtems_name  name,
    uint32_t    node,
    rtems_id   *id
);

#endif /* _RTEMS_MESSAGE_H */
