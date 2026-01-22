/*
 * tcelm_channel.h - Channel runtime for RTEMS
 *
 * Wraps RTEMS message queues to provide typed message-passing channels.
 * Channels are bounded queues with priority-aware blocking.
 */

#ifndef TCELM_CHANNEL_H
#define TCELM_CHANNEL_H

#include "tcelm_types.h"
#include "tcelm_arena.h"

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/message.h>
#else
/* Native stubs */
typedef uint32_t rtems_id;
typedef uint32_t rtems_status_code;
#define RTEMS_SUCCESSFUL 0
#endif

/*
 * Channel handle - wraps rtems_id for a message queue
 */
typedef struct tcelm_channel {
    rtems_id queue_id;          /* RTEMS message queue ID */
    void *native_data;          /* Native implementation data (64-bit safe) */
    size_t capacity;            /* Maximum messages */
    size_t message_size;        /* Size of each message slot */
    const char *name;           /* Channel name for debugging */
} tcelm_channel_t;

/*
 * Channel configuration
 */
typedef struct tcelm_channel_config {
    size_t capacity;            /* Max pending messages (default: 16) */
    size_t message_size;        /* Max message size in bytes (default: 256) */
    const char *name;           /* Optional name */
} tcelm_channel_config_t;

/* Default configuration */
extern const tcelm_channel_config_t TCELM_CHANNEL_DEFAULT_CONFIG;

/*
 * Initialize channel subsystem
 */
int tcelm_channel_init(void);

/*
 * Shutdown channel subsystem
 */
void tcelm_channel_shutdown(void);

/*
 * Create a new channel
 * Returns channel handle or NULL on failure
 */
tcelm_channel_t *tcelm_channel_create(
    tcelm_arena_t *arena,
    const tcelm_channel_config_t *config
);

/*
 * Create channel with default config and specified capacity
 */
tcelm_channel_t *tcelm_channel_create_default(
    tcelm_arena_t *arena,
    size_t capacity
);

/*
 * Send a message to a channel
 * Blocks if channel is full (RTEMS_WAIT)
 * Returns 0 on success, -1 on error
 */
int tcelm_channel_send(
    tcelm_channel_t *channel,
    tcelm_value_t *message
);

/*
 * Send with timeout (milliseconds)
 * Returns 0 on success, -1 on timeout/error
 */
int tcelm_channel_send_timeout(
    tcelm_channel_t *channel,
    tcelm_value_t *message,
    uint32_t timeout_ms
);

/*
 * Try to send without blocking
 * Returns 0 on success, -1 if channel is full
 */
int tcelm_channel_try_send(
    tcelm_channel_t *channel,
    tcelm_value_t *message
);

/*
 * Receive a message from a channel
 * Blocks until a message is available (RTEMS_WAIT)
 */
tcelm_value_t *tcelm_channel_receive(
    tcelm_arena_t *arena,
    tcelm_channel_t *channel
);

/*
 * Receive with timeout (milliseconds)
 * Returns Nothing on timeout, Just message on success
 */
tcelm_value_t *tcelm_channel_receive_timeout(
    tcelm_arena_t *arena,
    tcelm_channel_t *channel,
    uint32_t timeout_ms
);

/*
 * Try to receive without blocking
 * Returns Nothing if no message, Just message if available
 */
tcelm_value_t *tcelm_channel_try_receive(
    tcelm_arena_t *arena,
    tcelm_channel_t *channel
);

/*
 * Broadcast a message to all waiting receivers
 * Returns number of tasks that received the message
 */
int tcelm_channel_broadcast(
    tcelm_channel_t *channel,
    tcelm_value_t *message
);

/*
 * Flush all pending messages from a channel
 * Returns number of messages flushed
 */
int tcelm_channel_flush(tcelm_channel_t *channel);

/*
 * Get number of pending messages
 */
size_t tcelm_channel_pending(tcelm_channel_t *channel);

/*
 * Close and delete a channel
 */
void tcelm_channel_close(tcelm_channel_t *channel);

/*
 * Send an urgent message (goes to front of queue)
 * On RTEMS, uses rtems_message_queue_urgent
 */
int tcelm_channel_send_urgent(
    tcelm_channel_t *channel,
    tcelm_value_t *message
);

#endif /* TCELM_CHANNEL_H */
