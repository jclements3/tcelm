/*
 * tcelm_channel.c - Channel runtime for RTEMS
 *
 * Implementation of typed message-passing channels using RTEMS message queues.
 */

#include "tcelm_channel.h"
#include "tcelm_atomic.h"
#include <string.h>
#include <stdio.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/message.h>
#else
/* Native implementation using POSIX message queues or pipes */
#include <pthread.h>
#include <stdlib.h>
#include <errno.h>
#include <time.h>
#endif

/* Default channel configuration */
const tcelm_channel_config_t TCELM_CHANNEL_DEFAULT_CONFIG = {
    .capacity = 16,
    .message_size = 256,
    .name = "CHAN"
};

/* Channel counter for unique names */
static uint32_t channel_counter = 0;

/*
 * Message envelope - wraps tcelm_value_t pointer
 */
typedef struct channel_message {
    tcelm_value_t *value;
} channel_message_t;

/*
 * Initialize channel subsystem
 */
int tcelm_channel_init(void) {
    channel_counter = 0;
    return 0;
}

/*
 * Shutdown channel subsystem
 */
void tcelm_channel_shutdown(void) {
    /* Nothing to do */
}

#ifdef __rtems__

/*
 * Create a new channel (RTEMS implementation)
 */
tcelm_channel_t *tcelm_channel_create(
    tcelm_arena_t *arena,
    const tcelm_channel_config_t *config
) {
    rtems_status_code status;
    rtems_id queue_id;

    /* Allocate channel handle */
    tcelm_channel_t *channel = tcelm_arena_alloc(arena, sizeof(tcelm_channel_t));
    if (!channel) return NULL;

    /* Build unique queue name */
    char name[5];
    uint32_t num = tcelm_atomic_fetch_add_u32(&channel_counter, 1);
    snprintf(name, 5, "C%03u", num % 1000);
    rtems_name queue_name = rtems_build_name(name[0], name[1], name[2], name[3]);

    /* Create message queue */
    status = rtems_message_queue_create(
        queue_name,
        config->capacity,
        sizeof(channel_message_t),
        RTEMS_PRIORITY,  /* Priority-ordered queue */
        &queue_id
    );

    if (status != RTEMS_SUCCESSFUL) {
        return NULL;
    }

    channel->queue_id = queue_id;
    channel->capacity = config->capacity;
    channel->message_size = config->message_size;
    channel->name = config->name;

    return channel;
}

/*
 * Send message (RTEMS implementation)
 */
int tcelm_channel_send(
    tcelm_channel_t *channel,
    tcelm_value_t *message
) {
    channel_message_t envelope = { .value = message };

    rtems_status_code status = rtems_message_queue_send(
        channel->queue_id,
        &envelope,
        sizeof(channel_message_t)
    );

    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

/*
 * Send with timeout (RTEMS implementation)
 */
int tcelm_channel_send_timeout(
    tcelm_channel_t *channel,
    tcelm_value_t *message,
    uint32_t timeout_ms
) {
    /* RTEMS message_queue_send doesn't have timeout for send
     * We'd need to implement this with a timed semaphore
     * For now, use non-blocking try_send in a loop */
    (void)timeout_ms;
    return tcelm_channel_send(channel, message);
}

/*
 * Try send without blocking (RTEMS implementation)
 */
int tcelm_channel_try_send(
    tcelm_channel_t *channel,
    tcelm_value_t *message
) {
    /* RTEMS send is non-blocking if queue not full */
    return tcelm_channel_send(channel, message);
}

/*
 * Receive message (RTEMS implementation)
 */
tcelm_value_t *tcelm_channel_receive(
    tcelm_arena_t *arena,
    tcelm_channel_t *channel
) {
    (void)arena;
    channel_message_t envelope;
    size_t size;

    rtems_status_code status = rtems_message_queue_receive(
        channel->queue_id,
        &envelope,
        &size,
        RTEMS_WAIT,
        RTEMS_NO_TIMEOUT
    );

    if (status != RTEMS_SUCCESSFUL) {
        return NULL;
    }

    return envelope.value;
}

/*
 * Receive with timeout (RTEMS implementation)
 */
tcelm_value_t *tcelm_channel_receive_timeout(
    tcelm_arena_t *arena,
    tcelm_channel_t *channel,
    uint32_t timeout_ms
) {
    channel_message_t envelope;
    size_t size;

    /* Convert ms to ticks */
    rtems_interval ticks = (timeout_ms * rtems_clock_get_ticks_per_second()) / 1000;
    if (ticks == 0) ticks = 1;

    rtems_status_code status = rtems_message_queue_receive(
        channel->queue_id,
        &envelope,
        &size,
        RTEMS_WAIT,
        ticks
    );

    if (status == RTEMS_TIMEOUT) {
        return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
    }

    if (status != RTEMS_SUCCESSFUL) {
        return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
    }

    return tcelm_custom(arena, TCELM_CTOR_JUST, "Just", 1, envelope.value);
}

/*
 * Try receive without blocking (RTEMS implementation)
 */
tcelm_value_t *tcelm_channel_try_receive(
    tcelm_arena_t *arena,
    tcelm_channel_t *channel
) {
    channel_message_t envelope;
    size_t size;

    rtems_status_code status = rtems_message_queue_receive(
        channel->queue_id,
        &envelope,
        &size,
        RTEMS_NO_WAIT,
        0
    );

    if (status == RTEMS_UNSATISFIED) {
        return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
    }

    if (status != RTEMS_SUCCESSFUL) {
        return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
    }

    return tcelm_custom(arena, TCELM_CTOR_JUST, "Just", 1, envelope.value);
}

/*
 * Broadcast message (RTEMS implementation)
 */
int tcelm_channel_broadcast(
    tcelm_channel_t *channel,
    tcelm_value_t *message
) {
    channel_message_t envelope = { .value = message };
    uint32_t count;

    rtems_status_code status = rtems_message_queue_broadcast(
        channel->queue_id,
        &envelope,
        sizeof(channel_message_t),
        &count
    );

    return (status == RTEMS_SUCCESSFUL) ? (int)count : -1;
}

/*
 * Flush channel (RTEMS implementation)
 */
int tcelm_channel_flush(tcelm_channel_t *channel) {
    uint32_t count;

    rtems_status_code status = rtems_message_queue_flush(
        channel->queue_id,
        &count
    );

    return (status == RTEMS_SUCCESSFUL) ? (int)count : -1;
}

/*
 * Get pending count (RTEMS implementation)
 */
size_t tcelm_channel_pending(tcelm_channel_t *channel) {
    uint32_t count;

    rtems_status_code status = rtems_message_queue_get_number_pending(
        channel->queue_id,
        &count
    );

    return (status == RTEMS_SUCCESSFUL) ? (size_t)count : 0;
}

/*
 * Close channel (RTEMS implementation)
 */
void tcelm_channel_close(tcelm_channel_t *channel) {
    if (channel && channel->queue_id) {
        rtems_message_queue_delete(channel->queue_id);
        channel->queue_id = 0;
    }
}

/*
 * Send urgent message (RTEMS implementation)
 */
int tcelm_channel_send_urgent(
    tcelm_channel_t *channel,
    tcelm_value_t *message
) {
    channel_message_t envelope = { .value = message };

    rtems_status_code status = rtems_message_queue_urgent(
        channel->queue_id,
        &envelope,
        sizeof(channel_message_t)
    );

    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

#else /* Native implementation */

/*
 * Simple native channel using mutex + condition variable
 */
typedef struct native_channel_data {
    pthread_mutex_t mutex;
    pthread_cond_t not_empty;
    pthread_cond_t not_full;
    channel_message_t *buffer;
    size_t capacity;
    size_t head;
    size_t tail;
    size_t count;
} native_channel_data_t;

tcelm_channel_t *tcelm_channel_create(
    tcelm_arena_t *arena,
    const tcelm_channel_config_t *config
) {
    tcelm_channel_t *channel = tcelm_arena_alloc(arena, sizeof(tcelm_channel_t));
    if (!channel) return NULL;

    native_channel_data_t *data = malloc(sizeof(native_channel_data_t));
    if (!data) return NULL;

    data->buffer = malloc(config->capacity * sizeof(channel_message_t));
    if (!data->buffer) {
        free(data);
        return NULL;
    }

    pthread_mutex_init(&data->mutex, NULL);
    pthread_cond_init(&data->not_empty, NULL);
    pthread_cond_init(&data->not_full, NULL);
    data->capacity = config->capacity;
    data->head = 0;
    data->tail = 0;
    data->count = 0;

    channel->queue_id = 0;
    channel->native_data = data;
    channel->capacity = config->capacity;
    channel->message_size = config->message_size;
    channel->name = config->name;

    return channel;
}

int tcelm_channel_send(
    tcelm_channel_t *channel,
    tcelm_value_t *message
) {
    native_channel_data_t *data = (native_channel_data_t *)channel->native_data;

    pthread_mutex_lock(&data->mutex);

    while (data->count >= data->capacity) {
        pthread_cond_wait(&data->not_full, &data->mutex);
    }

    data->buffer[data->tail].value = message;
    data->tail = (data->tail + 1) % data->capacity;
    data->count++;

    pthread_cond_signal(&data->not_empty);
    pthread_mutex_unlock(&data->mutex);

    return 0;
}

int tcelm_channel_send_timeout(
    tcelm_channel_t *channel,
    tcelm_value_t *message,
    uint32_t timeout_ms
) {
    (void)timeout_ms;
    return tcelm_channel_send(channel, message);
}

int tcelm_channel_try_send(
    tcelm_channel_t *channel,
    tcelm_value_t *message
) {
    native_channel_data_t *data = (native_channel_data_t *)channel->native_data;

    pthread_mutex_lock(&data->mutex);

    if (data->count >= data->capacity) {
        pthread_mutex_unlock(&data->mutex);
        return -1;
    }

    data->buffer[data->tail].value = message;
    data->tail = (data->tail + 1) % data->capacity;
    data->count++;

    pthread_cond_signal(&data->not_empty);
    pthread_mutex_unlock(&data->mutex);

    return 0;
}

tcelm_value_t *tcelm_channel_receive(
    tcelm_arena_t *arena,
    tcelm_channel_t *channel
) {
    (void)arena;
    native_channel_data_t *data = (native_channel_data_t *)channel->native_data;

    pthread_mutex_lock(&data->mutex);

    while (data->count == 0) {
        pthread_cond_wait(&data->not_empty, &data->mutex);
    }

    tcelm_value_t *value = data->buffer[data->head].value;
    data->head = (data->head + 1) % data->capacity;
    data->count--;

    pthread_cond_signal(&data->not_full);
    pthread_mutex_unlock(&data->mutex);

    return value;
}

tcelm_value_t *tcelm_channel_receive_timeout(
    tcelm_arena_t *arena,
    tcelm_channel_t *channel,
    uint32_t timeout_ms
) {
    native_channel_data_t *data = (native_channel_data_t *)channel->native_data;

    pthread_mutex_lock(&data->mutex);

    if (data->count == 0) {
        /* Calculate absolute timeout */
        struct timespec ts;
        clock_gettime(CLOCK_REALTIME, &ts);
        ts.tv_sec += timeout_ms / 1000;
        ts.tv_nsec += (timeout_ms % 1000) * 1000000;
        if (ts.tv_nsec >= 1000000000) {
            ts.tv_sec++;
            ts.tv_nsec -= 1000000000;
        }

        while (data->count == 0) {
            int rc = pthread_cond_timedwait(&data->not_empty, &data->mutex, &ts);
            if (rc == ETIMEDOUT) {
                pthread_mutex_unlock(&data->mutex);
                return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
            }
        }
    }

    tcelm_value_t *value = data->buffer[data->head].value;
    data->head = (data->head + 1) % data->capacity;
    data->count--;

    pthread_cond_signal(&data->not_full);
    pthread_mutex_unlock(&data->mutex);

    return tcelm_custom(arena, TCELM_CTOR_JUST, "Just", 1, value);
}

tcelm_value_t *tcelm_channel_try_receive(
    tcelm_arena_t *arena,
    tcelm_channel_t *channel
) {
    native_channel_data_t *data = (native_channel_data_t *)channel->native_data;

    pthread_mutex_lock(&data->mutex);

    if (data->count == 0) {
        pthread_mutex_unlock(&data->mutex);
        return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
    }

    tcelm_value_t *value = data->buffer[data->head].value;
    data->head = (data->head + 1) % data->capacity;
    data->count--;

    pthread_cond_signal(&data->not_full);
    pthread_mutex_unlock(&data->mutex);

    return tcelm_custom(arena, TCELM_CTOR_JUST, "Just", 1, value);
}

int tcelm_channel_broadcast(
    tcelm_channel_t *channel,
    tcelm_value_t *message
) {
    /* Native: just send once */
    return tcelm_channel_send(channel, message) == 0 ? 1 : -1;
}

int tcelm_channel_flush(tcelm_channel_t *channel) {
    native_channel_data_t *data = (native_channel_data_t *)channel->native_data;

    pthread_mutex_lock(&data->mutex);
    int count = (int)data->count;
    data->head = 0;
    data->tail = 0;
    data->count = 0;
    pthread_cond_broadcast(&data->not_full);
    pthread_mutex_unlock(&data->mutex);

    return count;
}

size_t tcelm_channel_pending(tcelm_channel_t *channel) {
    native_channel_data_t *data = (native_channel_data_t *)channel->native_data;
    pthread_mutex_lock(&data->mutex);
    size_t count = data->count;
    pthread_mutex_unlock(&data->mutex);
    return count;
}

void tcelm_channel_close(tcelm_channel_t *channel) {
    if (channel && channel->native_data) {
        native_channel_data_t *data = (native_channel_data_t *)channel->native_data;
        pthread_mutex_destroy(&data->mutex);
        pthread_cond_destroy(&data->not_empty);
        pthread_cond_destroy(&data->not_full);
        free(data->buffer);
        free(data);
        channel->native_data = NULL;
    }
}

int tcelm_channel_send_urgent(
    tcelm_channel_t *channel,
    tcelm_value_t *message
) {
    /* Native: just send normally */
    return tcelm_channel_send(channel, message);
}

#endif /* __rtems__ */

/*
 * Create channel with default config
 */
tcelm_channel_t *tcelm_channel_create_default(
    tcelm_arena_t *arena,
    size_t capacity
) {
    tcelm_channel_config_t config = TCELM_CHANNEL_DEFAULT_CONFIG;
    config.capacity = capacity;
    return tcelm_channel_create(arena, &config);
}
