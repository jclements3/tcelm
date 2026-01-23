/*
 * tcelm_isr.c - ISR to Message Bridge for RTEMS
 *
 * Implements a deferred ISR processing model where interrupts
 * post to a queue and a worker task delivers messages to Elm.
 */

#include "tcelm_isr.h"
#include "tcelm_events.h"
#include <string.h>
#include <stdio.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/irq-extension.h>
#else
#include <pthread.h>
#include <signal.h>
#include <time.h>
#endif

/*
 * ISR message queue (ring buffer)
 * Must be lock-free for ISR safety
 */
#define ISR_QUEUE_SIZE 64

typedef struct {
    tcelm_isr_message_t messages[ISR_QUEUE_SIZE];
    volatile uint32_t head;
    volatile uint32_t tail;
} isr_queue_t;

/*
 * Global state
 */
static struct {
    bool initialized;
    isr_queue_t queue;
    tcelm_isr_registration_t registrations[TCELM_MAX_ISR_HANDLERS];
    uint32_t registration_count;

#ifdef __rtems__
    rtems_id worker_task_id;
    bool worker_running;
#else
    pthread_t worker_thread;
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    bool worker_running;
#endif
} isr_state;

/*
 * Lock-free queue operations
 */
static inline bool queue_push(isr_queue_t *q, const tcelm_isr_message_t *msg) {
    uint32_t next = (q->head + 1) % ISR_QUEUE_SIZE;
    if (next == q->tail) {
        return false;  /* Full */
    }
    q->messages[q->head] = *msg;
    __sync_synchronize();  /* Memory barrier */
    q->head = next;
    return true;
}

static inline bool queue_pop(isr_queue_t *q, tcelm_isr_message_t *msg) {
    if (q->tail == q->head) {
        return false;  /* Empty */
    }
    *msg = q->messages[q->tail];
    __sync_synchronize();  /* Memory barrier */
    q->tail = (q->tail + 1) % ISR_QUEUE_SIZE;
    return true;
}

static inline uint32_t queue_count(isr_queue_t *q) {
    return (q->head - q->tail + ISR_QUEUE_SIZE) % ISR_QUEUE_SIZE;
}

/*
 * Find registration by IRQ number
 */
static tcelm_isr_registration_t *find_registration(uint32_t irq_number) {
    for (uint32_t i = 0; i < isr_state.registration_count; i++) {
        if (isr_state.registrations[i].irq_number == irq_number) {
            return &isr_state.registrations[i];
        }
    }
    return NULL;
}

#ifdef __rtems__

/*
 * RTEMS ISR handler (called in ISR context)
 */
static void rtems_isr_handler(void *arg) {
    tcelm_isr_registration_t *reg = (tcelm_isr_registration_t *)arg;

    tcelm_isr_message_t msg = {
        .irq_number = reg->irq_number,
        .timestamp = rtems_clock_get_ticks_since_boot(),
        .data = 0,
        .user_data = reg->user_data
    };

    if (queue_push(&isr_state.queue, &msg)) {
        reg->count++;
        /* Wake worker task via event */
        rtems_event_send(isr_state.worker_task_id, RTEMS_EVENT_0);
    }
    /* If queue full, message is dropped (overrun) */
}

/*
 * RTEMS worker task
 */
static rtems_task isr_worker_task(rtems_task_argument arg) {
    (void)arg;

    while (isr_state.worker_running) {
        rtems_event_set events;
        rtems_status_code status = rtems_event_receive(
            RTEMS_EVENT_0,
            RTEMS_WAIT | RTEMS_EVENT_ANY,
            RTEMS_MILLISECONDS_TO_TICKS(100),
            &events
        );

        if (status == RTEMS_SUCCESSFUL || status == RTEMS_TIMEOUT) {
            tcelm_isr_process(0);
        }
    }

    rtems_task_delete(RTEMS_SELF);
}

#else /* Native implementation */

/*
 * Worker thread for native implementation
 */
static void *isr_worker_thread(void *arg) {
    (void)arg;

    while (isr_state.worker_running) {
        pthread_mutex_lock(&isr_state.mutex);

        /* Wait for signal or timeout */
        struct timespec ts;
        clock_gettime(CLOCK_REALTIME, &ts);
        ts.tv_nsec += 100000000;  /* 100ms */
        if (ts.tv_nsec >= 1000000000) {
            ts.tv_sec++;
            ts.tv_nsec -= 1000000000;
        }

        pthread_cond_timedwait(&isr_state.cond, &isr_state.mutex, &ts);
        pthread_mutex_unlock(&isr_state.mutex);

        tcelm_isr_process(0);
    }

    return NULL;
}

/*
 * Signal handler for simulating interrupts (native)
 */
static void native_signal_handler(int sig, siginfo_t *info, void *context) {
    (void)context;
    uint32_t irq = (uint32_t)(sig - SIGRTMIN);

    tcelm_isr_registration_t *reg = find_registration(irq);
    if (!reg || !reg->enabled) {
        return;
    }

    tcelm_isr_message_t msg = {
        .irq_number = irq,
        .timestamp = 0,  /* No tick counter in native */
        .data = info ? (uint32_t)info->si_value.sival_int : 0,
        .user_data = reg->user_data
    };

    if (queue_push(&isr_state.queue, &msg)) {
        reg->count++;
        pthread_cond_signal(&isr_state.cond);
    }
}

#endif /* __rtems__ */

/*
 * Initialize ISR bridge
 */
int tcelm_isr_init(void) {
    if (isr_state.initialized) {
        return 0;
    }

    memset(&isr_state, 0, sizeof(isr_state));
    isr_state.worker_running = true;

#ifdef __rtems__
    /* Create worker task */
    rtems_status_code status = rtems_task_create(
        rtems_build_name('I', 'S', 'R', 'W'),
        10,  /* High priority */
        RTEMS_MINIMUM_STACK_SIZE * 2,
        RTEMS_DEFAULT_MODES,
        RTEMS_DEFAULT_ATTRIBUTES,
        &isr_state.worker_task_id
    );

    if (status != RTEMS_SUCCESSFUL) {
        return -1;
    }

    status = rtems_task_start(
        isr_state.worker_task_id,
        isr_worker_task,
        0
    );

    if (status != RTEMS_SUCCESSFUL) {
        rtems_task_delete(isr_state.worker_task_id);
        return -1;
    }
#else
    pthread_mutex_init(&isr_state.mutex, NULL);
    pthread_cond_init(&isr_state.cond, NULL);
    pthread_create(&isr_state.worker_thread, NULL, isr_worker_thread, NULL);
#endif

    isr_state.initialized = true;
    return 0;
}

/*
 * Shutdown ISR bridge
 */
void tcelm_isr_shutdown(void) {
    if (!isr_state.initialized) {
        return;
    }

    isr_state.worker_running = false;

    /* Unregister all handlers */
    for (uint32_t i = 0; i < isr_state.registration_count; i++) {
        if (isr_state.registrations[i].enabled) {
            tcelm_isr_unregister(&isr_state.registrations[i]);
        }
    }

#ifdef __rtems__
    rtems_event_send(isr_state.worker_task_id, RTEMS_EVENT_0);
    /* Task will delete itself */
#else
    pthread_cond_signal(&isr_state.cond);
    pthread_join(isr_state.worker_thread, NULL);
    pthread_mutex_destroy(&isr_state.mutex);
    pthread_cond_destroy(&isr_state.cond);
#endif

    isr_state.initialized = false;
}

/*
 * Register an ISR handler
 */
tcelm_isr_registration_t *tcelm_isr_register(
    uint32_t irq_number,
    tcelm_isr_handler_t handler,
    void *user_data
) {
    if (!isr_state.initialized || !handler) {
        return NULL;
    }

    if (isr_state.registration_count >= TCELM_MAX_ISR_HANDLERS) {
        return NULL;
    }

    /* Check if already registered */
    if (find_registration(irq_number)) {
        return NULL;
    }

    tcelm_isr_registration_t *reg = &isr_state.registrations[isr_state.registration_count++];
    reg->irq_number = irq_number;
    reg->handler = handler;
    reg->user_data = user_data;
    reg->enabled = false;
    reg->count = 0;

#ifdef __rtems__
    /* Install RTEMS interrupt handler */
    rtems_status_code status = rtems_interrupt_handler_install(
        irq_number,
        "tcelm_isr",
        RTEMS_INTERRUPT_UNIQUE,
        rtems_isr_handler,
        reg
    );

    if (status != RTEMS_SUCCESSFUL) {
        isr_state.registration_count--;
        return NULL;
    }
    reg->rtems_handler = rtems_isr_handler;
#else
    /* Install signal handler for native testing */
    if (irq_number < 32) {
        struct sigaction sa;
        sa.sa_sigaction = native_signal_handler;
        sa.sa_flags = SA_SIGINFO;
        sigemptyset(&sa.sa_mask);

        if (sigaction(SIGRTMIN + irq_number, &sa, NULL) != 0) {
            isr_state.registration_count--;
            return NULL;
        }
    }
#endif

    return reg;
}

/*
 * Unregister an ISR handler
 */
void tcelm_isr_unregister(tcelm_isr_registration_t *reg) {
    if (!reg) {
        return;
    }

    tcelm_isr_disable(reg);

#ifdef __rtems__
    rtems_interrupt_handler_remove(
        reg->irq_number,
        reg->rtems_handler,
        reg
    );
#else
    /* Restore default signal handler */
    if (reg->irq_number < 32) {
        signal(SIGRTMIN + reg->irq_number, SIG_DFL);
    }
#endif

    reg->handler = NULL;
    reg->enabled = false;
}

/*
 * Enable ISR handler
 */
int tcelm_isr_enable(tcelm_isr_registration_t *reg) {
    if (!reg) {
        return -1;
    }
    reg->enabled = true;
    return 0;
}

/*
 * Disable ISR handler
 */
int tcelm_isr_disable(tcelm_isr_registration_t *reg) {
    if (!reg) {
        return -1;
    }
    reg->enabled = false;
    return 0;
}

/*
 * Get handler trigger count
 */
uint32_t tcelm_isr_get_count(tcelm_isr_registration_t *reg) {
    return reg ? reg->count : 0;
}

/*
 * Reset handler trigger count
 */
void tcelm_isr_reset_count(tcelm_isr_registration_t *reg) {
    if (reg) {
        reg->count = 0;
    }
}

/*
 * Post message from ISR context
 */
void tcelm_isr_post(uint32_t irq_number, uint32_t data) {
    tcelm_isr_message_t msg = {
        .irq_number = irq_number,
#ifdef __rtems__
        .timestamp = rtems_clock_get_ticks_since_boot(),
#else
        .timestamp = 0,
#endif
        .data = data,
        .user_data = NULL
    };

    tcelm_isr_registration_t *reg = find_registration(irq_number);
    if (reg) {
        msg.user_data = reg->user_data;
    }

    queue_push(&isr_state.queue, &msg);

#ifdef __rtems__
    if (isr_state.worker_task_id) {
        rtems_event_send(isr_state.worker_task_id, RTEMS_EVENT_0);
    }
#else
    pthread_cond_signal(&isr_state.cond);
#endif
}

/*
 * Process pending ISR messages
 */
int tcelm_isr_process(uint32_t timeout_ms) {
    (void)timeout_ms;

    int count = 0;
    tcelm_isr_message_t msg;

    while (queue_pop(&isr_state.queue, &msg)) {
        tcelm_isr_registration_t *reg = find_registration(msg.irq_number);
        if (reg && reg->handler && reg->enabled) {
            reg->handler(&msg, reg->user_data);
        }
        count++;
    }

    return count;
}

/*
 * Get pending message count
 */
uint32_t tcelm_isr_pending_count(void) {
    return queue_count(&isr_state.queue);
}

/*
 * Elm subscription support
 */
typedef struct {
    uint32_t irq_number;
    uint32_t elm_task_id;
    uint32_t msg_tag;
    tcelm_isr_registration_t *reg;
} elm_isr_subscription_t;

#define MAX_ELM_SUBSCRIPTIONS 32
static elm_isr_subscription_t elm_subscriptions[MAX_ELM_SUBSCRIPTIONS];
static uint32_t elm_subscription_count = 0;

static void elm_isr_handler(const tcelm_isr_message_t *message, void *user_data) {
    elm_isr_subscription_t *sub = (elm_isr_subscription_t *)user_data;
    if (!sub) return;

    /* Send message to Elm task */
    /* This would call into the Elm runtime to deliver the message */
    /* tcelm_task_send_int(sub->elm_task_id, sub->msg_tag, message->timestamp); */
    (void)message;
    (void)sub;
}

int32_t tcelm_isr_subscribe(
    uint32_t irq_number,
    uint32_t elm_task_id,
    uint32_t msg_tag
) {
    if (elm_subscription_count >= MAX_ELM_SUBSCRIPTIONS) {
        return -1;
    }

    elm_isr_subscription_t *sub = &elm_subscriptions[elm_subscription_count];
    sub->irq_number = irq_number;
    sub->elm_task_id = elm_task_id;
    sub->msg_tag = msg_tag;

    sub->reg = tcelm_isr_register(irq_number, elm_isr_handler, sub);
    if (!sub->reg) {
        return -1;
    }

    tcelm_isr_enable(sub->reg);

    return elm_subscription_count++;
}

void tcelm_isr_unsubscribe(int32_t subscription_id) {
    if (subscription_id < 0 || subscription_id >= (int32_t)elm_subscription_count) {
        return;
    }

    elm_isr_subscription_t *sub = &elm_subscriptions[subscription_id];
    if (sub->reg) {
        tcelm_isr_unregister(sub->reg);
        sub->reg = NULL;
    }
}
