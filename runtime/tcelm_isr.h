/*
 * tcelm_isr.h - ISR to Message Bridge for RTEMS
 *
 * Allows interrupt service routines to send messages to Elm tasks.
 * ISRs cannot directly call Elm code (not safe), so we use a deferred
 * processing model where ISRs post messages to a queue that a worker
 * task delivers to Elm.
 */

#ifndef TCELM_ISR_H
#define TCELM_ISR_H

#include "tcelm_types.h"
#include <stdint.h>
#include <stdbool.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/irq-extension.h>
#endif

/*
 * Maximum number of registered ISR handlers
 */
#define TCELM_MAX_ISR_HANDLERS 32

/*
 * ISR message (sent from ISR to Elm task)
 */
typedef struct tcelm_isr_message {
    uint32_t irq_number;        /* Which interrupt fired */
    uint32_t timestamp;         /* Tick count when interrupt occurred */
    uint32_t data;              /* Optional data from ISR */
    void *user_data;            /* User-provided context */
} tcelm_isr_message_t;

/*
 * ISR handler callback (called in task context, NOT ISR context)
 * This is safe to call Elm functions from.
 */
typedef void (*tcelm_isr_handler_t)(
    const tcelm_isr_message_t *message,
    void *user_data
);

/*
 * ISR registration handle
 */
typedef struct tcelm_isr_registration {
    uint32_t irq_number;
    tcelm_isr_handler_t handler;
    void *user_data;
    bool enabled;
    uint32_t count;             /* Number of times triggered */

#ifdef __rtems__
    rtems_interrupt_handler rtems_handler;
#endif
} tcelm_isr_registration_t;

/*
 * Initialize ISR bridge subsystem
 * Must be called before registering any handlers
 */
int tcelm_isr_init(void);

/*
 * Shutdown ISR bridge
 */
void tcelm_isr_shutdown(void);

/*
 * Register an ISR handler
 * irq_number: Hardware interrupt number
 * handler: Callback (called in task context, not ISR)
 * user_data: Passed to handler
 * Returns registration handle or NULL on failure
 */
tcelm_isr_registration_t *tcelm_isr_register(
    uint32_t irq_number,
    tcelm_isr_handler_t handler,
    void *user_data
);

/*
 * Unregister an ISR handler
 */
void tcelm_isr_unregister(tcelm_isr_registration_t *reg);

/*
 * Enable/disable an ISR handler
 */
int tcelm_isr_enable(tcelm_isr_registration_t *reg);
int tcelm_isr_disable(tcelm_isr_registration_t *reg);

/*
 * Get handler statistics
 */
uint32_t tcelm_isr_get_count(tcelm_isr_registration_t *reg);
void tcelm_isr_reset_count(tcelm_isr_registration_t *reg);

/*
 * Post a message from ISR context (INTERNAL)
 * This is called by the actual ISR to queue a message.
 * DO NOT call this from normal code - it's ISR-safe but limited.
 */
void tcelm_isr_post(uint32_t irq_number, uint32_t data);

/*
 * Process pending ISR messages (called by worker task)
 * timeout_ms: How long to wait for messages (0 = non-blocking)
 * Returns number of messages processed
 */
int tcelm_isr_process(uint32_t timeout_ms);

/*
 * Get pending message count
 */
uint32_t tcelm_isr_pending_count(void);

/*
 * Elm-friendly wrappers for interrupt subscription
 */

/*
 * Subscribe to interrupt (for Elm)
 * Returns subscription ID, or -1 on error
 */
int32_t tcelm_isr_subscribe(
    uint32_t irq_number,
    uint32_t elm_task_id,
    uint32_t msg_tag
);

/*
 * Unsubscribe from interrupt
 */
void tcelm_isr_unsubscribe(int32_t subscription_id);

#endif /* TCELM_ISR_H */
