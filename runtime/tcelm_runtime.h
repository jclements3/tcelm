/*
 * tcelm_runtime.h - Unified tcelm runtime header
 *
 * This header includes all tcelm runtime components.
 * Include this single header to get access to all tcelm functionality.
 *
 * The tcelm runtime follows the Ada model where:
 * - The compiler is a minimal translator (Elm -> C)
 * - The runtime provides comprehensive functionality via RTEMS
 *
 * Components:
 * - tcelm_types.h   - Elm value representations
 * - tcelm_arena.h   - Arena-based memory allocation
 * - tcelm_task.h    - Task monad (wraps rtems_task_*)
 * - tcelm_channel.h - Channel type (wraps rtems_message_queue_*)
 * - tcelm_mvar.h    - MVar type (wraps rtems_semaphore_*)
 * - tcelm_timer.h   - Timer subscriptions (wraps rtems_timer_*)
 * - tcelm_io.h      - IO monad operations
 *
 * On RTEMS builds (__rtems__ defined), these use real RTEMS primitives.
 * On native builds, they use POSIX pthreads for development/testing.
 */

#ifndef TCELM_RUNTIME_H
#define TCELM_RUNTIME_H

/* Core types and memory */
#include "tcelm_types.h"
#include "tcelm_arena.h"

/* Concurrency primitives */
#include "tcelm_task.h"
#include "tcelm_channel.h"
#include "tcelm_mvar.h"

/* Time and I/O */
#include "tcelm_timer.h"
#include "tcelm_io.h"

/*
 * Initialize all runtime subsystems
 * Call this before any tcelm operations
 */
static inline int tcelm_runtime_init(void) {
    int result = 0;
    result |= tcelm_task_init();
    result |= tcelm_channel_init();
    result |= tcelm_mvar_init();
    result |= tcelm_timer_init();
    result |= tcelm_io_init();
    return result;
}

/*
 * Shutdown all runtime subsystems
 * Call this before program exit
 */
static inline void tcelm_runtime_shutdown(void) {
    tcelm_io_shutdown();
    tcelm_timer_shutdown();
    tcelm_mvar_shutdown();
    tcelm_channel_shutdown();
    tcelm_task_shutdown();
}

/*
 * Store command line arguments for later retrieval
 * Call this from main() with argc/argv
 */
void tcelm_io_set_args(int argc, char **argv);

/*
 * ============================================================================
 * CONVENIENCE MACROS
 * ============================================================================
 */

/* Create arena with default size (64KB) */
#define TCELM_ARENA_DEFAULT() tcelm_arena_create(64 * 1024)

/* Create arena with specified size in KB */
#define TCELM_ARENA_KB(kb) tcelm_arena_create((kb) * 1024)

/* Common result patterns */
#define TCELM_OK(arena, value) \
    tcelm_custom((arena), TCELM_CTOR_OK, "Ok", 1, (value))

#define TCELM_ERR(arena, error) \
    tcelm_custom((arena), TCELM_CTOR_ERR, "Err", 1, (error))

#define TCELM_JUST(arena, value) \
    tcelm_custom((arena), TCELM_CTOR_JUST, "Just", 1, (value))

#define TCELM_NOTHING(arena) \
    tcelm_custom((arena), TCELM_CTOR_NOTHING, "Nothing", 0)

/* Check result type */
#define TCELM_IS_OK(val) \
    (TCELM_IS_CUSTOM(val) && tcelm_custom_ctor(val) == TCELM_CTOR_OK)

#define TCELM_IS_ERR(val) \
    (TCELM_IS_CUSTOM(val) && tcelm_custom_ctor(val) == TCELM_CTOR_ERR)

#define TCELM_IS_JUST(val) \
    (TCELM_IS_CUSTOM(val) && tcelm_custom_ctor(val) == TCELM_CTOR_JUST)

#define TCELM_IS_NOTHING(val) \
    (TCELM_IS_CUSTOM(val) && tcelm_custom_ctor(val) == TCELM_CTOR_NOTHING)

/* Extract value from Ok/Just */
#define TCELM_UNWRAP_OK(val) tcelm_custom_arg((val), 0)
#define TCELM_UNWRAP_ERR(val) tcelm_custom_arg((val), 0)
#define TCELM_UNWRAP_JUST(val) tcelm_custom_arg((val), 0)

/*
 * ============================================================================
 * TEA (The Elm Architecture) SUPPORT
 * ============================================================================
 */

/*
 * TEA program definition
 */
typedef struct tcelm_tea_program {
    /* Initial model */
    tcelm_value_t *(*init)(tcelm_arena_t *arena);

    /* Update function: (msg, model) -> (model, cmd) */
    tcelm_value_t *(*update)(tcelm_arena_t *arena, tcelm_value_t *msg, tcelm_value_t *model);

    /* View function (optional): model -> () */
    void (*view)(tcelm_arena_t *arena, tcelm_value_t *model);

    /* Subscriptions function (optional): model -> sub */
    tcelm_value_t *(*subscriptions)(tcelm_arena_t *arena, tcelm_value_t *model);
} tcelm_tea_program_t;

/*
 * Run a TEA program
 * This is the main event loop for Elm-style applications
 */
void tcelm_tea_run(tcelm_tea_program_t *program);

#endif /* TCELM_RUNTIME_H */
