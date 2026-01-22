/*
 * tcelm_mvar.h - MVar runtime for RTEMS
 *
 * MVars provide shared mutable state with mutual exclusion.
 * On RTEMS, uses binary semaphores with priority inheritance
 * to prevent priority inversion.
 */

#ifndef TCELM_MVAR_H
#define TCELM_MVAR_H

#include "tcelm_types.h"
#include "tcelm_arena.h"

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/sem.h>
#else
/* Native stubs */
typedef uint32_t rtems_id;
typedef uint32_t rtems_status_code;
#define RTEMS_SUCCESSFUL 0
#endif

/*
 * MVar handle - wraps semaphore + value storage
 */
typedef struct tcelm_mvar {
    rtems_id sem_id;            /* Binary semaphore for mutual exclusion */
    void *native_data;          /* Native implementation data (64-bit safe) */
    tcelm_value_t *value;       /* Current value (NULL if empty) */
    bool is_empty;              /* Is the MVar empty? */
    const char *name;           /* Name for debugging */
} tcelm_mvar_t;

/*
 * Initialize MVar subsystem
 */
int tcelm_mvar_init(void);

/*
 * Shutdown MVar subsystem
 */
void tcelm_mvar_shutdown(void);

/*
 * Create a new MVar containing the given value
 */
tcelm_mvar_t *tcelm_mvar_new(tcelm_arena_t *arena, tcelm_value_t *value);

/*
 * Create a new empty MVar
 */
tcelm_mvar_t *tcelm_mvar_new_empty(tcelm_arena_t *arena);

/*
 * Take the value from an MVar, leaving it empty
 * Blocks if the MVar is already empty
 */
tcelm_value_t *tcelm_mvar_take(tcelm_arena_t *arena, tcelm_mvar_t *mvar);

/*
 * Take with timeout (milliseconds)
 * Returns Nothing on timeout, Just value on success
 */
tcelm_value_t *tcelm_mvar_take_timeout(
    tcelm_arena_t *arena,
    tcelm_mvar_t *mvar,
    uint32_t timeout_ms
);

/*
 * Try to take without blocking
 * Returns Nothing if empty, Just value if available
 */
tcelm_value_t *tcelm_mvar_try_take(tcelm_arena_t *arena, tcelm_mvar_t *mvar);

/*
 * Put a value into an MVar
 * Blocks if the MVar is full (another task must take first)
 */
int tcelm_mvar_put(tcelm_mvar_t *mvar, tcelm_value_t *value);

/*
 * Put with timeout (milliseconds)
 * Returns 0 on success, -1 on timeout
 */
int tcelm_mvar_put_timeout(
    tcelm_mvar_t *mvar,
    tcelm_value_t *value,
    uint32_t timeout_ms
);

/*
 * Try to put without blocking
 * Returns 0 on success, -1 if MVar is full
 */
int tcelm_mvar_try_put(tcelm_mvar_t *mvar, tcelm_value_t *value);

/*
 * Read the value without removing it
 * Equivalent to take followed by put
 */
tcelm_value_t *tcelm_mvar_read(tcelm_arena_t *arena, tcelm_mvar_t *mvar);

/*
 * Atomically modify the contents of an MVar
 * Takes the value, applies function, puts result back
 * Returns the second element of the tuple returned by fn
 */
tcelm_value_t *tcelm_mvar_modify(
    tcelm_arena_t *arena,
    tcelm_mvar_t *mvar,
    tcelm_value_t *(*fn)(tcelm_arena_t *, tcelm_value_t *)
);

/*
 * Swap the contents of an MVar
 * Atomically replaces value and returns old one
 */
tcelm_value_t *tcelm_mvar_swap(
    tcelm_arena_t *arena,
    tcelm_mvar_t *mvar,
    tcelm_value_t *new_value
);

/*
 * Check if MVar is empty (non-blocking)
 */
bool tcelm_mvar_is_empty(tcelm_mvar_t *mvar);

/*
 * Delete an MVar
 */
void tcelm_mvar_delete(tcelm_mvar_t *mvar);

#endif /* TCELM_MVAR_H */
