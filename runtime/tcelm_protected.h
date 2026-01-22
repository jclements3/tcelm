/*
 * tcelm_protected.h - Protected types for RTEMS
 *
 * Provides Ada-style protected type semantics for safe concurrent access
 * to shared data. Uses RTEMS semaphores with priority inheritance.
 *
 * Protected types provide:
 * - Automatic mutual exclusion for procedures (write access)
 * - Shared access for functions (read-only, multiple readers)
 * - Entry guards (barrier conditions) for conditional execution
 * - Priority inheritance to prevent priority inversion
 *
 * Use cases:
 * - Shared data structures accessed by multiple tasks
 * - Producer/consumer buffers with conditional access
 * - State machines with guarded transitions
 */

#ifndef TCELM_PROTECTED_H
#define TCELM_PROTECTED_H

#include "tcelm_types.h"
#include "tcelm_arena.h"

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/sem.h>
#else
#include <pthread.h>
#include <stdbool.h>
#include <stdint.h>
typedef uint32_t rtems_id;
#endif

/*
 * Maximum number of entries per protected object
 */
#define TCELM_PROTECTED_MAX_ENTRIES 8

/*
 * Entry guard function type
 * Returns true if the entry can be executed, false to block
 */
typedef bool (*tcelm_entry_guard_t)(void *data);

/*
 * Entry procedure function type
 * Called when guard is satisfied, with exclusive access
 */
typedef void (*tcelm_entry_proc_t)(void *data, void *arg);

/*
 * Protected function type (read-only access)
 */
typedef void *(*tcelm_protected_func_t)(void *data, void *arg);

/*
 * Protected procedure type (write access)
 */
typedef void (*tcelm_protected_proc_t)(void *data, void *arg);

/*
 * Entry definition
 */
typedef struct tcelm_entry {
    const char *name;               /* Entry name for debugging */
    tcelm_entry_guard_t guard;      /* Guard condition (NULL = always open) */
    tcelm_entry_proc_t proc;        /* Entry procedure */
    bool active;                    /* Is this entry slot used? */
} tcelm_entry_t;

/*
 * Protected object handle
 */
typedef struct tcelm_protected {
    rtems_id mutex_id;              /* Mutex for exclusive access */
    void *native_data;              /* Native implementation data */
    void *data;                     /* User data pointer */
    size_t data_size;               /* Size of user data */
    tcelm_entry_t entries[TCELM_PROTECTED_MAX_ENTRIES];
    uint32_t entry_count;           /* Number of registered entries */
    uint32_t reader_count;          /* Current number of readers */
    bool writer_waiting;            /* Writer is waiting */
    const char *name;               /* Name for debugging */
} tcelm_protected_t;

/*
 * Protected object configuration
 */
typedef struct tcelm_protected_config {
    void *initial_data;             /* Initial data (copied into protected object) */
    size_t data_size;               /* Size of data */
    const char *name;               /* Optional name */
} tcelm_protected_config_t;

/*
 * Initialize protected subsystem
 */
int tcelm_protected_init(void);

/*
 * Shutdown protected subsystem
 */
void tcelm_protected_shutdown(void);

/*
 * Create a new protected object
 * Returns handle or NULL on failure
 */
tcelm_protected_t *tcelm_protected_create(
    tcelm_arena_t *arena,
    const tcelm_protected_config_t *config
);

/*
 * Create protected object with data pointer (convenience)
 * Note: Data is copied, caller can free original
 */
tcelm_protected_t *tcelm_protected_create_with_data(
    tcelm_arena_t *arena,
    void *data,
    size_t data_size
);

/*
 * ============================================================================
 * PROCEDURES (Exclusive write access)
 * ============================================================================
 */

/*
 * Call a procedure with exclusive access
 * Automatically locks/unlocks the protected object
 */
void tcelm_protected_call_proc(
    tcelm_protected_t *prot,
    tcelm_protected_proc_t proc,
    void *arg
);

/*
 * ============================================================================
 * FUNCTIONS (Shared read access)
 * ============================================================================
 */

/*
 * Call a function with shared read access
 * Multiple readers can execute concurrently
 * Returns the function's return value
 */
void *tcelm_protected_call_func(
    tcelm_protected_t *prot,
    tcelm_protected_func_t func,
    void *arg
);

/*
 * ============================================================================
 * ENTRIES (Guarded procedures with barriers)
 * ============================================================================
 */

/*
 * Register an entry with a guard condition
 * Returns entry index (0 to MAX_ENTRIES-1) or -1 on failure
 */
int tcelm_protected_add_entry(
    tcelm_protected_t *prot,
    const char *name,
    tcelm_entry_guard_t guard,
    tcelm_entry_proc_t proc
);

/*
 * Call an entry by index
 * Blocks until guard condition is true, then executes procedure
 * Returns 0 on success, -1 on error
 */
int tcelm_protected_call_entry(
    tcelm_protected_t *prot,
    int entry_index,
    void *arg
);

/*
 * Call an entry by name
 * Blocks until guard condition is true, then executes procedure
 * Returns 0 on success, -1 on error
 */
int tcelm_protected_call_entry_by_name(
    tcelm_protected_t *prot,
    const char *name,
    void *arg
);

/*
 * Call an entry with timeout (milliseconds)
 * Returns 0 on success, -1 on timeout or error
 */
int tcelm_protected_call_entry_timeout(
    tcelm_protected_t *prot,
    int entry_index,
    void *arg,
    uint32_t timeout_ms
);

/*
 * Try to call an entry without blocking
 * Returns 0 if executed, -1 if guard was false
 */
int tcelm_protected_try_call_entry(
    tcelm_protected_t *prot,
    int entry_index,
    void *arg
);

/*
 * ============================================================================
 * DATA ACCESS (Raw access for special cases)
 * ============================================================================
 */

/*
 * Lock for exclusive write access
 * Must be paired with unlock_write
 */
void tcelm_protected_lock_write(tcelm_protected_t *prot);

/*
 * Unlock after write access
 */
void tcelm_protected_unlock_write(tcelm_protected_t *prot);

/*
 * Lock for shared read access
 * Must be paired with unlock_read
 */
void tcelm_protected_lock_read(tcelm_protected_t *prot);

/*
 * Unlock after read access
 */
void tcelm_protected_unlock_read(tcelm_protected_t *prot);

/*
 * Get data pointer (must hold lock)
 */
void *tcelm_protected_get_data(tcelm_protected_t *prot);

/*
 * Signal that state has changed (re-evaluate entry guards)
 * Call after modifying data that affects guards
 */
void tcelm_protected_signal(tcelm_protected_t *prot);

/*
 * Delete a protected object
 */
void tcelm_protected_delete(tcelm_protected_t *prot);

/*
 * ============================================================================
 * CONVENIENCE MACROS
 * ============================================================================
 */

/* Define a protected procedure inline */
#define TCELM_PROTECTED_PROC(prot, code) do { \
    tcelm_protected_lock_write(prot); \
    { code } \
    tcelm_protected_unlock_write(prot); \
} while(0)

/* Define a protected function inline */
#define TCELM_PROTECTED_FUNC(prot, result_var, code) do { \
    tcelm_protected_lock_read(prot); \
    { result_var = (code); } \
    tcelm_protected_unlock_read(prot); \
} while(0)

#endif /* TCELM_PROTECTED_H */
