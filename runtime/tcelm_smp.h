/*
 * tcelm_smp.h - SMP support for RTEMS
 *
 * Provides CPU affinity control for multi-core systems.
 */

#ifndef TCELM_SMP_H
#define TCELM_SMP_H

#include "tcelm_types.h"
#include <stdint.h>
#include <stdbool.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/score/smpimpl.h>
#endif

/*
 * Processor set (bitmask)
 */
typedef uint32_t tcelm_cpu_set_t;

/*
 * Processor state
 */
typedef enum {
    TCELM_CPU_OFFLINE,
    TCELM_CPU_ONLINE,
    TCELM_CPU_STARTING
} tcelm_cpu_state_t;

/*
 * Constants
 */
#define TCELM_CPU_SET_ALL       0xFFFFFFFFu
#define TCELM_CPU_SET_NONE      0x00000000u
#define TCELM_CPU_SET_CPU(n)    (1u << (n))

/*
 * Initialize SMP subsystem
 */
int tcelm_smp_init(void);

/*
 * Get number of processors in the system
 * Returns 1 on uniprocessor systems
 */
uint32_t tcelm_smp_processor_count(void);

/*
 * Get current processor index (0-based)
 */
uint32_t tcelm_smp_current_processor(void);

/*
 * Check if SMP is enabled
 */
bool tcelm_smp_is_enabled(void);

/*
 * Get processor state
 */
tcelm_cpu_state_t tcelm_smp_get_processor_state(uint32_t cpu_index);

/*
 * Get task affinity
 * task_id: Task ID (0 = self)
 * Returns CPU set bitmask
 */
tcelm_cpu_set_t tcelm_smp_get_affinity(uint32_t task_id);

/*
 * Set task affinity
 * task_id: Task ID (0 = self)
 * cpu_set: Bitmask of allowed CPUs
 * Returns 0 on success
 */
int tcelm_smp_set_affinity(uint32_t task_id, tcelm_cpu_set_t cpu_set);

/*
 * Get default affinity for new tasks
 */
tcelm_cpu_set_t tcelm_smp_get_affinity_default(void);

/*
 * CPU set operations
 */
static inline tcelm_cpu_set_t tcelm_cpu_set_single(uint32_t cpu) {
    return (cpu < 32) ? (1u << cpu) : 0;
}

static inline tcelm_cpu_set_t tcelm_cpu_set_range(uint32_t start, uint32_t end) {
    if (start >= 32 || end >= 32 || start > end) return 0;
    return ((1u << (end - start + 1)) - 1) << start;
}

static inline bool tcelm_cpu_set_contains(tcelm_cpu_set_t set, uint32_t cpu) {
    return (cpu < 32) && ((set & (1u << cpu)) != 0);
}

static inline uint32_t tcelm_cpu_set_count(tcelm_cpu_set_t set) {
    /* Population count */
    uint32_t count = 0;
    while (set) {
        count += set & 1;
        set >>= 1;
    }
    return count;
}

#endif /* TCELM_SMP_H */
