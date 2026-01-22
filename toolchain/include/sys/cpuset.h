/*
 * sys/cpuset.h - CPU set types for tcelm/NUC toolchain
 *
 * Provides cpu_set_t and related macros for CPU affinity control.
 * This is a simplified implementation for single-core systems.
 */

#ifndef _SYS_CPUSET_H
#define _SYS_CPUSET_H

#include <stdint.h>
#include <stddef.h>

/* Maximum number of CPUs supported */
#define CPU_SETSIZE 32

/* CPU set type - bitmask of CPUs */
typedef struct {
    uint32_t __bits[CPU_SETSIZE / 32];
} cpu_set_t;

/* Clear all CPUs from set */
#define CPU_ZERO(set) \
    do { \
        for (size_t __i = 0; __i < sizeof((set)->__bits)/sizeof((set)->__bits[0]); __i++) \
            (set)->__bits[__i] = 0; \
    } while (0)

/* Add CPU to set */
#define CPU_SET(cpu, set) \
    ((set)->__bits[(cpu) / 32] |= (1U << ((cpu) % 32)))

/* Remove CPU from set */
#define CPU_CLR(cpu, set) \
    ((set)->__bits[(cpu) / 32] &= ~(1U << ((cpu) % 32)))

/* Test if CPU is in set */
#define CPU_ISSET(cpu, set) \
    (((set)->__bits[(cpu) / 32] & (1U << ((cpu) % 32))) != 0)

/* Count CPUs in set */
static inline int CPU_COUNT(const cpu_set_t *set) {
    int count = 0;
    for (size_t i = 0; i < sizeof(set->__bits)/sizeof(set->__bits[0]); i++) {
        uint32_t bits = set->__bits[i];
        while (bits) {
            count += bits & 1;
            bits >>= 1;
        }
    }
    return count;
}

/* Compare two CPU sets */
#define CPU_EQUAL(set1, set2) \
    (__cpu_set_equal((set1), (set2)))

static inline int __cpu_set_equal(const cpu_set_t *set1, const cpu_set_t *set2) {
    for (size_t i = 0; i < sizeof(set1->__bits)/sizeof(set1->__bits[0]); i++) {
        if (set1->__bits[i] != set2->__bits[i]) return 0;
    }
    return 1;
}

/* Logical operations */
#define CPU_AND(dest, set1, set2) \
    do { \
        for (size_t __i = 0; __i < sizeof((dest)->__bits)/sizeof((dest)->__bits[0]); __i++) \
            (dest)->__bits[__i] = (set1)->__bits[__i] & (set2)->__bits[__i]; \
    } while (0)

#define CPU_OR(dest, set1, set2) \
    do { \
        for (size_t __i = 0; __i < sizeof((dest)->__bits)/sizeof((dest)->__bits[0]); __i++) \
            (dest)->__bits[__i] = (set1)->__bits[__i] | (set2)->__bits[__i]; \
    } while (0)

#define CPU_XOR(dest, set1, set2) \
    do { \
        for (size_t __i = 0; __i < sizeof((dest)->__bits)/sizeof((dest)->__bits[0]); __i++) \
            (dest)->__bits[__i] = (set1)->__bits[__i] ^ (set2)->__bits[__i]; \
    } while (0)

/* Allocate/free dynamic CPU sets (not needed for static sets) */
#define CPU_ALLOC_SIZE(count) (sizeof(cpu_set_t))
#define CPU_ALLOC(count)      ((cpu_set_t *)malloc(sizeof(cpu_set_t)))
#define CPU_FREE(set)         free(set)

#endif /* _SYS_CPUSET_H */
