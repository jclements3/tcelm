/*
 * tcelm_atomic.h - Portable atomic operations
 *
 * Uses GCC builtins on GCC/Clang, C11 atomics on TCC
 */

#ifndef TCELM_ATOMIC_H
#define TCELM_ATOMIC_H

#include <stdint.h>

#if defined(__TINYC__)
/* TCC: Use C11 atomics */
#include <stdatomic.h>

static inline uint32_t tcelm_atomic_fetch_add_u32(uint32_t *ptr, uint32_t val) {
    return atomic_fetch_add((_Atomic uint32_t *)ptr, val);
}

static inline uint32_t tcelm_atomic_fetch_sub_u32(uint32_t *ptr, uint32_t val) {
    return atomic_fetch_sub((_Atomic uint32_t *)ptr, val);
}

#else
/* GCC/Clang: Use builtins */

static inline uint32_t tcelm_atomic_fetch_add_u32(uint32_t *ptr, uint32_t val) {
    return __sync_fetch_and_add(ptr, val);
}

static inline uint32_t tcelm_atomic_fetch_sub_u32(uint32_t *ptr, uint32_t val) {
    return __sync_fetch_and_sub(ptr, val);
}

#endif

#endif /* TCELM_ATOMIC_H */
