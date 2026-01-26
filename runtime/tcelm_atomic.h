/*
 * tcelm_atomic.h - Portable atomic operations
 *
 * Uses GCC builtins on GCC/Clang, C11 atomics on TCC
 * Provides atomic integers and booleans for lock-free programming.
 */

#ifndef TCELM_ATOMIC_H
#define TCELM_ATOMIC_H

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include "tcelm_types.h"

/*
 * Atomic integer type
 */
typedef struct tcelm_atomic_int {
    volatile int32_t value;
} tcelm_atomic_int_t;

/*
 * Atomic boolean type
 */
typedef struct tcelm_atomic_bool {
    volatile int32_t value;  /* Use int for better atomicity */
} tcelm_atomic_bool_t;

/*
 * Memory ordering
 */
#if defined(__TINYC__)
/* TCC: Use C11 atomics */
#include <stdatomic.h>

#define TCELM_MEMORY_BARRIER() atomic_thread_fence(memory_order_seq_cst)

static inline int32_t tcelm_atomic_load_i32(volatile int32_t *ptr) {
    return atomic_load((_Atomic int32_t *)ptr);
}

static inline void tcelm_atomic_store_i32(volatile int32_t *ptr, int32_t val) {
    atomic_store((_Atomic int32_t *)ptr, val);
}

static inline int32_t tcelm_atomic_fetch_add_i32(volatile int32_t *ptr, int32_t val) {
    return atomic_fetch_add((_Atomic int32_t *)ptr, val);
}

static inline int32_t tcelm_atomic_fetch_sub_i32(volatile int32_t *ptr, int32_t val) {
    return atomic_fetch_sub((_Atomic int32_t *)ptr, val);
}

static inline int32_t tcelm_atomic_exchange_i32(volatile int32_t *ptr, int32_t val) {
    return atomic_exchange((_Atomic int32_t *)ptr, val);
}

static inline bool tcelm_atomic_compare_exchange_i32(volatile int32_t *ptr, int32_t expected, int32_t desired) {
    return atomic_compare_exchange_strong((_Atomic int32_t *)ptr, &expected, desired);
}

static inline uint32_t tcelm_atomic_fetch_add_u32(uint32_t *ptr, uint32_t val) {
    return atomic_fetch_add((_Atomic uint32_t *)ptr, val);
}

static inline uint32_t tcelm_atomic_fetch_sub_u32(uint32_t *ptr, uint32_t val) {
    return atomic_fetch_sub((_Atomic uint32_t *)ptr, val);
}

#else
/* GCC/Clang: Use builtins */

#define TCELM_MEMORY_BARRIER() __sync_synchronize()

static inline int32_t tcelm_atomic_load_i32(volatile int32_t *ptr) {
    __sync_synchronize();
    int32_t val = *ptr;
    __sync_synchronize();
    return val;
}

static inline void tcelm_atomic_store_i32(volatile int32_t *ptr, int32_t val) {
    __sync_synchronize();
    *ptr = val;
    __sync_synchronize();
}

static inline int32_t tcelm_atomic_fetch_add_i32(volatile int32_t *ptr, int32_t val) {
    return __sync_fetch_and_add(ptr, val);
}

static inline int32_t tcelm_atomic_fetch_sub_i32(volatile int32_t *ptr, int32_t val) {
    return __sync_fetch_and_sub(ptr, val);
}

static inline int32_t tcelm_atomic_exchange_i32(volatile int32_t *ptr, int32_t val) {
    return __sync_lock_test_and_set(ptr, val);
}

static inline bool tcelm_atomic_compare_exchange_i32(volatile int32_t *ptr, int32_t expected, int32_t desired) {
    return __sync_bool_compare_and_swap(ptr, expected, desired);
}

static inline uint32_t tcelm_atomic_fetch_add_u32(uint32_t *ptr, uint32_t val) {
    return __sync_fetch_and_add(ptr, val);
}

static inline uint32_t tcelm_atomic_fetch_sub_u32(uint32_t *ptr, uint32_t val) {
    return __sync_fetch_and_sub(ptr, val);
}

#endif

/*
 * High-level atomic operations for Elm FFI
 */

/* Create a new atomic int */
static inline tcelm_atomic_int_t *tcelm_atomic_int_new(int32_t initial) {
    tcelm_atomic_int_t *ai = (tcelm_atomic_int_t *)malloc(sizeof(tcelm_atomic_int_t));
    if (ai) {
        ai->value = initial;
    }
    return ai;
}

/* Load atomic int */
static inline int32_t tcelm_atomic_int_load(tcelm_atomic_int_t *ai) {
    return tcelm_atomic_load_i32(&ai->value);
}

/* Store atomic int */
static inline void tcelm_atomic_int_store(tcelm_atomic_int_t *ai, int32_t val) {
    tcelm_atomic_store_i32(&ai->value, val);
}

/* Add to atomic int, return old value */
static inline int32_t tcelm_atomic_int_add(tcelm_atomic_int_t *ai, int32_t delta) {
    return tcelm_atomic_fetch_add_i32(&ai->value, delta);
}

/* Subtract from atomic int, return old value */
static inline int32_t tcelm_atomic_int_sub(tcelm_atomic_int_t *ai, int32_t delta) {
    return tcelm_atomic_fetch_sub_i32(&ai->value, delta);
}

/* Exchange atomic int, return old value */
static inline int32_t tcelm_atomic_int_exchange(tcelm_atomic_int_t *ai, int32_t newval) {
    return tcelm_atomic_exchange_i32(&ai->value, newval);
}

/* Compare and exchange atomic int */
static inline bool tcelm_atomic_int_compare_exchange(tcelm_atomic_int_t *ai, int32_t expected, int32_t desired) {
    return tcelm_atomic_compare_exchange_i32(&ai->value, expected, desired);
}

/* Create a new atomic bool */
static inline tcelm_atomic_bool_t *tcelm_atomic_bool_new(bool initial) {
    tcelm_atomic_bool_t *ab = (tcelm_atomic_bool_t *)malloc(sizeof(tcelm_atomic_bool_t));
    if (ab) {
        ab->value = initial ? 1 : 0;
    }
    return ab;
}

/* Load atomic bool */
static inline bool tcelm_atomic_bool_load(tcelm_atomic_bool_t *ab) {
    return tcelm_atomic_load_i32(&ab->value) != 0;
}

/* Store atomic bool */
static inline void tcelm_atomic_bool_store(tcelm_atomic_bool_t *ab, bool val) {
    tcelm_atomic_store_i32(&ab->value, val ? 1 : 0);
}

/* Exchange atomic bool, return old value */
static inline bool tcelm_atomic_bool_exchange(tcelm_atomic_bool_t *ab, bool newval) {
    return tcelm_atomic_exchange_i32(&ab->value, newval ? 1 : 0) != 0;
}

/* Compare and exchange atomic bool */
static inline bool tcelm_atomic_bool_compare_exchange(tcelm_atomic_bool_t *ab, bool expected, bool desired) {
    return tcelm_atomic_compare_exchange_i32(&ab->value, expected ? 1 : 0, desired ? 1 : 0);
}

/* Memory fence */
static inline void tcelm_atomic_fence(void) {
    TCELM_MEMORY_BARRIER();
}

#endif /* TCELM_ATOMIC_H */
