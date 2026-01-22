/*
 * smp.h - SMP (Symmetric Multiprocessing) support for tcelm/NUC toolchain
 *
 * Provides multi-core support for Intel NUC (up to 4 cores).
 */

#ifndef _SMP_H
#define _SMP_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/* Maximum number of CPUs supported */
#define SMP_MAX_CPUS 4

/* CPU states */
#define CPU_STATE_OFFLINE   0
#define CPU_STATE_STARTING  1
#define CPU_STATE_ONLINE    2
#define CPU_STATE_HALTED    3

/*
 * Spinlock - simple test-and-set lock for SMP synchronization
 * (Using test-and-set instead of ticket lock for TCC compatibility)
 */
typedef struct {
    volatile uint32_t locked;
} spinlock_t;

#define SPINLOCK_INITIALIZER { 0 }

/* Initialize a spinlock */
static inline void spinlock_init(spinlock_t *lock) {
    lock->locked = 0;
}

/* Atomic test-and-set using xchg instruction */
static inline uint32_t _atomic_xchg(volatile uint32_t *ptr, uint32_t val) {
    uint32_t result;
    __asm__ volatile (
        "xchgl %0, %1"
        : "=r"(result), "+m"(*ptr)
        : "0"(val)
        : "memory"
    );
    return result;
}

/* Acquire spinlock (busy-wait) */
static inline void spinlock_acquire(spinlock_t *lock) {
    while (_atomic_xchg(&lock->locked, 1) != 0) {
        /* Spin until we get the lock */
        while (lock->locked) {
            __asm__ volatile ("pause" ::: "memory");
        }
    }
}

/* Release spinlock */
static inline void spinlock_release(spinlock_t *lock) {
    __asm__ volatile ("" ::: "memory");  /* Compiler barrier */
    lock->locked = 0;
}

/* Try to acquire spinlock (non-blocking) */
static inline bool spinlock_try_acquire(spinlock_t *lock) {
    return _atomic_xchg(&lock->locked, 1) == 0;
}

/*
 * Per-CPU data structure
 */
typedef struct {
    uint32_t cpu_id;              /* CPU index (0-3) */
    uint32_t apic_id;             /* Local APIC ID */
    volatile uint32_t state;      /* CPU state */
    void *stack;                  /* CPU's stack pointer */
    void *idle_stack;             /* Idle task stack */
    uint32_t current_task;        /* Currently running task ID */
    volatile uint64_t ticks;      /* Per-CPU tick counter */
} percpu_t;

/*
 * SMP Global State
 */
extern percpu_t smp_percpu[SMP_MAX_CPUS];
extern volatile uint32_t smp_num_cpus;
extern volatile bool smp_initialized;

/*
 * SMP Functions
 */

/* Initialize SMP (called by BSP) */
void smp_init(void);

/* Get current CPU ID */
static inline uint32_t smp_cpu_id(void) {
    /* In a full SMP implementation, this would read the APIC ID.
     * For now, since we only run on BSP, return 0.
     * When APs are actually started, this should use CPUID or LAPIC ID. */
    return 0;
}

/* Get per-CPU data for current CPU */
static inline percpu_t *smp_this_cpu(void) {
    return &smp_percpu[smp_cpu_id()];
}

/* Get number of online CPUs */
static inline uint32_t smp_num_cpus_online(void) {
    return smp_num_cpus;
}

/* Check if SMP is active */
static inline bool smp_is_active(void) {
    return smp_initialized && smp_num_cpus > 1;
}

/* Send inter-processor interrupt */
void smp_send_ipi(uint32_t cpu_id, uint32_t vector);

/* Broadcast IPI to all other CPUs */
void smp_broadcast_ipi(uint32_t vector);

/* Memory barriers - use lock prefix for compatibility with TCC */
static inline void smp_mb(void) {
    /* Full memory barrier using lock prefix */
    volatile int dummy = 0;
    __asm__ volatile ("lock; addl $0, %0" : "+m"(dummy) :: "memory");
}

/* Read barrier */
static inline void smp_rmb(void) {
    /* Compiler barrier + serializing instruction */
    __asm__ volatile ("" ::: "memory");
}

/* Write barrier */
static inline void smp_wmb(void) {
    /* Compiler barrier is sufficient on x86 for store ordering */
    __asm__ volatile ("" ::: "memory");
}

/*
 * Atomic operations (TCC-compatible using inline assembly)
 */

static inline uint32_t atomic_read(volatile uint32_t *ptr) {
    return *ptr;
}

static inline void atomic_write(volatile uint32_t *ptr, uint32_t val) {
    *ptr = val;
    smp_wmb();
}

static inline uint32_t atomic_add(volatile uint32_t *ptr, uint32_t val) {
    uint32_t result;
    __asm__ volatile (
        "lock; xaddl %0, %1"
        : "=r"(result), "+m"(*ptr)
        : "0"(val)
        : "memory"
    );
    return result;
}

static inline uint32_t atomic_sub(volatile uint32_t *ptr, uint32_t val) {
    return atomic_add(ptr, (uint32_t)(-(int32_t)val));
}

static inline bool atomic_cas(volatile uint32_t *ptr, uint32_t old_val, uint32_t new_val) {
    uint32_t prev;
    __asm__ volatile (
        "lock; cmpxchgl %2, %1"
        : "=a"(prev), "+m"(*ptr)
        : "r"(new_val), "0"(old_val)
        : "memory"
    );
    return prev == old_val;
}

static inline uint32_t atomic_xchg(volatile uint32_t *ptr, uint32_t val) {
    return _atomic_xchg(ptr, val);
}

#endif /* _SMP_H */
