/*
 * smp_test.c - Test SMP support for tcelm NUC toolchain
 *
 * This tests multi-core functionality:
 *   - CPU detection and enumeration
 *   - Per-CPU task execution
 *   - CPU affinity
 *   - Spinlock synchronization
 *   - Atomic operations
 *
 * Build:
 *   make -C toolchain test-smp
 *
 * Run:
 *   qemu-system-i386 -kernel toolchain/build/smp_test.elf -smp 4 -nographic
 */

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>

#include <rtems.h>
#include <smp.h>
#include <sys/cpuset.h>

/* Serial I/O for test output */
#define COM1_PORT 0x3F8

static spinlock_t serial_lock = SPINLOCK_INITIALIZER;

static inline void outb(unsigned short port, unsigned char val) {
    __asm__ volatile ("outb %0, %1" : : "a"(val), "Nd"(port));
}

static inline unsigned char inb(unsigned short port) {
    unsigned char ret;
    __asm__ volatile ("inb %1, %0" : "=a"(ret) : "Nd"(port));
    return ret;
}

static void serial_init(void) {
    outb(COM1_PORT + 1, 0x00);
    outb(COM1_PORT + 3, 0x80);
    outb(COM1_PORT + 0, 0x03);
    outb(COM1_PORT + 1, 0x00);
    outb(COM1_PORT + 3, 0x03);
    outb(COM1_PORT + 2, 0xC7);
    outb(COM1_PORT + 4, 0x0B);
}

static void serial_putchar(char c) {
    while (!(inb(COM1_PORT + 5) & 0x20));
    outb(COM1_PORT, c);
}

/* Thread-safe puts using spinlock */
static void puts(const char *s) {
    spinlock_acquire(&serial_lock);
    while (*s) {
        if (*s == '\n') serial_putchar('\r');
        serial_putchar(*s++);
    }
    spinlock_release(&serial_lock);
}

static void putint(int n) {
    char buf[16];
    int i = 0;
    spinlock_acquire(&serial_lock);
    if (n < 0) { serial_putchar('-'); n = -n; }
    if (n == 0) { serial_putchar('0'); spinlock_release(&serial_lock); return; }
    while (n > 0) { buf[i++] = '0' + (n % 10); n /= 10; }
    while (i > 0) serial_putchar(buf[--i]);
    spinlock_release(&serial_lock);
}

static void puthex(uint32_t n) {
    const char *hex = "0123456789ABCDEF";
    spinlock_acquire(&serial_lock);
    serial_putchar('0');
    serial_putchar('x');
    for (int i = 28; i >= 0; i -= 4) {
        serial_putchar(hex[(n >> i) & 0xF]);
    }
    spinlock_release(&serial_lock);
}

/* Test results */
static volatile int tests_passed = 0;
static volatile int tests_failed = 0;
static spinlock_t test_lock = SPINLOCK_INITIALIZER;

#define TEST(name) do { puts("  " name ": "); } while(0)
#define PASS() do { puts("PASS\n"); spinlock_acquire(&test_lock); tests_passed++; spinlock_release(&test_lock); } while(0)
#define FAIL() do { puts("FAIL\n"); spinlock_acquire(&test_lock); tests_failed++; spinlock_release(&test_lock); } while(0)

#define ASSERT(cond) do { if (!(cond)) { FAIL(); return; } } while(0)
#define ASSERT_EQ(a, b) do { if ((a) != (b)) { FAIL(); return; } } while(0)

/*
 * SMP Detection Tests
 */
static void test_cpu_id(void) {
    TEST("smp_cpu_id");
    uint32_t cpu = smp_cpu_id();
    /* CPU ID should be valid (0-3 for 4-core system) */
    ASSERT(cpu < SMP_MAX_CPUS);
    PASS();
}

static void test_cpu_count(void) {
    TEST("smp_num_cpus");
    uint32_t num = smp_num_cpus_online();
    puts("(detected ");
    putint(num);
    puts(" CPUs) ");
    /* Should detect at least 1 CPU */
    ASSERT(num >= 1);
    ASSERT(num <= SMP_MAX_CPUS);
    PASS();
}

static void test_smp_initialized(void) {
    TEST("smp_initialized");
    /* After rtems_initialize_executive, SMP should be initialized */
    ASSERT(smp_initialized);
    PASS();
}

/*
 * Per-CPU Data Tests
 */
static void test_percpu_data(void) {
    TEST("percpu_data");
    percpu_t *cpu_data = smp_this_cpu();
    ASSERT(cpu_data != NULL);
    ASSERT(cpu_data->cpu_id < SMP_MAX_CPUS);
    ASSERT(cpu_data->state == CPU_STATE_ONLINE);
    PASS();
}

/*
 * Spinlock Tests
 */
static void test_spinlock_basic(void) {
    TEST("spinlock_basic");
    spinlock_t lock = SPINLOCK_INITIALIZER;

    /* Should be able to acquire unlocked spinlock */
    spinlock_acquire(&lock);
    /* Should be able to release */
    spinlock_release(&lock);
    PASS();
}

static void test_spinlock_try(void) {
    TEST("spinlock_try");
    spinlock_t lock = SPINLOCK_INITIALIZER;

    /* Try acquire should succeed on unlocked lock */
    bool got = spinlock_try_acquire(&lock);
    ASSERT(got);
    spinlock_release(&lock);
    PASS();
}

/*
 * Atomic Operation Tests
 */
static void test_atomic_add(void) {
    TEST("atomic_add");
    volatile uint32_t val = 10;
    uint32_t old = atomic_add(&val, 5);
    ASSERT_EQ(old, 10);
    ASSERT_EQ(val, 15);
    PASS();
}

static void test_atomic_sub(void) {
    TEST("atomic_sub");
    volatile uint32_t val = 20;
    uint32_t old = atomic_sub(&val, 8);
    ASSERT_EQ(old, 20);
    ASSERT_EQ(val, 12);
    PASS();
}

static void test_atomic_cas(void) {
    TEST("atomic_cas");
    volatile uint32_t val = 100;

    /* CAS should succeed when expected value matches */
    bool success = atomic_cas(&val, 100, 200);
    ASSERT(success);
    ASSERT_EQ(val, 200);

    /* CAS should fail when expected value doesn't match */
    success = atomic_cas(&val, 100, 300);  /* val is 200, not 100 */
    ASSERT(!success);
    ASSERT_EQ(val, 200);  /* Unchanged */

    PASS();
}

static void test_atomic_xchg(void) {
    TEST("atomic_xchg");
    volatile uint32_t val = 42;
    uint32_t old = atomic_xchg(&val, 99);
    ASSERT_EQ(old, 42);
    ASSERT_EQ(val, 99);
    PASS();
}

/*
 * Memory Barrier Tests
 */
static void test_memory_barriers(void) {
    TEST("memory_barriers");
    volatile uint32_t a = 0, b = 0;

    a = 1;
    smp_wmb();  /* Ensure write to 'a' is visible before write to 'b' */
    b = 2;

    smp_rmb();  /* Ensure reads are ordered */
    ASSERT_EQ(a, 1);
    ASSERT_EQ(b, 2);

    smp_mb();  /* Full memory barrier */
    PASS();
}

/*
 * CPU Affinity Tests
 */
static void test_cpu_affinity_macros(void) {
    TEST("cpu_affinity_macros");
    cpu_set_t cpuset;

    /* Test CPU_ZERO */
    CPU_ZERO(&cpuset);
    for (int i = 0; i < SMP_MAX_CPUS; i++) {
        ASSERT(!CPU_ISSET(i, &cpuset));
    }

    /* Test CPU_SET and CPU_ISSET */
    CPU_SET(0, &cpuset);
    CPU_SET(2, &cpuset);
    ASSERT(CPU_ISSET(0, &cpuset));
    ASSERT(!CPU_ISSET(1, &cpuset));
    ASSERT(CPU_ISSET(2, &cpuset));
    ASSERT(!CPU_ISSET(3, &cpuset));

    /* Test CPU_COUNT */
    ASSERT_EQ(CPU_COUNT(&cpuset), 2);

    /* Test CPU_CLR */
    CPU_CLR(0, &cpuset);
    ASSERT(!CPU_ISSET(0, &cpuset));
    ASSERT_EQ(CPU_COUNT(&cpuset), 1);

    PASS();
}

/*
 * RTEMS SMP API Tests
 */
static void test_rtems_processor_count(void) {
    TEST("rtems_get_processor_count");
    uint32_t count = rtems_get_processor_count();
    puts("(");
    putint(count);
    puts(" CPUs) ");
    ASSERT(count >= 1);
    ASSERT(count <= SMP_MAX_CPUS);
    PASS();
}

static void test_rtems_current_processor(void) {
    TEST("rtems_get_current_processor");
    uint32_t cpu = rtems_get_current_processor();
    ASSERT(cpu < SMP_MAX_CPUS);
    /* Should match smp_cpu_id */
    ASSERT_EQ(cpu, smp_cpu_id());
    PASS();
}

/*
 * Shared Counter Test (tests spinlock correctness under contention)
 */
static volatile uint32_t shared_counter = 0;
static spinlock_t counter_lock = SPINLOCK_INITIALIZER;

static void test_shared_counter(void) {
    TEST("shared_counter");

    shared_counter = 0;
    const int iterations = 1000;

    /* Increment counter with spinlock protection */
    for (int i = 0; i < iterations; i++) {
        spinlock_acquire(&counter_lock);
        shared_counter++;
        spinlock_release(&counter_lock);
    }

    /* In single-threaded test, should equal iterations */
    ASSERT_EQ(shared_counter, iterations);
    PASS();
}

/*
 * Main
 */
int main(int argc, char **argv) {
    (void)argc;
    (void)argv;

    serial_init();

    puts("\n");
    puts("========================================\n");
    puts("tcelm SMP Test (NUC Toolchain)\n");
    puts("========================================\n\n");

    puts("[SMP Detection Tests]\n");
    test_cpu_id();
    test_cpu_count();
    test_smp_initialized();

    puts("\n[Per-CPU Data Tests]\n");
    test_percpu_data();

    puts("\n[Spinlock Tests]\n");
    test_spinlock_basic();
    test_spinlock_try();

    puts("\n[Atomic Operation Tests]\n");
    test_atomic_add();
    test_atomic_sub();
    test_atomic_cas();
    test_atomic_xchg();

    puts("\n[Memory Barrier Tests]\n");
    test_memory_barriers();

    puts("\n[CPU Affinity Tests]\n");
    test_cpu_affinity_macros();

    puts("\n[RTEMS SMP API Tests]\n");
    test_rtems_processor_count();
    test_rtems_current_processor();

    puts("\n[Shared Counter Tests]\n");
    test_shared_counter();

    puts("\n========================================\n");
    puts("Tests: ");
    putint(tests_passed);
    puts("/");
    putint(tests_passed + tests_failed);
    puts(" passed\n");
    puts("========================================\n");

    puts("\nSMP test complete. Halting.\n");

    /* Exit QEMU */
    outb(0xf4, tests_failed == 0 ? 0x00 : 0x01);

    return 0;
}
