/*
 * tcelm_runtime_test.c - Test tcelm runtime with NUC toolchain
 *
 * This tests the full tcelm runtime compiled with TCC for NUC.
 *
 * Build:
 *   make -C toolchain test-runtime
 *
 * Run:
 *   qemu-system-i386 -kernel toolchain/build/tcelm_runtime_test.elf -nographic
 */

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>

/* Include tcelm runtime headers */
#include "tcelm_arena.h"
#include "tcelm_types.h"

/* Include RTEMS headers for new API tests */
#include <rtems.h>
#include <sys/cpuset.h>

/* Serial I/O for test output */
#define COM1_PORT 0x3F8

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

static void puts(const char *s) {
    while (*s) {
        if (*s == '\n') serial_putchar('\r');
        serial_putchar(*s++);
    }
}

static void putint(int n) {
    char buf[16];
    int i = 0;
    if (n < 0) { serial_putchar('-'); n = -n; }
    if (n == 0) { serial_putchar('0'); return; }
    while (n > 0) { buf[i++] = '0' + (n % 10); n /= 10; }
    while (i > 0) serial_putchar(buf[--i]);
}

/* Test results */
static int tests_passed = 0;
static int tests_failed = 0;

#define TEST(name) do { puts("  " name ": "); } while(0)
#define PASS() do { puts("PASS\n"); tests_passed++; } while(0)
#define FAIL() do { puts("FAIL\n"); tests_failed++; } while(0)

#define ASSERT(cond) do { if (!(cond)) { FAIL(); return; } } while(0)
#define ASSERT_EQ(a, b) do { if ((a) != (b)) { FAIL(); return; } } while(0)
#define ASSERT_STR_EQ(a, b) do { if (strcmp(a, b) != 0) { FAIL(); return; } } while(0)

/* Global arena */
static tcelm_arena_t *arena;

/*
 * Arena Tests
 */
static void test_arena_create(void) {
    TEST("arena_create");
    arena = tcelm_arena_create(64 * 1024);
    ASSERT(arena != NULL);
    PASS();
}

static void test_arena_alloc(void) {
    TEST("arena_alloc");
    void *p1 = tcelm_arena_alloc(arena, 100);
    void *p2 = tcelm_arena_alloc(arena, 200);
    ASSERT(p1 != NULL);
    ASSERT(p2 != NULL);
    ASSERT(p1 != p2);
    PASS();
}

static void test_arena_stats(void) {
    TEST("arena_stats");
    tcelm_arena_stats_t stats;
    tcelm_arena_get_stats(arena, &stats);
    ASSERT(stats.total_used > 0);
    ASSERT(stats.total_allocated > 0);
    PASS();
}

/*
 * Int Tests
 */
static void test_int_create(void) {
    TEST("int_create");
    tcelm_value_t *v = tcelm_int(arena, 42);
    ASSERT(v != NULL);
    ASSERT(TCELM_IS_INT(v));
    ASSERT_EQ(TCELM_AS_INT(v), 42);
    PASS();
}

static void test_int_negative(void) {
    TEST("int_negative");
    tcelm_value_t *v = tcelm_int(arena, -12345);
    ASSERT_EQ(TCELM_AS_INT(v), -12345);
    PASS();
}

static void test_int_large(void) {
    TEST("int_large");
    tcelm_value_t *v = tcelm_int(arena, 1000000000LL);
    ASSERT_EQ(TCELM_AS_INT(v), 1000000000LL);
    PASS();
}

/*
 * Float Tests
 */
static void test_float_create(void) {
    TEST("float_create");
    tcelm_value_t *v = tcelm_float(arena, 3.14159);
    ASSERT(v != NULL);
    ASSERT(TCELM_IS_FLOAT(v));
    /* Float comparison with tolerance */
    double diff = TCELM_AS_FLOAT(v) - 3.14159;
    if (diff < 0) diff = -diff;
    ASSERT(diff < 0.00001);
    PASS();
}

/*
 * Bool Tests
 */
static void test_bool_true(void) {
    TEST("bool_true");
    tcelm_value_t *v = tcelm_bool(arena, true);
    ASSERT(TCELM_IS_BOOL(v));
    ASSERT(TCELM_AS_BOOL(v) == true);
    PASS();
}

static void test_bool_false(void) {
    TEST("bool_false");
    tcelm_value_t *v = tcelm_bool(arena, false);
    ASSERT(TCELM_AS_BOOL(v) == false);
    PASS();
}

/*
 * Char Tests
 */
static void test_char_create(void) {
    TEST("char_create");
    tcelm_value_t *v = tcelm_char(arena, 'A');
    ASSERT(TCELM_IS_CHAR(v));
    ASSERT_EQ(TCELM_AS_CHAR(v), 'A');
    PASS();
}

/*
 * String Tests
 */
static void test_string_create(void) {
    TEST("string_create");
    tcelm_value_t *v = tcelm_string(arena, "hello");
    ASSERT(TCELM_IS_STRING(v));
    ASSERT_EQ(TCELM_AS_STRING(v)->length, 5);
    ASSERT_STR_EQ(TCELM_AS_STRING(v)->data, "hello");
    PASS();
}

static void test_string_empty(void) {
    TEST("string_empty");
    tcelm_value_t *v = tcelm_string(arena, "");
    ASSERT_EQ(TCELM_AS_STRING(v)->length, 0);
    PASS();
}

/*
 * Unit Tests
 */
static void test_unit(void) {
    TEST("unit");
    tcelm_value_t *v = tcelm_unit(arena);
    ASSERT(v != NULL);
    ASSERT(v->tag == TCELM_TAG_UNIT);
    ASSERT(v == TCELM_UNIT);
    PASS();
}

/*
 * List Tests
 */
static void test_list_nil(void) {
    TEST("list_nil");
    tcelm_value_t *v = tcelm_nil(arena);
    ASSERT(TCELM_IS_LIST(v));
    ASSERT(tcelm_is_nil(v));
    PASS();
}

static void test_list_cons(void) {
    TEST("list_cons");
    tcelm_value_t *list = tcelm_nil(arena);
    list = tcelm_cons(arena, tcelm_int(arena, 3), list);
    list = tcelm_cons(arena, tcelm_int(arena, 2), list);
    list = tcelm_cons(arena, tcelm_int(arena, 1), list);

    ASSERT(!tcelm_is_nil(list));
    tcelm_value_t *head = tcelm_list_head(list);
    ASSERT_EQ(TCELM_AS_INT(head), 1);
    PASS();
}

static void test_list_tail(void) {
    TEST("list_tail");
    tcelm_value_t *list = tcelm_nil(arena);
    list = tcelm_cons(arena, tcelm_int(arena, 2), list);
    list = tcelm_cons(arena, tcelm_int(arena, 1), list);

    tcelm_value_t *tail = tcelm_list_tail(list);
    ASSERT(!tcelm_is_nil(tail));
    ASSERT_EQ(TCELM_AS_INT(tcelm_list_head(tail)), 2);
    PASS();
}

/*
 * Tuple Tests
 */
static void test_tuple2(void) {
    TEST("tuple2");
    tcelm_value_t *t = tcelm_tuple2(arena,
        tcelm_int(arena, 10),
        tcelm_string(arena, "hello"));
    ASSERT(TCELM_IS_TUPLE2(t));
    ASSERT_EQ(TCELM_AS_INT(tcelm_tuple2_first(t)), 10);
    ASSERT_STR_EQ(TCELM_AS_STRING(tcelm_tuple2_second(t))->data, "hello");
    PASS();
}

static void test_tuple3(void) {
    TEST("tuple3");
    tcelm_value_t *t = tcelm_tuple3(arena,
        tcelm_int(arena, 1),
        tcelm_int(arena, 2),
        tcelm_int(arena, 3));
    ASSERT(TCELM_IS_TUPLE3(t));
    ASSERT_EQ(TCELM_AS_INT(tcelm_tuple3_first(t)), 1);
    ASSERT_EQ(TCELM_AS_INT(tcelm_tuple3_second(t)), 2);
    ASSERT_EQ(TCELM_AS_INT(tcelm_tuple3_third(t)), 3);
    PASS();
}

/*
 * Record Tests
 */
static void test_record(void) {
    TEST("record");
    tcelm_value_t *r = tcelm_record(arena, 2,
        "x", tcelm_int(arena, 100),
        "y", tcelm_int(arena, 200));
    ASSERT(TCELM_IS_RECORD(r));

    tcelm_value_t *x = tcelm_record_get(r, "x");
    tcelm_value_t *y = tcelm_record_get(r, "y");
    ASSERT(x != NULL);
    ASSERT(y != NULL);
    ASSERT_EQ(TCELM_AS_INT(x), 100);
    ASSERT_EQ(TCELM_AS_INT(y), 200);
    PASS();
}

static void test_record_update(void) {
    TEST("record_update");
    tcelm_value_t *r1 = tcelm_record(arena, 2,
        "x", tcelm_int(arena, 10),
        "y", tcelm_int(arena, 20));

    tcelm_value_t *r2 = tcelm_record_update(arena, r1, "x", tcelm_int(arena, 99));

    /* Original unchanged */
    ASSERT_EQ(TCELM_AS_INT(tcelm_record_get(r1, "x")), 10);
    /* New record updated */
    ASSERT_EQ(TCELM_AS_INT(tcelm_record_get(r2, "x")), 99);
    ASSERT_EQ(TCELM_AS_INT(tcelm_record_get(r2, "y")), 20);
    PASS();
}

/*
 * Custom Type Tests (Maybe)
 */
static void test_custom_nothing(void) {
    TEST("Maybe_Nothing");
    tcelm_value_t *v = tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
    ASSERT(TCELM_IS_CUSTOM(v));
    ASSERT_EQ(tcelm_custom_ctor(v), TCELM_CTOR_NOTHING);
    PASS();
}

static void test_custom_just(void) {
    TEST("Maybe_Just");
    tcelm_value_t *v = tcelm_custom(arena, TCELM_CTOR_JUST, "Just", 1,
        tcelm_int(arena, 42));
    ASSERT_EQ(tcelm_custom_ctor(v), TCELM_CTOR_JUST);
    ASSERT_EQ(TCELM_AS_INT(tcelm_custom_arg(v, 0)), 42);
    PASS();
}

/*
 * Closure Tests
 */
static tcelm_value_t *fn_add(tcelm_arena_t *a, tcelm_value_t **args) {
    return tcelm_int(a, TCELM_AS_INT(args[0]) + TCELM_AS_INT(args[1]));
}

static void test_closure_apply(void) {
    TEST("closure_apply");
    tcelm_value_t *add = tcelm_closure(arena, fn_add, 2);
    tcelm_value_t *result = tcelm_apply(arena,
        tcelm_apply(arena, add, tcelm_int(arena, 3)),
        tcelm_int(arena, 4));
    ASSERT_EQ(TCELM_AS_INT(result), 7);
    PASS();
}

static void test_closure_partial(void) {
    TEST("closure_partial");
    tcelm_value_t *add = tcelm_closure(arena, fn_add, 2);
    tcelm_value_t *add5 = tcelm_apply(arena, add, tcelm_int(arena, 5));

    ASSERT(TCELM_IS_CLOSURE(add5));

    tcelm_value_t *result = tcelm_apply(arena, add5, tcelm_int(arena, 10));
    ASSERT_EQ(TCELM_AS_INT(result), 15);
    PASS();
}

static tcelm_value_t *fn_mul3(tcelm_arena_t *a, tcelm_value_t **args) {
    return tcelm_int(a,
        TCELM_AS_INT(args[0]) * TCELM_AS_INT(args[1]) * TCELM_AS_INT(args[2]));
}

static void test_closure_arity3(void) {
    TEST("closure_arity3");
    tcelm_value_t *mul = tcelm_closure(arena, fn_mul3, 3);
    tcelm_value_t *mul2 = tcelm_apply(arena, mul, tcelm_int(arena, 2));
    tcelm_value_t *mul2_3 = tcelm_apply(arena, mul2, tcelm_int(arena, 3));
    tcelm_value_t *result = tcelm_apply(arena, mul2_3, tcelm_int(arena, 4));
    ASSERT_EQ(TCELM_AS_INT(result), 24);
    PASS();
}

/*
 * RTEMS API Tests (new APIs)
 */
static void test_rtems_yield_processor(void) {
    TEST("RTEMS_YIELD_PROCESSOR");
    /* RTEMS_YIELD_PROCESSOR should be defined as 0 */
    ASSERT_EQ(RTEMS_YIELD_PROCESSOR, 0);
    PASS();
}

static void test_rtems_clock_get_uptime(void) {
    TEST("clock_get_uptime");
    struct timespec ts;
    rtems_status_code rc = rtems_clock_get_uptime(&ts);
    ASSERT_EQ(rc, RTEMS_SUCCESSFUL);
    /* Uptime should be >= 0 */
    ASSERT(ts.tv_sec >= 0);
    ASSERT(ts.tv_nsec >= 0);
    ASSERT(ts.tv_nsec < 1000000000L);
    PASS();
}

static void test_rtems_cpu_affinity(void) {
    TEST("cpu_affinity");
    cpu_set_t cpuset;

    /* Test CPU_ZERO */
    CPU_ZERO(&cpuset);
    ASSERT(!CPU_ISSET(0, &cpuset));

    /* Test CPU_SET */
    CPU_SET(0, &cpuset);
    ASSERT(CPU_ISSET(0, &cpuset));

    /* Test CPU_CLR */
    CPU_CLR(0, &cpuset);
    ASSERT(!CPU_ISSET(0, &cpuset));

    /* Test CPU_COUNT */
    CPU_ZERO(&cpuset);
    CPU_SET(0, &cpuset);
    CPU_SET(2, &cpuset);
    ASSERT_EQ(CPU_COUNT(&cpuset), 2);

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
    puts("tcelm Runtime Test (NUC Toolchain)\n");
    puts("========================================\n\n");

    puts("[Arena Tests]\n");
    test_arena_create();
    test_arena_alloc();
    test_arena_stats();

    puts("\n[Int Tests]\n");
    test_int_create();
    test_int_negative();
    test_int_large();

    puts("\n[Float Tests]\n");
    test_float_create();

    puts("\n[Bool Tests]\n");
    test_bool_true();
    test_bool_false();

    puts("\n[Char Tests]\n");
    test_char_create();

    puts("\n[String Tests]\n");
    test_string_create();
    test_string_empty();

    puts("\n[Unit Tests]\n");
    test_unit();

    puts("\n[List Tests]\n");
    test_list_nil();
    test_list_cons();
    test_list_tail();

    puts("\n[Tuple Tests]\n");
    test_tuple2();
    test_tuple3();

    puts("\n[Record Tests]\n");
    test_record();
    test_record_update();

    puts("\n[Custom Type Tests]\n");
    test_custom_nothing();
    test_custom_just();

    puts("\n[Closure Tests]\n");
    test_closure_apply();
    test_closure_partial();
    test_closure_arity3();

    puts("\n[RTEMS API Tests]\n");
    test_rtems_yield_processor();
    test_rtems_clock_get_uptime();
    test_rtems_cpu_affinity();

    puts("\n========================================\n");
    puts("Tests: ");
    putint(tests_passed);
    puts("/");
    putint(tests_passed + tests_failed);
    puts(" passed\n");
    puts("========================================\n");

    puts("\nTest complete. Halting.\n");

    /* Exit QEMU */
    outb(0xf4, tests_failed == 0 ? 0x00 : 0x01);

    return 0;
}
