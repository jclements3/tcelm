/*
 * test_integration.c - Test the new RTEMS integration layer
 *
 * Tests Task, Channel, MVar, Timer, and IO operations using
 * the native (pthread) implementation for development.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>

#include "tcelm_runtime.h"

static int tests_run = 0;
static int tests_passed = 0;

#define TEST(name) do { \
    printf("  Testing %s... ", name); \
    tests_run++; \
} while(0)

#define PASS() do { \
    printf("PASS\n"); \
    tests_passed++; \
} while(0)

#define FAIL(msg) do { \
    printf("FAIL: %s\n", msg); \
} while(0)

/*
 * Test arena allocation
 */
void test_arena(void) {
    printf("\n=== Arena Tests ===\n");

    TEST("arena create");
    tcelm_arena_t *arena = tcelm_arena_create(4096);
    assert(arena != NULL);
    PASS();

    TEST("arena alloc");
    void *ptr = tcelm_arena_alloc(arena, 100);
    assert(ptr != NULL);
    PASS();

    TEST("arena reset");
    tcelm_arena_reset(arena);
    void *ptr2 = tcelm_arena_alloc(arena, 100);
    assert(ptr2 == ptr);  /* Same address after reset */
    PASS();

    TEST("arena destroy");
    tcelm_arena_free(arena);
    PASS();
}

/*
 * Test basic types
 */
void test_types(void) {
    printf("\n=== Type Tests ===\n");

    tcelm_arena_t *arena = tcelm_arena_create(4096);

    TEST("int creation");
    tcelm_value_t *i = tcelm_int(arena, 42);
    assert(TCELM_IS_INT(i));
    assert(TCELM_AS_INT(i) == 42);
    PASS();

    TEST("float creation");
    tcelm_value_t *f = tcelm_float(arena, 3.14);
    assert(TCELM_IS_FLOAT(f));
    assert(TCELM_AS_FLOAT(f) > 3.13 && TCELM_AS_FLOAT(f) < 3.15);
    PASS();

    TEST("bool creation");
    tcelm_value_t *t = tcelm_bool(arena, true);
    tcelm_value_t *fv = tcelm_bool(arena, false);
    assert(TCELM_IS_BOOL(t));
    assert(TCELM_AS_BOOL(t) == true);
    assert(TCELM_AS_BOOL(fv) == false);
    PASS();

    TEST("string creation");
    tcelm_value_t *s = tcelm_string(arena, "hello");
    assert(TCELM_IS_STRING(s));
    PASS();

    TEST("Maybe Just");
    tcelm_value_t *just = TCELM_JUST(arena, tcelm_int(arena, 10));
    assert(TCELM_IS_JUST(just));
    assert(TCELM_AS_INT(TCELM_UNWRAP_JUST(just)) == 10);
    PASS();

    TEST("Maybe Nothing");
    tcelm_value_t *nothing = TCELM_NOTHING(arena);
    assert(TCELM_IS_NOTHING(nothing));
    PASS();

    TEST("Result Ok");
    tcelm_value_t *ok = TCELM_OK(arena, tcelm_int(arena, 42));
    assert(TCELM_IS_OK(ok));
    assert(TCELM_AS_INT(TCELM_UNWRAP_OK(ok)) == 42);
    PASS();

    TEST("Result Err");
    tcelm_value_t *err = TCELM_ERR(arena, tcelm_string(arena, "error"));
    assert(TCELM_IS_ERR(err));
    PASS();

    tcelm_arena_free(arena);
}

/*
 * Test Channel operations
 */
void test_channel(void) {
    printf("\n=== Channel Tests ===\n");

    tcelm_arena_t *arena = tcelm_arena_create(64 * 1024);

    TEST("channel create");
    tcelm_channel_t *chan = tcelm_channel_create_default(arena, 10);
    assert(chan != NULL);
    PASS();

    TEST("channel send/receive");
    tcelm_value_t *msg = tcelm_int(arena, 42);
    int result = tcelm_channel_send(chan, msg);
    assert(result == 0);
    tcelm_value_t *received = tcelm_channel_receive(arena, chan);
    assert(received != NULL);
    assert(TCELM_IS_INT(received));
    assert(TCELM_AS_INT(received) == 42);
    PASS();

    TEST("channel try_receive empty");
    tcelm_value_t *maybe = tcelm_channel_try_receive(arena, chan);
    assert(TCELM_IS_NOTHING(maybe));
    PASS();

    TEST("channel try_receive with message");
    tcelm_channel_send(chan, tcelm_int(arena, 99));
    maybe = tcelm_channel_try_receive(arena, chan);
    assert(TCELM_IS_JUST(maybe));
    assert(TCELM_AS_INT(TCELM_UNWRAP_JUST(maybe)) == 99);
    PASS();

    TEST("channel pending count");
    tcelm_channel_send(chan, tcelm_int(arena, 1));
    tcelm_channel_send(chan, tcelm_int(arena, 2));
    tcelm_channel_send(chan, tcelm_int(arena, 3));
    assert(tcelm_channel_pending(chan) == 3);
    PASS();

    TEST("channel flush");
    int flushed = tcelm_channel_flush(chan);
    assert(flushed == 3);
    assert(tcelm_channel_pending(chan) == 0);
    PASS();

    TEST("channel close");
    tcelm_channel_close(chan);
    PASS();

    tcelm_arena_free(arena);
}

/*
 * Test MVar operations
 */
void test_mvar(void) {
    printf("\n=== MVar Tests ===\n");

    tcelm_arena_t *arena = tcelm_arena_create(64 * 1024);

    TEST("mvar new");
    tcelm_mvar_t *mvar = tcelm_mvar_new(arena, tcelm_int(arena, 42));
    assert(mvar != NULL);
    assert(!tcelm_mvar_is_empty(mvar));
    PASS();

    TEST("mvar take");
    tcelm_value_t *val = tcelm_mvar_take(arena, mvar);
    assert(TCELM_IS_INT(val));
    assert(TCELM_AS_INT(val) == 42);
    assert(tcelm_mvar_is_empty(mvar));
    PASS();

    TEST("mvar put");
    int result = tcelm_mvar_put(mvar, tcelm_int(arena, 100));
    assert(result == 0);
    assert(!tcelm_mvar_is_empty(mvar));
    PASS();

    TEST("mvar try_take");
    tcelm_value_t *maybe = tcelm_mvar_try_take(arena, mvar);
    assert(TCELM_IS_JUST(maybe));
    assert(TCELM_AS_INT(TCELM_UNWRAP_JUST(maybe)) == 100);
    PASS();

    TEST("mvar try_take empty");
    maybe = tcelm_mvar_try_take(arena, mvar);
    assert(TCELM_IS_NOTHING(maybe));
    PASS();

    TEST("mvar new_empty");
    tcelm_mvar_t *empty = tcelm_mvar_new_empty(arena);
    assert(empty != NULL);
    assert(tcelm_mvar_is_empty(empty));
    PASS();

    TEST("mvar swap");
    tcelm_mvar_put(mvar, tcelm_int(arena, 1));
    tcelm_value_t *old = tcelm_mvar_swap(arena, mvar, tcelm_int(arena, 2));
    assert(TCELM_AS_INT(old) == 1);
    val = tcelm_mvar_take(arena, mvar);
    assert(TCELM_AS_INT(val) == 2);
    PASS();

    TEST("mvar delete");
    tcelm_mvar_delete(mvar);
    tcelm_mvar_delete(empty);
    PASS();

    tcelm_arena_free(arena);
}

/*
 * Test Timer operations
 */
void test_timer(void) {
    printf("\n=== Timer Tests ===\n");

    tcelm_arena_t *arena = tcelm_arena_create(4096);

    TEST("timer now");
    uint64_t t1 = tcelm_timer_now_ms();
    usleep(10000);  /* 10ms */
    uint64_t t2 = tcelm_timer_now_ms();
    assert(t2 >= t1);
    assert(t2 - t1 >= 5);  /* At least 5ms passed */
    PASS();

    TEST("timer ticks_per_second");
    uint32_t tps = tcelm_timer_ticks_per_second();
    assert(tps > 0);
    PASS();

    TEST("timer ms_to_ticks");
    uint32_t ticks = tcelm_timer_ms_to_ticks(1000);
    assert(ticks == tps);
    PASS();

    tcelm_arena_free(arena);
}

/*
 * Test IO operations
 */
void test_io(void) {
    printf("\n=== IO Tests ===\n");

    tcelm_arena_t *arena = tcelm_arena_create(64 * 1024);

    TEST("io print");
    tcelm_io_print("  (print test) ");
    PASS();

    TEST("io file exists");
    bool exists = tcelm_file_exists("/tmp");
    assert(exists == true);
    exists = tcelm_file_exists("/nonexistent_path_12345");
    assert(exists == false);
    PASS();

    TEST("io file write_all");
    const char *test_content = "Hello, tcelm!";
    int result = tcelm_file_write_all("/tmp/tcelm_test.txt", test_content);
    assert(result == 0);
    PASS();

    TEST("io file read_all");
    tcelm_value_t *content = tcelm_file_read_all(arena, "/tmp/tcelm_test.txt");
    assert(TCELM_IS_OK(content));
    PASS();

    TEST("io file size");
    long size = tcelm_file_size("/tmp/tcelm_test.txt");
    assert(size == (long)strlen(test_content));
    PASS();

    TEST("io file delete");
    result = tcelm_file_delete("/tmp/tcelm_test.txt");
    assert(result == 0);
    assert(!tcelm_file_exists("/tmp/tcelm_test.txt"));
    PASS();

    TEST("io get_env");
    tcelm_value_t *home = tcelm_io_get_env(arena, "HOME");
    assert(TCELM_IS_JUST(home));
    tcelm_value_t *nonexistent = tcelm_io_get_env(arena, "TCELM_NONEXISTENT_VAR_12345");
    assert(TCELM_IS_NOTHING(nonexistent));
    PASS();

    tcelm_arena_free(arena);
}

/*
 * Test Task operations (basic - without spawning threads)
 */
void test_task_basic(void) {
    printf("\n=== Task Basic Tests ===\n");

    tcelm_arena_t *arena = tcelm_arena_create(4096);

    TEST("task succeed");
    tcelm_value_t *success = tcelm_task_succeed(arena, tcelm_int(arena, 42));
    assert(TCELM_IS_OK(success));
    assert(TCELM_AS_INT(TCELM_UNWRAP_OK(success)) == 42);
    PASS();

    TEST("task fail");
    tcelm_value_t *failure = tcelm_task_fail(arena, tcelm_string(arena, "error"));
    assert(TCELM_IS_ERR(failure));
    PASS();

    TEST("task sleep");
    uint64_t before = tcelm_timer_now_ms();
    tcelm_task_sleep(50);  /* 50ms */
    uint64_t after = tcelm_timer_now_ms();
    assert(after - before >= 40);  /* At least 40ms passed */
    PASS();

    TEST("task yield");
    tcelm_task_yield();  /* Should not block */
    PASS();

    tcelm_arena_free(arena);
}

int main(int argc, char **argv) {
    printf("tcelm Integration Layer Tests\n");
    printf("==============================\n");

    /* Store args for tcelm_io_get_args */
    tcelm_io_set_args(argc, argv);

    /* Initialize runtime */
    tcelm_runtime_init();

    /* Run tests */
    test_arena();
    test_types();
    test_channel();
    test_mvar();
    test_timer();
    test_io();
    test_task_basic();

    /* Summary */
    printf("\n==============================\n");
    printf("Tests: %d/%d passed\n", tests_passed, tests_run);

    /* Shutdown runtime */
    tcelm_runtime_shutdown();

    return (tests_passed == tests_run) ? 0 : 1;
}
