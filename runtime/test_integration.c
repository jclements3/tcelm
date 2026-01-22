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
#include <pthread.h>

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
 * Test Semaphore operations
 */
void test_semaphore(void) {
    printf("\n=== Semaphore Tests ===\n");

    tcelm_arena_t *arena = tcelm_arena_create(4096);

    TEST("semaphore create");
    tcelm_semaphore_t *sem = tcelm_semaphore_create_with_count(arena, 3);
    assert(sem != NULL);
    PASS();

    TEST("semaphore acquire");
    int result = tcelm_semaphore_acquire(sem);
    assert(result == 0);
    PASS();

    TEST("semaphore try_acquire");
    result = tcelm_semaphore_try_acquire(sem);  /* Count was 3, now 1 */
    assert(result == 0);
    result = tcelm_semaphore_try_acquire(sem);  /* Count now 0 */
    assert(result == 0);
    result = tcelm_semaphore_try_acquire(sem);  /* Should fail, count is 0 */
    assert(result == -1);
    PASS();

    TEST("semaphore release");
    result = tcelm_semaphore_release(sem);
    assert(result == 0);
    PASS();

    TEST("semaphore release multiple");
    result = tcelm_semaphore_release_multiple(sem, 2);
    assert(result == 0);
    PASS();

    TEST("semaphore get count");
    uint32_t count = tcelm_semaphore_get_count(sem);
    assert(count == 3);  /* Started with 3, acquired 3, released 1+2 = 3 */
    PASS();

    TEST("semaphore acquire timeout");
    /* Drain the semaphore */
    tcelm_semaphore_acquire(sem);
    tcelm_semaphore_acquire(sem);
    tcelm_semaphore_acquire(sem);
    /* Now try with timeout - should fail */
    result = tcelm_semaphore_acquire_timeout(sem, 10);  /* 10ms timeout */
    assert(result == -1);
    PASS();

    TEST("semaphore delete");
    tcelm_semaphore_delete(sem);
    PASS();

    tcelm_arena_free(arena);
}

/*
 * Test Barrier operations
 */
static void *barrier_thread_fn(void *arg);

typedef struct barrier_test_data {
    tcelm_barrier_t *barrier;
    int thread_id;
    int *arrival_order;
    int *arrival_index;
    pthread_mutex_t *mutex;
} barrier_test_data_t;

static void *barrier_thread_fn(void *arg) {
    barrier_test_data_t *data = (barrier_test_data_t *)arg;

    /* Small delay based on thread id to stagger arrivals */
    usleep(data->thread_id * 1000);

    /* Wait at barrier */
    tcelm_barrier_wait_result_t result = tcelm_barrier_wait(data->barrier);

    /* Record that we passed the barrier */
    pthread_mutex_lock(data->mutex);
    data->arrival_order[(*data->arrival_index)++] = data->thread_id;
    pthread_mutex_unlock(data->mutex);

    return (void *)(intptr_t)result;
}

void test_barrier(void) {
    printf("\n=== Barrier Tests ===\n");

    tcelm_arena_t *arena = tcelm_arena_create(4096);

    TEST("barrier create");
    tcelm_barrier_t *barrier = tcelm_barrier_create_with_count(arena, 3);
    assert(barrier != NULL);
    PASS();

    TEST("barrier wait (3 threads)");
    {
        pthread_t threads[3];
        barrier_test_data_t data[3];
        int arrival_order[3] = {-1, -1, -1};
        int arrival_index = 0;
        pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

        for (int i = 0; i < 3; i++) {
            data[i].barrier = barrier;
            data[i].thread_id = i;
            data[i].arrival_order = arrival_order;
            data[i].arrival_index = &arrival_index;
            data[i].mutex = &mutex;
            pthread_create(&threads[i], NULL, barrier_thread_fn, &data[i]);
        }

        for (int i = 0; i < 3; i++) {
            pthread_join(threads[i], NULL);
        }

        /* All threads should have passed the barrier */
        assert(arrival_index == 3);
        pthread_mutex_destroy(&mutex);
    }
    PASS();

    TEST("barrier wait timeout");
    {
        /* Create a barrier for 2 threads but only one will wait */
        tcelm_barrier_t *b2 = tcelm_barrier_create_with_count(arena, 2);
        tcelm_barrier_wait_result_t result = tcelm_barrier_wait_timeout(b2, 10);
        assert(result == TCELM_BARRIER_WAIT_TIMEOUT);
        tcelm_barrier_delete(b2);
    }
    PASS();

    TEST("barrier release");
    {
        tcelm_barrier_t *b3 = tcelm_barrier_create_with_count(arena, 5);
        /* No one waiting yet */
        uint32_t released = tcelm_barrier_release(b3);
        assert(released == 0);
        tcelm_barrier_delete(b3);
    }
    PASS();

    TEST("barrier delete");
    tcelm_barrier_delete(barrier);
    PASS();

    tcelm_arena_free(arena);
}

/*
 * Test Budget (execution time) operations
 */
void test_budget(void) {
    printf("\n=== Budget Tests ===\n");

    tcelm_arena_t *arena = tcelm_arena_create(4096);

    TEST("budget create");
    tcelm_budget_t *budget = tcelm_budget_create_ms(arena, 100);  /* 100ms budget */
    assert(budget != NULL);
    PASS();

    TEST("budget attach");
    int result = tcelm_budget_attach(budget);
    assert(result == 0);
    PASS();

    TEST("budget start/stop");
    result = tcelm_budget_start(budget);
    assert(result == 0);
    usleep(10000);  /* 10ms of work */
    result = tcelm_budget_stop(budget);
    assert(result == 0);
    PASS();

    TEST("budget get used");
    uint64_t used = tcelm_budget_get_used(budget);
    assert(used > 0);  /* Should have used some CPU time */
    PASS();

    TEST("budget get remaining");
    uint64_t remaining = tcelm_budget_get_remaining(budget);
    assert(remaining > 0);  /* Should have budget remaining */
    assert(remaining < 100000);  /* Less than full 100ms */
    PASS();

    TEST("budget not exhausted");
    bool exhausted = tcelm_budget_is_exhausted(budget);
    assert(exhausted == false);
    PASS();

    TEST("budget reset");
    result = tcelm_budget_reset(budget);
    assert(result == 0);
    used = tcelm_budget_get_used(budget);
    assert(used == 0);
    PASS();

    TEST("budget set limit");
    result = tcelm_budget_set_limit(budget, 50000);  /* 50ms */
    assert(result == 0);
    PASS();

    TEST("cpu time self");
    uint64_t cpu_time = tcelm_cpu_time_self_us();
    assert(cpu_time > 0);
    PASS();

    TEST("budget delete");
    tcelm_budget_delete(budget);
    PASS();

    tcelm_arena_free(arena);
}

/*
 * Test Protected type operations
 */

/* Test data structure for protected object */
typedef struct {
    int counter;
    bool ready;
} test_protected_data_t;

/* Test guard: only proceed when ready */
static bool test_guard_ready(void *data) {
    test_protected_data_t *d = (test_protected_data_t *)data;
    return d->ready;
}

/* Test procedure: increment counter */
static void test_proc_increment(void *data, void *arg) {
    test_protected_data_t *d = (test_protected_data_t *)data;
    int amount = arg ? *(int *)arg : 1;
    d->counter += amount;
}

/* Test function: get counter value */
static void *test_func_get_counter(void *data, void *arg) {
    (void)arg;
    test_protected_data_t *d = (test_protected_data_t *)data;
    return (void *)(intptr_t)d->counter;
}

void test_protected(void) {
    printf("\n=== Protected Type Tests ===\n");

    tcelm_arena_t *arena = tcelm_arena_create(4096);

    TEST("protected create");
    test_protected_data_t init_data = { .counter = 0, .ready = false };
    tcelm_protected_t *prot = tcelm_protected_create_with_data(
        arena,
        &init_data,
        sizeof(test_protected_data_t)
    );
    assert(prot != NULL);
    PASS();

    TEST("protected call proc");
    int amount = 5;
    tcelm_protected_call_proc(prot, test_proc_increment, &amount);
    PASS();

    TEST("protected call func");
    void *result = tcelm_protected_call_func(prot, test_func_get_counter, NULL);
    assert((intptr_t)result == 5);
    PASS();

    TEST("protected add entry");
    int entry_idx = tcelm_protected_add_entry(
        prot,
        "increment_when_ready",
        test_guard_ready,
        test_proc_increment
    );
    assert(entry_idx >= 0);
    PASS();

    TEST("protected try_call_entry (guard false)");
    int rc = tcelm_protected_try_call_entry(prot, entry_idx, &amount);
    assert(rc == -1);  /* Guard is false, should fail */
    PASS();

    TEST("protected manual lock/unlock");
    tcelm_protected_lock_write(prot);
    test_protected_data_t *data = (test_protected_data_t *)tcelm_protected_get_data(prot);
    data->ready = true;  /* Set guard condition */
    data->counter = 10;
    tcelm_protected_unlock_write(prot);
    PASS();

    TEST("protected try_call_entry (guard true)");
    amount = 7;
    rc = tcelm_protected_try_call_entry(prot, entry_idx, &amount);
    assert(rc == 0);  /* Guard is true, should succeed */
    result = tcelm_protected_call_func(prot, test_func_get_counter, NULL);
    assert((intptr_t)result == 17);  /* 10 + 7 */
    PASS();

    TEST("protected call entry by name");
    amount = 3;
    rc = tcelm_protected_call_entry_by_name(prot, "increment_when_ready", &amount);
    assert(rc == 0);
    result = tcelm_protected_call_func(prot, test_func_get_counter, NULL);
    assert((intptr_t)result == 20);  /* 17 + 3 */
    PASS();

    TEST("protected delete");
    tcelm_protected_delete(prot);
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
    test_semaphore();
    test_barrier();
    test_budget();
    test_protected();
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
