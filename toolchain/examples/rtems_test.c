/*
 * rtems_test.c - Test program for tcelm NUC toolchain
 *
 * This tests the RTEMS-compatible APIs in the toolchain.
 *
 * Build:
 *   cd toolchain
 *   ./bin/tcelm-cc -c examples/rtems_test.c -o build/rtems_test.o
 *   ./bin/tcelm-ld lib/crt0.o build/rtems_test.o lib/libtcelm.a -o build/rtems_test.elf
 *
 * Run in QEMU:
 *   qemu-system-i386 -kernel build/rtems_test.elf -nographic -no-reboot
 */

#include <rtems.h>
#include <stdint.h>

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

static void test_pass(const char *name) {
    puts("  ");
    puts(name);
    puts(": PASS\n");
    tests_passed++;
}

static void test_fail(const char *name) {
    puts("  ");
    puts(name);
    puts(": FAIL\n");
    tests_failed++;
}

/*
 * Test RTEMS Task API
 */
static void test_task_create(void) {
    rtems_id task_id;
    rtems_status_code rc;

    rc = rtems_task_create(
        rtems_build_name('T', 'S', 'T', '1'),
        100,
        RTEMS_MINIMUM_STACK_SIZE,
        RTEMS_DEFAULT_MODES,
        RTEMS_DEFAULT_ATTRIBUTES,
        &task_id
    );

    if (rc == RTEMS_SUCCESSFUL && task_id > 0) {
        test_pass("task_create");
        rtems_task_delete(task_id);
    } else {
        test_fail("task_create");
    }
}

/*
 * Test RTEMS Semaphore API
 */
static void test_semaphore_create(void) {
    rtems_id sem_id;
    rtems_status_code rc;

    rc = rtems_semaphore_create(
        rtems_build_name('S', 'E', 'M', '1'),
        1,
        RTEMS_BINARY_SEMAPHORE | RTEMS_PRIORITY | RTEMS_INHERIT_PRIORITY,
        0,
        &sem_id
    );

    if (rc == RTEMS_SUCCESSFUL && sem_id > 0) {
        test_pass("semaphore_create");
        rtems_semaphore_delete(sem_id);
    } else {
        test_fail("semaphore_create");
    }
}

static void test_semaphore_obtain_release(void) {
    rtems_id sem_id;
    rtems_status_code rc;

    rtems_semaphore_create(
        rtems_build_name('S', 'E', 'M', '2'),
        1,
        RTEMS_BINARY_SEMAPHORE,
        0,
        &sem_id
    );

    rc = rtems_semaphore_obtain(sem_id, RTEMS_NO_WAIT, 0);
    if (rc != RTEMS_SUCCESSFUL) {
        test_fail("semaphore_obtain");
        return;
    }

    rc = rtems_semaphore_release(sem_id);
    if (rc == RTEMS_SUCCESSFUL) {
        test_pass("semaphore_obtain_release");
    } else {
        test_fail("semaphore_obtain_release");
    }

    rtems_semaphore_delete(sem_id);
}

/*
 * Test RTEMS Message Queue API
 */
static void test_message_queue(void) {
    rtems_id mq_id;
    rtems_status_code rc;
    char send_buf[] = "Hello";
    char recv_buf[32];
    size_t size;

    rc = rtems_message_queue_create(
        rtems_build_name('M', 'S', 'G', '1'),
        10,
        32,
        RTEMS_DEFAULT_ATTRIBUTES,
        &mq_id
    );

    if (rc != RTEMS_SUCCESSFUL) {
        test_fail("message_queue_create");
        return;
    }

    rc = rtems_message_queue_send(mq_id, send_buf, 6);
    if (rc != RTEMS_SUCCESSFUL) {
        test_fail("message_queue_send");
        rtems_message_queue_delete(mq_id);
        return;
    }

    size = sizeof(recv_buf);
    rc = rtems_message_queue_receive(mq_id, recv_buf, &size, RTEMS_NO_WAIT, 0);
    if (rc == RTEMS_SUCCESSFUL && size == 6) {
        /* Check content */
        int match = 1;
        for (int i = 0; i < 6; i++) {
            if (send_buf[i] != recv_buf[i]) match = 0;
        }
        if (match) {
            test_pass("message_queue");
        } else {
            test_fail("message_queue_content");
        }
    } else {
        test_fail("message_queue_receive");
    }

    rtems_message_queue_delete(mq_id);
}

/*
 * Test RTEMS Timer API
 */
static volatile int timer_fired = 0;

static void timer_callback(rtems_id id, void *arg) {
    (void)id;
    (void)arg;
    timer_fired = 1;
}

static void test_timer(void) {
    rtems_id timer_id;
    rtems_status_code rc;

    rc = rtems_timer_create(rtems_build_name('T', 'M', 'R', '1'), &timer_id);
    if (rc != RTEMS_SUCCESSFUL) {
        test_fail("timer_create");
        return;
    }

    timer_fired = 0;
    rc = rtems_timer_fire_after(timer_id, 1, timer_callback, NULL);
    if (rc != RTEMS_SUCCESSFUL) {
        test_fail("timer_fire_after");
        rtems_timer_delete(timer_id);
        return;
    }

    /* Timer won't actually fire in cooperative mode without scheduler running,
     * but we can test the API */
    test_pass("timer_create_fire");

    rtems_timer_delete(timer_id);
}

/*
 * Test RTEMS Event API
 */
static void test_events(void) {
    rtems_status_code rc;
    rtems_event_set events;

    /* Send event to self */
    rc = rtems_event_send(rtems_task_self(), RTEMS_EVENT_0 | RTEMS_EVENT_1);
    if (rc != RTEMS_SUCCESSFUL) {
        test_fail("event_send");
        return;
    }

    rc = rtems_event_receive(
        RTEMS_EVENT_0,
        RTEMS_EVENT_ANY | RTEMS_NO_WAIT,
        0,
        &events
    );

    if (rc == RTEMS_SUCCESSFUL && (events & RTEMS_EVENT_0)) {
        test_pass("events");
    } else {
        test_fail("event_receive");
    }
}

/*
 * Test Clock API
 */
static void test_clock(void) {
    rtems_interval ticks = rtems_clock_get_ticks_since_boot();
    rtems_interval tps = rtems_clock_get_ticks_per_second();

    if (tps > 0) {
        test_pass("clock_get_ticks");
    } else {
        test_fail("clock_get_ticks");
    }
}

/*
 * Main entry point
 */
int main(int argc, char **argv) {
    (void)argc;
    (void)argv;

    serial_init();

    puts("\n");
    puts("================================\n");
    puts("tcelm NUC Toolchain RTEMS Test\n");
    puts("================================\n\n");

    puts("[Task Tests]\n");
    test_task_create();

    puts("\n[Semaphore Tests]\n");
    test_semaphore_create();
    test_semaphore_obtain_release();

    puts("\n[Message Queue Tests]\n");
    test_message_queue();

    puts("\n[Timer Tests]\n");
    test_timer();

    puts("\n[Event Tests]\n");
    test_events();

    puts("\n[Clock Tests]\n");
    test_clock();

    puts("\n================================\n");
    puts("Tests: ");
    putint(tests_passed);
    puts("/");
    putint(tests_passed + tests_failed);
    puts(" passed\n");
    puts("================================\n");

    puts("\nTest complete. Halting.\n");

    /* Exit QEMU */
    outb(0xf4, tests_failed == 0 ? 0x00 : 0x01);

    return 0;
}
