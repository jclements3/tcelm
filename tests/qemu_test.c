/*
 * qemu_test.c - Simple QEMU test for TCC-compiled code
 *
 * This is a standalone kernel that tests basic functionality
 * without requiring RTEMS. Output goes to serial port (COM1).
 *
 * Compile with TCC:
 *   tcc -m32 -nostdlib -nostdinc \
 *       multiboot.o qemu_test.c \
 *       -Tlinkcmds.qemu -o qemu_test.elf
 *
 * Run with QEMU:
 *   qemu-system-i386 -kernel qemu_test.elf -nographic
 */

/* Serial port I/O (COM1 = 0x3F8) */
#define COM1_PORT 0x3F8

static inline void outb(unsigned short port, unsigned char val) {
    __asm__ volatile ("outb %0, %1" : : "a"(val), "Nd"(port));
}

static inline unsigned char inb(unsigned short port) {
    unsigned char ret;
    __asm__ volatile ("inb %1, %0" : "=a"(ret) : "Nd"(port));
    return ret;
}

/* Initialize serial port */
static void serial_init(void) {
    outb(COM1_PORT + 1, 0x00);  /* Disable interrupts */
    outb(COM1_PORT + 3, 0x80);  /* Enable DLAB */
    outb(COM1_PORT + 0, 0x03);  /* Baud rate divisor lo (38400) */
    outb(COM1_PORT + 1, 0x00);  /* Baud rate divisor hi */
    outb(COM1_PORT + 3, 0x03);  /* 8 bits, no parity, 1 stop bit */
    outb(COM1_PORT + 2, 0xC7);  /* Enable FIFO */
    outb(COM1_PORT + 4, 0x0B);  /* IRQs enabled, RTS/DSR set */
}

/* Check if transmit buffer is empty */
static int serial_is_transmit_empty(void) {
    return inb(COM1_PORT + 5) & 0x20;
}

/* Write a character to serial port */
static void serial_putchar(char c) {
    while (!serial_is_transmit_empty());
    outb(COM1_PORT, c);
}

/* Write a string to serial port */
static void serial_puts(const char *s) {
    while (*s) {
        if (*s == '\n') {
            serial_putchar('\r');
        }
        serial_putchar(*s++);
    }
}

/* Simple integer to string */
static void serial_putint(int n) {
    char buf[16];
    int i = 0;
    int neg = 0;

    if (n < 0) {
        neg = 1;
        n = -n;
    }

    do {
        buf[i++] = '0' + (n % 10);
        n /= 10;
    } while (n > 0);

    if (neg) {
        serial_putchar('-');
    }

    while (i > 0) {
        serial_putchar(buf[--i]);
    }
}

/* Test arithmetic */
static int test_arithmetic(void) {
    int a = 42;
    int b = 17;
    int pass = 1;

    serial_puts("  add: ");
    serial_putint(a + b);
    if (a + b != 59) pass = 0;
    serial_puts(pass ? " PASS\n" : " FAIL\n");

    serial_puts("  sub: ");
    serial_putint(a - b);
    if (a - b != 25) pass = 0;
    serial_puts(pass ? " PASS\n" : " FAIL\n");

    serial_puts("  mul: ");
    serial_putint(a * b);
    if (a * b != 714) pass = 0;
    serial_puts(pass ? " PASS\n" : " FAIL\n");

    serial_puts("  div: ");
    serial_putint(a / b);
    if (a / b != 2) pass = 0;
    serial_puts(pass ? " PASS\n" : " FAIL\n");

    return pass;
}

/* Test memory */
static int test_memory(void) {
    static int array[10];
    int i, pass = 1;

    serial_puts("  array write/read: ");
    for (i = 0; i < 10; i++) {
        array[i] = i * i;
    }
    for (i = 0; i < 10; i++) {
        if (array[i] != i * i) {
            pass = 0;
            break;
        }
    }
    serial_puts(pass ? "PASS\n" : "FAIL\n");

    return pass;
}

/* Test function calls */
static int add(int x, int y) { return x + y; }
static int mul(int x, int y) { return x * y; }

static int test_functions(void) {
    int pass = 1;

    serial_puts("  function call: ");
    if (add(3, 4) != 7) pass = 0;
    if (mul(5, 6) != 30) pass = 0;
    serial_puts(pass ? "PASS\n" : "FAIL\n");

    return pass;
}

/* Test structs */
typedef struct {
    int x;
    int y;
    int z;
} point_t;

static int test_structs(void) {
    point_t p = {10, 20, 30};
    int pass = 1;

    serial_puts("  struct access: ");
    if (p.x != 10 || p.y != 20 || p.z != 30) pass = 0;
    
    p.x = 100;
    if (p.x != 100) pass = 0;
    
    serial_puts(pass ? "PASS\n" : "FAIL\n");

    return pass;
}

/*
 * kernel_main - Entry point from multiboot
 */
void kernel_main(unsigned int magic, void *info) {
    int total = 0, passed = 0;
    (void)magic;
    (void)info;

    serial_init();

    serial_puts("\n");
    serial_puts("================================\n");
    serial_puts("TCC/QEMU Test Suite\n");
    serial_puts("================================\n");
    serial_puts("\n");

    serial_puts("[Arithmetic Tests]\n");
    total += 4;
    if (test_arithmetic()) passed += 4;

    serial_puts("\n[Memory Tests]\n");
    total += 1;
    if (test_memory()) passed += 1;

    serial_puts("\n[Function Tests]\n");
    total += 1;
    if (test_functions()) passed += 1;

    serial_puts("\n[Struct Tests]\n");
    total += 1;
    if (test_structs()) passed += 1;

    serial_puts("\n================================\n");
    serial_puts("Tests: ");
    serial_putint(passed);
    serial_puts("/");
    serial_putint(total);
    serial_puts(" passed\n");
    serial_puts("================================\n");

    /* Signal QEMU to exit (debug port) */
    serial_puts("\nTest complete. Halting.\n");
    
    /* Exit QEMU with success code */
    outb(0xf4, passed == total ? 0x00 : 0x01);
}
