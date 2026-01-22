/*
 * qemu_libc.h - Minimal libc for bare-metal QEMU testing
 *
 * Provides just enough to run tcelm runtime tests without an OS.
 */

#ifndef QEMU_LIBC_H
#define QEMU_LIBC_H

/* Standard types */
typedef unsigned long size_t;
typedef long ssize_t;
typedef long int64_t;
typedef unsigned long uint64_t;
typedef int int32_t;
typedef unsigned int uint32_t;
typedef short int16_t;
typedef unsigned short uint16_t;
typedef signed char int8_t;
typedef unsigned char uint8_t;
typedef int bool;
#define true 1
#define false 0
#define NULL ((void *)0)

/* Variable arguments */
typedef __builtin_va_list va_list;
#define va_start(ap, last) __builtin_va_start(ap, last)
#define va_end(ap) __builtin_va_end(ap)
#define va_arg(ap, type) __builtin_va_arg(ap, type)

/* Serial I/O */
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

static int serial_is_transmit_empty(void) {
    return inb(COM1_PORT + 5) & 0x20;
}

static void serial_putchar(char c) {
    while (!serial_is_transmit_empty());
    outb(COM1_PORT, c);
}

static void serial_puts(const char *s) {
    while (*s) {
        if (*s == '\n') serial_putchar('\r');
        serial_putchar(*s++);
    }
}

static void serial_putint(int64_t n) {
    char buf[24];
    int i = 0;
    int neg = 0;

    if (n < 0) {
        neg = 1;
        n = -n;
    }
    if (n == 0) {
        serial_putchar('0');
        return;
    }
    while (n > 0) {
        buf[i++] = '0' + (n % 10);
        n /= 10;
    }
    if (neg) serial_putchar('-');
    while (i > 0) serial_putchar(buf[--i]);
}

static void serial_puthex(uint64_t n) {
    const char *hex = "0123456789abcdef";
    serial_puts("0x");
    for (int i = 60; i >= 0; i -= 4) {
        serial_putchar(hex[(n >> i) & 0xf]);
    }
}

/* String functions */
static size_t strlen(const char *s) {
    size_t len = 0;
    while (*s++) len++;
    return len;
}

static int strcmp(const char *s1, const char *s2) {
    while (*s1 && *s1 == *s2) {
        s1++;
        s2++;
    }
    return (unsigned char)*s1 - (unsigned char)*s2;
}

static void *memcpy(void *dest, const void *src, size_t n) {
    unsigned char *d = dest;
    const unsigned char *s = src;
    while (n--) *d++ = *s++;
    return dest;
}

static void *memset(void *s, int c, size_t n) {
    unsigned char *p = s;
    while (n--) *p++ = (unsigned char)c;
    return s;
}

static char *strcpy(char *dest, const char *src) {
    char *d = dest;
    while ((*d++ = *src++));
    return dest;
}

static char *strcat(char *dest, const char *src) {
    char *d = dest;
    while (*d) d++;
    while ((*d++ = *src++));
    return dest;
}

/* Simple heap allocator using a static buffer */
#define HEAP_SIZE (1024 * 1024)  /* 1MB heap */
static char heap_buffer[HEAP_SIZE];
static size_t heap_used = 0;

static void *malloc(size_t size) {
    /* Align to 8 bytes */
    size = (size + 7) & ~7;
    if (heap_used + size > HEAP_SIZE) {
        serial_puts("OUT OF MEMORY!\n");
        return NULL;
    }
    void *ptr = &heap_buffer[heap_used];
    heap_used += size;
    return ptr;
}

static void free(void *ptr) {
    /* No-op for simple bump allocator */
    (void)ptr;
}

/* Thread-local storage stub (single-threaded in bare-metal) */
#define TCELM_TLS

/* Test assertion macros */
static int test_passed = 0;
static int test_failed = 0;

#define TEST(name) \
    serial_puts("  " name ": "); \

#define ASSERT(cond) \
    if (!(cond)) { \
        serial_puts("FAIL ("); \
        serial_puts(#cond); \
        serial_puts(")\n"); \
        test_failed++; \
        return; \
    }

#define ASSERT_EQ(a, b) \
    if ((a) != (b)) { \
        serial_puts("FAIL ("); \
        serial_putint(a); \
        serial_puts(" != "); \
        serial_putint(b); \
        serial_puts(")\n"); \
        test_failed++; \
        return; \
    }

#define ASSERT_STR_EQ(a, b) \
    if (strcmp(a, b) != 0) { \
        serial_puts("FAIL (\""); \
        serial_puts(a); \
        serial_puts("\" != \""); \
        serial_puts(b); \
        serial_puts("\")\n"); \
        test_failed++; \
        return; \
    }

#define PASS() \
    serial_puts("PASS\n"); \
    test_passed++;

#endif /* QEMU_LIBC_H */
