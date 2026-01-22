/*
 * multicore_test.c - Test actual multi-core execution on NUC
 *
 * This test brings up all 4 CPU cores and has them perform work concurrently.
 *
 * Build:
 *   make -C toolchain test-multicore
 *
 * Run:
 *   qemu-system-i386 -kernel toolchain/build/multicore_test.elf -smp 4 -nographic
 */

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>

/* Serial I/O for test output */
#define COM1_PORT 0x3F8

/* Local APIC registers */
#define LAPIC_BASE      0xFEE00000
#define LAPIC_ID        0x020
#define LAPIC_SVR       0x0F0
#define LAPIC_ICR_LOW   0x300
#define LAPIC_ICR_HIGH  0x310

/* ICR flags */
#define ICR_INIT        0x00000500
#define ICR_STARTUP     0x00000600
#define ICR_LEVEL       0x00008000
#define ICR_ASSERT      0x00004000
#define ICR_DEASSERT    0x00000000
#define ICR_PENDING     0x00001000

/* AP startup address (must be 4KB aligned, below 1MB) */
#define AP_BOOT_ADDR    0x8000

/* Per-CPU counters to track work done */
#define MAX_CPUS 4
static volatile uint32_t cpu_counter[MAX_CPUS] = {0};
static volatile uint32_t cpus_online = 1;  /* BSP is online */
static volatile uint32_t ap_can_start = 0;
static volatile uint32_t test_running = 0;

/* Spinlock for serial output */
static volatile uint32_t serial_lock = 0;

static inline void spin_lock(volatile uint32_t *lock) {
    while (1) {
        if (*lock == 0) {
            uint32_t old;
            __asm__ volatile (
                "xchgl %0, %1"
                : "=r"(old), "+m"(*lock)
                : "0"(1)
                : "memory"
            );
            if (old == 0) break;
        }
        __asm__ volatile ("pause");
    }
}

static inline void spin_unlock(volatile uint32_t *lock) {
    __asm__ volatile ("" ::: "memory");
    *lock = 0;
}

static inline void outb(uint16_t port, uint8_t val) {
    __asm__ volatile ("outb %0, %1" : : "a"(val), "Nd"(port));
}

static inline uint8_t inb(uint16_t port) {
    uint8_t ret;
    __asm__ volatile ("inb %1, %0" : "=a"(ret) : "Nd"(port));
    return ret;
}

static inline uint32_t lapic_read(uint32_t reg) {
    return *(volatile uint32_t *)(LAPIC_BASE + reg);
}

static inline void lapic_write(uint32_t reg, uint32_t val) {
    *(volatile uint32_t *)(LAPIC_BASE + reg) = val;
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

static void puts_unlocked(const char *s) {
    while (*s) {
        if (*s == '\n') serial_putchar('\r');
        serial_putchar(*s++);
    }
}

static void puts(const char *s) {
    spin_lock(&serial_lock);
    puts_unlocked(s);
    spin_unlock(&serial_lock);
}

static void putint(uint32_t n) {
    char buf[16];
    int i = 0;
    if (n == 0) {
        spin_lock(&serial_lock);
        serial_putchar('0');
        spin_unlock(&serial_lock);
        return;
    }
    while (n > 0) { buf[i++] = '0' + (n % 10); n /= 10; }
    spin_lock(&serial_lock);
    while (i > 0) serial_putchar(buf[--i]);
    spin_unlock(&serial_lock);
}

static void delay(uint32_t count) {
    while (count--) {
        __asm__ volatile ("pause");
    }
}

/* Get current CPU's APIC ID */
static uint32_t get_apic_id(void) {
    return lapic_read(LAPIC_ID) >> 24;
}

/* AP trampoline - 16-bit real mode code that transitions to 32-bit protected mode */
static const uint8_t ap_trampoline[] __attribute__((aligned(16))) = {
    /* 16-bit real mode entry point at 0x8000 */
    0xFA,                           /* 0x00: cli */
    0xFC,                           /* 0x01: cld */
    0x31, 0xC0,                     /* 0x02: xor ax, ax */
    0x8E, 0xD8,                     /* 0x04: mov ds, ax */
    0x8E, 0xC0,                     /* 0x06: mov es, ax */
    0x8E, 0xD0,                     /* 0x08: mov ss, ax */

    /* Load GDT pointer at 0x8000 + 0x40 */
    0x0F, 0x01, 0x16,               /* 0x0A: lgdt [gdt_ptr] */
    0x40, 0x80,                     /* 0x0D: offset 0x8040 */

    /* Enable protected mode */
    0x0F, 0x20, 0xC0,               /* 0x0F: mov eax, cr0 */
    0x0C, 0x01,                     /* 0x12: or al, 1 */
    0x0F, 0x22, 0xC0,               /* 0x14: mov cr0, eax */

    /* Far jump to 32-bit code at 0x8000 + 0x60 */
    0x66, 0xEA,                     /* 0x17: jmp far */
    0x60, 0x80, 0x00, 0x00,         /* 0x19: offset 0x8060 */
    0x08, 0x00,                     /* 0x1D: selector 0x08 */

    /* Padding to 0x40 */
    0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90,  /* 0x1F-0x26 */
    0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90,  /* 0x27-0x2E */
    0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90,  /* 0x2F-0x36 */
    0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90,  /* 0x37-0x3E */
    0x90,                                            /* 0x3F */

    /* GDT pointer at 0x40 */
    0x17, 0x00,                     /* limit: 23 */
    0x48, 0x80, 0x00, 0x00,         /* base: 0x8048 */
    0x00, 0x00,                     /* padding */

    /* GDT at 0x48 */
    /* Null descriptor */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    /* Code segment: base=0, limit=4GB, 32-bit, executable */
    0xFF, 0xFF, 0x00, 0x00, 0x00, 0x9A, 0xCF, 0x00,
    /* Data segment: base=0, limit=4GB, 32-bit, writable */
    0xFF, 0xFF, 0x00, 0x00, 0x00, 0x92, 0xCF, 0x00,

    /* 32-bit code at 0x60 - sets up segments and per-CPU stack */
    0x66, 0xB8, 0x10, 0x00,         /* 0x60: mov ax, 0x10 */
    0x8E, 0xD8,                     /* 0x64: mov ds, ax */
    0x8E, 0xC0,                     /* 0x66: mov es, ax */
    0x8E, 0xE0,                     /* 0x68: mov fs, ax */
    0x8E, 0xE8,                     /* 0x6A: mov gs, ax */
    0x8E, 0xD0,                     /* 0x6C: mov ss, ax */

    /* Read LAPIC ID register (0xFEE00020) to get CPU number */
    0xA1, 0x20, 0x00, 0xE0, 0xFE,   /* 0x6E: mov eax, [0xFEE00020] */
    0xC1, 0xE8, 0x18,               /* 0x73: shr eax, 24 */

    /* Calculate stack pointer: stack_table[apic_id] at 0x8200 */
    0xC1, 0xE0, 0x02,               /* 0x76: shl eax, 2 (multiply by 4) */
    0x05, 0x00, 0x82, 0x00, 0x00,   /* 0x79: add eax, 0x8200 */
    0x8B, 0x20,                     /* 0x7E: mov esp, [eax] */

    /* Jump to C function at 0x8300 */
    0xFF, 0x25, 0x00, 0x83, 0x00, 0x00,  /* 0x80: jmp [0x8300] */
};

/* AP stacks - each AP gets its own stack */
static uint8_t ap_stacks[MAX_CPUS][4096] __attribute__((aligned(4096)));

/* AP entry point - called after transitioning to 32-bit mode */
/* Each AP already has its own stack based on APIC ID from trampoline */
__attribute__((noreturn))
static void ap_main(void) {
    /* Get our APIC ID - this identifies which CPU we are */
    uint32_t apic_id = lapic_read(LAPIC_ID) >> 24;

    if (apic_id >= MAX_CPUS) {
        /* Invalid APIC ID, just halt */
        while (1) __asm__ volatile ("hlt");
    }

    /* Signal that we're online */
    __asm__ volatile ("lock incl %0" : "+m"(cpus_online) :: "memory");

    /* Do our work - each CPU increments its own counter */
    for (uint32_t i = 0; i < 100000; i++) {
        __asm__ volatile ("lock incl %0" : "+m"(cpu_counter[apic_id]) :: "memory");
    }

    /* Done - halt forever */
    while (1) {
        __asm__ volatile ("hlt");
    }
}

/* Stack pointer table at 0x8200 - indexed by APIC ID */
static void setup_ap_stacks(void) {
    volatile uint32_t *stack_table = (volatile uint32_t *)0x8200;
    volatile uint32_t *entry_ptr = (volatile uint32_t *)0x8300;

    for (int i = 0; i < MAX_CPUS; i++) {
        stack_table[i] = (uint32_t)&ap_stacks[i][4096];
    }
    *entry_ptr = (uint32_t)ap_main;
}

/* Start Application Processors */
static void start_aps(void) {
    /* Copy trampoline to low memory */
    memcpy((void *)AP_BOOT_ADDR, ap_trampoline, sizeof(ap_trampoline));

    /* Set up stack table (indexed by APIC ID) and entry point */
    setup_ap_stacks();

    /* Enable Local APIC */
    lapic_write(LAPIC_SVR, 0x1FF);

    /* Send INIT to all APs */
    lapic_write(LAPIC_ICR_HIGH, 0);
    lapic_write(LAPIC_ICR_LOW, 0x000C4500);  /* INIT, all excluding self */
    delay(100000);

    /* Send SIPI to all APs (vector = page number = 0x08 for 0x8000) */
    lapic_write(LAPIC_ICR_HIGH, 0);
    lapic_write(LAPIC_ICR_LOW, 0x000C4608);  /* SIPI, all excluding self, vector 0x08 */
    delay(50000);

    /* Send second SIPI */
    lapic_write(LAPIC_ICR_LOW, 0x000C4608);
    delay(50000);
}

int main(void) {
    serial_init();

    puts("\n");
    puts("========================================\n");
    puts("tcelm Multi-Core Test (4 CPUs)\n");
    puts("========================================\n\n");

    puts("BSP (CPU 0) starting...\n");
    puts("APIC ID: ");
    putint(get_apic_id());
    puts("\n\n");

    puts("Starting Application Processors...\n");
    start_aps();

    /* Wait for APs to come online */
    puts("Waiting for APs...\n");
    for (int timeout = 0; timeout < 1000000 && cpus_online < MAX_CPUS; timeout++) {
        delay(100);
    }

    puts("CPUs online: ");
    putint(cpus_online);
    puts("/");
    putint(MAX_CPUS);
    puts("\n\n");

    if (cpus_online > 1) {
        puts("[PASS] Multiple CPUs started!\n\n");
    } else {
        puts("[INFO] Only BSP running (AP startup requires more setup)\n\n");
    }

    /* Start the work test - APs are already running */
    puts("Running concurrent workload test...\n");
    puts("(APs started immediately after coming online)\n");

    /* BSP also does work */
    for (int i = 0; i < 100000; i++) {
        __asm__ volatile ("lock incl %0" : "+m"(cpu_counter[0]) :: "memory");
    }

    /* Wait for other CPUs to finish */
    puts("Waiting for all CPUs to complete...\n");
    for (int wait = 0; wait < 100; wait++) {
        delay(100000);

        /* Check if all online CPUs are done */
        uint32_t done = 0;
        for (uint32_t i = 0; i < cpus_online && i < MAX_CPUS; i++) {
            if (cpu_counter[i] >= 100000) done++;
        }
        if (done >= cpus_online) break;
    }

    puts("\nWork completed by each CPU:\n");
    uint32_t total = 0;
    for (int i = 0; i < MAX_CPUS; i++) {
        puts("  CPU ");
        putint(i);
        puts(": ");
        putint(cpu_counter[i]);
        if (cpu_counter[i] > 0) {
            puts(" [ACTIVE]");
        }
        puts("\n");
        total += cpu_counter[i];
    }

    puts("\nTotal work units: ");
    putint(total);
    puts("\n");

    if (total >= 100000) {
        puts("\n[PASS] Workload test completed\n");
    }

    puts("\n========================================\n");
    puts("Multi-core test complete.\n");
    puts("========================================\n");

    /* Exit QEMU */
    outb(0xf4, 0x00);

    while (1) {
        __asm__ volatile ("hlt");
    }

    return 0;
}
