/*
 * ipi_test.c - Inter-Processor Interrupt (IPI) demonstration
 *
 * This test demonstrates cores signaling each other using IPIs via the LAPIC.
 *
 * Build:
 *   make -C toolchain test-ipi
 *
 * Run:
 *   qemu-system-i386 -kernel toolchain/build/ipi_test.elf -smp 4 -nographic
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
#define LAPIC_EOI       0x0B0
#define LAPIC_SVR       0x0F0
#define LAPIC_ICR_LOW   0x300
#define LAPIC_ICR_HIGH  0x310
#define LAPIC_LVT_TIMER 0x320
#define LAPIC_TIMER_ICR 0x380
#define LAPIC_TIMER_CCR 0x390
#define LAPIC_TIMER_DCR 0x3E0

/* ICR delivery modes */
#define ICR_FIXED       0x00000000
#define ICR_LOWEST      0x00000100
#define ICR_SMI         0x00000200
#define ICR_NMI         0x00000400
#define ICR_INIT        0x00000500
#define ICR_STARTUP     0x00000600

/* ICR flags */
#define ICR_PHYSICAL    0x00000000
#define ICR_LOGICAL     0x00000800
#define ICR_IDLE        0x00000000
#define ICR_PENDING     0x00001000
#define ICR_DEASSERT    0x00000000
#define ICR_ASSERT      0x00004000
#define ICR_EDGE        0x00000000
#define ICR_LEVEL       0x00008000

/* IPI vectors - use vectors 0x40-0x4F for our IPIs */
#define IPI_VECTOR_PING     0x40
#define IPI_VECTOR_PONG     0x41
#define IPI_VECTOR_WORK     0x42
#define IPI_VECTOR_DONE     0x43

/* AP startup address */
#define AP_BOOT_ADDR    0x8000

/* Maximum CPUs */
#define MAX_CPUS 4

/* Per-CPU data */
static volatile uint32_t cpus_online = 1;
static volatile uint32_t ipi_received[MAX_CPUS] = {0};
static volatile uint32_t ipi_ping_count[MAX_CPUS] = {0};
static volatile uint32_t ipi_pong_count[MAX_CPUS] = {0};
static volatile uint32_t work_requests[MAX_CPUS] = {0};
static volatile uint32_t work_results[MAX_CPUS] = {0};

/* Message passing via shared memory */
static volatile uint32_t message_data[MAX_CPUS] = {0};
static volatile uint32_t message_ready[MAX_CPUS] = {0};

/* Spinlock for serial output */
static volatile uint32_t serial_lock = 0;

/* IDT and GDT structures */
struct idt_entry {
    uint16_t offset_low;
    uint16_t selector;
    uint8_t  zero;
    uint8_t  type_attr;
    uint16_t offset_high;
} __attribute__((packed));

struct idt_ptr {
    uint16_t limit;
    uint32_t base;
} __attribute__((packed));

/* IDT with 256 entries */
static struct idt_entry idt[256] __attribute__((aligned(8)));
static struct idt_ptr idtp;

/* Forward declarations for interrupt handlers */
void ipi_handler_ping(void);
void ipi_handler_pong(void);
void ipi_handler_work(void);
void ipi_handler_done(void);
void default_handler(void);

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

static inline void lapic_eoi(void) {
    lapic_write(LAPIC_EOI, 0);
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

static void puthex(uint32_t n) {
    const char *hex = "0123456789ABCDEF";
    spin_lock(&serial_lock);
    for (int i = 7; i >= 0; i--) {
        serial_putchar(hex[(n >> (i * 4)) & 0xF]);
    }
    spin_unlock(&serial_lock);
}

static void delay(uint32_t count) {
    while (count--) {
        __asm__ volatile ("pause");
    }
}

static uint32_t get_apic_id(void) {
    return lapic_read(LAPIC_ID) >> 24;
}

/* Set up an IDT entry */
static void idt_set_gate(uint8_t num, uint32_t handler, uint16_t sel, uint8_t flags) {
    idt[num].offset_low = handler & 0xFFFF;
    idt[num].offset_high = (handler >> 16) & 0xFFFF;
    idt[num].selector = sel;
    idt[num].zero = 0;
    idt[num].type_attr = flags;
}

/* IPI interrupt handlers - these are called from assembly stubs */
void ipi_ping_handler_c(void) {
    uint32_t cpu = get_apic_id();
    ipi_received[cpu]++;
    ipi_ping_count[cpu]++;

    /* Send pong back to CPU 0 */
    if (cpu != 0) {
        /* Set destination to CPU 0 */
        lapic_write(LAPIC_ICR_HIGH, 0 << 24);
        /* Send PONG IPI */
        lapic_write(LAPIC_ICR_LOW, ICR_FIXED | ICR_PHYSICAL | ICR_EDGE | ICR_ASSERT | IPI_VECTOR_PONG);
    }

    lapic_eoi();
}

void ipi_pong_handler_c(void) {
    uint32_t cpu = get_apic_id();
    ipi_received[cpu]++;
    ipi_pong_count[cpu]++;
    lapic_eoi();
}

void ipi_work_handler_c(void) {
    uint32_t cpu = get_apic_id();
    ipi_received[cpu]++;

    /* Get work data from message buffer */
    uint32_t data = message_data[cpu];

    /* Do some "work" - compute square */
    uint32_t result = data * data;

    /* Store result */
    work_results[cpu] = result;
    work_requests[cpu]++;

    /* Signal done to CPU 0 */
    lapic_write(LAPIC_ICR_HIGH, 0 << 24);
    lapic_write(LAPIC_ICR_LOW, ICR_FIXED | ICR_PHYSICAL | ICR_EDGE | ICR_ASSERT | IPI_VECTOR_DONE);

    lapic_eoi();
}

void ipi_done_handler_c(void) {
    uint32_t cpu = get_apic_id();
    ipi_received[cpu]++;
    message_ready[cpu]++;
    lapic_eoi();
}

/* Default handler for spurious/unhandled interrupts */
void default_handler_c(void) {
    lapic_eoi();
}

/* Assembly interrupt stubs - save registers and call C handlers */
__asm__(
    ".global ipi_handler_ping\n"
    "ipi_handler_ping:\n"
    "    pusha\n"
    "    call ipi_ping_handler_c\n"
    "    popa\n"
    "    iret\n"

    ".global ipi_handler_pong\n"
    "ipi_handler_pong:\n"
    "    pusha\n"
    "    call ipi_pong_handler_c\n"
    "    popa\n"
    "    iret\n"

    ".global ipi_handler_work\n"
    "ipi_handler_work:\n"
    "    pusha\n"
    "    call ipi_work_handler_c\n"
    "    popa\n"
    "    iret\n"

    ".global ipi_handler_done\n"
    "ipi_handler_done:\n"
    "    pusha\n"
    "    call ipi_done_handler_c\n"
    "    popa\n"
    "    iret\n"

    ".global default_handler\n"
    "default_handler:\n"
    "    pusha\n"
    "    call default_handler_c\n"
    "    popa\n"
    "    iret\n"
);

/* Initialize IDT */
static void idt_init(void) {
    /* Set default handler for ALL vectors first */
    for (int i = 0; i < 256; i++) {
        idt_set_gate(i, (uint32_t)default_handler, 0x08, 0x8E);
    }

    /* Set up IPI handlers - interrupt gate (0x8E = present, ring 0, 32-bit interrupt gate) */
    idt_set_gate(IPI_VECTOR_PING, (uint32_t)ipi_handler_ping, 0x08, 0x8E);
    idt_set_gate(IPI_VECTOR_PONG, (uint32_t)ipi_handler_pong, 0x08, 0x8E);
    idt_set_gate(IPI_VECTOR_WORK, (uint32_t)ipi_handler_work, 0x08, 0x8E);
    idt_set_gate(IPI_VECTOR_DONE, (uint32_t)ipi_handler_done, 0x08, 0x8E);

    /* Set up IDT pointer */
    idtp.limit = sizeof(idt) - 1;
    idtp.base = (uint32_t)&idt;

    /* Load IDT */
    __asm__ volatile ("lidt %0" : : "m"(idtp));
}

/* Initialize LAPIC for receiving interrupts */
static void lapic_init(void) {
    /* Enable LAPIC with spurious vector 0xFF */
    lapic_write(LAPIC_SVR, 0x1FF);
}

/* Send IPI to a specific CPU */
static void send_ipi(uint32_t dest_cpu, uint32_t vector) {
    /* Wait for any pending IPI to complete */
    while (lapic_read(LAPIC_ICR_LOW) & ICR_PENDING) {
        __asm__ volatile ("pause");
    }

    /* Set destination */
    lapic_write(LAPIC_ICR_HIGH, dest_cpu << 24);

    /* Send IPI */
    lapic_write(LAPIC_ICR_LOW, ICR_FIXED | ICR_PHYSICAL | ICR_EDGE | ICR_ASSERT | vector);
}

/* Send IPI to all other CPUs (broadcast excluding self) */
static void send_ipi_broadcast(uint32_t vector) {
    while (lapic_read(LAPIC_ICR_LOW) & ICR_PENDING) {
        __asm__ volatile ("pause");
    }

    lapic_write(LAPIC_ICR_HIGH, 0);
    /* 0xC0000 = all excluding self */
    lapic_write(LAPIC_ICR_LOW, ICR_FIXED | ICR_PHYSICAL | ICR_EDGE | ICR_ASSERT | 0x000C0000 | vector);
}

/* AP trampoline code */
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
    0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90,
    0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90,
    0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90,
    0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90,
    0x90,

    /* GDT pointer at 0x40 */
    0x17, 0x00,                     /* limit: 23 */
    0x48, 0x80, 0x00, 0x00,         /* base: 0x8048 */
    0x00, 0x00,                     /* padding */

    /* GDT at 0x48 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  /* Null */
    0xFF, 0xFF, 0x00, 0x00, 0x00, 0x9A, 0xCF, 0x00,  /* Code */
    0xFF, 0xFF, 0x00, 0x00, 0x00, 0x92, 0xCF, 0x00,  /* Data */

    /* 32-bit code at 0x60 */
    0x66, 0xB8, 0x10, 0x00,         /* 0x60: mov ax, 0x10 */
    0x8E, 0xD8,                     /* 0x64: mov ds, ax */
    0x8E, 0xC0,                     /* 0x66: mov es, ax */
    0x8E, 0xE0,                     /* 0x68: mov fs, ax */
    0x8E, 0xE8,                     /* 0x6A: mov gs, ax */
    0x8E, 0xD0,                     /* 0x6C: mov ss, ax */

    /* Read LAPIC ID and load per-CPU stack */
    0xA1, 0x20, 0x00, 0xE0, 0xFE,   /* 0x6E: mov eax, [0xFEE00020] */
    0xC1, 0xE8, 0x18,               /* 0x73: shr eax, 24 */
    0xC1, 0xE0, 0x02,               /* 0x76: shl eax, 2 */
    0x05, 0x00, 0x82, 0x00, 0x00,   /* 0x79: add eax, 0x8200 */
    0x8B, 0x20,                     /* 0x7E: mov esp, [eax] */

    /* Jump to C function at 0x8300 */
    0xFF, 0x25, 0x00, 0x83, 0x00, 0x00,
};

/* AP stacks */
static uint8_t ap_stacks[MAX_CPUS][4096] __attribute__((aligned(4096)));

/* Volatile flag for AP setup synchronization */
static volatile uint32_t aps_ready = 0;

/* AP entry point */
__attribute__((noreturn))
static void ap_main(void) {
    uint32_t cpu = get_apic_id();

    /* Signal online FIRST */
    __asm__ volatile ("lock incl %0" : "+m"(cpus_online) :: "memory");

    /* Wait for BSP to signal that setup is complete */
    while (!aps_ready) {
        __asm__ volatile ("pause");
    }

    /* Initialize LAPIC for this CPU */
    lapic_init();

    /* Load IDT (shared with BSP) */
    __asm__ volatile ("lidt %0" : : "m"(idtp));

    /* Enable interrupts */
    __asm__ volatile ("sti");

    /* Wait for IPIs - hlt will wake on interrupt */
    while (1) {
        __asm__ volatile ("hlt");
    }
}

static void setup_ap_stacks(void) {
    volatile uint32_t *stack_table = (volatile uint32_t *)0x8200;
    volatile uint32_t *entry_ptr = (volatile uint32_t *)0x8300;

    for (int i = 0; i < MAX_CPUS; i++) {
        stack_table[i] = (uint32_t)&ap_stacks[i][4096];
    }
    *entry_ptr = (uint32_t)ap_main;
}

static void start_aps(void) {
    memcpy((void *)AP_BOOT_ADDR, ap_trampoline, sizeof(ap_trampoline));
    setup_ap_stacks();

    lapic_write(LAPIC_SVR, 0x1FF);

    /* INIT */
    lapic_write(LAPIC_ICR_HIGH, 0);
    lapic_write(LAPIC_ICR_LOW, 0x000C4500);
    delay(100000);

    /* SIPI */
    lapic_write(LAPIC_ICR_HIGH, 0);
    lapic_write(LAPIC_ICR_LOW, 0x000C4608);
    delay(50000);

    /* Second SIPI */
    lapic_write(LAPIC_ICR_LOW, 0x000C4608);
    delay(50000);
}

int main(void) {
    serial_init();

    puts("\n");
    puts("========================================\n");
    puts("tcelm IPI (Inter-Processor Interrupt) Demo\n");
    puts("========================================\n\n");

    puts("BSP (CPU 0) initializing...\n");
    puts("APIC ID: ");
    putint(get_apic_id());
    puts("\n\n");

    /* Initialize IDT (but don't enable interrupts yet) */
    puts("Setting up IDT for IPI vectors 0x40-0x43...\n");
    idt_init();

    /* Start APs (this also initializes LAPIC on BSP) */
    puts("Starting Application Processors...\n");
    start_aps();

    /* Wait for APs with shorter timeout */
    for (int timeout = 0; timeout < 100000 && cpus_online < MAX_CPUS; timeout++) {
        delay(100);
    }

    puts("CPUs online: ");
    putint(cpus_online);
    puts("/");
    putint(MAX_CPUS);
    puts("\n");

    if (cpus_online < 2) {
        puts("[FAIL] Need at least 2 CPUs for IPI test\n");
        goto done;
    }

    puts("[PASS] Multiple CPUs started!\n\n");

    /* Signal APs to complete their setup (load IDT, enable interrupts) */
    puts("Enabling interrupts on all CPUs...\n");
    aps_ready = 1;  /* Signal APs */

    /* Wait for APs to complete IDT setup */
    delay(200000);

    /* Now enable interrupts on BSP */
    __asm__ volatile ("sti");

    puts("[PASS] Interrupts enabled on all CPUs!\n\n");

    /* ============================================ */
    /* Test 1: Ping-Pong between CPU 0 and CPU 1   */
    /* ============================================ */
    puts("----------------------------------------\n");
    puts("Test 1: Ping-Pong IPI between CPU 0 and CPU 1\n");
    puts("----------------------------------------\n");

    uint32_t initial_pong = ipi_pong_count[0];

    puts("  Sending PING to CPU 1...\n");
    send_ipi(1, IPI_VECTOR_PING);

    /* Wait for pong response */
    for (int i = 0; i < 100000 && ipi_pong_count[0] == initial_pong; i++) {
        delay(100);
    }

    if (ipi_pong_count[0] > initial_pong) {
        puts("  Received PONG from CPU 1!\n");
        puts("  [PASS] Ping-Pong test\n\n");
    } else {
        puts("  [FAIL] No PONG received\n\n");
    }

    /* ============================================ */
    /* Test 2: Broadcast IPI to all cores          */
    /* ============================================ */
    puts("----------------------------------------\n");
    puts("Test 2: Broadcast PING to all cores\n");
    puts("----------------------------------------\n");

    uint32_t ping_before[MAX_CPUS];
    for (int i = 0; i < MAX_CPUS; i++) {
        ping_before[i] = ipi_ping_count[i];
    }

    puts("  Broadcasting PING to all other CPUs...\n");
    send_ipi_broadcast(IPI_VECTOR_PING);

    delay(500000);

    puts("  PING received by:\n");
    int broadcast_success = 0;
    for (int i = 1; i < MAX_CPUS; i++) {
        puts("    CPU ");
        putint(i);
        puts(": ");
        if (ipi_ping_count[i] > ping_before[i]) {
            puts("YES\n");
            broadcast_success++;
        } else {
            puts("NO\n");
        }
    }

    if (broadcast_success >= (int)cpus_online - 1) {
        puts("  [PASS] Broadcast IPI test\n\n");
    } else {
        puts("  [FAIL] Not all CPUs received broadcast\n\n");
    }

    /* ============================================ */
    /* Test 3: Work distribution via IPI           */
    /* ============================================ */
    puts("----------------------------------------\n");
    puts("Test 3: Distribute work via IPI\n");
    puts("----------------------------------------\n");

    puts("  Sending work (compute squares) to each CPU...\n");

    /* Send work to CPUs 1, 2, 3 */
    for (uint32_t cpu = 1; cpu < cpus_online && cpu < MAX_CPUS; cpu++) {
        message_data[cpu] = cpu + 5;  /* Compute (cpu+5)^2 */
        uint32_t before = message_ready[0];

        puts("    CPU ");
        putint(cpu);
        puts(": compute ");
        putint(cpu + 5);
        puts("^2 = ");

        send_ipi(cpu, IPI_VECTOR_WORK);

        /* Wait for done signal */
        for (int i = 0; i < 100000 && message_ready[0] == before; i++) {
            delay(100);
        }

        putint(work_results[cpu]);

        if (work_results[cpu] == (cpu + 5) * (cpu + 5)) {
            puts(" [OK]\n");
        } else {
            puts(" [WRONG]\n");
        }
    }

    puts("  [PASS] Work distribution test\n\n");

    /* ============================================ */
    /* Test 4: Multiple rapid IPIs                 */
    /* ============================================ */
    puts("----------------------------------------\n");
    puts("Test 4: Rapid IPI stress test (100 IPIs)\n");
    puts("----------------------------------------\n");

    uint32_t rapid_before = ipi_ping_count[1];

    puts("  Sending 100 PINGs to CPU 1...\n");
    for (int i = 0; i < 100; i++) {
        send_ipi(1, IPI_VECTOR_PING);
        /* Small delay between IPIs */
        for (int j = 0; j < 1000; j++) {
            __asm__ volatile ("pause");
        }
    }

    delay(500000);

    uint32_t rapid_received = ipi_ping_count[1] - rapid_before;
    puts("  CPU 1 received: ");
    putint(rapid_received);
    puts("/100 PINGs\n");

    if (rapid_received >= 95) {  /* Allow small margin */
        puts("  [PASS] Rapid IPI test\n\n");
    } else {
        puts("  [FAIL] Too many IPIs lost\n\n");
    }

    /* ============================================ */
    /* Summary                                     */
    /* ============================================ */
    puts("========================================\n");
    puts("IPI Statistics:\n");
    puts("========================================\n");

    for (uint32_t i = 0; i < cpus_online && i < MAX_CPUS; i++) {
        puts("  CPU ");
        putint(i);
        puts(": ");
        putint(ipi_received[i]);
        puts(" IPIs received (");
        putint(ipi_ping_count[i]);
        puts(" ping, ");
        putint(ipi_pong_count[i]);
        puts(" pong, ");
        putint(work_requests[i]);
        puts(" work)\n");
    }

    puts("\n========================================\n");
    puts("IPI Demo Complete!\n");
    puts("========================================\n");

done:
    /* Exit QEMU */
    outb(0xf4, 0x00);

    while (1) {
        __asm__ volatile ("hlt");
    }

    return 0;
}
