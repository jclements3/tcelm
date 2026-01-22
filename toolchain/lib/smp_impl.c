/*
 * smp_impl.c - SMP implementation for tcelm/NUC toolchain
 *
 * Provides multi-core startup and management for x86 systems.
 * Uses Local APIC for inter-processor communication.
 */

#include <smp.h>
#include <string.h>
#include <stdlib.h>

/* Local APIC registers (memory-mapped at 0xFEE00000) */
#define LAPIC_BASE          0xFEE00000
#define LAPIC_ID            0x020   /* Local APIC ID */
#define LAPIC_VERSION       0x030   /* Local APIC Version */
#define LAPIC_TPR           0x080   /* Task Priority Register */
#define LAPIC_EOI           0x0B0   /* End of Interrupt */
#define LAPIC_SVR           0x0F0   /* Spurious Interrupt Vector */
#define LAPIC_ESR           0x280   /* Error Status Register */
#define LAPIC_ICR_LOW       0x300   /* Interrupt Command Register (low) */
#define LAPIC_ICR_HIGH      0x310   /* Interrupt Command Register (high) */
#define LAPIC_TIMER         0x320   /* LVT Timer Register */
#define LAPIC_TIMER_INIT    0x380   /* Timer Initial Count */
#define LAPIC_TIMER_CURR    0x390   /* Timer Current Count */
#define LAPIC_TIMER_DIV     0x3E0   /* Timer Divide Configuration */

/* ICR delivery modes */
#define ICR_FIXED           0x00000000
#define ICR_INIT            0x00000500
#define ICR_STARTUP         0x00000600

/* ICR flags */
#define ICR_PHYSICAL        0x00000000
#define ICR_LOGICAL         0x00000800
#define ICR_PENDING         0x00001000
#define ICR_ASSERT          0x00004000
#define ICR_DEASSERT        0x00000000
#define ICR_EDGE            0x00000000
#define ICR_LEVEL           0x00008000

/* ICR destination shortcuts */
#define ICR_NO_SHORTHAND    0x00000000
#define ICR_SELF            0x00040000
#define ICR_ALL_INCL_SELF   0x00080000
#define ICR_ALL_EXCL_SELF   0x000C0000

/* AP startup code location (must be < 1MB, aligned to 4KB) */
#define AP_STARTUP_ADDR     0x8000
#define AP_STACK_SIZE       8192

/* Global SMP state */
percpu_t smp_percpu[SMP_MAX_CPUS];
volatile uint32_t smp_num_cpus = 1;  /* Start with BSP only */
volatile bool smp_initialized = false;

/* Synchronization for AP startup */
static volatile uint32_t ap_started = 0;
static spinlock_t smp_lock = SPINLOCK_INITIALIZER;

/* Read from Local APIC register */
static inline uint32_t lapic_read(uint32_t reg) {
    return *(volatile uint32_t *)(LAPIC_BASE + reg);
}

/* Write to Local APIC register */
static inline void lapic_write(uint32_t reg, uint32_t val) {
    *(volatile uint32_t *)(LAPIC_BASE + reg) = val;
}

/* Get this CPU's APIC ID */
static uint32_t lapic_id(void) {
    return lapic_read(LAPIC_ID) >> 24;
}

/* Initialize Local APIC */
static void lapic_init(void) {
    /* Enable Local APIC by setting spurious interrupt vector */
    lapic_write(LAPIC_SVR, 0x1FF);  /* Enable APIC + spurious vector 0xFF */

    /* Clear error status */
    lapic_write(LAPIC_ESR, 0);
    lapic_write(LAPIC_ESR, 0);

    /* Set task priority to 0 (accept all interrupts) */
    lapic_write(LAPIC_TPR, 0);

    /* Acknowledge any pending interrupts */
    lapic_write(LAPIC_EOI, 0);
}

/* Send IPI to specific CPU */
void smp_send_ipi(uint32_t cpu_id, uint32_t vector) {
    if (cpu_id >= SMP_MAX_CPUS) return;

    uint32_t apic_id = smp_percpu[cpu_id].apic_id;

    /* Wait for previous IPI to complete */
    while (lapic_read(LAPIC_ICR_LOW) & ICR_PENDING) {
        __asm__ volatile ("pause");
    }

    /* Set destination APIC ID */
    lapic_write(LAPIC_ICR_HIGH, apic_id << 24);

    /* Send IPI */
    lapic_write(LAPIC_ICR_LOW, vector | ICR_FIXED | ICR_PHYSICAL | ICR_ASSERT | ICR_EDGE);
}

/* Broadcast IPI to all CPUs except self */
void smp_broadcast_ipi(uint32_t vector) {
    /* Wait for previous IPI to complete */
    while (lapic_read(LAPIC_ICR_LOW) & ICR_PENDING) {
        __asm__ volatile ("pause");
    }

    /* Send to all excluding self */
    lapic_write(LAPIC_ICR_LOW, vector | ICR_FIXED | ICR_ALL_EXCL_SELF | ICR_ASSERT | ICR_EDGE);
}

/* Delay using a simple busy loop */
static void delay_us(uint32_t us) {
    /* Approximate delay - not accurate but sufficient for startup */
    volatile uint32_t count = us * 100;
    while (count--) {
        __asm__ volatile ("pause");
    }
}

/* AP trampoline code (16-bit real mode startup) */
/* This gets copied to AP_STARTUP_ADDR */
static const uint8_t ap_trampoline[] = {
    /* 16-bit real mode code */
    0xFA,                           /* cli */
    0x31, 0xC0,                     /* xor ax, ax */
    0x8E, 0xD8,                     /* mov ds, ax */

    /* Load GDT */
    0x0F, 0x01, 0x16,               /* lgdt [gdt_ptr] */
    0x20, 0x80,                     /* gdt_ptr offset (0x8020) */

    /* Enable protected mode */
    0x0F, 0x20, 0xC0,               /* mov eax, cr0 */
    0x0C, 0x01,                     /* or al, 1 */
    0x0F, 0x22, 0xC0,               /* mov cr0, eax */

    /* Far jump to 32-bit code */
    0x66, 0xEA,                     /* jmp far */
    0x30, 0x80, 0x00, 0x00,         /* offset 0x8030 */
    0x08, 0x00,                     /* selector 0x08 (code segment) */

    /* Padding to offset 0x20 (gdt_ptr) */
    0x00, 0x00, 0x00, 0x00,

    /* GDT pointer at offset 0x20 */
    0x17, 0x00,                     /* limit: 23 (3 entries * 8 - 1) */
    0x28, 0x80, 0x00, 0x00,         /* base: 0x8028 */

    /* GDT at offset 0x28 */
    /* Null descriptor */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    /* Code segment: base=0, limit=4GB, 32-bit, executable */
    0xFF, 0xFF, 0x00, 0x00, 0x00, 0x9A, 0xCF, 0x00,
    /* Data segment: base=0, limit=4GB, 32-bit, writable */
    0xFF, 0xFF, 0x00, 0x00, 0x00, 0x92, 0xCF, 0x00,

    /* 32-bit protected mode code at offset 0x30 */
    0x66, 0xB8, 0x10, 0x00,         /* mov ax, 0x10 (data selector) */
    0x8E, 0xD8,                     /* mov ds, ax */
    0x8E, 0xC0,                     /* mov es, ax */
    0x8E, 0xD0,                     /* mov ss, ax */

    /* Load stack pointer from known location */
    0x8B, 0x25,                     /* mov esp, [ap_stack_ptr] */
    0x00, 0x81, 0x00, 0x00,         /* address 0x8100 */

    /* Jump to AP entry function (address at 0x8104) */
    0xFF, 0x25,                     /* jmp [ap_entry_ptr] */
    0x04, 0x81, 0x00, 0x00,         /* address 0x8104 */
};

/* AP entry point (called from trampoline in 32-bit mode) */
static void ap_entry(void);

/* Start an Application Processor */
static bool start_ap(uint32_t apic_id, uint32_t cpu_id) {
    /* Allocate stack for this AP */
    void *stack = malloc(AP_STACK_SIZE);
    if (!stack) return false;

    /* Set up AP's stack pointer and entry point */
    *(volatile uint32_t *)0x8100 = (uint32_t)stack + AP_STACK_SIZE;
    *(volatile uint32_t *)0x8104 = (uint32_t)ap_entry;
    *(volatile uint32_t *)0x8108 = cpu_id;

    smp_percpu[cpu_id].stack = stack;
    smp_percpu[cpu_id].apic_id = apic_id;
    smp_percpu[cpu_id].state = CPU_STATE_STARTING;

    ap_started = 0;
    smp_wmb();

    /* Send INIT IPI */
    while (lapic_read(LAPIC_ICR_LOW) & ICR_PENDING);
    lapic_write(LAPIC_ICR_HIGH, apic_id << 24);
    lapic_write(LAPIC_ICR_LOW, ICR_INIT | ICR_PHYSICAL | ICR_ASSERT | ICR_LEVEL);

    delay_us(10000);  /* 10ms delay */

    /* Send INIT deassert */
    lapic_write(LAPIC_ICR_LOW, ICR_INIT | ICR_PHYSICAL | ICR_DEASSERT | ICR_LEVEL);

    delay_us(200);

    /* Send SIPI (Startup IPI) twice */
    for (int i = 0; i < 2; i++) {
        while (lapic_read(LAPIC_ICR_LOW) & ICR_PENDING);
        lapic_write(LAPIC_ICR_HIGH, apic_id << 24);
        /* Vector = page number of startup code (0x08 for 0x8000) */
        lapic_write(LAPIC_ICR_LOW, ICR_STARTUP | ICR_PHYSICAL | ICR_ASSERT | ICR_EDGE | 0x08);

        delay_us(200);
    }

    /* Wait for AP to start (with timeout) */
    for (int timeout = 0; timeout < 100000; timeout++) {
        if (ap_started) {
            return true;
        }
        delay_us(10);
    }

    /* AP failed to start */
    smp_percpu[cpu_id].state = CPU_STATE_OFFLINE;
    free(stack);
    smp_percpu[cpu_id].stack = NULL;
    return false;
}

/* AP entry point */
static void ap_entry(void) {
    /* Get our CPU ID */
    uint32_t cpu_id = *(volatile uint32_t *)0x8108;

    /* Initialize our Local APIC */
    lapic_init();

    /* Mark ourselves as online */
    smp_percpu[cpu_id].cpu_id = cpu_id;
    smp_percpu[cpu_id].apic_id = lapic_id();
    smp_percpu[cpu_id].state = CPU_STATE_ONLINE;
    smp_percpu[cpu_id].current_task = 0;
    smp_percpu[cpu_id].ticks = 0;

    /* Signal BSP that we're up */
    atomic_add(&smp_num_cpus, 1);
    ap_started = 1;
    smp_wmb();

    /* Enter idle loop - scheduler will give us work */
    while (1) {
        __asm__ volatile ("hlt");
    }
}

/* Detect number of CPUs using CPUID/ACPI */
static uint32_t detect_cpu_count(void) {
    /* Try CPUID first */
    uint32_t eax, ebx, ecx, edx;

    /* Check if CPUID supports leaf 0x0B (extended topology) */
    __asm__ volatile (
        "cpuid"
        : "=a"(eax), "=b"(ebx), "=c"(ecx), "=d"(edx)
        : "a"(0)
    );

    if (eax >= 0x0B) {
        /* Use extended topology enumeration */
        __asm__ volatile (
            "cpuid"
            : "=a"(eax), "=b"(ebx), "=c"(ecx), "=d"(edx)
            : "a"(0x0B), "c"(1)
        );

        if (ebx > 0 && ebx <= SMP_MAX_CPUS) {
            return ebx;
        }
    }

    /* Fall back to basic CPUID leaf 1 */
    __asm__ volatile (
        "cpuid"
        : "=a"(eax), "=b"(ebx), "=c"(ecx), "=d"(edx)
        : "a"(1)
    );

    /* EBX[23:16] contains max logical processors */
    uint32_t max_cpus = (ebx >> 16) & 0xFF;
    if (max_cpus > 0 && max_cpus <= SMP_MAX_CPUS) {
        return max_cpus;
    }

    /* Default: assume 4 cores for NUC */
    return SMP_MAX_CPUS;
}

/* Initialize SMP subsystem */
void smp_init(void) {
    if (smp_initialized) return;

    spinlock_acquire(&smp_lock);

    /* Initialize BSP's per-CPU data */
    memset(smp_percpu, 0, sizeof(smp_percpu));

    smp_percpu[0].cpu_id = 0;
    smp_percpu[0].apic_id = 0;  /* BSP is always APIC ID 0 in QEMU */
    smp_percpu[0].state = CPU_STATE_ONLINE;
    smp_percpu[0].current_task = 0;
    smp_percpu[0].ticks = 0;

    /* Detect number of CPUs using CPUID */
    uint32_t num_cpus = detect_cpu_count();

    /* For QEMU, we can detect SMP from CPUID but starting APs requires
     * more complex ACPI/MP table parsing and INIT-SIPI-SIPI sequences.
     * For now, we just record the detected CPU count but only use BSP.
     *
     * In a full implementation, we would:
     * 1. Parse ACPI MADT or MP tables to find APIC IDs
     * 2. Initialize Local APIC
     * 3. Send INIT-SIPI-SIPI to each AP
     * 4. Wait for APs to start and signal ready
     *
     * For testing purposes, we mark all CPUs as potentially available
     * but only CPU 0 (BSP) is actually online.
     */

    /* Mark other CPUs as offline but configured */
    for (uint32_t cpu = 1; cpu < num_cpus && cpu < SMP_MAX_CPUS; cpu++) {
        smp_percpu[cpu].cpu_id = cpu;
        smp_percpu[cpu].apic_id = cpu;
        smp_percpu[cpu].state = CPU_STATE_OFFLINE;
    }

    /* Record detected CPU count (for information purposes) */
    /* smp_num_cpus stays at 1 since only BSP is actually running */

    smp_initialized = true;
    spinlock_release(&smp_lock);
}

/* Get the number of configured CPUs (may not all be online) */
uint32_t smp_get_configured_cpus(void) {
    return detect_cpu_count();
}

/* Get current CPU's per-CPU data (alternative to inline version) */
percpu_t *smp_get_percpu(void) {
    return &smp_percpu[smp_cpu_id()];
}
