/*
 * crt0.S - C runtime startup for tcelm/NUC toolchain
 *
 * This provides:
 *   - Multiboot header for QEMU/GRUB boot
 *   - Stack setup
 *   - BSS clearing
 *   - Call to main()
 *   - Halt on return
 */

/* Multiboot constants */
.set MULTIBOOT_MAGIC,    0x1BADB002
.set MULTIBOOT_FLAGS,    0x00000003  /* PAGE_ALIGN | MEMORY_INFO */
.set MULTIBOOT_CHECKSUM, -(MULTIBOOT_MAGIC + MULTIBOOT_FLAGS)

/* Multiboot header (must be in first 8KB) */
    .section .multiboot, "ax"
    .align 4

multiboot_header:
    .long   MULTIBOOT_MAGIC
    .long   MULTIBOOT_FLAGS
    .long   MULTIBOOT_CHECKSUM

/* Entry point */
    .section .text
    .global _start
    .type _start, @function

_start:
    /* Disable interrupts */
    cli

    /* Set up stack */
    movl    $__stack_top, %esp
    xorl    %ebp, %ebp

    /* Save multiboot info */
    pushl   $0              /* Placeholder for argv */
    pushl   $0              /* Placeholder for argc */
    pushl   %ebx            /* Multiboot info pointer */
    pushl   %eax            /* Multiboot magic */

    /* Clear BSS section */
    movl    $__bss_start, %edi
    movl    $__bss_end, %ecx
    subl    %edi, %ecx
    shrl    $2, %ecx        /* Count in 32-bit words */
    xorl    %eax, %eax
    rep stosl

    /* Initialize RTEMS-compatible runtime */
    call    rtems_initialize_executive

    /* Call main(argc, argv) or main(void) */
    /* We pass multiboot magic and info as argc/argv for compatibility */
    popl    %eax            /* magic */
    popl    %ebx            /* info */
    pushl   %ebx
    pushl   %eax
    call    main
    addl    $8, %esp

    /* If main returns, halt */
halt:
    cli
    hlt
    jmp     halt

    .size _start, . - _start

/*
 * Weak symbol for kernel_main (optional QEMU test compatibility)
 */
    .weak kernel_main
kernel_main:
    ret

/*
 * Stack area
 */
    .section .bss
    .align 16

stack_bottom:
    .space 65536            /* 64KB stack */
stack_top:

/* Export stack symbols */
    .global stack_top
    .global __stack_top
    .set __stack_top, stack_top
