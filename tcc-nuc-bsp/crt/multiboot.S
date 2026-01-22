/*
 * multiboot.S - Multiboot header for QEMU direct boot
 */

/* Multiboot constants (pre-calculated) */
.set MULTIBOOT_MAGIC,    0x1BADB002
.set MULTIBOOT_FLAGS,    0x00000003   /* PAGE_ALIGN | MEMORY_INFO */
.set MULTIBOOT_CHECKSUM, 0xE4524FFB   /* -(MAGIC + FLAGS) */

    .section .multiboot, "ax"
    .align 4

/* Multiboot header - must be in first 8KB of image */
multiboot_header:
    .long   MULTIBOOT_MAGIC
    .long   MULTIBOOT_FLAGS
    .long   MULTIBOOT_CHECKSUM

    .text
    .global _start
    .type _start, @function

/*
 * _start - Multiboot entry point
 */
_start:
    cli
    movl    $stack_top, %esp
    xorl    %ebp, %ebp
    pushl   %ebx    /* multiboot info */
    pushl   %eax    /* magic */
    call    kernel_main
halt:
    cli
    hlt
    jmp     halt

    .size _start, . - _start

/* Stack - 16KB */
    .section .bss
    .align 16
stack_bottom:
    .skip 16384
stack_top:
