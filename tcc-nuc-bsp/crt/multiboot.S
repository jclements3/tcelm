/*
 * multiboot.S - Multiboot header for QEMU/GRUB booting
 *
 * This allows the kernel to be loaded by QEMU -kernel or GRUB
 */

/* Multiboot header constants */
.set MULTIBOOT_MAGIC,       0x1BADB002
.set MULTIBOOT_FLAGS,       0x00000007  /* Align modules, memory info, video mode */
.set MULTIBOOT_CHECKSUM,    -(MULTIBOOT_MAGIC + MULTIBOOT_FLAGS)

/* Video mode request: SXGA 1280x1024x32 */
.set VIDEO_MODE_TYPE,       0           /* 0 = linear graphics mode */
.set VIDEO_WIDTH,           1280
.set VIDEO_HEIGHT,          1024
.set VIDEO_DEPTH,           32

/* Multiboot header - must be in first 8KB of kernel */
.section .multiboot, "ax"
.align 4
multiboot_header:
    .long MULTIBOOT_MAGIC
    .long MULTIBOOT_FLAGS
    .long MULTIBOOT_CHECKSUM
    /* Video mode fields (required when bit 2 of flags is set) */
    .long VIDEO_MODE_TYPE
    .long VIDEO_WIDTH
    .long VIDEO_HEIGHT
    .long VIDEO_DEPTH

/*
 * Boot entry point - called by bootloader
 * Sets up stack and jumps to _start
 */
.section .text.boot, "ax"
.global _boot
.global multiboot_info_ptr
.type _boot, @function
_boot:
    /* Disable interrupts */
    cli

    /* Set up stack */
    movl    $_stack_top, %esp

    /* Clear EFLAGS */
    pushl   $0
    popf

    /* Save multiboot info pointer for C code */
    movl    %ebx, multiboot_info_ptr

    /* Push multiboot info pointer (ebx) and magic (eax) */
    pushl   %ebx    /* Multiboot info structure */
    pushl   %eax    /* Multiboot magic number */

    /* Jump to C runtime startup */
    call    _start

    /* Should not return, but halt if it does */
1:  cli
    hlt
    jmp     1b

.size _boot, . - _boot

/*
 * Data section for multiboot info pointer
 */
.section .data
.align 4
multiboot_info_ptr:
    .long 0

/*
 * Stack for boot
 */
.section .bss
.align 16
_stack_bottom:
    .space 16384    /* 16 KB boot stack */
_stack_top:
