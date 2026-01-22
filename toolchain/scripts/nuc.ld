/*
 * nuc.ld - Linker script for tcelm on Intel NUC (32-bit)
 *
 * Memory layout for multiboot-compatible kernel:
 *   0x00100000 - Start of kernel (1MB mark, above BIOS)
 *   .multiboot - Multiboot header (must be in first 8KB)
 *   .text      - Code
 *   .rodata    - Read-only data
 *   .data      - Initialized data
 *   .bss       - Uninitialized data
 */

OUTPUT_FORMAT("elf32-i386")
OUTPUT_ARCH(i386)
ENTRY(_start)

SECTIONS
{
    /* Start at 1MB (standard location for multiboot kernels) */
    . = 0x00100000;

    /* Multiboot header must be first (within first 8KB) */
    .multiboot ALIGN(4) : {
        *(.multiboot)
    }

    /* Code section */
    .text ALIGN(4096) : {
        __text_start = .;
        *(.text)
        *(.text.*)
        __text_end = .;
    }

    /* Read-only data */
    .rodata ALIGN(4096) : {
        __rodata_start = .;
        *(.rodata)
        *(.rodata.*)
        __rodata_end = .;
    }

    /* Initialized data */
    .data ALIGN(4096) : {
        __data_start = .;
        *(.data)
        *(.data.*)
        __data_end = .;
    }

    /* Uninitialized data */
    .bss ALIGN(4096) : {
        __bss_start = .;
        *(.bss)
        *(.bss.*)
        *(COMMON)
        __bss_end = .;
    }

    /* Stack (grows down from end of BSS) */
    . = ALIGN(16);
    . += 0x10000;  /* 64KB stack */
    __stack_top = .;

    /* End of kernel */
    __kernel_end = .;

    /* Discard unwanted sections */
    /DISCARD/ : {
        *(.comment)
        *(.note*)
        *(.eh_frame*)
    }
}

/* Provide symbols for C code */
PROVIDE(__heap_start = __kernel_end);
PROVIDE(__heap_end = 0x10000000);  /* 256MB heap limit */
