/*
 * crti.S - C runtime init section prologue (32-bit i386)
 *
 * This provides the prologue for .init and .fini sections.
 * The linker combines crti.o + user code + crtn.o to form
 * complete _init and _fini functions.
 */

    .section .init, "ax", @progbits
    .global _init
    .type _init, @function
_init:
    pushl   %ebp
    movl    %esp, %ebp
    /* User .init code goes here (inserted by linker) */

    .section .fini, "ax", @progbits
    .global _fini
    .type _fini, @function
_fini:
    pushl   %ebp
    movl    %esp, %ebp
    /* User .fini code goes here (inserted by linker) */
