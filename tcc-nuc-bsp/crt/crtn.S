/*
 * crtn.S - C runtime init section epilogue (32-bit i386)
 *
 * This provides the epilogue for .init and .fini sections.
 * The linker combines crti.o + user code + crtn.o to form
 * complete _init and _fini functions.
 */

    .section .init, "ax", @progbits
    /* Epilogue for _init */
    popl    %ebp
    ret

    .section .fini, "ax", @progbits
    /* Epilogue for _fini */
    popl    %ebp
    ret
