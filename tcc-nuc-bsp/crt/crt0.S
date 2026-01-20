/*
 * crt0.S - C runtime startup for TCC-NUC-BSP (32-bit i386)
 *
 * This is the entry point for TCC-compiled RTEMS applications.
 * It sets up the environment and calls the RTEMS Init task.
 *
 * For RTEMS, the actual startup is handled by the BSP. This CRT
 * provides the bridge between TCC's expectations and RTEMS.
 */

    .text
    .global _start
    .type _start, @function

/*
 * _start - Entry point
 *
 * RTEMS BSP calls this after hardware initialization.
 * We set up the C environment and call Init().
 */
_start:
    /* Clear frame pointer for stack traces */
    xorl    %ebp, %ebp

    /* Align stack to 16 bytes (i386 ABI requirement for SSE) */
    andl    $-16, %esp

    /* Call global constructors */
    call    __do_global_ctors

    /* Call RTEMS Init task (defined by application) */
    pushl   $0              /* arg = 0 */
    call    Init
    addl    $4, %esp        /* clean up stack */

    /* If Init returns, call destructors and exit */
    call    __do_global_dtors

    /* RTEMS: delete self (should not reach here) */
    pushl   $0              /* RTEMS_SELF */
    call    rtems_task_delete
    addl    $4, %esp

    /* Infinite loop if we somehow continue */
1:  hlt
    jmp     1b

    .size _start, . - _start

/*
 * __do_global_ctors - Call global constructors
 *
 * TCC places constructor pointers in .ctors section
 */
    .type __do_global_ctors, @function
__do_global_ctors:
    pushl   %ebx
    pushl   %esi

    /* Get start of .ctors */
    movl    $__CTOR_LIST__, %ebx
    /* Get end of .ctors */
    movl    $__CTOR_END__, %esi

.Lctor_loop:
    cmpl    %esi, %ebx
    jge     .Lctor_done

    movl    (%ebx), %eax
    testl   %eax, %eax
    jz      .Lctor_next

    /* Skip -1 marker */
    cmpl    $-1, %eax
    je      .Lctor_next

    /* Call constructor */
    call    *%eax

.Lctor_next:
    addl    $4, %ebx
    jmp     .Lctor_loop

.Lctor_done:
    popl    %esi
    popl    %ebx
    ret

    .size __do_global_ctors, . - __do_global_ctors

/*
 * __do_global_dtors - Call global destructors
 */
    .type __do_global_dtors, @function
__do_global_dtors:
    pushl   %ebx
    pushl   %esi

    /* Get end of .dtors (iterate backwards) */
    movl    $__DTOR_END__, %ebx
    movl    $__DTOR_LIST__, %esi

.Ldtor_loop:
    subl    $4, %ebx
    cmpl    %esi, %ebx
    jl      .Ldtor_done

    movl    (%ebx), %eax
    testl   %eax, %eax
    jz      .Ldtor_loop

    /* Skip -1 marker */
    cmpl    $-1, %eax
    je      .Ldtor_loop

    /* Call destructor */
    call    *%eax
    jmp     .Ldtor_loop

.Ldtor_done:
    popl    %esi
    popl    %ebx
    ret

    .size __do_global_dtors, . - __do_global_dtors

/*
 * Constructor/destructor list markers
 */
    .section .ctors, "aw", @progbits
    .align 4
    .global __CTOR_LIST__
__CTOR_LIST__:
    .long -1

    .section .ctors.end, "aw", @progbits
    .align 4
    .global __CTOR_END__
__CTOR_END__:
    .long 0

    .section .dtors, "aw", @progbits
    .align 4
    .global __DTOR_LIST__
__DTOR_LIST__:
    .long -1

    .section .dtors.end, "aw", @progbits
    .align 4
    .global __DTOR_END__
__DTOR_END__:
    .long 0
