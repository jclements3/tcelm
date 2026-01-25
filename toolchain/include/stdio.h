/*
 * stdio.h - Minimal stdio for tcelm bare-metal
 */

#ifndef _STDIO_H
#define _STDIO_H

#include <stddef.h>
#include <stdarg.h>

/* Serial output - implemented in libc_impl.c */
extern void serial_putchar(char c);
extern void serial_puts(const char *s);
extern void serial_print_int(long long n);

/* printf - minimal implementation */
int printf(const char *fmt, ...);

/* NULL stream - not used in bare metal */
typedef void FILE;
#define stdout ((FILE *)1)
#define stderr ((FILE *)2)

#endif /* _STDIO_H */
