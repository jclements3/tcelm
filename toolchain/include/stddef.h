/*
 * stddef.h - Standard definitions for tcelm/NUC toolchain
 */

#ifndef _STDDEF_H
#define _STDDEF_H

#ifdef __i386__
typedef unsigned int size_t;
typedef int          ssize_t;
typedef int          ptrdiff_t;
#else
typedef unsigned long size_t;
typedef long          ssize_t;
typedef long          ptrdiff_t;
#endif

#define NULL ((void *)0)

#define offsetof(type, member) ((size_t)&((type *)0)->member)

#endif /* _STDDEF_H */
