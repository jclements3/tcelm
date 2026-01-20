/*
 * stddef.h - Standard definitions for TCC-NUC-BSP
 */

#ifndef _STDDEF_H
#define _STDDEF_H

#include <stdint.h>

#ifndef NULL
#define NULL ((void*)0)
#endif

#ifndef offsetof
#define offsetof(type, member) ((size_t)&((type *)0)->member)
#endif

typedef int32_t ptrdiff_t;
typedef uint32_t size_t;
typedef int wchar_t;

#endif /* _STDDEF_H */
