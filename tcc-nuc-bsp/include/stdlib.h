/*
 * stdlib.h - Standard library for TCC-NUC-BSP
 *
 * Minimal implementation - tcelm uses arena allocation
 */

#ifndef _STDLIB_H
#define _STDLIB_H

#include <stddef.h>

/* We don't use malloc/free - tcelm uses arena allocation */
/* These are stubs that will cause link errors if used */
void *malloc(size_t size);
void *calloc(size_t nmemb, size_t size);
void *realloc(void *ptr, size_t size);
void free(void *ptr);

/* Conversion functions */
int atoi(const char *nptr);
long atol(const char *nptr);
double atof(const char *nptr);
long strtol(const char *nptr, char **endptr, int base);
unsigned long strtoul(const char *nptr, char **endptr, int base);
double strtod(const char *nptr, char **endptr);

/* Pseudo-random numbers */
#define RAND_MAX 32767
int rand(void);
void srand(unsigned int seed);

/* Program termination */
void abort(void);
void exit(int status);

/* Environment (not available on RTEMS bare-metal) */
char *getenv(const char *name);

/* Absolute value */
int abs(int j);
long labs(long j);

/* Division */
typedef struct { int quot; int rem; } div_t;
typedef struct { long quot; long rem; } ldiv_t;
div_t div(int numer, int denom);
ldiv_t ldiv(long numer, long denom);

#endif /* _STDLIB_H */
