/*
 * stdlib.h - Standard library for tcelm/NUC toolchain
 */

#ifndef _STDLIB_H
#define _STDLIB_H

#include <stddef.h>

/* Memory allocation */
void *malloc(size_t size);
void *calloc(size_t nmemb, size_t size);
void *realloc(void *ptr, size_t size);
void  free(void *ptr);

/* String conversion */
int    atoi(const char *nptr);
long   atol(const char *nptr);
long   strtol(const char *nptr, char **endptr, int base);
unsigned long strtoul(const char *nptr, char **endptr, int base);
double strtod(const char *nptr, char **endptr);

/* Random numbers */
int   rand(void);
void  srand(unsigned int seed);
#define RAND_MAX 2147483647

/* Program termination */
void  abort(void);
void  exit(int status);
int   atexit(void (*func)(void));

/* Environment */
char *getenv(const char *name);

/* Sorting and searching */
void  qsort(void *base, size_t nmemb, size_t size,
            int (*compar)(const void *, const void *));
void *bsearch(const void *key, const void *base, size_t nmemb, size_t size,
              int (*compar)(const void *, const void *));

/* Integer arithmetic */
int   abs(int j);
long  labs(long j);

typedef struct {
    int quot;
    int rem;
} div_t;

typedef struct {
    long quot;
    long rem;
} ldiv_t;

div_t  div(int numer, int denom);
ldiv_t ldiv(long numer, long denom);

#endif /* _STDLIB_H */
