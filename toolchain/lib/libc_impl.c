/*
 * libc_impl.c - Minimal C library implementation for tcelm/NUC toolchain
 *
 * Provides basic libc functions for bare-metal operation.
 */

#include <stdint.h>
#include <stddef.h>

/* Forward declarations */
void *malloc(size_t size);
void free(void *ptr);
long strtol(const char *nptr, char **endptr, int base);

/* div_t and ldiv_t types */
typedef struct {
    int quot;
    int rem;
} div_t;

typedef struct {
    long quot;
    long rem;
} ldiv_t;

/*
 * Memory functions
 */

void *memcpy(void *dest, const void *src, size_t n) {
    unsigned char *d = dest;
    const unsigned char *s = src;
    while (n--) *d++ = *s++;
    return dest;
}

void *memmove(void *dest, const void *src, size_t n) {
    unsigned char *d = dest;
    const unsigned char *s = src;
    if (d < s) {
        while (n--) *d++ = *s++;
    } else {
        d += n;
        s += n;
        while (n--) *--d = *--s;
    }
    return dest;
}

void *memset(void *s, int c, size_t n) {
    unsigned char *p = s;
    while (n--) *p++ = (unsigned char)c;
    return s;
}

int memcmp(const void *s1, const void *s2, size_t n) {
    const unsigned char *p1 = s1;
    const unsigned char *p2 = s2;
    while (n--) {
        if (*p1 != *p2) return *p1 - *p2;
        p1++;
        p2++;
    }
    return 0;
}

void *memchr(const void *s, int c, size_t n) {
    const unsigned char *p = s;
    while (n--) {
        if (*p == (unsigned char)c) return (void *)p;
        p++;
    }
    return NULL;
}

/*
 * String functions
 */

size_t strlen(const char *s) {
    size_t len = 0;
    while (*s++) len++;
    return len;
}

char *strcpy(char *dest, const char *src) {
    char *d = dest;
    while ((*d++ = *src++));
    return dest;
}

char *strncpy(char *dest, const char *src, size_t n) {
    char *d = dest;
    while (n && (*d++ = *src++)) n--;
    while (n--) *d++ = '\0';
    return dest;
}

char *strcat(char *dest, const char *src) {
    char *d = dest;
    while (*d) d++;
    while ((*d++ = *src++));
    return dest;
}

char *strncat(char *dest, const char *src, size_t n) {
    char *d = dest;
    while (*d) d++;
    while (n-- && (*d++ = *src++));
    *d = '\0';
    return dest;
}

int strcmp(const char *s1, const char *s2) {
    while (*s1 && *s1 == *s2) {
        s1++;
        s2++;
    }
    return (unsigned char)*s1 - (unsigned char)*s2;
}

int strncmp(const char *s1, const char *s2, size_t n) {
    while (n && *s1 && *s1 == *s2) {
        s1++;
        s2++;
        n--;
    }
    if (n == 0) return 0;
    return (unsigned char)*s1 - (unsigned char)*s2;
}

char *strchr(const char *s, int c) {
    while (*s) {
        if (*s == c) return (char *)s;
        s++;
    }
    return (c == '\0') ? (char *)s : NULL;
}

char *strrchr(const char *s, int c) {
    const char *last = NULL;
    while (*s) {
        if (*s == c) last = s;
        s++;
    }
    return (c == '\0') ? (char *)s : (char *)last;
}

char *strstr(const char *haystack, const char *needle) {
    if (!*needle) return (char *)haystack;
    while (*haystack) {
        const char *h = haystack;
        const char *n = needle;
        while (*h && *n && *h == *n) {
            h++;
            n++;
        }
        if (!*n) return (char *)haystack;
        haystack++;
    }
    return NULL;
}

/*
 * Simple heap allocator
 */

#define HEAP_SIZE (2 * 1024 * 1024)  /* 2MB heap */

/* Heap memory block */
static char heap_memory[HEAP_SIZE] __attribute__((aligned(16)));
static size_t heap_ptr = 0;

/* Simple bump allocator (no free) */
void *malloc(size_t size) {
    if (size == 0) return NULL;

    /* Align to 16 bytes */
    size = (size + 15) & ~15;

    if (heap_ptr + size > HEAP_SIZE) {
        return NULL;  /* Out of memory */
    }

    void *ptr = &heap_memory[heap_ptr];
    heap_ptr += size;
    return ptr;
}

void *calloc(size_t nmemb, size_t size) {
    size_t total = nmemb * size;
    void *ptr = malloc(total);
    if (ptr) memset(ptr, 0, total);
    return ptr;
}

void *realloc(void *ptr, size_t size) {
    if (!ptr) return malloc(size);
    if (size == 0) {
        free(ptr);
        return NULL;
    }
    /* Simple implementation: allocate new and copy */
    void *new_ptr = malloc(size);
    if (new_ptr) {
        /* We don't know original size, so just copy what we can */
        memcpy(new_ptr, ptr, size);
    }
    return new_ptr;
}

void free(void *ptr) {
    /* No-op in bump allocator */
    (void)ptr;
}

/*
 * String conversion
 */

int atoi(const char *nptr) {
    return (int)strtol(nptr, NULL, 10);
}

long atol(const char *nptr) {
    return strtol(nptr, NULL, 10);
}

long strtol(const char *nptr, char **endptr, int base) {
    const char *s = nptr;
    long result = 0;
    int neg = 0;

    /* Skip whitespace */
    while (*s == ' ' || *s == '\t' || *s == '\n') s++;

    /* Sign */
    if (*s == '-') {
        neg = 1;
        s++;
    } else if (*s == '+') {
        s++;
    }

    /* Auto-detect base */
    if (base == 0) {
        if (*s == '0') {
            s++;
            if (*s == 'x' || *s == 'X') {
                base = 16;
                s++;
            } else {
                base = 8;
            }
        } else {
            base = 10;
        }
    } else if (base == 16 && *s == '0' && (s[1] == 'x' || s[1] == 'X')) {
        s += 2;
    }

    /* Parse digits */
    while (*s) {
        int digit;
        if (*s >= '0' && *s <= '9') {
            digit = *s - '0';
        } else if (*s >= 'a' && *s <= 'z') {
            digit = *s - 'a' + 10;
        } else if (*s >= 'A' && *s <= 'Z') {
            digit = *s - 'A' + 10;
        } else {
            break;
        }
        if (digit >= base) break;
        result = result * base + digit;
        s++;
    }

    if (endptr) *endptr = (char *)s;
    return neg ? -result : result;
}

unsigned long strtoul(const char *nptr, char **endptr, int base) {
    return (unsigned long)strtol(nptr, endptr, base);
}

double strtod(const char *nptr, char **endptr) {
    const char *s = nptr;
    double result = 0.0;
    double fraction = 0.0;
    double divisor = 1.0;
    int neg = 0;
    int has_fraction = 0;

    /* Skip whitespace */
    while (*s == ' ' || *s == '\t' || *s == '\n') s++;

    /* Sign */
    if (*s == '-') {
        neg = 1;
        s++;
    } else if (*s == '+') {
        s++;
    }

    /* Integer part */
    while (*s >= '0' && *s <= '9') {
        result = result * 10.0 + (*s - '0');
        s++;
    }

    /* Fraction part */
    if (*s == '.') {
        s++;
        has_fraction = 1;
        while (*s >= '0' && *s <= '9') {
            divisor *= 10.0;
            fraction += (*s - '0') / divisor;
            s++;
        }
    }

    result += fraction;

    /* Exponent */
    if (*s == 'e' || *s == 'E') {
        s++;
        int exp_neg = 0;
        int exp = 0;
        if (*s == '-') {
            exp_neg = 1;
            s++;
        } else if (*s == '+') {
            s++;
        }
        while (*s >= '0' && *s <= '9') {
            exp = exp * 10 + (*s - '0');
            s++;
        }
        double multiplier = 1.0;
        while (exp--) multiplier *= 10.0;
        if (exp_neg) {
            result /= multiplier;
        } else {
            result *= multiplier;
        }
    }

    if (endptr) *endptr = (char *)s;
    return neg ? -result : result;
}

/*
 * Integer arithmetic
 */

int abs(int j) {
    return j < 0 ? -j : j;
}

long labs(long j) {
    return j < 0 ? -j : j;
}

div_t div(int numer, int denom) {
    div_t result;
    result.quot = numer / denom;
    result.rem = numer % denom;
    return result;
}

ldiv_t ldiv(long numer, long denom) {
    ldiv_t result;
    result.quot = numer / denom;
    result.rem = numer % denom;
    return result;
}

/*
 * Random numbers (simple LCG)
 */

static unsigned int rand_seed = 1;

int rand(void) {
    rand_seed = rand_seed * 1103515245 + 12345;
    return (int)((rand_seed >> 16) & 0x7fff);
}

void srand(unsigned int seed) {
    rand_seed = seed;
}

/*
 * Sorting
 */

void qsort(void *base, size_t nmemb, size_t size,
           int (*compar)(const void *, const void *)) {
    /* Simple insertion sort (good enough for small arrays) */
    char *b = base;
    char *temp = malloc(size);
    if (!temp) return;

    for (size_t i = 1; i < nmemb; i++) {
        memcpy(temp, b + i * size, size);
        size_t j = i;
        while (j > 0 && compar(b + (j-1) * size, temp) > 0) {
            memcpy(b + j * size, b + (j-1) * size, size);
            j--;
        }
        memcpy(b + j * size, temp, size);
    }
}

void *bsearch(const void *key, const void *base, size_t nmemb, size_t size,
              int (*compar)(const void *, const void *)) {
    const char *b = base;
    size_t lo = 0, hi = nmemb;

    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        int cmp = compar(key, b + mid * size);
        if (cmp == 0) return (void *)(b + mid * size);
        if (cmp < 0) {
            hi = mid;
        } else {
            lo = mid + 1;
        }
    }
    return NULL;
}

/*
 * Program termination
 */

static void (*atexit_funcs[32])(void);
static int atexit_count = 0;

int atexit(void (*func)(void)) {
    if (atexit_count >= 32) return -1;
    atexit_funcs[atexit_count++] = func;
    return 0;
}

void exit(int status) {
    while (atexit_count > 0) {
        atexit_funcs[--atexit_count]();
    }
    /* Halt */
    __asm__ volatile ("cli; hlt");
    while(1);
}

void abort(void) {
    __asm__ volatile ("cli; hlt");
    while(1);
}

/*
 * Environment (stub)
 */

char *getenv(const char *name) {
    (void)name;
    return NULL;
}

/*
 * Duplicate string
 */

char *strdup(const char *s) {
    size_t len = strlen(s) + 1;
    char *dup = malloc(len);
    if (dup) memcpy(dup, s, len);
    return dup;
}
