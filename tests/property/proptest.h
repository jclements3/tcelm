/*
 * proptest.h - Lightweight property-based testing for C
 *
 * A QuickCheck-inspired testing framework for tcelm runtime testing.
 * Generates random inputs and verifies properties hold.
 */

#ifndef PROPTEST_H
#define PROPTEST_H

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>

/* Configuration */
#define PROPTEST_DEFAULT_ITERATIONS 100
#define PROPTEST_MAX_STRING_LEN 256
#define PROPTEST_MAX_LIST_LEN 50

/* Test result */
typedef enum {
    PROPTEST_PASS,
    PROPTEST_FAIL,
    PROPTEST_SKIP
} proptest_result_t;

/* Random generator state */
typedef struct {
    uint64_t state;
    uint64_t inc;
} proptest_rng_t;

/* Test context */
typedef struct {
    proptest_rng_t rng;
    int iteration;
    int total_iterations;
    char failure_msg[512];
    void *user_data;
} proptest_ctx_t;

/* Property function type */
typedef proptest_result_t (*proptest_fn)(proptest_ctx_t *ctx);

/* Statistics */
typedef struct {
    int passed;
    int failed;
    int skipped;
    const char *first_failure;
} proptest_stats_t;

/*
 * Initialize RNG with seed
 */
static inline void proptest_rng_init(proptest_rng_t *rng, uint64_t seed) {
    rng->state = seed;
    rng->inc = (seed << 1) | 1;
}

/*
 * Generate random uint64
 * PCG algorithm for good distribution
 */
static inline uint64_t proptest_rand64(proptest_rng_t *rng) {
    uint64_t old_state = rng->state;
    rng->state = old_state * 6364136223846793005ULL + rng->inc;
    uint64_t xorshifted = ((old_state >> 18u) ^ old_state) >> 27u;
    uint64_t rot = old_state >> 59u;
    return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
}

/*
 * Generate random int64 in range [min, max]
 */
static inline int64_t proptest_rand_int(proptest_ctx_t *ctx, int64_t min, int64_t max) {
    if (min >= max) return min;
    uint64_t range = (uint64_t)(max - min + 1);
    return min + (int64_t)(proptest_rand64(&ctx->rng) % range);
}

/*
 * Generate random double in range [min, max]
 */
static inline double proptest_rand_double(proptest_ctx_t *ctx, double min, double max) {
    double normalized = (double)proptest_rand64(&ctx->rng) / (double)UINT64_MAX;
    return min + normalized * (max - min);
}

/*
 * Generate random boolean
 */
static inline bool proptest_rand_bool(proptest_ctx_t *ctx) {
    return proptest_rand64(&ctx->rng) & 1;
}

/*
 * Generate random printable ASCII character
 */
static inline char proptest_rand_char(proptest_ctx_t *ctx) {
    return (char)proptest_rand_int(ctx, 32, 126);
}

/*
 * Generate random alphanumeric character
 */
static inline char proptest_rand_alnum(proptest_ctx_t *ctx) {
    const char *chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    return chars[proptest_rand_int(ctx, 0, 61)];
}

/*
 * Generate random string (caller must free)
 */
static inline char *proptest_rand_string(proptest_ctx_t *ctx, size_t min_len, size_t max_len) {
    size_t len = (size_t)proptest_rand_int(ctx, (int64_t)min_len, (int64_t)max_len);
    char *str = malloc(len + 1);
    if (!str) return NULL;
    for (size_t i = 0; i < len; i++) {
        str[i] = proptest_rand_char(ctx);
    }
    str[len] = '\0';
    return str;
}

/*
 * Generate random identifier (starts with letter)
 */
static inline char *proptest_rand_identifier(proptest_ctx_t *ctx, size_t min_len, size_t max_len) {
    if (min_len < 1) min_len = 1;
    size_t len = (size_t)proptest_rand_int(ctx, (int64_t)min_len, (int64_t)max_len);
    char *str = malloc(len + 1);
    if (!str) return NULL;

    const char *alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
    str[0] = alpha[proptest_rand_int(ctx, 0, 52)];

    const char *alnum = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";
    for (size_t i = 1; i < len; i++) {
        str[i] = alnum[proptest_rand_int(ctx, 0, 62)];
    }
    str[len] = '\0';
    return str;
}

/*
 * Generate random array of int64 (caller must free)
 */
static inline int64_t *proptest_rand_int_array(proptest_ctx_t *ctx, size_t *out_len,
                                                size_t min_len, size_t max_len,
                                                int64_t min_val, int64_t max_val) {
    *out_len = (size_t)proptest_rand_int(ctx, (int64_t)min_len, (int64_t)max_len);
    int64_t *arr = malloc(*out_len * sizeof(int64_t));
    if (!arr) return NULL;
    for (size_t i = 0; i < *out_len; i++) {
        arr[i] = proptest_rand_int(ctx, min_val, max_val);
    }
    return arr;
}

/*
 * Shrinking helpers for finding minimal failing cases
 */
static inline int64_t proptest_shrink_int(int64_t value) {
    if (value == 0) return 0;
    if (value > 0) return value / 2;
    return value / 2 + 1;
}

/*
 * Run a single property test
 */
static inline proptest_stats_t proptest_run(const char *name, proptest_fn prop,
                                             int iterations, uint64_t seed) {
    proptest_stats_t stats = {0, 0, 0, NULL};
    proptest_ctx_t ctx;

    if (seed == 0) {
        seed = (uint64_t)time(NULL);
    }
    proptest_rng_init(&ctx.rng, seed);
    ctx.total_iterations = iterations;
    ctx.user_data = NULL;

    printf("  Testing: %s (seed=%lu)\n", name, (unsigned long)seed);

    for (int i = 0; i < iterations; i++) {
        ctx.iteration = i;
        ctx.failure_msg[0] = '\0';

        proptest_result_t result = prop(&ctx);

        switch (result) {
            case PROPTEST_PASS:
                stats.passed++;
                break;
            case PROPTEST_FAIL:
                stats.failed++;
                if (!stats.first_failure) {
                    stats.first_failure = strdup(ctx.failure_msg);
                }
                printf("    FAIL at iteration %d: %s\n", i, ctx.failure_msg);
                break;
            case PROPTEST_SKIP:
                stats.skipped++;
                break;
        }
    }

    return stats;
}

/*
 * Assertion macros
 */
#define PROP_ASSERT(ctx, cond, msg) do { \
    if (!(cond)) { \
        snprintf((ctx)->failure_msg, sizeof((ctx)->failure_msg), \
                 "%s:%d: %s", __FILE__, __LINE__, msg); \
        return PROPTEST_FAIL; \
    } \
} while(0)

#define PROP_ASSERT_EQ(ctx, a, b, fmt) do { \
    if ((a) != (b)) { \
        snprintf((ctx)->failure_msg, sizeof((ctx)->failure_msg), \
                 "%s:%d: Expected " fmt " == " fmt, __FILE__, __LINE__, (a), (b)); \
        return PROPTEST_FAIL; \
    } \
} while(0)

#define PROP_ASSERT_STR_EQ(ctx, a, b) do { \
    if (strcmp((a), (b)) != 0) { \
        snprintf((ctx)->failure_msg, sizeof((ctx)->failure_msg), \
                 "%s:%d: Expected \"%s\" == \"%s\"", __FILE__, __LINE__, (a), (b)); \
        return PROPTEST_FAIL; \
    } \
} while(0)

/*
 * Test suite runner
 */
typedef struct {
    const char *name;
    proptest_fn fn;
    int iterations;
} proptest_case_t;

static inline int proptest_run_suite(const char *suite_name,
                                      proptest_case_t *cases, size_t count,
                                      uint64_t seed) {
    printf("\n=== Property Test Suite: %s ===\n\n", suite_name);

    proptest_stats_t total = {0, 0, 0, NULL};

    for (size_t i = 0; i < count; i++) {
        int iters = cases[i].iterations > 0 ? cases[i].iterations : PROPTEST_DEFAULT_ITERATIONS;
        proptest_stats_t stats = proptest_run(cases[i].name, cases[i].fn, iters, seed + i);
        total.passed += stats.passed;
        total.failed += stats.failed;
        total.skipped += stats.skipped;
    }

    printf("\n--- Summary ---\n");
    printf("Passed:  %d\n", total.passed);
    printf("Failed:  %d\n", total.failed);
    printf("Skipped: %d\n", total.skipped);
    printf("Total:   %d\n", total.passed + total.failed + total.skipped);

    return total.failed > 0 ? 1 : 0;
}

#endif /* PROPTEST_H */
