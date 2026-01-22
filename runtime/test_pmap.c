/*
 * test_pmap.c - Test parallel map (pmap) implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "tcelm_arena.h"
#include "tcelm_types.h"
#include "tcelm_list.h"

/* Test function: square a number */
static tcelm_value_t *square_fn(tcelm_arena_t *arena, tcelm_value_t **args) {
    int64_t n = TCELM_AS_INT(args[0]);
    return tcelm_int(arena, n * n);
}

/* Create a closure for square */
static tcelm_value_t *make_square_closure(tcelm_arena_t *arena) {
    return tcelm_closure(arena, square_fn, 1);
}

/* Print a list of ints */
static void print_list(const char *label, tcelm_value_t *list) {
    printf("%s: [", label);
    int first = 1;
    while (!tcelm_is_nil(list)) {
        if (!first) printf(", ");
        printf("%ld", TCELM_AS_INT(tcelm_list_head(list)));
        list = tcelm_list_tail(list);
        first = 0;
    }
    printf("]\n");
}

/* Compute sum of list */
static int64_t sum_list(tcelm_value_t *list) {
    int64_t sum = 0;
    while (!tcelm_is_nil(list)) {
        sum += TCELM_AS_INT(tcelm_list_head(list));
        list = tcelm_list_tail(list);
    }
    return sum;
}

/* Get current time in milliseconds */
static long get_time_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

int main(void) {
    printf("=== pmap (Parallel Map) Tests ===\n\n");

    int num_cores = tcelm_get_num_cores();
    printf("Detected %d CPU core(s)\n\n", num_cores);

    tcelm_arena_t *arena = tcelm_arena_create(1024 * 1024);  /* 1MB arena */
    tcelm_current_arena = arena;  /* Set thread-local arena for list operations */

    /* Test 1: Small list */
    printf("Test 1: Small list (should use sequential map)\n");
    {
        tcelm_value_t *list = TCELM_NIL;
        for (int i = 5; i >= 1; i--) {
            list = tcelm_cons(arena, tcelm_int(arena, i), list);
        }
        print_list("  Input", list);

        tcelm_value_t *fn = make_square_closure(arena);
        tcelm_value_t *result = tcelm_list_pmap(arena, fn, list);
        print_list("  Output (squares)", result);

        /* Verify: 1+4+9+16+25 = 55 */
        int64_t sum = sum_list(result);
        printf("  Sum of squares: %ld (expected 55)\n", sum);
        printf("  %s\n\n", sum == 55 ? "PASS" : "FAIL");
    }

    /* Test 2: Medium list */
    printf("Test 2: Medium list (%d elements, using all cores)\n", num_cores * 4);
    {
        int n = num_cores * 4;
        tcelm_value_t *list = TCELM_NIL;
        for (int i = n; i >= 1; i--) {
            list = tcelm_cons(arena, tcelm_int(arena, i), list);
        }

        tcelm_value_t *fn = make_square_closure(arena);

        long start = get_time_ms();
        tcelm_value_t *result = tcelm_list_pmap(arena, fn, list);
        long elapsed = get_time_ms() - start;

        /* Verify order is preserved */
        int64_t first = TCELM_AS_INT(tcelm_list_head(result));
        printf("  First element squared: %ld (expected 1)\n", first);

        /* Sum of 1^2 + 2^2 + ... + n^2 = n(n+1)(2n+1)/6 */
        int64_t expected_sum = (int64_t)n * (n + 1) * (2 * n + 1) / 6;
        int64_t actual_sum = sum_list(result);
        printf("  Sum of squares: %ld (expected %ld)\n", actual_sum, expected_sum);
        printf("  Time: %ld ms\n", elapsed);
        printf("  %s\n\n", (first == 1 && actual_sum == expected_sum) ? "PASS" : "FAIL");
    }

    /* Test 3: Large list - compare sequential vs parallel */
    printf("Test 3: Large list (1000 elements) - sequential vs parallel\n");
    {
        int n = 1000;
        tcelm_value_t *list = TCELM_NIL;
        for (int i = n; i >= 1; i--) {
            list = tcelm_cons(arena, tcelm_int(arena, i), list);
        }

        tcelm_value_t *fn = make_square_closure(arena);

        /* Sequential */
        long start_seq = get_time_ms();
        tcelm_value_t *result_seq = tcelm_list_map(arena, fn, list);
        long elapsed_seq = get_time_ms() - start_seq;
        int64_t sum_seq = sum_list(result_seq);

        /* Parallel */
        long start_par = get_time_ms();
        tcelm_value_t *result_par = tcelm_list_pmap(arena, fn, list);
        long elapsed_par = get_time_ms() - start_par;
        int64_t sum_par = sum_list(result_par);

        printf("  Sequential map: %ld ms, sum=%ld\n", elapsed_seq, sum_seq);
        printf("  Parallel pmap:  %ld ms, sum=%ld\n", elapsed_par, sum_par);
        printf("  Results match: %s\n", sum_seq == sum_par ? "YES" : "NO");
        printf("  %s\n\n", sum_seq == sum_par ? "PASS" : "FAIL");
    }

    /* Test 4: Explicit worker count */
    printf("Test 4: Explicit worker count (pmapN with 2 workers)\n");
    {
        tcelm_value_t *list = TCELM_NIL;
        for (int i = 10; i >= 1; i--) {
            list = tcelm_cons(arena, tcelm_int(arena, i), list);
        }

        tcelm_value_t *fn = make_square_closure(arena);
        tcelm_value_t *result = tcelm_list_pmapN(arena, 2, fn, list);

        /* 1+4+9+16+25+36+49+64+81+100 = 385 */
        int64_t sum = sum_list(result);
        printf("  Sum of 1^2..10^2: %ld (expected 385)\n", sum);
        printf("  %s\n\n", sum == 385 ? "PASS" : "FAIL");
    }

    /* Test 5: Empty list */
    printf("Test 5: Empty list\n");
    {
        tcelm_value_t *fn = make_square_closure(arena);
        tcelm_value_t *result = tcelm_list_pmap(arena, fn, TCELM_NIL);
        printf("  Result is nil: %s\n", tcelm_is_nil(result) ? "YES" : "NO");
        printf("  %s\n\n", tcelm_is_nil(result) ? "PASS" : "FAIL");
    }

    tcelm_arena_free(arena);

    printf("=== All pmap tests completed ===\n");
    return 0;
}
