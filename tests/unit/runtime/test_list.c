/*
 * test_list.c - Property-based tests for tcelm list operations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../property/proptest.h"
#include "tcelm_arena.h"
#include "tcelm_types.h"
#include "tcelm_basics.h"
#include "tcelm_list.h"

static tcelm_arena_t test_arena_storage;
static tcelm_arena_t *test_arena;

static void setup_arena(void) {
    tcelm_arena_init(&test_arena_storage, 1024 * 1024);
    test_arena = &test_arena_storage;
    tcelm_current_arena = test_arena;  /* Set global arena for functions that use it */
}

static void teardown_arena(void) {
    tcelm_arena_destroy(test_arena);
    test_arena = NULL;
}

/* Helper: create list from array of ints */
static tcelm_value_t *list_from_ints(int64_t *arr, size_t len) {
    tcelm_value_t *list = TCELM_NIL;
    for (size_t i = len; i > 0; i--) {
        list = tcelm_cons(test_arena, tcelm_int(test_arena, arr[i-1]), list);
    }
    return list;
}

/* Helper: get list length */
static size_t list_len(tcelm_value_t *list) {
    size_t len = 0;
    while (!tcelm_is_nil(list)) {
        len++;
        list = tcelm_list_tail(list);
    }
    return len;
}

/*
 * Property: Empty list is nil
 */
static proptest_result_t prop_nil_is_empty(proptest_ctx_t *ctx) {
    (void)ctx;
    setup_arena();

    tcelm_value_t *list = tcelm_nil(test_arena);
    PROP_ASSERT(ctx, tcelm_is_nil(list), "nil should be empty");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: cons creates non-empty list
 */
static proptest_result_t prop_cons_not_nil(proptest_ctx_t *ctx) {
    setup_arena();

    int64_t val = proptest_rand_int(ctx, -1000, 1000);
    tcelm_value_t *elem = tcelm_int(test_arena, val);
    tcelm_value_t *list = tcelm_cons(test_arena, elem, TCELM_NIL);

    PROP_ASSERT(ctx, !tcelm_is_nil(list), "cons result should not be nil");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: head(cons(x, xs)) = x
 */
static proptest_result_t prop_head_cons(proptest_ctx_t *ctx) {
    setup_arena();

    int64_t val = proptest_rand_int(ctx, -1000000, 1000000);
    tcelm_value_t *elem = tcelm_int(test_arena, val);
    tcelm_value_t *list = tcelm_cons(test_arena, elem, TCELM_NIL);

    tcelm_value_t *head = tcelm_list_head(list);

    PROP_ASSERT_EQ(ctx, TCELM_AS_INT(head), val, "%lld");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: tail(cons(x, xs)) = xs
 */
static proptest_result_t prop_tail_cons(proptest_ctx_t *ctx) {
    setup_arena();

    size_t len;
    int64_t *arr = proptest_rand_int_array(ctx, &len, 0, 10, -1000, 1000);
    if (!arr) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *xs = list_from_ints(arr, len);
    int64_t x = proptest_rand_int(ctx, -1000, 1000);
    tcelm_value_t *cons_list = tcelm_cons(test_arena, tcelm_int(test_arena, x), xs);

    tcelm_value_t *tail = tcelm_list_tail(cons_list);

    /* Compare lengths */
    PROP_ASSERT_EQ(ctx, list_len(tail), len, "%zu");

    free(arr);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: length([]) = 0
 */
static proptest_result_t prop_length_nil(proptest_ctx_t *ctx) {
    (void)ctx;
    setup_arena();

    tcelm_value_t *len = tcelm_list_length(test_arena, TCELM_NIL);

    PROP_ASSERT_EQ(ctx, TCELM_AS_INT(len), 0LL, "%lld");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: length(cons(x, xs)) = 1 + length(xs)
 */
static proptest_result_t prop_length_cons(proptest_ctx_t *ctx) {
    setup_arena();

    size_t arr_len;
    int64_t *arr = proptest_rand_int_array(ctx, &arr_len, 0, 20, -1000, 1000);
    if (!arr) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *xs = list_from_ints(arr, arr_len);
    tcelm_value_t *len_xs = tcelm_list_length(test_arena, xs);

    int64_t x = proptest_rand_int(ctx, -1000, 1000);
    tcelm_value_t *cons_list = tcelm_cons(test_arena, tcelm_int(test_arena, x), xs);
    tcelm_value_t *len_cons = tcelm_list_length(test_arena, cons_list);

    PROP_ASSERT_EQ(ctx, TCELM_AS_INT(len_cons), TCELM_AS_INT(len_xs) + 1, "%lld");

    free(arr);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: reverse(reverse(xs)) = xs
 */
static proptest_result_t prop_reverse_involutive(proptest_ctx_t *ctx) {
    setup_arena();

    size_t len;
    int64_t *arr = proptest_rand_int_array(ctx, &len, 0, 20, -1000, 1000);
    if (!arr) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *xs = list_from_ints(arr, len);
    tcelm_value_t *rev = tcelm_list_reverse(test_arena, xs);
    tcelm_value_t *rev_rev = tcelm_list_reverse(test_arena, rev);

    /* Compare element by element */
    tcelm_value_t *p1 = xs;
    tcelm_value_t *p2 = rev_rev;
    size_t i = 0;
    while (!tcelm_is_nil(p1) && !tcelm_is_nil(p2)) {
        int64_t v1 = TCELM_AS_INT(tcelm_list_head(p1));
        int64_t v2 = TCELM_AS_INT(tcelm_list_head(p2));
        if (v1 != v2) {
            snprintf(ctx->failure_msg, sizeof(ctx->failure_msg),
                     "Mismatch at index %zu: %lld != %lld", i, (long long)v1, (long long)v2);
            free(arr);
            teardown_arena();
            return PROPTEST_FAIL;
        }
        p1 = tcelm_list_tail(p1);
        p2 = tcelm_list_tail(p2);
        i++;
    }

    PROP_ASSERT(ctx, tcelm_is_nil(p1) && tcelm_is_nil(p2), "Lists should have same length");

    free(arr);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: length(reverse(xs)) = length(xs)
 */
static proptest_result_t prop_reverse_preserves_length(proptest_ctx_t *ctx) {
    setup_arena();

    size_t len;
    int64_t *arr = proptest_rand_int_array(ctx, &len, 0, 20, -1000, 1000);
    if (!arr) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *xs = list_from_ints(arr, len);
    tcelm_value_t *rev = tcelm_list_reverse(test_arena, xs);

    tcelm_value_t *len_xs = tcelm_list_length(test_arena, xs);
    tcelm_value_t *len_rev = tcelm_list_length(test_arena, rev);

    PROP_ASSERT_EQ(ctx, TCELM_AS_INT(len_xs), TCELM_AS_INT(len_rev), "%lld");

    free(arr);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: append([], ys) = ys
 */
static proptest_result_t prop_append_nil_left(proptest_ctx_t *ctx) {
    setup_arena();

    size_t len;
    int64_t *arr = proptest_rand_int_array(ctx, &len, 0, 10, -1000, 1000);
    if (!arr) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *ys = list_from_ints(arr, len);
    tcelm_value_t *result = tcelm_list_append(test_arena, TCELM_NIL, ys);

    /* Compare lengths */
    PROP_ASSERT_EQ(ctx, list_len(result), len, "%zu");

    free(arr);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: append(xs, []) = xs
 */
static proptest_result_t prop_append_nil_right(proptest_ctx_t *ctx) {
    setup_arena();

    size_t len;
    int64_t *arr = proptest_rand_int_array(ctx, &len, 0, 10, -1000, 1000);
    if (!arr) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *xs = list_from_ints(arr, len);
    tcelm_value_t *result = tcelm_list_append(test_arena, xs, TCELM_NIL);

    PROP_ASSERT_EQ(ctx, list_len(result), len, "%zu");

    free(arr);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: length(append(xs, ys)) = length(xs) + length(ys)
 */
static proptest_result_t prop_append_length(proptest_ctx_t *ctx) {
    setup_arena();

    size_t len1, len2;
    int64_t *arr1 = proptest_rand_int_array(ctx, &len1, 0, 10, -1000, 1000);
    int64_t *arr2 = proptest_rand_int_array(ctx, &len2, 0, 10, -1000, 1000);
    if (!arr1 || !arr2) {
        free(arr1);
        free(arr2);
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *xs = list_from_ints(arr1, len1);
    tcelm_value_t *ys = list_from_ints(arr2, len2);
    tcelm_value_t *result = tcelm_list_append(test_arena, xs, ys);

    tcelm_value_t *len_result = tcelm_list_length(test_arena, result);

    PROP_ASSERT_EQ(ctx, TCELM_AS_INT(len_result), (int64_t)(len1 + len2), "%lld");

    free(arr1);
    free(arr2);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: map(identity, xs) = xs
 */
static proptest_result_t prop_map_identity(proptest_ctx_t *ctx) {
    setup_arena();

    size_t len;
    int64_t *arr = proptest_rand_int_array(ctx, &len, 0, 15, -1000, 1000);
    if (!arr) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *xs = list_from_ints(arr, len);

    /* Create identity closure */
    tcelm_value_t *id_fn = tcelm_closure(test_arena, tcelm_identity_impl, 1);

    tcelm_value_t *result = tcelm_list_map(test_arena, id_fn, xs);

    /* Compare element by element */
    tcelm_value_t *p1 = xs;
    tcelm_value_t *p2 = result;
    while (!tcelm_is_nil(p1) && !tcelm_is_nil(p2)) {
        int64_t v1 = TCELM_AS_INT(tcelm_list_head(p1));
        int64_t v2 = TCELM_AS_INT(tcelm_list_head(p2));
        PROP_ASSERT_EQ(ctx, v1, v2, "%lld");
        p1 = tcelm_list_tail(p1);
        p2 = tcelm_list_tail(p2);
    }

    PROP_ASSERT(ctx, tcelm_is_nil(p1) && tcelm_is_nil(p2), "Lists should have same length");

    free(arr);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: length(map(f, xs)) = length(xs)
 */
static proptest_result_t prop_map_preserves_length(proptest_ctx_t *ctx) {
    setup_arena();

    size_t len;
    int64_t *arr = proptest_rand_int_array(ctx, &len, 0, 15, -1000, 1000);
    if (!arr) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *xs = list_from_ints(arr, len);
    tcelm_value_t *id_fn = tcelm_closure(test_arena, tcelm_identity_impl, 1);
    tcelm_value_t *result = tcelm_list_map(test_arena, id_fn, xs);

    PROP_ASSERT_EQ(ctx, list_len(result), len, "%zu");

    free(arr);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: filter with always-true returns same list
 */
static tcelm_value_t *always_true_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    (void)args;
    return tcelm_bool(arena, true);
}

static proptest_result_t prop_filter_true(proptest_ctx_t *ctx) {
    setup_arena();

    size_t len;
    int64_t *arr = proptest_rand_int_array(ctx, &len, 0, 15, -1000, 1000);
    if (!arr) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *xs = list_from_ints(arr, len);
    tcelm_value_t *pred = tcelm_closure(test_arena, always_true_impl, 1);
    tcelm_value_t *result = tcelm_list_filter(test_arena, pred, xs);

    PROP_ASSERT_EQ(ctx, list_len(result), len, "%zu");

    free(arr);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: filter with always-false returns empty list
 */
static tcelm_value_t *always_false_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    (void)args;
    return tcelm_bool(arena, false);
}

static proptest_result_t prop_filter_false(proptest_ctx_t *ctx) {
    setup_arena();

    size_t len;
    int64_t *arr = proptest_rand_int_array(ctx, &len, 0, 15, -1000, 1000);
    if (!arr) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *xs = list_from_ints(arr, len);
    tcelm_value_t *pred = tcelm_closure(test_arena, always_false_impl, 1);
    tcelm_value_t *result = tcelm_list_filter(test_arena, pred, xs);

    PROP_ASSERT(ctx, tcelm_is_nil(result), "Filter with false should return empty list");

    free(arr);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: length(filter(p, xs)) <= length(xs)
 */
static tcelm_value_t *is_positive_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_bool(arena, TCELM_AS_INT(args[0]) > 0);
}

static proptest_result_t prop_filter_length_le(proptest_ctx_t *ctx) {
    setup_arena();

    size_t len;
    int64_t *arr = proptest_rand_int_array(ctx, &len, 0, 20, -1000, 1000);
    if (!arr) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *xs = list_from_ints(arr, len);
    tcelm_value_t *pred = tcelm_closure(test_arena, is_positive_impl, 1);
    tcelm_value_t *result = tcelm_list_filter(test_arena, pred, xs);

    PROP_ASSERT(ctx, list_len(result) <= len, "Filtered list should not be longer");

    free(arr);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: foldl (+) 0 xs = sum of xs
 */
static tcelm_value_t *add_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_add_int(arena, args[0], args[1]);
}

static proptest_result_t prop_foldl_sum(proptest_ctx_t *ctx) {
    setup_arena();

    size_t len;
    int64_t *arr = proptest_rand_int_array(ctx, &len, 0, 20, -100, 100);
    if (!arr) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    /* Calculate expected sum */
    int64_t expected = 0;
    for (size_t i = 0; i < len; i++) {
        expected += arr[i];
    }

    tcelm_value_t *xs = list_from_ints(arr, len);
    tcelm_value_t *add_fn = tcelm_closure(test_arena, add_impl, 2);
    tcelm_value_t *zero = tcelm_int(test_arena, 0);
    tcelm_value_t *result = tcelm_list_foldl(test_arena, add_fn, zero, xs);

    PROP_ASSERT_EQ(ctx, TCELM_AS_INT(result), expected, "%lld");

    free(arr);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Test suite definition
 */
proptest_case_t list_tests[] = {
    {"nil_is_empty", prop_nil_is_empty, 1},
    {"cons_not_nil", prop_cons_not_nil, 100},
    {"head_cons", prop_head_cons, 100},
    {"tail_cons", prop_tail_cons, 100},
    {"length_nil", prop_length_nil, 1},
    {"length_cons", prop_length_cons, 100},
    {"reverse_involutive", prop_reverse_involutive, 100},
    {"reverse_preserves_length", prop_reverse_preserves_length, 100},
    {"append_nil_left", prop_append_nil_left, 100},
    {"append_nil_right", prop_append_nil_right, 100},
    {"append_length", prop_append_length, 100},
    {"map_identity", prop_map_identity, 100},
    {"map_preserves_length", prop_map_preserves_length, 100},
    {"filter_true", prop_filter_true, 100},
    {"filter_false", prop_filter_false, 100},
    {"filter_length_le", prop_filter_length_le, 100},
    {"foldl_sum", prop_foldl_sum, 100},
};

int main(int argc, char **argv) {
    uint64_t seed = 0;
    if (argc > 1) {
        seed = (uint64_t)atoll(argv[1]);
    }

    return proptest_run_suite("tcelm List",
                              list_tests,
                              sizeof(list_tests) / sizeof(list_tests[0]),
                              seed);
}
