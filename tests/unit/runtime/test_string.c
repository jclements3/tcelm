/*
 * test_string.c - Property-based tests for tcelm string operations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../property/proptest.h"
#include "tcelm_arena.h"
#include "tcelm_types.h"
#include "tcelm_basics.h"
#include "tcelm_string.h"

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

/*
 * Property: String length is non-negative
 */
static proptest_result_t prop_length_non_negative(proptest_ctx_t *ctx) {
    setup_arena();

    char *str = proptest_rand_string(ctx, 0, 100);
    if (!str) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *s = tcelm_string(test_arena, str);
    tcelm_value_t *len = tcelm_string_length(test_arena, s);

    PROP_ASSERT(ctx, TCELM_AS_INT(len) >= 0, "String length should be non-negative");

    free(str);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: Empty string has length 0
 */
static proptest_result_t prop_empty_length_zero(proptest_ctx_t *ctx) {
    (void)ctx;
    setup_arena();

    tcelm_value_t *s = tcelm_string(test_arena, "");
    tcelm_value_t *len = tcelm_string_length(test_arena, s);

    PROP_ASSERT_EQ(ctx, TCELM_AS_INT(len), 0LL, "%lld");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: append(a, "") = a
 */
static proptest_result_t prop_append_empty_right(proptest_ctx_t *ctx) {
    setup_arena();

    char *str = proptest_rand_string(ctx, 0, 50);
    if (!str) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *a = tcelm_string(test_arena, str);
    tcelm_value_t *empty = tcelm_string(test_arena, "");
    tcelm_value_t *result = tcelm_string_append(test_arena, a, empty);

    PROP_ASSERT_STR_EQ(ctx, TCELM_AS_STRING(result)->data, str);

    free(str);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: append("", a) = a
 */
static proptest_result_t prop_append_empty_left(proptest_ctx_t *ctx) {
    setup_arena();

    char *str = proptest_rand_string(ctx, 0, 50);
    if (!str) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *a = tcelm_string(test_arena, str);
    tcelm_value_t *empty = tcelm_string(test_arena, "");
    tcelm_value_t *result = tcelm_string_append(test_arena, empty, a);

    PROP_ASSERT_STR_EQ(ctx, TCELM_AS_STRING(result)->data, str);

    free(str);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: length(append(a, b)) = length(a) + length(b)
 */
static proptest_result_t prop_append_length(proptest_ctx_t *ctx) {
    setup_arena();

    char *str1 = proptest_rand_string(ctx, 0, 50);
    char *str2 = proptest_rand_string(ctx, 0, 50);
    if (!str1 || !str2) {
        free(str1);
        free(str2);
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *a = tcelm_string(test_arena, str1);
    tcelm_value_t *b = tcelm_string(test_arena, str2);
    tcelm_value_t *result = tcelm_string_append(test_arena, a, b);

    tcelm_value_t *len_a = tcelm_string_length(test_arena, a);
    tcelm_value_t *len_b = tcelm_string_length(test_arena, b);
    tcelm_value_t *len_result = tcelm_string_length(test_arena, result);

    PROP_ASSERT_EQ(ctx, TCELM_AS_INT(len_result),
                   TCELM_AS_INT(len_a) + TCELM_AS_INT(len_b), "%lld");

    free(str1);
    free(str2);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: slice(0, length(s), s) = s
 */
static proptest_result_t prop_slice_full(proptest_ctx_t *ctx) {
    setup_arena();

    char *str = proptest_rand_string(ctx, 0, 50);
    if (!str) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *s = tcelm_string(test_arena, str);
    tcelm_value_t *len = tcelm_string_length(test_arena, s);
    tcelm_value_t *zero = tcelm_int(test_arena, 0);
    tcelm_value_t *result = tcelm_string_slice(test_arena, zero, len, s);

    PROP_ASSERT_STR_EQ(ctx, TCELM_AS_STRING(result)->data, str);

    free(str);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: slice(0, 0, s) = ""
 */
static proptest_result_t prop_slice_empty(proptest_ctx_t *ctx) {
    setup_arena();

    char *str = proptest_rand_string(ctx, 1, 50);
    if (!str) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *s = tcelm_string(test_arena, str);
    tcelm_value_t *zero = tcelm_int(test_arena, 0);
    tcelm_value_t *result = tcelm_string_slice(test_arena, zero, zero, s);

    PROP_ASSERT_STR_EQ(ctx, TCELM_AS_STRING(result)->data, "");

    free(str);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: left(n, s) ++ dropLeft(n, s) = s
 */
static proptest_result_t prop_left_dropLeft(proptest_ctx_t *ctx) {
    setup_arena();

    /* Generate ASCII string for simpler length calculation */
    size_t len = (size_t)proptest_rand_int(ctx, 5, 30);
    char *str = malloc(len + 1);
    if (!str) {
        teardown_arena();
        return PROPTEST_SKIP;
    }
    for (size_t i = 0; i < len; i++) {
        str[i] = (char)proptest_rand_int(ctx, 'a', 'z');
    }
    str[len] = '\0';

    int64_t n = proptest_rand_int(ctx, 0, (int64_t)len);

    tcelm_value_t *s = tcelm_string(test_arena, str);
    tcelm_value_t *vn = tcelm_int(test_arena, n);

    tcelm_value_t *left_part = tcelm_string_left(test_arena, vn, s);
    tcelm_value_t *right_part = tcelm_string_dropLeft(test_arena, vn, s);
    tcelm_value_t *result = tcelm_string_append(test_arena, left_part, right_part);

    PROP_ASSERT_STR_EQ(ctx, TCELM_AS_STRING(result)->data, str);

    free(str);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: isEmpty("") = True
 */
static proptest_result_t prop_isEmpty_empty(proptest_ctx_t *ctx) {
    (void)ctx;
    setup_arena();

    tcelm_value_t *s = tcelm_string(test_arena, "");
    tcelm_value_t *result = tcelm_string_isEmpty(test_arena, s);

    PROP_ASSERT(ctx, TCELM_AS_BOOL(result) == true, "Empty string should be empty");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: isEmpty(non-empty) = False
 */
static proptest_result_t prop_isEmpty_nonempty(proptest_ctx_t *ctx) {
    setup_arena();

    char *str = proptest_rand_string(ctx, 1, 50);
    if (!str) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *s = tcelm_string(test_arena, str);
    tcelm_value_t *result = tcelm_string_isEmpty(test_arena, s);

    PROP_ASSERT(ctx, TCELM_AS_BOOL(result) == false, "Non-empty string should not be empty");

    free(str);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: contains(s, s) = True
 */
static proptest_result_t prop_contains_self(proptest_ctx_t *ctx) {
    setup_arena();

    char *str = proptest_rand_string(ctx, 1, 30);
    if (!str) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *s = tcelm_string(test_arena, str);
    tcelm_value_t *result = tcelm_string_contains(test_arena, s, s);

    PROP_ASSERT(ctx, TCELM_AS_BOOL(result) == true, "String should contain itself");

    free(str);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: contains("", s) = True (empty string is in everything)
 */
static proptest_result_t prop_contains_empty(proptest_ctx_t *ctx) {
    setup_arena();

    char *str = proptest_rand_string(ctx, 0, 30);
    if (!str) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *s = tcelm_string(test_arena, str);
    tcelm_value_t *empty = tcelm_string(test_arena, "");
    tcelm_value_t *result = tcelm_string_contains(test_arena, empty, s);

    PROP_ASSERT(ctx, TCELM_AS_BOOL(result) == true, "Empty string is in every string");

    free(str);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: startsWith(s, append(s, t)) = True
 */
static proptest_result_t prop_startsWith_prefix(proptest_ctx_t *ctx) {
    setup_arena();

    char *prefix = proptest_rand_string(ctx, 1, 20);
    char *suffix = proptest_rand_string(ctx, 0, 20);
    if (!prefix || !suffix) {
        free(prefix);
        free(suffix);
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *p = tcelm_string(test_arena, prefix);
    tcelm_value_t *t = tcelm_string(test_arena, suffix);
    tcelm_value_t *combined = tcelm_string_append(test_arena, p, t);
    tcelm_value_t *result = tcelm_string_startsWith(test_arena, p, combined);

    PROP_ASSERT(ctx, TCELM_AS_BOOL(result) == true, "String should start with its prefix");

    free(prefix);
    free(suffix);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: endsWith(s, append(t, s)) = True
 */
static proptest_result_t prop_endsWith_suffix(proptest_ctx_t *ctx) {
    setup_arena();

    char *prefix = proptest_rand_string(ctx, 0, 20);
    char *suffix = proptest_rand_string(ctx, 1, 20);
    if (!prefix || !suffix) {
        free(prefix);
        free(suffix);
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *p = tcelm_string(test_arena, prefix);
    tcelm_value_t *s = tcelm_string(test_arena, suffix);
    tcelm_value_t *combined = tcelm_string_append(test_arena, p, s);
    tcelm_value_t *result = tcelm_string_endsWith(test_arena, s, combined);

    PROP_ASSERT(ctx, TCELM_AS_BOOL(result) == true, "String should end with its suffix");

    free(prefix);
    free(suffix);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: reverse(reverse(s)) = s
 */
static proptest_result_t prop_reverse_involutive(proptest_ctx_t *ctx) {
    setup_arena();

    /* Use ASCII for simpler testing */
    size_t len = (size_t)proptest_rand_int(ctx, 0, 30);
    char *str = malloc(len + 1);
    if (!str) {
        teardown_arena();
        return PROPTEST_SKIP;
    }
    for (size_t i = 0; i < len; i++) {
        str[i] = (char)proptest_rand_int(ctx, 'a', 'z');
    }
    str[len] = '\0';

    tcelm_value_t *s = tcelm_string(test_arena, str);
    tcelm_value_t *rev = tcelm_string_reverse(test_arena, s);
    tcelm_value_t *rev_rev = tcelm_string_reverse(test_arena, rev);

    PROP_ASSERT_STR_EQ(ctx, TCELM_AS_STRING(rev_rev)->data, str);

    free(str);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: length(reverse(s)) = length(s)
 */
static proptest_result_t prop_reverse_preserves_length(proptest_ctx_t *ctx) {
    setup_arena();

    char *str = proptest_rand_string(ctx, 0, 50);
    if (!str) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *s = tcelm_string(test_arena, str);
    tcelm_value_t *rev = tcelm_string_reverse(test_arena, s);

    tcelm_value_t *len_s = tcelm_string_length(test_arena, s);
    tcelm_value_t *len_rev = tcelm_string_length(test_arena, rev);

    PROP_ASSERT_EQ(ctx, TCELM_AS_INT(len_s), TCELM_AS_INT(len_rev), "%lld");

    free(str);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: join(sep, split(sep, s)) contains all original chars
 */
static proptest_result_t prop_split_join_roundtrip(proptest_ctx_t *ctx) {
    setup_arena();

    /* Use a simple separator and string */
    char *str = proptest_rand_string(ctx, 5, 40);
    if (!str) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    /* Insert some separators */
    const char *sep = ",";
    tcelm_value_t *s = tcelm_string(test_arena, str);
    tcelm_value_t *vsep = tcelm_string(test_arena, sep);

    tcelm_value_t *parts = tcelm_string_split(test_arena, vsep, s);
    tcelm_value_t *joined = tcelm_string_join(test_arena, vsep, parts);

    PROP_ASSERT_STR_EQ(ctx, TCELM_AS_STRING(joined)->data, str);

    free(str);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: replace(a, a, s) = s
 */
static proptest_result_t prop_replace_same(proptest_ctx_t *ctx) {
    setup_arena();

    char *str = proptest_rand_string(ctx, 0, 30);
    char *pattern = proptest_rand_string(ctx, 1, 5);
    if (!str || !pattern) {
        free(str);
        free(pattern);
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *s = tcelm_string(test_arena, str);
    tcelm_value_t *p = tcelm_string(test_arena, pattern);
    tcelm_value_t *result = tcelm_string_replace(test_arena, p, p, s);

    PROP_ASSERT_STR_EQ(ctx, TCELM_AS_STRING(result)->data, str);

    free(str);
    free(pattern);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: fromInt(n) parses back to n
 */
static proptest_result_t prop_fromInt_roundtrip(proptest_ctx_t *ctx) {
    setup_arena();

    int64_t n = proptest_rand_int(ctx, -1000000, 1000000);
    tcelm_value_t *vn = tcelm_int(test_arena, n);
    tcelm_value_t *str = tcelm_string_from_int(test_arena, vn);

    /* Parse back manually */
    const char *s = TCELM_AS_STRING(str)->data;
    int64_t parsed = atoll(s);

    PROP_ASSERT_EQ(ctx, parsed, n, "%lld");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Test suite definition
 */
proptest_case_t string_tests[] = {
    {"length_non_negative", prop_length_non_negative, 100},
    {"empty_length_zero", prop_empty_length_zero, 1},
    {"append_empty_right", prop_append_empty_right, 100},
    {"append_empty_left", prop_append_empty_left, 100},
    {"append_length", prop_append_length, 100},
    {"slice_full", prop_slice_full, 100},
    {"slice_empty", prop_slice_empty, 100},
    {"left_dropLeft", prop_left_dropLeft, 100},
    {"isEmpty_empty", prop_isEmpty_empty, 1},
    {"isEmpty_nonempty", prop_isEmpty_nonempty, 100},
    {"contains_self", prop_contains_self, 100},
    {"contains_empty", prop_contains_empty, 100},
    {"startsWith_prefix", prop_startsWith_prefix, 100},
    {"endsWith_suffix", prop_endsWith_suffix, 100},
    {"reverse_involutive", prop_reverse_involutive, 100},
    {"reverse_preserves_length", prop_reverse_preserves_length, 100},
    {"split_join_roundtrip", prop_split_join_roundtrip, 100},
    {"replace_same", prop_replace_same, 100},
    {"fromInt_roundtrip", prop_fromInt_roundtrip, 100},
};

int main(int argc, char **argv) {
    uint64_t seed = 0;
    if (argc > 1) {
        seed = (uint64_t)atoll(argv[1]);
    }

    return proptest_run_suite("tcelm String",
                              string_tests,
                              sizeof(string_tests) / sizeof(string_tests[0]),
                              seed);
}
