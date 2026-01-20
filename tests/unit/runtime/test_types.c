/*
 * test_types.c - Property-based tests for tcelm type operations
 *
 * Tests value creation, type checking, and basic operations.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../property/proptest.h"
#include "tcelm_arena.h"
#include "tcelm_types.h"
#include "tcelm_basics.h"

/* Test arena - created fresh for each property test */
static tcelm_arena_t test_arena_storage;
static tcelm_arena_t *test_arena;

static void setup_arena(void) {
    tcelm_arena_init(&test_arena_storage, 1024 * 1024);  /* 1MB */
    test_arena = &test_arena_storage;
    tcelm_current_arena = test_arena;  /* Set global arena for functions that use it */
}

static void teardown_arena(void) {
    tcelm_arena_destroy(test_arena);
    test_arena = NULL;
}

/*
 * Property: Int creation preserves value
 */
static proptest_result_t prop_int_roundtrip(proptest_ctx_t *ctx) {
    setup_arena();

    int64_t value = proptest_rand_int(ctx, INT64_MIN / 2, INT64_MAX / 2);
    tcelm_value_t *v = tcelm_int(test_arena, value);

    PROP_ASSERT(ctx, TCELM_IS_INT(v), "Expected INT tag");
    PROP_ASSERT_EQ(ctx, TCELM_AS_INT(v), value, "%lld");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: Float creation preserves value
 */
static proptest_result_t prop_float_roundtrip(proptest_ctx_t *ctx) {
    setup_arena();

    double value = proptest_rand_double(ctx, -1e10, 1e10);
    tcelm_value_t *v = tcelm_float(test_arena, value);

    PROP_ASSERT(ctx, TCELM_IS_FLOAT(v), "Expected FLOAT tag");
    PROP_ASSERT_EQ(ctx, TCELM_AS_FLOAT(v), value, "%f");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: Bool creation preserves value
 */
static proptest_result_t prop_bool_roundtrip(proptest_ctx_t *ctx) {
    setup_arena();

    bool value = proptest_rand_bool(ctx);
    tcelm_value_t *v = tcelm_bool(test_arena, value);

    PROP_ASSERT(ctx, TCELM_IS_BOOL(v), "Expected BOOL tag");
    PROP_ASSERT_EQ(ctx, TCELM_AS_BOOL(v), value, "%d");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: Char creation preserves value (Unicode codepoints)
 */
static proptest_result_t prop_char_roundtrip(proptest_ctx_t *ctx) {
    setup_arena();

    uint32_t value = (uint32_t)proptest_rand_int(ctx, 0, 0x10FFFF);
    /* Skip surrogate pairs */
    if (value >= 0xD800 && value <= 0xDFFF) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *v = tcelm_char(test_arena, value);

    PROP_ASSERT(ctx, TCELM_IS_CHAR(v), "Expected CHAR tag");
    PROP_ASSERT_EQ(ctx, TCELM_AS_CHAR(v), value, "%u");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: String creation preserves content
 */
static proptest_result_t prop_string_roundtrip(proptest_ctx_t *ctx) {
    setup_arena();

    char *value = proptest_rand_string(ctx, 0, 100);
    if (!value) {
        teardown_arena();
        return PROPTEST_SKIP;
    }

    tcelm_value_t *v = tcelm_string(test_arena, value);

    PROP_ASSERT(ctx, TCELM_IS_STRING(v), "Expected STRING tag");
    PROP_ASSERT_STR_EQ(ctx, TCELM_AS_STRING(v)->data, value);

    free(value);
    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: Int addition is commutative
 */
static proptest_result_t prop_int_add_commutative(proptest_ctx_t *ctx) {
    setup_arena();

    int64_t a = proptest_rand_int(ctx, -1000000, 1000000);
    int64_t b = proptest_rand_int(ctx, -1000000, 1000000);

    tcelm_value_t *va = tcelm_int(test_arena, a);
    tcelm_value_t *vb = tcelm_int(test_arena, b);

    tcelm_value_t *ab = tcelm_add_int(test_arena, va, vb);
    tcelm_value_t *ba = tcelm_add_int(test_arena, vb, va);

    PROP_ASSERT_EQ(ctx, TCELM_AS_INT(ab), TCELM_AS_INT(ba), "%lld");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: Int addition is associative
 */
static proptest_result_t prop_int_add_associative(proptest_ctx_t *ctx) {
    setup_arena();

    int64_t a = proptest_rand_int(ctx, -100000, 100000);
    int64_t b = proptest_rand_int(ctx, -100000, 100000);
    int64_t c = proptest_rand_int(ctx, -100000, 100000);

    tcelm_value_t *va = tcelm_int(test_arena, a);
    tcelm_value_t *vb = tcelm_int(test_arena, b);
    tcelm_value_t *vc = tcelm_int(test_arena, c);

    /* (a + b) + c */
    tcelm_value_t *ab = tcelm_add_int(test_arena, va, vb);
    tcelm_value_t *ab_c = tcelm_add_int(test_arena, ab, vc);

    /* a + (b + c) */
    tcelm_value_t *bc = tcelm_add_int(test_arena, vb, vc);
    tcelm_value_t *a_bc = tcelm_add_int(test_arena, va, bc);

    PROP_ASSERT_EQ(ctx, TCELM_AS_INT(ab_c), TCELM_AS_INT(a_bc), "%lld");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: Int multiplication is commutative
 */
static proptest_result_t prop_int_mul_commutative(proptest_ctx_t *ctx) {
    setup_arena();

    int64_t a = proptest_rand_int(ctx, -10000, 10000);
    int64_t b = proptest_rand_int(ctx, -10000, 10000);

    tcelm_value_t *va = tcelm_int(test_arena, a);
    tcelm_value_t *vb = tcelm_int(test_arena, b);

    tcelm_value_t *ab = tcelm_mul_int(test_arena, va, vb);
    tcelm_value_t *ba = tcelm_mul_int(test_arena, vb, va);

    PROP_ASSERT_EQ(ctx, TCELM_AS_INT(ab), TCELM_AS_INT(ba), "%lld");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: Int subtraction a - a = 0
 */
static proptest_result_t prop_int_sub_self(proptest_ctx_t *ctx) {
    setup_arena();

    int64_t a = proptest_rand_int(ctx, INT64_MIN / 2, INT64_MAX / 2);
    tcelm_value_t *va = tcelm_int(test_arena, a);

    tcelm_value_t *result = tcelm_sub_int(test_arena, va, va);

    PROP_ASSERT_EQ(ctx, TCELM_AS_INT(result), 0LL, "%lld");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: Negate twice returns original
 */
static proptest_result_t prop_int_negate_twice(proptest_ctx_t *ctx) {
    setup_arena();

    int64_t a = proptest_rand_int(ctx, INT64_MIN / 2 + 1, INT64_MAX / 2);
    tcelm_value_t *va = tcelm_int(test_arena, a);

    tcelm_value_t *neg = tcelm_negate_int(test_arena, va);
    tcelm_value_t *neg_neg = tcelm_negate_int(test_arena, neg);

    PROP_ASSERT_EQ(ctx, TCELM_AS_INT(neg_neg), a, "%lld");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: abs(x) >= 0
 */
static proptest_result_t prop_int_abs_non_negative(proptest_ctx_t *ctx) {
    setup_arena();

    int64_t a = proptest_rand_int(ctx, INT64_MIN / 2 + 1, INT64_MAX / 2);
    tcelm_value_t *va = tcelm_int(test_arena, a);

    tcelm_value_t *result = tcelm_abs_int(test_arena, va);

    PROP_ASSERT(ctx, TCELM_AS_INT(result) >= 0, "abs should be non-negative");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: abs(x) = abs(-x)
 */
static proptest_result_t prop_int_abs_symmetric(proptest_ctx_t *ctx) {
    setup_arena();

    int64_t a = proptest_rand_int(ctx, INT64_MIN / 2 + 1, INT64_MAX / 2 - 1);
    tcelm_value_t *va = tcelm_int(test_arena, a);
    tcelm_value_t *neg_va = tcelm_negate_int(test_arena, va);

    tcelm_value_t *abs_a = tcelm_abs_int(test_arena, va);
    tcelm_value_t *abs_neg_a = tcelm_abs_int(test_arena, neg_va);

    PROP_ASSERT_EQ(ctx, TCELM_AS_INT(abs_a), TCELM_AS_INT(abs_neg_a), "%lld");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: Comparison reflexivity (a == a)
 */
static proptest_result_t prop_eq_reflexive(proptest_ctx_t *ctx) {
    setup_arena();

    int64_t a = proptest_rand_int(ctx, INT64_MIN / 2, INT64_MAX / 2);
    tcelm_value_t *va = tcelm_int(test_arena, a);

    tcelm_value_t *result = tcelm_eq(test_arena, va, va);

    PROP_ASSERT(ctx, TCELM_AS_BOOL(result) == true, "a == a should be true");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: Comparison symmetry (a == b) <=> (b == a)
 */
static proptest_result_t prop_eq_symmetric(proptest_ctx_t *ctx) {
    setup_arena();

    int64_t a = proptest_rand_int(ctx, -1000000, 1000000);
    int64_t b = proptest_rand_int(ctx, -1000000, 1000000);

    tcelm_value_t *va = tcelm_int(test_arena, a);
    tcelm_value_t *vb = tcelm_int(test_arena, b);

    tcelm_value_t *ab = tcelm_eq(test_arena, va, vb);
    tcelm_value_t *ba = tcelm_eq(test_arena, vb, va);

    PROP_ASSERT_EQ(ctx, TCELM_AS_BOOL(ab), TCELM_AS_BOOL(ba), "%d");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: lt is irreflexive (not a < a)
 */
static proptest_result_t prop_lt_irreflexive(proptest_ctx_t *ctx) {
    setup_arena();

    int64_t a = proptest_rand_int(ctx, INT64_MIN / 2, INT64_MAX / 2);
    tcelm_value_t *va = tcelm_int(test_arena, a);

    tcelm_value_t *result = tcelm_lt(test_arena, va, va);

    PROP_ASSERT(ctx, TCELM_AS_BOOL(result) == false, "a < a should be false");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: lt is asymmetric (a < b) => not (b < a)
 */
static proptest_result_t prop_lt_asymmetric(proptest_ctx_t *ctx) {
    setup_arena();

    int64_t a = proptest_rand_int(ctx, -1000000, 1000000);
    int64_t b = proptest_rand_int(ctx, -1000000, 1000000);

    tcelm_value_t *va = tcelm_int(test_arena, a);
    tcelm_value_t *vb = tcelm_int(test_arena, b);

    tcelm_value_t *a_lt_b = tcelm_lt(test_arena, va, vb);
    tcelm_value_t *b_lt_a = tcelm_lt(test_arena, vb, va);

    if (TCELM_AS_BOOL(a_lt_b)) {
        PROP_ASSERT(ctx, !TCELM_AS_BOOL(b_lt_a), "a < b => not (b < a)");
    }

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: min(a, b) <= a and min(a, b) <= b
 */
static proptest_result_t prop_min_le_both(proptest_ctx_t *ctx) {
    setup_arena();

    int64_t a = proptest_rand_int(ctx, -1000000, 1000000);
    int64_t b = proptest_rand_int(ctx, -1000000, 1000000);

    tcelm_value_t *va = tcelm_int(test_arena, a);
    tcelm_value_t *vb = tcelm_int(test_arena, b);

    tcelm_value_t *min_ab = tcelm_min(test_arena, va, vb);
    int64_t min_val = TCELM_AS_INT(min_ab);

    PROP_ASSERT(ctx, min_val <= a, "min(a,b) <= a");
    PROP_ASSERT(ctx, min_val <= b, "min(a,b) <= b");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: max(a, b) >= a and max(a, b) >= b
 */
static proptest_result_t prop_max_ge_both(proptest_ctx_t *ctx) {
    setup_arena();

    int64_t a = proptest_rand_int(ctx, -1000000, 1000000);
    int64_t b = proptest_rand_int(ctx, -1000000, 1000000);

    tcelm_value_t *va = tcelm_int(test_arena, a);
    tcelm_value_t *vb = tcelm_int(test_arena, b);

    tcelm_value_t *max_ab = tcelm_max(test_arena, va, vb);
    int64_t max_val = TCELM_AS_INT(max_ab);

    PROP_ASSERT(ctx, max_val >= a, "max(a,b) >= a");
    PROP_ASSERT(ctx, max_val >= b, "max(a,b) >= b");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: Boolean AND is commutative
 */
static proptest_result_t prop_and_commutative(proptest_ctx_t *ctx) {
    setup_arena();

    bool a = proptest_rand_bool(ctx);
    bool b = proptest_rand_bool(ctx);

    tcelm_value_t *va = tcelm_bool(test_arena, a);
    tcelm_value_t *vb = tcelm_bool(test_arena, b);

    tcelm_value_t *ab = tcelm_and(test_arena, va, vb);
    tcelm_value_t *ba = tcelm_and(test_arena, vb, va);

    PROP_ASSERT_EQ(ctx, TCELM_AS_BOOL(ab), TCELM_AS_BOOL(ba), "%d");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: Boolean OR is commutative
 */
static proptest_result_t prop_or_commutative(proptest_ctx_t *ctx) {
    setup_arena();

    bool a = proptest_rand_bool(ctx);
    bool b = proptest_rand_bool(ctx);

    tcelm_value_t *va = tcelm_bool(test_arena, a);
    tcelm_value_t *vb = tcelm_bool(test_arena, b);

    tcelm_value_t *ab = tcelm_or(test_arena, va, vb);
    tcelm_value_t *ba = tcelm_or(test_arena, vb, va);

    PROP_ASSERT_EQ(ctx, TCELM_AS_BOOL(ab), TCELM_AS_BOOL(ba), "%d");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Property: not(not(x)) = x
 */
static proptest_result_t prop_not_involutive(proptest_ctx_t *ctx) {
    setup_arena();

    bool a = proptest_rand_bool(ctx);
    tcelm_value_t *va = tcelm_bool(test_arena, a);

    tcelm_value_t *not_a = tcelm_not(test_arena, va);
    tcelm_value_t *not_not_a = tcelm_not(test_arena, not_a);

    PROP_ASSERT_EQ(ctx, TCELM_AS_BOOL(not_not_a), a, "%d");

    teardown_arena();
    return PROPTEST_PASS;
}

/*
 * Test suite definition
 */
proptest_case_t type_tests[] = {
    {"int_roundtrip", prop_int_roundtrip, 100},
    {"float_roundtrip", prop_float_roundtrip, 100},
    {"bool_roundtrip", prop_bool_roundtrip, 100},
    {"char_roundtrip", prop_char_roundtrip, 100},
    {"string_roundtrip", prop_string_roundtrip, 100},
    {"int_add_commutative", prop_int_add_commutative, 100},
    {"int_add_associative", prop_int_add_associative, 100},
    {"int_mul_commutative", prop_int_mul_commutative, 100},
    {"int_sub_self", prop_int_sub_self, 100},
    {"int_negate_twice", prop_int_negate_twice, 100},
    {"int_abs_non_negative", prop_int_abs_non_negative, 100},
    {"int_abs_symmetric", prop_int_abs_symmetric, 100},
    {"eq_reflexive", prop_eq_reflexive, 100},
    {"eq_symmetric", prop_eq_symmetric, 100},
    {"lt_irreflexive", prop_lt_irreflexive, 100},
    {"lt_asymmetric", prop_lt_asymmetric, 100},
    {"min_le_both", prop_min_le_both, 100},
    {"max_ge_both", prop_max_ge_both, 100},
    {"and_commutative", prop_and_commutative, 100},
    {"or_commutative", prop_or_commutative, 100},
    {"not_involutive", prop_not_involutive, 100},
};

int main(int argc, char **argv) {
    uint64_t seed = 0;
    if (argc > 1) {
        seed = (uint64_t)atoll(argv[1]);
    }

    return proptest_run_suite("tcelm Types",
                              type_tests,
                              sizeof(type_tests) / sizeof(type_tests[0]),
                              seed);
}
