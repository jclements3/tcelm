/*
 * test_runtime.c - Test the tcelm runtime
 *
 * Compile with: tcc -o test_runtime test_runtime.c tcelm_arena.c tcelm_types.c tcelm_basics.c -lm
 * Or with gcc:  gcc -o test_runtime test_runtime.c tcelm_arena.c tcelm_types.c tcelm_basics.c -lm
 */

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "tcelm_arena.h"
#include "tcelm_types.h"
#include "tcelm_basics.h"

/* Test helper to run a test and report result */
#define TEST(name, expr) do { \
    printf("  %-40s ", name); \
    if (expr) { printf("[PASS]\n"); } \
    else { printf("[FAIL]\n"); failures++; } \
} while(0)

int main(void) {
    int failures = 0;
    tcelm_arena_t arena;

    printf("=== tcelm Runtime Tests ===\n\n");

    /* Initialize arena */
    printf("Arena initialization...\n");
    assert(tcelm_arena_init(&arena, 64 * 1024) == 0);
    tcelm_current_arena = &arena;
    printf("  Arena created with 64KB block size\n\n");

    /* Test primitives */
    printf("Primitive types:\n");
    {
        tcelm_value_t *i = tcelm_int(&arena, 42);
        TEST("Int creation", TCELM_IS_INT(i) && TCELM_AS_INT(i) == 42);

        tcelm_value_t *f = tcelm_float(&arena, 3.14159);
        TEST("Float creation", TCELM_IS_FLOAT(f) && TCELM_AS_FLOAT(f) > 3.14);

        tcelm_value_t *c = tcelm_char(&arena, 'A');
        TEST("Char creation", TCELM_IS_CHAR(c) && TCELM_AS_CHAR(c) == 'A');

        tcelm_value_t *s = tcelm_string(&arena, "Hello, World!");
        TEST("String creation", TCELM_IS_STRING(s) && TCELM_AS_STRING(s)->length == 13);

        tcelm_value_t *t = tcelm_bool(&arena, true);
        TEST("Bool True", TCELM_IS_BOOL(t) && TCELM_AS_BOOL(t) == true);

        tcelm_value_t *fb = tcelm_bool(&arena, false);
        TEST("Bool False", TCELM_IS_BOOL(fb) && TCELM_AS_BOOL(fb) == false);

        tcelm_value_t *u = tcelm_unit(&arena);
        TEST("Unit", u->tag == TCELM_TAG_UNIT);
    }
    printf("\n");

    /* Test arithmetic */
    printf("Int arithmetic:\n");
    {
        tcelm_value_t *a = tcelm_int(&arena, 10);
        tcelm_value_t *b = tcelm_int(&arena, 3);

        tcelm_value_t *sum = tcelm_add_int(&arena, a, b);
        TEST("10 + 3 = 13", TCELM_AS_INT(sum) == 13);

        tcelm_value_t *diff = tcelm_sub_int(&arena, a, b);
        TEST("10 - 3 = 7", TCELM_AS_INT(diff) == 7);

        tcelm_value_t *prod = tcelm_mul_int(&arena, a, b);
        TEST("10 * 3 = 30", TCELM_AS_INT(prod) == 30);

        tcelm_value_t *quot = tcelm_div_int(&arena, a, b);
        TEST("10 // 3 = 3", TCELM_AS_INT(quot) == 3);

        tcelm_value_t *rem = tcelm_mod_int(&arena, a, b);
        TEST("10 % 3 = 1", TCELM_AS_INT(rem) == 1);
    }
    printf("\n");

    /* Test float math */
    printf("Float math:\n");
    {
        tcelm_value_t *x = tcelm_float(&arena, 2.0);
        tcelm_value_t *sq = tcelm_sqrt(&arena, x);
        TEST("sqrt(2.0) ≈ 1.414", TCELM_AS_FLOAT(sq) > 1.41 && TCELM_AS_FLOAT(sq) < 1.42);

        tcelm_value_t *zero = tcelm_float(&arena, 0.0);
        tcelm_value_t *s = tcelm_sin(&arena, zero);
        TEST("sin(0) = 0", TCELM_AS_FLOAT(s) == 0.0);

        tcelm_value_t *c = tcelm_cos(&arena, zero);
        TEST("cos(0) = 1", TCELM_AS_FLOAT(c) == 1.0);
    }
    printf("\n");

    /* Test comparisons */
    printf("Comparisons:\n");
    {
        tcelm_value_t *a = tcelm_int(&arena, 5);
        tcelm_value_t *b = tcelm_int(&arena, 10);

        TEST("5 < 10", TCELM_AS_BOOL(tcelm_lt(&arena, a, b)));
        TEST("5 <= 10", TCELM_AS_BOOL(tcelm_le(&arena, a, b)));
        TEST("!(5 > 10)", !TCELM_AS_BOOL(tcelm_gt(&arena, a, b)));
        TEST("5 /= 10", TCELM_AS_BOOL(tcelm_neq(&arena, a, b)));

        tcelm_value_t *same = tcelm_int(&arena, 5);
        TEST("5 == 5", TCELM_AS_BOOL(tcelm_eq(&arena, a, same)));
    }
    printf("\n");

    /* Test lists */
    printf("Lists:\n");
    {
        tcelm_value_t *empty = tcelm_nil(&arena);
        TEST("[] is nil", tcelm_is_nil(empty));

        tcelm_value_t *one = tcelm_cons(&arena, tcelm_int(&arena, 1), empty);
        TEST("[1] head = 1", TCELM_AS_INT(tcelm_list_head(one)) == 1);
        TEST("[1] tail is nil", tcelm_is_nil(tcelm_list_tail(one)));

        tcelm_value_t *list = tcelm_cons(&arena, tcelm_int(&arena, 1),
                              tcelm_cons(&arena, tcelm_int(&arena, 2),
                              tcelm_cons(&arena, tcelm_int(&arena, 3), empty)));
        tcelm_value_t *len = tcelm_list_length(&arena, list);
        TEST("[1,2,3] length = 3", TCELM_AS_INT(len) == 3);

        tcelm_value_t *rev = tcelm_list_reverse(&arena, list);
        TEST("reverse [1,2,3] head = 3", TCELM_AS_INT(tcelm_list_head(rev)) == 3);

        tcelm_value_t *range = tcelm_list_range(&arena, tcelm_int(&arena, 1), tcelm_int(&arena, 5));
        tcelm_value_t *rlen = tcelm_list_length(&arena, range);
        TEST("List.range 1 5 length = 5", TCELM_AS_INT(rlen) == 5);
    }
    printf("\n");

    /* Test tuples */
    printf("Tuples:\n");
    {
        tcelm_value_t *t2 = tcelm_tuple2(&arena, tcelm_int(&arena, 1), tcelm_string(&arena, "hello"));
        TEST("(1, \"hello\") is tuple2", TCELM_IS_TUPLE2(t2));
        TEST("Tuple.first = 1", TCELM_AS_INT(tcelm_tuple2_first(t2)) == 1);

        tcelm_value_t *t3 = tcelm_tuple3(&arena,
            tcelm_int(&arena, 1),
            tcelm_int(&arena, 2),
            tcelm_int(&arena, 3));
        TEST("(1, 2, 3) is tuple3", TCELM_IS_TUPLE3(t3));
    }
    printf("\n");

    /* Test records */
    printf("Records:\n");
    {
        tcelm_value_t *rec = tcelm_record(&arena, 2,
            "x", tcelm_int(&arena, 10),
            "y", tcelm_int(&arena, 20));
        TEST("{ x = 10, y = 20 } is record", TCELM_IS_RECORD(rec));

        tcelm_value_t *x = tcelm_record_get(rec, "x");
        TEST("record.x = 10", x != NULL && TCELM_AS_INT(x) == 10);

        tcelm_value_t *y = tcelm_record_get(rec, "y");
        TEST("record.y = 20", y != NULL && TCELM_AS_INT(y) == 20);

        tcelm_value_t *updated = tcelm_record_update(&arena, rec, "x", tcelm_int(&arena, 100));
        tcelm_value_t *newx = tcelm_record_get(updated, "x");
        TEST("{ record | x = 100 }.x = 100", TCELM_AS_INT(newx) == 100);

        /* Original should be unchanged */
        tcelm_value_t *oldx = tcelm_record_get(rec, "x");
        TEST("original record.x still 10", TCELM_AS_INT(oldx) == 10);
    }
    printf("\n");

    /* Test Maybe */
    printf("Maybe:\n");
    {
        tcelm_value_t *nothing = tcelm_nothing(&arena);
        TEST("Nothing ctor = 0", tcelm_custom_ctor(nothing) == TCELM_CTOR_NOTHING);

        tcelm_value_t *just42 = tcelm_just(&arena, tcelm_int(&arena, 42));
        TEST("Just 42 is just", tcelm_is_just(just42));
        TEST("Just 42 unwrap = 42", TCELM_AS_INT(tcelm_maybe_unwrap(just42)) == 42);
    }
    printf("\n");

    /* Test Result */
    printf("Result:\n");
    {
        tcelm_value_t *okVal = tcelm_ok(&arena, tcelm_int(&arena, 100));
        TEST("Ok 100 is ok", tcelm_is_ok(okVal));
        TEST("Ok 100 unwrap = 100", TCELM_AS_INT(tcelm_result_unwrap(okVal)) == 100);

        tcelm_value_t *errVal = tcelm_err(&arena, tcelm_string(&arena, "error!"));
        TEST("Err is not ok", !tcelm_is_ok(errVal));
    }
    printf("\n");

    /* Test strings */
    printf("Strings:\n");
    {
        tcelm_value_t *s1 = tcelm_string(&arena, "Hello, ");
        tcelm_value_t *s2 = tcelm_string(&arena, "World!");
        tcelm_value_t *cat = tcelm_string_append(&arena, s1, s2);
        TEST("\"Hello, \" ++ \"World!\"",
             strcmp(TCELM_AS_STRING(cat)->data, "Hello, World!") == 0);

        tcelm_value_t *len = tcelm_string_length(&arena, cat);
        TEST("String.length = 13", TCELM_AS_INT(len) == 13);

        tcelm_value_t *fromInt = tcelm_string_from_int(&arena, tcelm_int(&arena, 12345));
        TEST("String.fromInt 12345", strcmp(TCELM_AS_STRING(fromInt)->data, "12345") == 0);
    }
    printf("\n");

    /* Test arena stats */
    printf("Arena stats:\n");
    {
        tcelm_arena_stats_t stats;
        tcelm_arena_get_stats(&arena, &stats);
        printf("  Total allocated: %zu bytes\n", stats.total_allocated);
        printf("  Total used:      %zu bytes\n", stats.total_used);
        printf("  Block count:     %zu\n", stats.block_count);
        printf("  Efficiency:      %.1f%%\n",
               100.0 * stats.total_used / stats.total_allocated);
    }
    printf("\n");

    /* Test arena reset */
    printf("Arena reset:\n");
    {
        size_t used_before;
        tcelm_arena_stats_t stats;

        tcelm_arena_get_stats(&arena, &stats);
        used_before = stats.total_used;

        tcelm_arena_reset(&arena);
        tcelm_arena_get_stats(&arena, &stats);

        TEST("Reset clears used memory", stats.total_used == 0);
        printf("  Used before reset: %zu bytes\n", used_before);
        printf("  Used after reset:  %zu bytes\n", stats.total_used);

        /* Can still allocate after reset */
        tcelm_value_t *after = tcelm_int(&arena, 999);
        TEST("Can allocate after reset", TCELM_AS_INT(after) == 999);
    }
    printf("\n");

    /* Cleanup */
    tcelm_arena_destroy(&arena);

    /* Summary */
    printf("=== Test Summary ===\n");
    if (failures == 0) {
        printf("All tests passed!\n");
    } else {
        printf("%d test(s) FAILED\n", failures);
    }

    return failures;
}
