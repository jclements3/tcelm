/*
 * tcelm_basics.c - Basic operations implementation
 */

#include "tcelm_basics.h"
#include <math.h>
#include <stdio.h>
#include <string.h>

/* Order type pre-allocated values */
static tcelm_custom_t order_lt_custom = { .ctor_id = TCELM_CTOR_LT, .arg_count = 0, .ctor_name = "LT" };
static tcelm_custom_t order_eq_custom = { .ctor_id = TCELM_CTOR_EQ, .arg_count = 0, .ctor_name = "EQ" };
static tcelm_custom_t order_gt_custom = { .ctor_id = TCELM_CTOR_GT, .arg_count = 0, .ctor_name = "GT" };

tcelm_value_t tcelm_order_lt = { .tag = TCELM_TAG_CUSTOM, .data.custom = &order_lt_custom };
tcelm_value_t tcelm_order_eq = { .tag = TCELM_TAG_CUSTOM, .data.custom = &order_eq_custom };
tcelm_value_t tcelm_order_gt = { .tag = TCELM_TAG_CUSTOM, .data.custom = &order_gt_custom };

/*
 * Int arithmetic
 */

tcelm_value_t *tcelm_add_int(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    return tcelm_int(arena, TCELM_AS_INT(a) + TCELM_AS_INT(b));
}

tcelm_value_t *tcelm_sub_int(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    return tcelm_int(arena, TCELM_AS_INT(a) - TCELM_AS_INT(b));
}

tcelm_value_t *tcelm_mul_int(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    return tcelm_int(arena, TCELM_AS_INT(a) * TCELM_AS_INT(b));
}

tcelm_value_t *tcelm_div_int(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    int64_t bv = TCELM_AS_INT(b);
    if (bv == 0) return tcelm_int(arena, 0); /* Elm truncates to 0 on div by 0 */
    return tcelm_int(arena, TCELM_AS_INT(a) / bv);
}

tcelm_value_t *tcelm_mod_int(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    int64_t bv = TCELM_AS_INT(b);
    if (bv == 0) return tcelm_int(arena, 0);
    return tcelm_int(arena, TCELM_AS_INT(a) % bv);
}

tcelm_value_t *tcelm_negate_int(tcelm_arena_t *arena, tcelm_value_t *a) {
    return tcelm_int(arena, -TCELM_AS_INT(a));
}

tcelm_value_t *tcelm_abs_int(tcelm_arena_t *arena, tcelm_value_t *a) {
    int64_t v = TCELM_AS_INT(a);
    return tcelm_int(arena, v < 0 ? -v : v);
}

/*
 * Float arithmetic
 */

tcelm_value_t *tcelm_add_float(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    return tcelm_float(arena, TCELM_AS_FLOAT(a) + TCELM_AS_FLOAT(b));
}

tcelm_value_t *tcelm_sub_float(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    return tcelm_float(arena, TCELM_AS_FLOAT(a) - TCELM_AS_FLOAT(b));
}

tcelm_value_t *tcelm_mul_float(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    return tcelm_float(arena, TCELM_AS_FLOAT(a) * TCELM_AS_FLOAT(b));
}

tcelm_value_t *tcelm_div_float(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    return tcelm_float(arena, TCELM_AS_FLOAT(a) / TCELM_AS_FLOAT(b));
}

tcelm_value_t *tcelm_negate_float(tcelm_arena_t *arena, tcelm_value_t *a) {
    return tcelm_float(arena, -TCELM_AS_FLOAT(a));
}

tcelm_value_t *tcelm_abs_float(tcelm_arena_t *arena, tcelm_value_t *a) {
    return tcelm_float(arena, fabs(TCELM_AS_FLOAT(a)));
}

/*
 * Math functions
 */

tcelm_value_t *tcelm_sqrt(tcelm_arena_t *arena, tcelm_value_t *a) {
    return tcelm_float(arena, sqrt(TCELM_AS_FLOAT(a)));
}

tcelm_value_t *tcelm_sin(tcelm_arena_t *arena, tcelm_value_t *a) {
    return tcelm_float(arena, sin(TCELM_AS_FLOAT(a)));
}

tcelm_value_t *tcelm_cos(tcelm_arena_t *arena, tcelm_value_t *a) {
    return tcelm_float(arena, cos(TCELM_AS_FLOAT(a)));
}

tcelm_value_t *tcelm_tan(tcelm_arena_t *arena, tcelm_value_t *a) {
    return tcelm_float(arena, tan(TCELM_AS_FLOAT(a)));
}

tcelm_value_t *tcelm_atan2(tcelm_arena_t *arena, tcelm_value_t *y, tcelm_value_t *x) {
    return tcelm_float(arena, atan2(TCELM_AS_FLOAT(y), TCELM_AS_FLOAT(x)));
}

tcelm_value_t *tcelm_exp(tcelm_arena_t *arena, tcelm_value_t *a) {
    return tcelm_float(arena, exp(TCELM_AS_FLOAT(a)));
}

tcelm_value_t *tcelm_log(tcelm_arena_t *arena, tcelm_value_t *a) {
    return tcelm_float(arena, log(TCELM_AS_FLOAT(a)));
}

tcelm_value_t *tcelm_pow(tcelm_arena_t *arena, tcelm_value_t *base, tcelm_value_t *exp) {
    return tcelm_float(arena, pow(TCELM_AS_FLOAT(base), TCELM_AS_FLOAT(exp)));
}

/*
 * Comparisons
 */

/* Compare two values, returns -1, 0, or 1 */
static int value_compare(tcelm_value_t *a, tcelm_value_t *b) {
    if (a->tag != b->tag) {
        return (int)a->tag - (int)b->tag;
    }

    switch (a->tag) {
        case TCELM_TAG_INT: {
            int64_t av = TCELM_AS_INT(a);
            int64_t bv = TCELM_AS_INT(b);
            return (av > bv) - (av < bv);
        }
        case TCELM_TAG_FLOAT: {
            double av = TCELM_AS_FLOAT(a);
            double bv = TCELM_AS_FLOAT(b);
            return (av > bv) - (av < bv);
        }
        case TCELM_TAG_CHAR:
            return (int)TCELM_AS_CHAR(a) - (int)TCELM_AS_CHAR(b);
        case TCELM_TAG_STRING:
            return strcmp(TCELM_AS_STRING(a)->data, TCELM_AS_STRING(b)->data);
        case TCELM_TAG_BOOL:
            return (int)TCELM_AS_BOOL(a) - (int)TCELM_AS_BOOL(b);
        default:
            return 0; /* Unsupported comparison */
    }
}

tcelm_value_t *tcelm_eq(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    (void)arena;
    return value_compare(a, b) == 0 ? TCELM_TRUE : TCELM_FALSE;
}

tcelm_value_t *tcelm_neq(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    (void)arena;
    return value_compare(a, b) != 0 ? TCELM_TRUE : TCELM_FALSE;
}

tcelm_value_t *tcelm_lt(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    (void)arena;
    return value_compare(a, b) < 0 ? TCELM_TRUE : TCELM_FALSE;
}

tcelm_value_t *tcelm_le(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    (void)arena;
    return value_compare(a, b) <= 0 ? TCELM_TRUE : TCELM_FALSE;
}

tcelm_value_t *tcelm_gt(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    (void)arena;
    return value_compare(a, b) > 0 ? TCELM_TRUE : TCELM_FALSE;
}

tcelm_value_t *tcelm_ge(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    (void)arena;
    return value_compare(a, b) >= 0 ? TCELM_TRUE : TCELM_FALSE;
}

tcelm_value_t *tcelm_compare(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    (void)arena;
    int cmp = value_compare(a, b);
    if (cmp < 0) return TCELM_LT;
    if (cmp > 0) return TCELM_GT;
    return TCELM_EQ;
}

tcelm_value_t *tcelm_min(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    (void)arena;
    return value_compare(a, b) <= 0 ? a : b;
}

tcelm_value_t *tcelm_max(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    (void)arena;
    return value_compare(a, b) >= 0 ? a : b;
}

/*
 * Boolean operations
 */

tcelm_value_t *tcelm_and(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    (void)arena;
    return (TCELM_AS_BOOL(a) && TCELM_AS_BOOL(b)) ? TCELM_TRUE : TCELM_FALSE;
}

tcelm_value_t *tcelm_or(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    (void)arena;
    return (TCELM_AS_BOOL(a) || TCELM_AS_BOOL(b)) ? TCELM_TRUE : TCELM_FALSE;
}

tcelm_value_t *tcelm_not(tcelm_arena_t *arena, tcelm_value_t *a) {
    (void)arena;
    return TCELM_AS_BOOL(a) ? TCELM_FALSE : TCELM_TRUE;
}

tcelm_value_t *tcelm_xor(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    (void)arena;
    return (TCELM_AS_BOOL(a) != TCELM_AS_BOOL(b)) ? TCELM_TRUE : TCELM_FALSE;
}

/*
 * Conversions
 */

tcelm_value_t *tcelm_to_float(tcelm_arena_t *arena, tcelm_value_t *a) {
    return tcelm_float(arena, (double)TCELM_AS_INT(a));
}

tcelm_value_t *tcelm_round(tcelm_arena_t *arena, tcelm_value_t *a) {
    return tcelm_int(arena, (int64_t)round(TCELM_AS_FLOAT(a)));
}

tcelm_value_t *tcelm_floor(tcelm_arena_t *arena, tcelm_value_t *a) {
    return tcelm_int(arena, (int64_t)floor(TCELM_AS_FLOAT(a)));
}

tcelm_value_t *tcelm_ceiling(tcelm_arena_t *arena, tcelm_value_t *a) {
    return tcelm_int(arena, (int64_t)ceil(TCELM_AS_FLOAT(a)));
}

tcelm_value_t *tcelm_truncate(tcelm_arena_t *arena, tcelm_value_t *a) {
    return tcelm_int(arena, (int64_t)trunc(TCELM_AS_FLOAT(a)));
}

/*
 * String operations - now in tcelm_string.c
 * Basic convenience wrappers kept for backwards compatibility
 */

tcelm_value_t *tcelm_string_from_int(tcelm_arena_t *arena, tcelm_value_t *n) {
    char buf[32];
    int len = snprintf(buf, sizeof(buf), "%ld", (long)TCELM_AS_INT(n));
    return tcelm_string_len(arena, buf, (size_t)len);
}

tcelm_value_t *tcelm_string_from_float(tcelm_arena_t *arena, tcelm_value_t *f) {
    char buf[64];
    int len = snprintf(buf, sizeof(buf), "%g", TCELM_AS_FLOAT(f));
    return tcelm_string_len(arena, buf, (size_t)len);
}

tcelm_value_t *tcelm_string_to_int(tcelm_arena_t *arena, tcelm_value_t *s) {
    const char *str = TCELM_AS_STRING(s);
    char *endptr;
    long val = strtol(str, &endptr, 10);

    // Check if entire string was consumed and no overflow
    if (endptr == str || *endptr != '\0') {
        // Parse failed - return Nothing
        return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
    }

    // Return Just val
    return tcelm_custom(arena, TCELM_CTOR_JUST, "Just", 1, tcelm_int(arena, (int)val));
}

tcelm_value_t *tcelm_string_to_float(tcelm_arena_t *arena, tcelm_value_t *s) {
    const char *str = TCELM_AS_STRING(s);
    char *endptr;
    double val = strtod(str, &endptr);

    // Check if entire string was consumed
    if (endptr == str || *endptr != '\0') {
        // Parse failed - return Nothing
        return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
    }

    // Return Just val
    return tcelm_custom(arena, TCELM_CTOR_JUST, "Just", 1, tcelm_float(arena, val));
}

/*
 * List operations - now in tcelm_list.c
 * Keeping tcelm_list_length and tcelm_list_reverse with different names
 * to avoid conflicts with the tcelm_list_*_fn versions
 */

/*
 * Maybe operations - now in tcelm_maybe.c
 * Keeping basic constructors for backwards compatibility
 */

tcelm_value_t *tcelm_nothing(tcelm_arena_t *arena) {
    return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
}

tcelm_value_t *tcelm_just(tcelm_arena_t *arena, tcelm_value_t *value) {
    return tcelm_custom(arena, TCELM_CTOR_JUST, "Just", 1, value);
}

bool tcelm_is_just(tcelm_value_t *maybe) {
    return tcelm_custom_ctor(maybe) == TCELM_CTOR_JUST;
}

/*
 * Result operations
 */

tcelm_value_t *tcelm_err(tcelm_arena_t *arena, tcelm_value_t *error) {
    return tcelm_custom(arena, TCELM_CTOR_ERR, "Err", 1, error);
}

tcelm_value_t *tcelm_ok(tcelm_arena_t *arena, tcelm_value_t *value) {
    return tcelm_custom(arena, TCELM_CTOR_OK, "Ok", 1, value);
}

bool tcelm_is_ok(tcelm_value_t *result) {
    return tcelm_custom_ctor(result) == TCELM_CTOR_OK;
}

tcelm_value_t *tcelm_result_unwrap(tcelm_value_t *result) {
    return tcelm_custom_arg(result, 0);
}

/*
 * Function operations
 */

tcelm_value_t *tcelm_identity(tcelm_arena_t *arena, tcelm_value_t *a) {
    (void)arena;
    return a;
}

tcelm_value_t *tcelm_always(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    (void)arena;
    (void)b;
    return a;
}

/*
 * Closure wrapper functions
 */

tcelm_value_t *tcelm_identity_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_identity(arena, args[0]);
}

tcelm_value_t *tcelm_to_float_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_to_float(arena, args[0]);
}

tcelm_value_t *tcelm_min_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_min(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_max_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_max(arena, args[0], args[1]);
}

/*
 * Debug
 */

void tcelm_debug_print(tcelm_value_t *value) {
    if (!value) {
        printf("NULL");
        return;
    }

    switch (value->tag) {
        case TCELM_TAG_INT:
            printf("%ld", (long)TCELM_AS_INT(value));
            break;
        case TCELM_TAG_FLOAT:
            printf("%g", TCELM_AS_FLOAT(value));
            break;
        case TCELM_TAG_CHAR:
            printf("'%c'", (char)TCELM_AS_CHAR(value));
            break;
        case TCELM_TAG_STRING:
            printf("\"%s\"", TCELM_AS_STRING(value)->data);
            break;
        case TCELM_TAG_BOOL:
            printf("%s", TCELM_AS_BOOL(value) ? "True" : "False");
            break;
        case TCELM_TAG_UNIT:
            printf("()");
            break;
        case TCELM_TAG_LIST:
            printf("[");
            while (!tcelm_is_nil(value)) {
                tcelm_debug_print(tcelm_list_head(value));
                value = tcelm_list_tail(value);
                if (!tcelm_is_nil(value)) printf(", ");
            }
            printf("]");
            break;
        case TCELM_TAG_TUPLE2:
            printf("(");
            tcelm_debug_print(value->data.tuple2->first);
            printf(", ");
            tcelm_debug_print(value->data.tuple2->second);
            printf(")");
            break;
        case TCELM_TAG_CUSTOM:
            printf("%s", value->data.custom->ctor_name);
            if (value->data.custom->arg_count > 0) {
                printf("(");
                for (uint16_t i = 0; i < value->data.custom->arg_count; i++) {
                    if (i > 0) printf(", ");
                    tcelm_debug_print(value->data.custom->args[i]);
                }
                printf(")");
            }
            break;
        case TCELM_TAG_CLOSURE:
            printf("<function/%d>", value->data.closure->arity - value->data.closure->applied);
            break;
        default:
            printf("<unknown>");
            break;
    }
}
