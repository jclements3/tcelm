/*
 * tcelm_basics.h - Basic operations and Elm primitives
 *
 * These functions implement the Elm Basics module operations
 * and other core language features.
 */

#ifndef TCELM_BASICS_H
#define TCELM_BASICS_H

#include "tcelm_types.h"

/* Include standard library modules */
#include "tcelm_string.h"
#include "tcelm_list.h"
#include "tcelm_char.h"
#include "tcelm_maybe.h"

/*
 * Arithmetic operations (Int)
 */
tcelm_value_t *tcelm_add_int(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_sub_int(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_mul_int(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_div_int(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_mod_int(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_negate_int(tcelm_arena_t *arena, tcelm_value_t *a);
tcelm_value_t *tcelm_abs_int(tcelm_arena_t *arena, tcelm_value_t *a);

/*
 * Arithmetic operations (Float)
 */
tcelm_value_t *tcelm_add_float(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_sub_float(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_mul_float(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_div_float(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_negate_float(tcelm_arena_t *arena, tcelm_value_t *a);
tcelm_value_t *tcelm_abs_float(tcelm_arena_t *arena, tcelm_value_t *a);

/*
 * Generic numeric operations (dispatch based on type)
 */
tcelm_value_t *tcelm_negate(tcelm_arena_t *arena, tcelm_value_t *a);
tcelm_value_t *tcelm_abs(tcelm_arena_t *arena, tcelm_value_t *a);

/*
 * Math functions
 */
tcelm_value_t *tcelm_sqrt(tcelm_arena_t *arena, tcelm_value_t *a);
tcelm_value_t *tcelm_sin(tcelm_arena_t *arena, tcelm_value_t *a);
tcelm_value_t *tcelm_cos(tcelm_arena_t *arena, tcelm_value_t *a);
tcelm_value_t *tcelm_tan(tcelm_arena_t *arena, tcelm_value_t *a);
tcelm_value_t *tcelm_atan2(tcelm_arena_t *arena, tcelm_value_t *y, tcelm_value_t *x);
tcelm_value_t *tcelm_exp(tcelm_arena_t *arena, tcelm_value_t *a);
tcelm_value_t *tcelm_log(tcelm_arena_t *arena, tcelm_value_t *a);
tcelm_value_t *tcelm_pow(tcelm_arena_t *arena, tcelm_value_t *base, tcelm_value_t *exp);

/*
 * Comparison operations
 */
tcelm_value_t *tcelm_eq(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_neq(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_lt(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_le(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_gt(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_ge(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_compare(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_min(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_max(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);

/*
 * Boolean operations
 */
tcelm_value_t *tcelm_and(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_or(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_not(tcelm_arena_t *arena, tcelm_value_t *a);
tcelm_value_t *tcelm_xor(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);

/*
 * Conversion operations
 */
tcelm_value_t *tcelm_to_float(tcelm_arena_t *arena, tcelm_value_t *a);
tcelm_value_t *tcelm_round(tcelm_arena_t *arena, tcelm_value_t *a);
tcelm_value_t *tcelm_floor(tcelm_arena_t *arena, tcelm_value_t *a);
tcelm_value_t *tcelm_ceiling(tcelm_arena_t *arena, tcelm_value_t *a);
tcelm_value_t *tcelm_truncate(tcelm_arena_t *arena, tcelm_value_t *a);

/*
 * String operations (see tcelm_string.h for full API)
 * Legacy compatibility functions
 */
tcelm_value_t *tcelm_string_from_int(tcelm_arena_t *arena, tcelm_value_t *n);
tcelm_value_t *tcelm_string_from_float(tcelm_arena_t *arena, tcelm_value_t *f);
tcelm_value_t *tcelm_string_to_int(tcelm_arena_t *arena, tcelm_value_t *s);
tcelm_value_t *tcelm_string_to_float(tcelm_arena_t *arena, tcelm_value_t *s);

/*
 * List operations - see tcelm_list.h for full API
 */

/*
 * Maybe operations - see tcelm_maybe.h for full API
 * Legacy compatibility functions
 */
#define TCELM_CTOR_NOTHING 0
#define TCELM_CTOR_JUST    1

tcelm_value_t *tcelm_nothing(tcelm_arena_t *arena);
tcelm_value_t *tcelm_just(tcelm_arena_t *arena, tcelm_value_t *value);
bool tcelm_is_just(tcelm_value_t *maybe);

/*
 * Result operations
 * Result error value = Ok value | Err error
 * Uses TCELM_CTOR_OK and TCELM_CTOR_ERR from tcelm_types.h
 */
tcelm_value_t *tcelm_err(tcelm_arena_t *arena, tcelm_value_t *error);
tcelm_value_t *tcelm_ok(tcelm_arena_t *arena, tcelm_value_t *value);
bool tcelm_is_ok(tcelm_value_t *result);
tcelm_value_t *tcelm_result_unwrap(tcelm_value_t *result);

/*
 * Order type (for comparison results)
 * Order = LT | EQ | GT
 */
#define TCELM_CTOR_LT 0
#define TCELM_CTOR_EQ 1
#define TCELM_CTOR_GT 2

extern tcelm_value_t tcelm_order_lt;
extern tcelm_value_t tcelm_order_eq;
extern tcelm_value_t tcelm_order_gt;

#define TCELM_LT (&tcelm_order_lt)
#define TCELM_EQ (&tcelm_order_eq)
#define TCELM_GT (&tcelm_order_gt)

/*
 * Function composition and piping
 *
 * Forward composition (>>): f >> g means \x -> g(f(x))
 * Backward composition (<<): f << g means \x -> f(g(x))
 */
tcelm_value_t *tcelm_compose(tcelm_arena_t *arena, tcelm_value_t *f, tcelm_value_t *g);
tcelm_value_t *tcelm_compose_fwd(tcelm_arena_t *arena, tcelm_value_t *f, tcelm_value_t *g);
tcelm_value_t *tcelm_compose_bwd(tcelm_arena_t *arena, tcelm_value_t *f, tcelm_value_t *g);
tcelm_value_t *tcelm_compose_fwd_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_compose_bwd_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_identity(tcelm_arena_t *arena, tcelm_value_t *a);
tcelm_value_t *tcelm_always(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);

/*
 * Closure wrapper functions for use with tcelm_closure
 * These unpack args[] and call the actual implementation
 */
tcelm_value_t *tcelm_identity_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_to_float_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_min_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_max_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_not_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_negate_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_abs_impl(tcelm_arena_t *arena, tcelm_value_t **args);

/*
 * Debug/Development
 */
void tcelm_debug_print(tcelm_value_t *value);

#endif /* TCELM_BASICS_H */
