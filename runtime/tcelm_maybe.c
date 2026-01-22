/*
 * tcelm_maybe.c - Elm Maybe module implementation
 */

#include "tcelm_maybe.h"

/*
 * Maybe constructors
 */

tcelm_value_t *tcelm_maybe_nothing(tcelm_arena_t *arena) {
    return tcelm_custom(arena, TCELM_MAYBE_NOTHING, "Nothing", 0);
}

tcelm_value_t *tcelm_maybe_just(tcelm_arena_t *arena, tcelm_value_t *value) {
    return tcelm_custom(arena, TCELM_MAYBE_JUST, "Just", 1, value);
}

/*
 * Maybe.withDefault : a -> Maybe a -> a
 */
tcelm_value_t *tcelm_maybe_withDefault(tcelm_arena_t *arena, tcelm_value_t *def, tcelm_value_t *maybe) {
    (void)arena;
    if (tcelm_custom_ctor(maybe) == TCELM_MAYBE_NOTHING) {
        return def;
    }
    return tcelm_custom_arg(maybe, 0);
}

/*
 * Maybe.map : (a -> b) -> Maybe a -> Maybe b
 */
tcelm_value_t *tcelm_maybe_map(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *maybe) {
    if (tcelm_custom_ctor(maybe) == TCELM_MAYBE_NOTHING) {
        return maybe;
    }

    tcelm_value_t *value = tcelm_custom_arg(maybe, 0);
    tcelm_value_t *mapped = tcelm_apply(arena, fn, value);
    return tcelm_maybe_just(arena, mapped);
}

/*
 * Maybe.map2 : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
 */
tcelm_value_t *tcelm_maybe_map2(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *maybeA, tcelm_value_t *maybeB) {
    if (tcelm_custom_ctor(maybeA) == TCELM_MAYBE_NOTHING) {
        return maybeA;
    }
    if (tcelm_custom_ctor(maybeB) == TCELM_MAYBE_NOTHING) {
        return maybeB;
    }

    tcelm_value_t *a = tcelm_custom_arg(maybeA, 0);
    tcelm_value_t *b = tcelm_custom_arg(maybeB, 0);

    tcelm_value_t *fn_with_a = tcelm_apply(arena, fn, a);
    tcelm_value_t *result = tcelm_apply(arena, fn_with_a, b);

    return tcelm_maybe_just(arena, result);
}

/*
 * Maybe.andThen : (a -> Maybe b) -> Maybe a -> Maybe b
 */
tcelm_value_t *tcelm_maybe_andThen(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *maybe) {
    if (tcelm_custom_ctor(maybe) == TCELM_MAYBE_NOTHING) {
        return maybe;
    }

    tcelm_value_t *value = tcelm_custom_arg(maybe, 0);
    return tcelm_apply(arena, fn, value);
}

/*
 * Maybe predicates
 */

bool tcelm_maybe_isJust(tcelm_value_t *maybe) {
    return maybe->tag == TCELM_TAG_CUSTOM &&
           tcelm_custom_ctor(maybe) == TCELM_MAYBE_JUST;
}

bool tcelm_maybe_isNothing(tcelm_value_t *maybe) {
    return maybe->tag == TCELM_TAG_CUSTOM &&
           tcelm_custom_ctor(maybe) == TCELM_MAYBE_NOTHING;
}

/*
 * Maybe unwrap (internal use, assumes Just)
 */
tcelm_value_t *tcelm_maybe_unwrap(tcelm_value_t *maybe) {
    return tcelm_custom_arg(maybe, 0);
}

/*
 * Closure-compatible _impl wrappers
 */

tcelm_value_t *tcelm_maybe_withDefault_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_maybe_withDefault(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_maybe_map_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_maybe_map(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_maybe_andThen_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_maybe_andThen(arena, args[0], args[1]);
}
