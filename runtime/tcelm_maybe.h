/*
 * tcelm_maybe.h - Elm Maybe module for tcelm runtime
 */

#ifndef TCELM_MAYBE_H
#define TCELM_MAYBE_H

#include "tcelm_types.h"
#include "tcelm_arena.h"

/* Maybe constructor IDs */
#define TCELM_MAYBE_NOTHING 0
#define TCELM_MAYBE_JUST    1

/* Maybe constructors */
tcelm_value_t *tcelm_maybe_nothing(tcelm_arena_t *arena);
tcelm_value_t *tcelm_maybe_just(tcelm_arena_t *arena, tcelm_value_t *value);

/* Maybe operations */
tcelm_value_t *tcelm_maybe_withDefault(tcelm_arena_t *arena, tcelm_value_t *def, tcelm_value_t *maybe);
tcelm_value_t *tcelm_maybe_map(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *maybe);
tcelm_value_t *tcelm_maybe_map2(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *maybeA, tcelm_value_t *maybeB);
tcelm_value_t *tcelm_maybe_andThen(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *maybe);

/* Maybe predicates */
bool tcelm_maybe_isJust(tcelm_value_t *maybe);
bool tcelm_maybe_isNothing(tcelm_value_t *maybe);

/* Maybe unwrap (for internal use) */
tcelm_value_t *tcelm_maybe_unwrap(tcelm_value_t *maybe);

/* Closure-compatible _impl wrappers */
tcelm_value_t *tcelm_maybe_withDefault_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_maybe_map_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_maybe_andThen_impl(tcelm_arena_t *arena, tcelm_value_t **args);

#endif /* TCELM_MAYBE_H */
