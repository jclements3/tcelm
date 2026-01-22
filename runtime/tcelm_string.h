/*
 * tcelm_string.h - Elm String module for tcelm runtime
 *
 * Implements String.* functions needed for self-hosting
 */

#ifndef TCELM_STRING_H
#define TCELM_STRING_H

#include "tcelm_types.h"
#include "tcelm_arena.h"

/* String creation */
tcelm_value_t *tcelm_string_fromInt(tcelm_arena_t *arena, tcelm_value_t *n);
tcelm_value_t *tcelm_string_fromFloat(tcelm_arena_t *arena, tcelm_value_t *f);
tcelm_value_t *tcelm_string_fromChar(tcelm_arena_t *arena, tcelm_value_t *c);

/* String operations */
tcelm_value_t *tcelm_string_append(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_string_concat(tcelm_arena_t *arena, tcelm_value_t *list);
tcelm_value_t *tcelm_string_join(tcelm_arena_t *arena, tcelm_value_t *sep, tcelm_value_t *list);
tcelm_value_t *tcelm_string_length(tcelm_arena_t *arena, tcelm_value_t *s);
tcelm_value_t *tcelm_string_slice(tcelm_arena_t *arena, tcelm_value_t *start, tcelm_value_t *end, tcelm_value_t *s);
tcelm_value_t *tcelm_string_left(tcelm_arena_t *arena, tcelm_value_t *n, tcelm_value_t *s);
tcelm_value_t *tcelm_string_right(tcelm_arena_t *arena, tcelm_value_t *n, tcelm_value_t *s);
tcelm_value_t *tcelm_string_dropLeft(tcelm_arena_t *arena, tcelm_value_t *n, tcelm_value_t *s);
tcelm_value_t *tcelm_string_dropRight(tcelm_arena_t *arena, tcelm_value_t *n, tcelm_value_t *s);

/* String inspection */
tcelm_value_t *tcelm_string_isEmpty(tcelm_arena_t *arena, tcelm_value_t *s);
tcelm_value_t *tcelm_string_contains(tcelm_arena_t *arena, tcelm_value_t *sub, tcelm_value_t *s);
tcelm_value_t *tcelm_string_startsWith(tcelm_arena_t *arena, tcelm_value_t *prefix, tcelm_value_t *s);
tcelm_value_t *tcelm_string_endsWith(tcelm_arena_t *arena, tcelm_value_t *suffix, tcelm_value_t *s);

/* String conversion */
tcelm_value_t *tcelm_string_toList(tcelm_arena_t *arena, tcelm_value_t *s);
tcelm_value_t *tcelm_string_fromList(tcelm_arena_t *arena, tcelm_value_t *list);
tcelm_value_t *tcelm_string_toUpper(tcelm_arena_t *arena, tcelm_value_t *s);
tcelm_value_t *tcelm_string_toLower(tcelm_arena_t *arena, tcelm_value_t *s);

/* String utility */
tcelm_value_t *tcelm_string_uncons(tcelm_arena_t *arena, tcelm_value_t *s);
tcelm_value_t *tcelm_string_cons(tcelm_arena_t *arena, tcelm_value_t *c, tcelm_value_t *s);
tcelm_value_t *tcelm_string_repeat(tcelm_arena_t *arena, tcelm_value_t *n, tcelm_value_t *s);
tcelm_value_t *tcelm_string_reverse(tcelm_arena_t *arena, tcelm_value_t *s);
tcelm_value_t *tcelm_string_trim(tcelm_arena_t *arena, tcelm_value_t *s);
tcelm_value_t *tcelm_string_split(tcelm_arena_t *arena, tcelm_value_t *sep, tcelm_value_t *s);
tcelm_value_t *tcelm_string_replace(tcelm_arena_t *arena, tcelm_value_t *from, tcelm_value_t *to, tcelm_value_t *s);

/* Closure-compatible _impl wrappers */
tcelm_value_t *tcelm_string_join_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_append_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_contains_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_startsWith_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_endsWith_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_isEmpty_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_concat_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_replace_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_left_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_right_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_dropLeft_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_dropRight_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_slice_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_length_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_repeat_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_split_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_toList_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_fromList_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_toUpper_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_toLower_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_trim_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_reverse_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_uncons_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_cons_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_fromInt_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_fromFloat_impl(tcelm_arena_t *arena, tcelm_value_t **args);
tcelm_value_t *tcelm_string_fromChar_impl(tcelm_arena_t *arena, tcelm_value_t **args);

#endif /* TCELM_STRING_H */
