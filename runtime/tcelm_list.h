/*
 * tcelm_list.h - Elm List module for tcelm runtime
 *
 * Implements List.* functions needed for self-hosting
 */

#ifndef TCELM_LIST_H
#define TCELM_LIST_H

#include "tcelm_types.h"
#include "tcelm_arena.h"

/* List creation */
tcelm_value_t *tcelm_list_singleton(tcelm_arena_t *arena, tcelm_value_t *x);
tcelm_value_t *tcelm_list_repeat(tcelm_arena_t *arena, tcelm_value_t *n, tcelm_value_t *x);
tcelm_value_t *tcelm_list_range(tcelm_arena_t *arena, tcelm_value_t *lo, tcelm_value_t *hi);

/* List transforms */
tcelm_value_t *tcelm_list_map(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *list);
tcelm_value_t *tcelm_list_indexedMap(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *list);
tcelm_value_t *tcelm_list_foldl(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *acc, tcelm_value_t *list);
tcelm_value_t *tcelm_list_foldr(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *acc, tcelm_value_t *list);
tcelm_value_t *tcelm_list_filter(tcelm_arena_t *arena, tcelm_value_t *pred, tcelm_value_t *list);
tcelm_value_t *tcelm_list_filterMap(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *list);
tcelm_value_t *tcelm_list_map2(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *listA, tcelm_value_t *listB);
tcelm_value_t *tcelm_list_concatMap(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *list);

/* List utilities */
tcelm_value_t *tcelm_list_length_fn(tcelm_arena_t *arena, tcelm_value_t *list);
tcelm_value_t *tcelm_list_reverse_fn(tcelm_arena_t *arena, tcelm_value_t *list);
tcelm_value_t *tcelm_list_member(tcelm_arena_t *arena, tcelm_value_t *x, tcelm_value_t *list);
tcelm_value_t *tcelm_list_all(tcelm_arena_t *arena, tcelm_value_t *pred, tcelm_value_t *list);
tcelm_value_t *tcelm_list_any(tcelm_arena_t *arena, tcelm_value_t *pred, tcelm_value_t *list);
tcelm_value_t *tcelm_list_maximum(tcelm_arena_t *arena, tcelm_value_t *list);
tcelm_value_t *tcelm_list_minimum(tcelm_arena_t *arena, tcelm_value_t *list);
tcelm_value_t *tcelm_list_sum(tcelm_arena_t *arena, tcelm_value_t *list);
tcelm_value_t *tcelm_list_product(tcelm_arena_t *arena, tcelm_value_t *list);

/* List combining */
tcelm_value_t *tcelm_list_append(tcelm_arena_t *arena, tcelm_value_t *xs, tcelm_value_t *ys);
tcelm_value_t *tcelm_list_concat_fn(tcelm_arena_t *arena, tcelm_value_t *lists);
tcelm_value_t *tcelm_list_intersperse(tcelm_arena_t *arena, tcelm_value_t *sep, tcelm_value_t *list);

/* List sublists */
tcelm_value_t *tcelm_list_head_fn(tcelm_arena_t *arena, tcelm_value_t *list);
tcelm_value_t *tcelm_list_tail_fn(tcelm_arena_t *arena, tcelm_value_t *list);
tcelm_value_t *tcelm_list_take(tcelm_arena_t *arena, tcelm_value_t *n, tcelm_value_t *list);
tcelm_value_t *tcelm_list_drop(tcelm_arena_t *arena, tcelm_value_t *n, tcelm_value_t *list);
tcelm_value_t *tcelm_list_partition(tcelm_arena_t *arena, tcelm_value_t *pred, tcelm_value_t *list);
tcelm_value_t *tcelm_list_unzip(tcelm_arena_t *arena, tcelm_value_t *list);

/* List testing */
tcelm_value_t *tcelm_list_isEmpty(tcelm_arena_t *arena, tcelm_value_t *list);

/* Compatibility aliases for backwards compatibility */
#define tcelm_list_length tcelm_list_length_fn
#define tcelm_list_reverse tcelm_list_reverse_fn
#define tcelm_list_concat tcelm_list_concat_fn

#endif /* TCELM_LIST_H */
