/*
 * tcelm_list.c - Elm List module implementation
 */

#include "tcelm_list.h"
#include <string.h>

/* Helper to compare two values for equality */
static bool values_equal(tcelm_value_t *a, tcelm_value_t *b);

/*
 * List.singleton : a -> List a
 */
tcelm_value_t *tcelm_list_singleton(tcelm_arena_t *arena, tcelm_value_t *x) {
    return tcelm_cons(arena, x, TCELM_NIL);
}

/*
 * List.repeat : Int -> a -> List a
 */
tcelm_value_t *tcelm_list_repeat(tcelm_arena_t *arena, tcelm_value_t *n, tcelm_value_t *x) {
    int64_t count = TCELM_AS_INT(n);
    tcelm_value_t *result = TCELM_NIL;

    for (int64_t i = 0; i < count; i++) {
        result = tcelm_cons(arena, x, result);
    }

    return result;
}

/*
 * List.range : Int -> Int -> List Int
 */
tcelm_value_t *tcelm_list_range(tcelm_arena_t *arena, tcelm_value_t *lo, tcelm_value_t *hi) {
    int64_t low = TCELM_AS_INT(lo);
    int64_t high = TCELM_AS_INT(hi);
    tcelm_value_t *result = TCELM_NIL;

    /* Build in reverse */
    for (int64_t i = high; i >= low; i--) {
        result = tcelm_cons(arena, tcelm_int(arena, i), result);
    }

    return result;
}

/*
 * List.map : (a -> b) -> List a -> List b
 */
tcelm_value_t *tcelm_list_map(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *list) {
    /* Build result in reverse, then reverse */
    tcelm_value_t *result = TCELM_NIL;
    tcelm_value_t *curr = list;

    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *head = tcelm_list_head(curr);
        tcelm_value_t *mapped = tcelm_apply(arena, fn, head);
        result = tcelm_cons(arena, mapped, result);
        curr = tcelm_list_tail(curr);
    }

    /* Reverse to get correct order */
    return tcelm_list_reverse_fn(arena, result);
}

/*
 * List.indexedMap : (Int -> a -> b) -> List a -> List b
 */
tcelm_value_t *tcelm_list_indexedMap(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *list) {
    tcelm_value_t *result = TCELM_NIL;
    tcelm_value_t *curr = list;
    int64_t index = 0;

    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *head = tcelm_list_head(curr);
        tcelm_value_t *fn_with_index = tcelm_apply(arena, fn, tcelm_int(arena, index));
        tcelm_value_t *mapped = tcelm_apply(arena, fn_with_index, head);
        result = tcelm_cons(arena, mapped, result);
        curr = tcelm_list_tail(curr);
        index++;
    }

    return tcelm_list_reverse_fn(arena, result);
}

/*
 * List.foldl : (a -> b -> b) -> b -> List a -> b
 */
tcelm_value_t *tcelm_list_foldl(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *acc, tcelm_value_t *list) {
    tcelm_value_t *result = acc;
    tcelm_value_t *curr = list;

    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *head = tcelm_list_head(curr);
        tcelm_value_t *fn_with_item = tcelm_apply(arena, fn, head);
        result = tcelm_apply(arena, fn_with_item, result);
        curr = tcelm_list_tail(curr);
    }

    return result;
}

/*
 * List.foldr : (a -> b -> b) -> b -> List a -> b
 */
tcelm_value_t *tcelm_list_foldr(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *acc, tcelm_value_t *list) {
    /* Reverse first, then foldl */
    tcelm_value_t *reversed = tcelm_list_reverse_fn(arena, list);
    return tcelm_list_foldl(arena, fn, acc, reversed);
}

/*
 * List.filter : (a -> Bool) -> List a -> List a
 */
tcelm_value_t *tcelm_list_filter(tcelm_arena_t *arena, tcelm_value_t *pred, tcelm_value_t *list) {
    tcelm_value_t *result = TCELM_NIL;
    tcelm_value_t *curr = list;

    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *head = tcelm_list_head(curr);
        tcelm_value_t *test = tcelm_apply(arena, pred, head);

        if (TCELM_AS_BOOL(test)) {
            result = tcelm_cons(arena, head, result);
        }

        curr = tcelm_list_tail(curr);
    }

    return tcelm_list_reverse_fn(arena, result);
}

/*
 * List.filterMap : (a -> Maybe b) -> List a -> List b
 */
tcelm_value_t *tcelm_list_filterMap(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *list) {
    tcelm_value_t *result = TCELM_NIL;
    tcelm_value_t *curr = list;

    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *head = tcelm_list_head(curr);
        tcelm_value_t *maybe = tcelm_apply(arena, fn, head);

        /* Check if it's Just (ctor_id == 1) */
        if (maybe->tag == TCELM_TAG_CUSTOM && tcelm_custom_ctor(maybe) == 1) {
            tcelm_value_t *value = tcelm_custom_arg(maybe, 0);
            result = tcelm_cons(arena, value, result);
        }

        curr = tcelm_list_tail(curr);
    }

    return tcelm_list_reverse_fn(arena, result);
}

/*
 * List.map2 : (a -> b -> c) -> List a -> List b -> List c
 */
tcelm_value_t *tcelm_list_map2(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *listA, tcelm_value_t *listB) {
    tcelm_value_t *result = TCELM_NIL;
    tcelm_value_t *currA = listA;
    tcelm_value_t *currB = listB;

    while (!tcelm_is_nil(currA) && !tcelm_is_nil(currB)) {
        tcelm_value_t *headA = tcelm_list_head(currA);
        tcelm_value_t *headB = tcelm_list_head(currB);

        tcelm_value_t *fn_with_a = tcelm_apply(arena, fn, headA);
        tcelm_value_t *mapped = tcelm_apply(arena, fn_with_a, headB);
        result = tcelm_cons(arena, mapped, result);

        currA = tcelm_list_tail(currA);
        currB = tcelm_list_tail(currB);
    }

    return tcelm_list_reverse_fn(arena, result);
}

/*
 * List.concatMap : (a -> List b) -> List a -> List b
 */
tcelm_value_t *tcelm_list_concatMap(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *list) {
    tcelm_value_t *mapped = tcelm_list_map(arena, fn, list);
    return tcelm_list_concat_fn(arena, mapped);
}

/*
 * List.length : List a -> Int
 */
tcelm_value_t *tcelm_list_length_fn(tcelm_arena_t *arena, tcelm_value_t *list) {
    int64_t count = 0;
    tcelm_value_t *curr = list;

    while (!tcelm_is_nil(curr)) {
        count++;
        curr = tcelm_list_tail(curr);
    }

    return tcelm_int(arena, count);
}

/*
 * List.reverse : List a -> List a
 */
tcelm_value_t *tcelm_list_reverse_fn(tcelm_arena_t *arena, tcelm_value_t *list) {
    tcelm_value_t *result = TCELM_NIL;
    tcelm_value_t *curr = list;

    while (!tcelm_is_nil(curr)) {
        result = tcelm_cons(arena, tcelm_list_head(curr), result);
        curr = tcelm_list_tail(curr);
    }

    return result;
}

/*
 * List.member : a -> List a -> Bool
 */
tcelm_value_t *tcelm_list_member(tcelm_arena_t *arena, tcelm_value_t *x, tcelm_value_t *list) {
    (void)arena;
    tcelm_value_t *curr = list;

    while (!tcelm_is_nil(curr)) {
        if (values_equal(x, tcelm_list_head(curr))) {
            return TCELM_TRUE;
        }
        curr = tcelm_list_tail(curr);
    }

    return TCELM_FALSE;
}

/*
 * List.all : (a -> Bool) -> List a -> Bool
 */
tcelm_value_t *tcelm_list_all(tcelm_arena_t *arena, tcelm_value_t *pred, tcelm_value_t *list) {
    tcelm_value_t *curr = list;

    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *head = tcelm_list_head(curr);
        tcelm_value_t *test = tcelm_apply(arena, pred, head);

        if (!TCELM_AS_BOOL(test)) {
            return TCELM_FALSE;
        }

        curr = tcelm_list_tail(curr);
    }

    return TCELM_TRUE;
}

/*
 * List.any : (a -> Bool) -> List a -> Bool
 */
tcelm_value_t *tcelm_list_any(tcelm_arena_t *arena, tcelm_value_t *pred, tcelm_value_t *list) {
    tcelm_value_t *curr = list;

    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *head = tcelm_list_head(curr);
        tcelm_value_t *test = tcelm_apply(arena, pred, head);

        if (TCELM_AS_BOOL(test)) {
            return TCELM_TRUE;
        }

        curr = tcelm_list_tail(curr);
    }

    return TCELM_FALSE;
}

/*
 * List.maximum : List comparable -> Maybe comparable
 */
tcelm_value_t *tcelm_list_maximum(tcelm_arena_t *arena, tcelm_value_t *list) {
    if (tcelm_is_nil(list)) {
        return tcelm_custom(arena, 0, "Nothing", 0);
    }

    tcelm_value_t *max = tcelm_list_head(list);
    tcelm_value_t *curr = tcelm_list_tail(list);

    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *head = tcelm_list_head(curr);

        /* Compare based on type */
        if (max->tag == TCELM_TAG_INT) {
            if (TCELM_AS_INT(head) > TCELM_AS_INT(max)) {
                max = head;
            }
        } else if (max->tag == TCELM_TAG_FLOAT) {
            if (TCELM_AS_FLOAT(head) > TCELM_AS_FLOAT(max)) {
                max = head;
            }
        }

        curr = tcelm_list_tail(curr);
    }

    return tcelm_custom(arena, 1, "Just", 1, max);
}

/*
 * List.minimum : List comparable -> Maybe comparable
 */
tcelm_value_t *tcelm_list_minimum(tcelm_arena_t *arena, tcelm_value_t *list) {
    if (tcelm_is_nil(list)) {
        return tcelm_custom(arena, 0, "Nothing", 0);
    }

    tcelm_value_t *min = tcelm_list_head(list);
    tcelm_value_t *curr = tcelm_list_tail(list);

    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *head = tcelm_list_head(curr);

        if (min->tag == TCELM_TAG_INT) {
            if (TCELM_AS_INT(head) < TCELM_AS_INT(min)) {
                min = head;
            }
        } else if (min->tag == TCELM_TAG_FLOAT) {
            if (TCELM_AS_FLOAT(head) < TCELM_AS_FLOAT(min)) {
                min = head;
            }
        }

        curr = tcelm_list_tail(curr);
    }

    return tcelm_custom(arena, 1, "Just", 1, min);
}

/*
 * List.sum : List number -> number
 */
tcelm_value_t *tcelm_list_sum(tcelm_arena_t *arena, tcelm_value_t *list) {
    if (tcelm_is_nil(list)) {
        return tcelm_int(arena, 0);
    }

    tcelm_value_t *first = tcelm_list_head(list);
    bool is_float = (first->tag == TCELM_TAG_FLOAT);

    if (is_float) {
        double sum = 0.0;
        tcelm_value_t *curr = list;
        while (!tcelm_is_nil(curr)) {
            sum += TCELM_AS_FLOAT(tcelm_list_head(curr));
            curr = tcelm_list_tail(curr);
        }
        return tcelm_float(arena, sum);
    } else {
        int64_t sum = 0;
        tcelm_value_t *curr = list;
        while (!tcelm_is_nil(curr)) {
            sum += TCELM_AS_INT(tcelm_list_head(curr));
            curr = tcelm_list_tail(curr);
        }
        return tcelm_int(arena, sum);
    }
}

/*
 * List.product : List number -> number
 */
tcelm_value_t *tcelm_list_product(tcelm_arena_t *arena, tcelm_value_t *list) {
    if (tcelm_is_nil(list)) {
        return tcelm_int(arena, 1);
    }

    tcelm_value_t *first = tcelm_list_head(list);
    bool is_float = (first->tag == TCELM_TAG_FLOAT);

    if (is_float) {
        double prod = 1.0;
        tcelm_value_t *curr = list;
        while (!tcelm_is_nil(curr)) {
            prod *= TCELM_AS_FLOAT(tcelm_list_head(curr));
            curr = tcelm_list_tail(curr);
        }
        return tcelm_float(arena, prod);
    } else {
        int64_t prod = 1;
        tcelm_value_t *curr = list;
        while (!tcelm_is_nil(curr)) {
            prod *= TCELM_AS_INT(tcelm_list_head(curr));
            curr = tcelm_list_tail(curr);
        }
        return tcelm_int(arena, prod);
    }
}

/*
 * List.append (++) : List a -> List a -> List a
 */
tcelm_value_t *tcelm_list_append(tcelm_arena_t *arena, tcelm_value_t *xs, tcelm_value_t *ys) {
    if (tcelm_is_nil(xs)) {
        return ys;
    }
    if (tcelm_is_nil(ys)) {
        return xs;
    }

    /* Build xs in reverse, then cons onto ys */
    tcelm_value_t *reversed_xs = tcelm_list_reverse_fn(arena, xs);
    tcelm_value_t *result = ys;

    while (!tcelm_is_nil(reversed_xs)) {
        result = tcelm_cons(arena, tcelm_list_head(reversed_xs), result);
        reversed_xs = tcelm_list_tail(reversed_xs);
    }

    return result;
}

/*
 * List.concat : List (List a) -> List a
 */
tcelm_value_t *tcelm_list_concat_fn(tcelm_arena_t *arena, tcelm_value_t *lists) {
    tcelm_value_t *result = TCELM_NIL;
    tcelm_value_t *curr = lists;

    /* Collect all elements in reverse */
    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *inner = tcelm_list_head(curr);

        while (!tcelm_is_nil(inner)) {
            result = tcelm_cons(arena, tcelm_list_head(inner), result);
            inner = tcelm_list_tail(inner);
        }

        curr = tcelm_list_tail(curr);
    }

    return tcelm_list_reverse_fn(arena, result);
}

/*
 * List.intersperse : a -> List a -> List a
 */
tcelm_value_t *tcelm_list_intersperse(tcelm_arena_t *arena, tcelm_value_t *sep, tcelm_value_t *list) {
    if (tcelm_is_nil(list)) {
        return TCELM_NIL;
    }

    tcelm_value_t *result = TCELM_NIL;
    tcelm_value_t *curr = list;
    bool first = true;

    while (!tcelm_is_nil(curr)) {
        if (!first) {
            result = tcelm_cons(arena, sep, result);
        }
        result = tcelm_cons(arena, tcelm_list_head(curr), result);
        first = false;
        curr = tcelm_list_tail(curr);
    }

    return tcelm_list_reverse_fn(arena, result);
}

/*
 * List.head : List a -> Maybe a
 */
tcelm_value_t *tcelm_list_head_fn(tcelm_arena_t *arena, tcelm_value_t *list) {
    if (tcelm_is_nil(list)) {
        return tcelm_custom(arena, 0, "Nothing", 0);
    }
    return tcelm_custom(arena, 1, "Just", 1, tcelm_list_head(list));
}

/*
 * List.tail : List a -> Maybe (List a)
 */
tcelm_value_t *tcelm_list_tail_fn(tcelm_arena_t *arena, tcelm_value_t *list) {
    if (tcelm_is_nil(list)) {
        return tcelm_custom(arena, 0, "Nothing", 0);
    }
    return tcelm_custom(arena, 1, "Just", 1, tcelm_list_tail(list));
}

/*
 * List.take : Int -> List a -> List a
 */
tcelm_value_t *tcelm_list_take(tcelm_arena_t *arena, tcelm_value_t *n, tcelm_value_t *list) {
    int64_t count = TCELM_AS_INT(n);
    tcelm_value_t *result = TCELM_NIL;
    tcelm_value_t *curr = list;

    while (count > 0 && !tcelm_is_nil(curr)) {
        result = tcelm_cons(arena, tcelm_list_head(curr), result);
        curr = tcelm_list_tail(curr);
        count--;
    }

    return tcelm_list_reverse_fn(arena, result);
}

/*
 * List.drop : Int -> List a -> List a
 */
tcelm_value_t *tcelm_list_drop(tcelm_arena_t *arena, tcelm_value_t *n, tcelm_value_t *list) {
    (void)arena;
    int64_t count = TCELM_AS_INT(n);
    tcelm_value_t *curr = list;

    while (count > 0 && !tcelm_is_nil(curr)) {
        curr = tcelm_list_tail(curr);
        count--;
    }

    return curr;
}

/*
 * List.partition : (a -> Bool) -> List a -> (List a, List a)
 */
tcelm_value_t *tcelm_list_partition(tcelm_arena_t *arena, tcelm_value_t *pred, tcelm_value_t *list) {
    tcelm_value_t *trues = TCELM_NIL;
    tcelm_value_t *falses = TCELM_NIL;
    tcelm_value_t *curr = list;

    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *head = tcelm_list_head(curr);
        tcelm_value_t *test = tcelm_apply(arena, pred, head);

        if (TCELM_AS_BOOL(test)) {
            trues = tcelm_cons(arena, head, trues);
        } else {
            falses = tcelm_cons(arena, head, falses);
        }

        curr = tcelm_list_tail(curr);
    }

    trues = tcelm_list_reverse_fn(arena, trues);
    falses = tcelm_list_reverse_fn(arena, falses);

    return tcelm_tuple2(arena, trues, falses);
}

/*
 * List.unzip : List (a, b) -> (List a, List b)
 */
tcelm_value_t *tcelm_list_unzip(tcelm_arena_t *arena, tcelm_value_t *list) {
    tcelm_value_t *as = TCELM_NIL;
    tcelm_value_t *bs = TCELM_NIL;
    tcelm_value_t *curr = list;

    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *head = tcelm_list_head(curr);
        as = tcelm_cons(arena, tcelm_tuple2_first(head), as);
        bs = tcelm_cons(arena, tcelm_tuple2_second(head), bs);
        curr = tcelm_list_tail(curr);
    }

    as = tcelm_list_reverse_fn(arena, as);
    bs = tcelm_list_reverse_fn(arena, bs);

    return tcelm_tuple2(arena, as, bs);
}

/*
 * List.isEmpty : List a -> Bool
 */
tcelm_value_t *tcelm_list_isEmpty(tcelm_arena_t *arena, tcelm_value_t *list) {
    (void)arena;
    return tcelm_is_nil(list) ? TCELM_TRUE : TCELM_FALSE;
}

/*
 * Helper: Compare two values for equality
 */
static bool values_equal(tcelm_value_t *a, tcelm_value_t *b) {
    if (a->tag != b->tag) {
        return false;
    }

    switch (a->tag) {
        case TCELM_TAG_INT:
            return TCELM_AS_INT(a) == TCELM_AS_INT(b);
        case TCELM_TAG_FLOAT:
            return TCELM_AS_FLOAT(a) == TCELM_AS_FLOAT(b);
        case TCELM_TAG_CHAR:
            return TCELM_AS_CHAR(a) == TCELM_AS_CHAR(b);
        case TCELM_TAG_BOOL:
            return TCELM_AS_BOOL(a) == TCELM_AS_BOOL(b);
        case TCELM_TAG_STRING:
            return strcmp(TCELM_AS_STRING(a)->data, TCELM_AS_STRING(b)->data) == 0;
        case TCELM_TAG_UNIT:
            return true;
        default:
            /* For complex types, compare by pointer (reference equality) */
            return a == b;
    }
}
