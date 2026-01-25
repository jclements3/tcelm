/*
 * tcelm_list.c - Elm List module implementation
 */

#include "tcelm_list.h"
#include "tcelm_arena.h"  /* For TCELM_TLS and tcelm_current_arena */
#include <string.h>
#include <stdlib.h>

#ifdef __rtems__
#include <rtems.h>
#include <sys/cpuset.h>
#else
#include <pthread.h>
#include <unistd.h>
#endif

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

/* =========================================================================
 * Sorting Functions
 * ========================================================================= */

/*
 * Helper: Compare two values (returns -1, 0, or 1)
 * Supports Int, Float, Char, String
 */
static int values_compare(tcelm_value_t *a, tcelm_value_t *b) {
    /* Handle different types - compare by tag first */
    if (a->tag != b->tag) {
        return (a->tag < b->tag) ? -1 : 1;
    }

    switch (a->tag) {
        case TCELM_TAG_INT: {
            int64_t av = TCELM_AS_INT(a);
            int64_t bv = TCELM_AS_INT(b);
            return (av < bv) ? -1 : (av > bv) ? 1 : 0;
        }
        case TCELM_TAG_FLOAT: {
            double av = TCELM_AS_FLOAT(a);
            double bv = TCELM_AS_FLOAT(b);
            return (av < bv) ? -1 : (av > bv) ? 1 : 0;
        }
        case TCELM_TAG_CHAR: {
            uint32_t av = TCELM_AS_CHAR(a);
            uint32_t bv = TCELM_AS_CHAR(b);
            return (av < bv) ? -1 : (av > bv) ? 1 : 0;
        }
        case TCELM_TAG_STRING:
            return strcmp(TCELM_AS_STRING(a)->data, TCELM_AS_STRING(b)->data);
        default:
            /* For other types, compare by pointer */
            return (a < b) ? -1 : (a > b) ? 1 : 0;
    }
}

/*
 * Helper: Merge two sorted lists
 */
static tcelm_value_t *merge_lists(tcelm_arena_t *arena, tcelm_value_t *left, tcelm_value_t *right) {
    if (tcelm_is_nil(left)) return right;
    if (tcelm_is_nil(right)) return left;

    tcelm_value_t *result = TCELM_NIL;
    tcelm_value_t **tail_ptr = &result;

    while (!tcelm_is_nil(left) && !tcelm_is_nil(right)) {
        tcelm_value_t *left_head = tcelm_list_head(left);
        tcelm_value_t *right_head = tcelm_list_head(right);

        if (values_compare(left_head, right_head) <= 0) {
            *tail_ptr = tcelm_cons(arena, left_head, TCELM_NIL);
            tail_ptr = &((*tail_ptr)->data.list->tail);
            left = tcelm_list_tail(left);
        } else {
            *tail_ptr = tcelm_cons(arena, right_head, TCELM_NIL);
            tail_ptr = &((*tail_ptr)->data.list->tail);
            right = tcelm_list_tail(right);
        }
    }

    /* Append remaining elements */
    *tail_ptr = tcelm_is_nil(left) ? right : left;

    return result;
}

/*
 * Helper: Merge two sorted lists with custom comparison function
 */
static tcelm_value_t *merge_lists_with(tcelm_arena_t *arena, tcelm_value_t *cmp,
                                        tcelm_value_t *left, tcelm_value_t *right) {
    if (tcelm_is_nil(left)) return right;
    if (tcelm_is_nil(right)) return left;

    tcelm_value_t *result = TCELM_NIL;
    tcelm_value_t **tail_ptr = &result;

    while (!tcelm_is_nil(left) && !tcelm_is_nil(right)) {
        tcelm_value_t *left_head = tcelm_list_head(left);
        tcelm_value_t *right_head = tcelm_list_head(right);

        /* Apply comparison function: cmp a b returns Order (LT=0, EQ=1, GT=2) */
        tcelm_value_t *cmp_with_a = tcelm_apply(arena, cmp, left_head);
        tcelm_value_t *order = tcelm_apply(arena, cmp_with_a, right_head);
        int cmp_result = (int)tcelm_custom_ctor(order);  /* LT=0, EQ=1, GT=2 */

        if (cmp_result <= 1) {  /* LT or EQ */
            *tail_ptr = tcelm_cons(arena, left_head, TCELM_NIL);
            tail_ptr = &((*tail_ptr)->data.list->tail);
            left = tcelm_list_tail(left);
        } else {
            *tail_ptr = tcelm_cons(arena, right_head, TCELM_NIL);
            tail_ptr = &((*tail_ptr)->data.list->tail);
            right = tcelm_list_tail(right);
        }
    }

    *tail_ptr = tcelm_is_nil(left) ? right : left;
    return result;
}

/*
 * Helper: Split list in half for merge sort
 */
static void split_list(tcelm_value_t *list, tcelm_value_t **left, tcelm_value_t **right) {
    if (tcelm_is_nil(list) || tcelm_is_nil(tcelm_list_tail(list))) {
        *left = list;
        *right = TCELM_NIL;
        return;
    }

    /* Use slow/fast pointer technique */
    tcelm_value_t *slow = list;
    tcelm_value_t *fast = tcelm_list_tail(list);

    while (!tcelm_is_nil(fast) && !tcelm_is_nil(tcelm_list_tail(fast))) {
        slow = tcelm_list_tail(slow);
        fast = tcelm_list_tail(tcelm_list_tail(fast));
    }

    *left = list;
    *right = tcelm_list_tail(slow);

    /* Cut the list */
    slow->data.list->tail = NULL;
}

/*
 * Helper: Merge sort implementation
 */
static tcelm_value_t *merge_sort(tcelm_arena_t *arena, tcelm_value_t *list) {
    if (tcelm_is_nil(list) || tcelm_is_nil(tcelm_list_tail(list))) {
        return list;
    }

    tcelm_value_t *left, *right;
    split_list(list, &left, &right);

    left = merge_sort(arena, left);
    right = merge_sort(arena, right);

    return merge_lists(arena, left, right);
}

/*
 * Helper: Merge sort with custom comparison
 */
static tcelm_value_t *merge_sort_with(tcelm_arena_t *arena, tcelm_value_t *cmp, tcelm_value_t *list) {
    if (tcelm_is_nil(list) || tcelm_is_nil(tcelm_list_tail(list))) {
        return list;
    }

    tcelm_value_t *left, *right;
    split_list(list, &left, &right);

    left = merge_sort_with(arena, cmp, left);
    right = merge_sort_with(arena, cmp, right);

    return merge_lists_with(arena, cmp, left, right);
}

/*
 * List.sort : List comparable -> List comparable
 *
 * Sort a list of comparable values (Int, Float, Char, String).
 * Uses stable merge sort - O(n log n).
 */
tcelm_value_t *tcelm_list_sort(tcelm_arena_t *arena, tcelm_value_t *list) {
    if (tcelm_is_nil(list)) {
        return TCELM_NIL;
    }

    /* Make a copy to avoid mutating original during sort */
    tcelm_value_t *copy = TCELM_NIL;
    tcelm_value_t *curr = list;
    while (!tcelm_is_nil(curr)) {
        copy = tcelm_cons(arena, tcelm_list_head(curr), copy);
        curr = tcelm_list_tail(curr);
    }
    copy = tcelm_list_reverse_fn(arena, copy);

    return merge_sort(arena, copy);
}

/*
 * List.sortBy : (a -> comparable) -> List a -> List a
 *
 * Sort a list by a derived comparable value.
 */
tcelm_value_t *tcelm_list_sortBy(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *list) {
    if (tcelm_is_nil(list)) {
        return TCELM_NIL;
    }

    /* Convert to list of (key, value) pairs */
    tcelm_value_t *pairs = TCELM_NIL;
    tcelm_value_t *curr = list;
    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *elem = tcelm_list_head(curr);
        tcelm_value_t *key = tcelm_apply(arena, fn, elem);
        pairs = tcelm_cons(arena, tcelm_tuple2(arena, key, elem), pairs);
        curr = tcelm_list_tail(curr);
    }
    pairs = tcelm_list_reverse_fn(arena, pairs);

    /* Sort by first element of tuple */
    /* Use sortWith with a custom comparator that compares first elements */
    tcelm_value_t *sorted = TCELM_NIL;
    tcelm_value_t **tail_ptr = &sorted;

    /* Simple insertion sort for sortBy - stable */
    curr = pairs;
    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *pair = tcelm_list_head(curr);
        tcelm_value_t *key = tcelm_tuple2_first(pair);
        tcelm_value_t *val = tcelm_tuple2_second(pair);

        /* Find insertion point */
        tcelm_value_t **insert_ptr = &sorted;
        while (*insert_ptr != TCELM_NIL) {
            tcelm_value_t *other_key = tcelm_tuple2_first(tcelm_list_head(*insert_ptr));
            if (values_compare(key, other_key) < 0) {
                break;
            }
            insert_ptr = &((*insert_ptr)->data.list->tail);
        }

        /* Insert */
        tcelm_value_t *new_node = tcelm_cons(arena, tcelm_tuple2(arena, key, val), *insert_ptr);
        *insert_ptr = new_node;

        curr = tcelm_list_tail(curr);
    }

    /* Extract values from sorted pairs */
    tcelm_value_t *result = TCELM_NIL;
    curr = sorted;
    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *pair = tcelm_list_head(curr);
        result = tcelm_cons(arena, tcelm_tuple2_second(pair), result);
        curr = tcelm_list_tail(curr);
    }

    return tcelm_list_reverse_fn(arena, result);
}

/*
 * List.sortWith : (a -> a -> Order) -> List a -> List a
 *
 * Sort a list with a custom comparison function.
 * The comparison function returns Order (LT, EQ, GT).
 */
tcelm_value_t *tcelm_list_sortWith(tcelm_arena_t *arena, tcelm_value_t *cmp, tcelm_value_t *list) {
    if (tcelm_is_nil(list)) {
        return TCELM_NIL;
    }

    /* Make a copy */
    tcelm_value_t *copy = TCELM_NIL;
    tcelm_value_t *curr = list;
    while (!tcelm_is_nil(curr)) {
        copy = tcelm_cons(arena, tcelm_list_head(curr), copy);
        curr = tcelm_list_tail(curr);
    }
    copy = tcelm_list_reverse_fn(arena, copy);

    return merge_sort_with(arena, cmp, copy);
}

/*
 * List.last : List a -> Maybe a
 *
 * Get the last element of a list.
 */
tcelm_value_t *tcelm_list_last(tcelm_arena_t *arena, tcelm_value_t *list) {
    if (tcelm_is_nil(list)) {
        return tcelm_custom(arena, 0, "Nothing", 0);
    }

    tcelm_value_t *curr = list;
    while (!tcelm_is_nil(tcelm_list_tail(curr))) {
        curr = tcelm_list_tail(curr);
    }

    return tcelm_custom(arena, 1, "Just", 1, tcelm_list_head(curr));
}

/* =========================================================================
 * Parallel Map (pmap) Implementation
 * ========================================================================= */

/*
 * Get number of available CPU cores
 */
int tcelm_get_num_cores(void) {
#ifdef __rtems__
    /* RTEMS: use configured processor count */
    return (int)rtems_scheduler_get_processor_maximum();
#else
    /* POSIX: use sysconf */
    long cores = sysconf(_SC_NPROCESSORS_ONLN);
    return (cores > 0) ? (int)cores : 1;
#endif
}

/*
 * Worker data for parallel map - uses array-based approach
 */
typedef struct pmap_worker_data {
    tcelm_arena_t *arena;       /* Shared arena (main arena) */
    tcelm_value_t *fn;          /* Function to apply */
    tcelm_value_t **input;      /* Input array slice */
    tcelm_value_t **output;     /* Output array slice */
    int start_idx;              /* Start index in arrays */
    int count;                  /* Number of elements to process */
    volatile int done;          /* Completion flag */
} pmap_worker_data_t;

/*
 * Worker function - maps fn over array slice
 */
static void pmap_worker_fn(pmap_worker_data_t *data) {
    /* Set thread-local arena */
    tcelm_current_arena = data->arena;

    for (int i = 0; i < data->count; i++) {
        data->output[data->start_idx + i] = tcelm_apply(data->arena, data->fn, data->input[data->start_idx + i]);
    }

    data->done = 1;
}

#ifdef __rtems__
/*
 * RTEMS task wrapper - runs worker and deletes self
 */
static void pmap_rtems_wrapper(rtems_task_argument arg) {
    pmap_worker_data_t *data = (pmap_worker_data_t *)arg;
    pmap_worker_fn(data);
    rtems_task_delete(RTEMS_SELF);
}
#else
/*
 * pthread wrapper for worker
 */
static void *pmap_pthread_wrapper(void *arg) {
    pmap_worker_data_t *data = (pmap_worker_data_t *)arg;
    pmap_worker_fn(data);
    return NULL;
}
#endif

/*
 * Helper: get list length as int
 */
static int list_length_int(tcelm_value_t *list) {
    int count = 0;
    while (!tcelm_is_nil(list)) {
        count++;
        list = tcelm_list_tail(list);
    }
    return count;
}

/*
 * List.pmapN : Int -> (a -> b) -> List a -> List b
 *
 * Parallel map with explicit worker count.
 * Uses array-based approach: convert list to array, map in parallel, convert back.
 */
tcelm_value_t *tcelm_list_pmapN(tcelm_arena_t *arena, int num_workers, tcelm_value_t *fn, tcelm_value_t *list) {
    int len = list_length_int(list);

    /* For empty lists, return empty */
    if (len == 0) {
        return TCELM_NIL;
    }

    /* For small lists or single worker, use sequential map */
    if (num_workers <= 1 || len < num_workers * 2) {
        return tcelm_list_map(arena, fn, list);
    }

    /* Cap workers to reasonable number */
    if (num_workers > len / 2) {
        num_workers = len / 2;
    }
    if (num_workers > 16) {
        num_workers = 16;  /* Cap at 16 workers to avoid overhead */
    }

    /* Convert list to array */
    tcelm_value_t **input = malloc(len * sizeof(tcelm_value_t *));
    tcelm_value_t **output = malloc(len * sizeof(tcelm_value_t *));
    if (!input || !output) {
        free(input);
        free(output);
        return tcelm_list_map(arena, fn, list);
    }

    tcelm_value_t *curr = list;
    for (int i = 0; i < len; i++) {
        input[i] = tcelm_list_head(curr);
        curr = tcelm_list_tail(curr);
    }

    /* Allocate worker data */
    pmap_worker_data_t *workers = malloc(num_workers * sizeof(pmap_worker_data_t));
    if (!workers) {
        free(input);
        free(output);
        return tcelm_list_map(arena, fn, list);
    }

#ifndef __rtems__
    pthread_t *threads = malloc(num_workers * sizeof(pthread_t));
    if (!threads) {
        free(workers);
        free(input);
        free(output);
        return tcelm_list_map(arena, fn, list);
    }
#endif

    /* Calculate chunk sizes and spawn workers */
    int base_chunk_size = len / num_workers;
    int remainder = len % num_workers;
    int offset = 0;

    for (int i = 0; i < num_workers; i++) {
        int chunk_size = base_chunk_size + (i < remainder ? 1 : 0);

        workers[i].arena = arena;  /* Share main arena */
        workers[i].fn = fn;
        workers[i].input = input;
        workers[i].output = output;
        workers[i].start_idx = offset;
        workers[i].count = chunk_size;
        workers[i].done = 0;

        offset += chunk_size;

#ifdef __rtems__
        /* RTEMS: create task pinned to core */
        rtems_id task_id;
        rtems_name task_name = rtems_build_name('P', 'M', 'A', '0' + (i % 10));
        rtems_status_code status = rtems_task_create(
            task_name,
            100,            /* priority */
            RTEMS_MINIMUM_STACK_SIZE + 4096,  /* stack */
            RTEMS_DEFAULT_MODES,
            RTEMS_DEFAULT_ATTRIBUTES,
            &task_id
        );
        if (status == RTEMS_SUCCESSFUL) {
            /* Pin to core (NUC has 4 cores: 0-3) */
            int num_cores = tcelm_get_num_cores();
            if (num_cores > 1) {
                cpu_set_t cpuset;
                CPU_ZERO(&cpuset);
                CPU_SET(i % num_cores, &cpuset);
                rtems_task_set_affinity(task_id, sizeof(cpuset), &cpuset);
            }
            rtems_task_start(task_id, pmap_rtems_wrapper, (rtems_task_argument)&workers[i]);
        }
#else
        pthread_create(&threads[i], NULL, pmap_pthread_wrapper, &workers[i]);
#endif
    }

    /* Wait for all workers */
#ifdef __rtems__
    for (int i = 0; i < num_workers; i++) {
        while (!workers[i].done) {
            rtems_task_wake_after(1);
        }
    }
#else
    for (int i = 0; i < num_workers; i++) {
        pthread_join(threads[i], NULL);
    }
    free(threads);
#endif

    /* Restore main thread's arena */
    tcelm_current_arena = arena;

    /* Convert output array back to list */
    tcelm_value_t *result = TCELM_NIL;
    for (int i = len - 1; i >= 0; i--) {
        result = tcelm_cons(arena, output[i], result);
    }

    free(workers);
    free(input);
    free(output);

    return result;
}

/*
 * List.pmap : (a -> b) -> List a -> List b
 *
 * Parallel map using all available cores.
 */
tcelm_value_t *tcelm_list_pmap(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *list) {
    int num_cores = tcelm_get_num_cores();
    return tcelm_list_pmapN(arena, num_cores, fn, list);
}

/*
 * Closure-compatible _impl wrappers
 * These unpack args[] and call the actual implementation
 */

tcelm_value_t *tcelm_list_any_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_any(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_list_all_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_all(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_list_filter_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_filter(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_list_map_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_map(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_list_indexedMap_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_indexedMap(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_list_foldl_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_foldl(arena, args[0], args[1], args[2]);
}

tcelm_value_t *tcelm_list_foldr_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_foldr(arena, args[0], args[1], args[2]);
}

tcelm_value_t *tcelm_list_map2_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_map2(arena, args[0], args[1], args[2]);
}

tcelm_value_t *tcelm_list_concatMap_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_concatMap(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_list_member_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_member(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_list_repeat_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_repeat(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_list_range_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_range(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_list_drop_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_drop(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_list_take_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_take(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_list_filterMap_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_filterMap(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_list_partition_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_partition(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_list_concat_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_concat_fn(arena, args[0]);
}

tcelm_value_t *tcelm_list_sort_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_sort(arena, args[0]);
}

tcelm_value_t *tcelm_list_sortBy_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_sortBy(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_list_sortWith_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_sortWith(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_list_last_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_list_last(arena, args[0]);
}
