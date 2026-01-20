/*
 * tcelm_types.c - Elm type operations implementation
 */

#include "tcelm_types.h"
#include <string.h>
#include <stdarg.h>

/* Pre-allocated constants (no arena needed) */
tcelm_value_t tcelm_true_val = { .tag = TCELM_TAG_BOOL, .data.b = true };
tcelm_value_t tcelm_false_val = { .tag = TCELM_TAG_BOOL, .data.b = false };
tcelm_value_t tcelm_unit_val = { .tag = TCELM_TAG_UNIT };
tcelm_value_t tcelm_nil_val = { .tag = TCELM_TAG_LIST, .data.list = NULL };

/*
 * Primitive constructors
 */

tcelm_value_t *tcelm_int(tcelm_arena_t *arena, int64_t n) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    v->tag = TCELM_TAG_INT;
    v->data.i = n;
    return v;
}

tcelm_value_t *tcelm_float(tcelm_arena_t *arena, double f) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    v->tag = TCELM_TAG_FLOAT;
    v->data.f = f;
    return v;
}

tcelm_value_t *tcelm_char(tcelm_arena_t *arena, uint32_t c) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    v->tag = TCELM_TAG_CHAR;
    v->data.c = c;
    return v;
}

tcelm_value_t *tcelm_bool(tcelm_arena_t *arena, bool b) {
    /* Use pre-allocated constants */
    (void)arena;
    return b ? TCELM_TRUE : TCELM_FALSE;
}

/* Count UTF-8 characters in a string */
static size_t utf8_char_count(const char *s, size_t len) {
    size_t count = 0;
    for (size_t i = 0; i < len; ) {
        unsigned char c = (unsigned char)s[i];
        if (c < 0x80) {
            i += 1;
        } else if ((c & 0xE0) == 0xC0) {
            i += 2;
        } else if ((c & 0xF0) == 0xE0) {
            i += 3;
        } else {
            i += 4;
        }
        count++;
    }
    return count;
}

tcelm_value_t *tcelm_string(tcelm_arena_t *arena, const char *s) {
    return tcelm_string_len(arena, s, strlen(s));
}

tcelm_value_t *tcelm_string_len(tcelm_arena_t *arena, const char *s, size_t len) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_string_t *str = tcelm_arena_alloc(arena, sizeof(tcelm_string_t) + len + 1);

    str->length = len;
    str->char_count = utf8_char_count(s, len);
    memcpy(str->data, s, len);
    str->data[len] = '\0';

    v->tag = TCELM_TAG_STRING;
    v->data.s = str;
    return v;
}

tcelm_value_t *tcelm_unit(tcelm_arena_t *arena) {
    (void)arena;
    return TCELM_UNIT;
}

/*
 * List operations
 */

tcelm_value_t *tcelm_nil(tcelm_arena_t *arena) {
    (void)arena;
    return TCELM_NIL;
}

tcelm_value_t *tcelm_cons(tcelm_arena_t *arena, tcelm_value_t *head, tcelm_value_t *tail) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_list_t *cell = tcelm_arena_alloc(arena, sizeof(tcelm_list_t));

    cell->head = head;
    cell->tail = (tail->tag == TCELM_TAG_LIST) ? tail->data.list : NULL;

    v->tag = TCELM_TAG_LIST;
    v->data.list = cell;
    return v;
}

bool tcelm_is_nil(tcelm_value_t *list) {
    return list->tag == TCELM_TAG_LIST && list->data.list == NULL;
}

tcelm_value_t *tcelm_list_head(tcelm_value_t *list) {
    if (list->data.list == NULL) return NULL;
    return list->data.list->head;
}

tcelm_value_t *tcelm_list_tail(tcelm_value_t *list) {
    if (list->data.list == NULL) return TCELM_NIL;
    tcelm_list_t *tail = list->data.list->tail;
    if (tail == NULL) return TCELM_NIL;

    /* Return a value pointing to the tail */
    /* Note: This reuses the tail structure directly */
    tcelm_value_t *v = tcelm_arena_alloc(tcelm_current_arena, sizeof(tcelm_value_t));
    v->tag = TCELM_TAG_LIST;
    v->data.list = tail;
    return v;
}

/*
 * Tuple operations
 */

tcelm_value_t *tcelm_tuple2(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_tuple2_t *t = tcelm_arena_alloc(arena, sizeof(tcelm_tuple2_t));

    t->first = a;
    t->second = b;

    v->tag = TCELM_TAG_TUPLE2;
    v->data.tuple2 = t;
    return v;
}

tcelm_value_t *tcelm_tuple3(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b, tcelm_value_t *c) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_tuple3_t *t = tcelm_arena_alloc(arena, sizeof(tcelm_tuple3_t));

    t->first = a;
    t->second = b;
    t->third = c;

    v->tag = TCELM_TAG_TUPLE3;
    v->data.tuple3 = t;
    return v;
}

tcelm_value_t *tcelm_tuple2_first(tcelm_value_t *tuple) {
    return tuple->data.tuple2->first;
}

tcelm_value_t *tcelm_tuple2_second(tcelm_value_t *tuple) {
    return tuple->data.tuple2->second;
}

tcelm_value_t *tcelm_tuple3_first(tcelm_value_t *tuple) {
    return tuple->data.tuple3->first;
}

tcelm_value_t *tcelm_tuple3_second(tcelm_value_t *tuple) {
    return tuple->data.tuple3->second;
}

tcelm_value_t *tcelm_tuple3_third(tcelm_value_t *tuple) {
    return tuple->data.tuple3->third;
}

/*
 * Record operations
 */

/* Binary search for field by name */
static int field_compare(const void *a, const void *b) {
    const tcelm_field_t *fa = (const tcelm_field_t *)a;
    const tcelm_field_t *fb = (const tcelm_field_t *)b;
    return strcmp(fa->name, fb->name);
}

tcelm_value_t *tcelm_record(tcelm_arena_t *arena, size_t field_count, ...) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_record_t *rec = tcelm_arena_alloc(arena,
        sizeof(tcelm_record_t) + field_count * sizeof(tcelm_field_t));

    rec->field_count = field_count;

    va_list args;
    va_start(args, field_count);
    for (size_t i = 0; i < field_count; i++) {
        rec->fields[i].name = va_arg(args, const char *);
        rec->fields[i].value = va_arg(args, tcelm_value_t *);
    }
    va_end(args);

    /* Sort fields by name for binary search */
    /* Note: For small records, linear search might be faster */
    if (field_count > 4) {
        /* Use insertion sort for small arrays (stable, fast for small n) */
        for (size_t i = 1; i < field_count; i++) {
            tcelm_field_t key = rec->fields[i];
            size_t j = i;
            while (j > 0 && strcmp(rec->fields[j-1].name, key.name) > 0) {
                rec->fields[j] = rec->fields[j-1];
                j--;
            }
            rec->fields[j] = key;
        }
    }

    v->tag = TCELM_TAG_RECORD;
    v->data.record = rec;
    return v;
}

tcelm_value_t *tcelm_record_get(tcelm_value_t *record, const char *field) {
    tcelm_record_t *rec = record->data.record;

    /* Linear search for small records, binary search for large */
    if (rec->field_count <= 8) {
        for (size_t i = 0; i < rec->field_count; i++) {
            if (strcmp(rec->fields[i].name, field) == 0) {
                return rec->fields[i].value;
            }
        }
    } else {
        /* Binary search */
        size_t lo = 0, hi = rec->field_count;
        while (lo < hi) {
            size_t mid = lo + (hi - lo) / 2;
            int cmp = strcmp(rec->fields[mid].name, field);
            if (cmp == 0) {
                return rec->fields[mid].value;
            } else if (cmp < 0) {
                lo = mid + 1;
            } else {
                hi = mid;
            }
        }
    }

    return NULL; /* Field not found */
}

tcelm_value_t *tcelm_record_update(tcelm_arena_t *arena, tcelm_value_t *record,
                                    const char *field, tcelm_value_t *value) {
    tcelm_record_t *old = record->data.record;
    size_t n = old->field_count;

    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_record_t *rec = tcelm_arena_alloc(arena,
        sizeof(tcelm_record_t) + n * sizeof(tcelm_field_t));

    rec->field_count = n;

    /* Copy fields, replacing the updated one */
    for (size_t i = 0; i < n; i++) {
        rec->fields[i].name = old->fields[i].name;
        if (strcmp(old->fields[i].name, field) == 0) {
            rec->fields[i].value = value;
        } else {
            rec->fields[i].value = old->fields[i].value;
        }
    }

    v->tag = TCELM_TAG_RECORD;
    v->data.record = rec;
    return v;
}

/*
 * Closure operations
 */

tcelm_value_t *tcelm_closure(tcelm_arena_t *arena, tcelm_fn_ptr fn, uint8_t arity) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_closure_t *cl = tcelm_arena_alloc(arena,
        sizeof(tcelm_closure_t) + arity * sizeof(tcelm_value_t *));

    cl->fn = fn;
    cl->arity = arity;
    cl->applied = 0;

    v->tag = TCELM_TAG_CLOSURE;
    v->data.closure = cl;
    return v;
}

tcelm_value_t *tcelm_apply(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *arg) {
    tcelm_closure_t *cl = fn->data.closure;

    if (cl->applied + 1 < cl->arity) {
        /* Still need more arguments - create new closure */
        tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
        tcelm_closure_t *new_cl = tcelm_arena_alloc(arena,
            sizeof(tcelm_closure_t) + cl->arity * sizeof(tcelm_value_t *));

        new_cl->fn = cl->fn;
        new_cl->arity = cl->arity;
        new_cl->applied = cl->applied + 1;

        /* Copy existing arguments */
        for (uint8_t i = 0; i < cl->applied; i++) {
            new_cl->args[i] = cl->args[i];
        }
        new_cl->args[cl->applied] = arg;

        v->tag = TCELM_TAG_CLOSURE;
        v->data.closure = new_cl;
        return v;
    } else {
        /* All arguments provided - call function */
        tcelm_value_t *args[16]; /* Max 16 arguments */
        for (uint8_t i = 0; i < cl->applied; i++) {
            args[i] = cl->args[i];
        }
        args[cl->applied] = arg;

        return cl->fn(arena, args);
    }
}

tcelm_value_t *tcelm_apply_n(tcelm_arena_t *arena, tcelm_value_t *fn, int n, ...) {
    va_list args;
    va_start(args, n);

    tcelm_value_t *result = fn;
    for (int i = 0; i < n; i++) {
        tcelm_value_t *arg = va_arg(args, tcelm_value_t *);
        result = tcelm_apply(arena, result, arg);
    }

    va_end(args);
    return result;
}

/*
 * Custom type operations
 */

tcelm_value_t *tcelm_custom(tcelm_arena_t *arena, uint16_t ctor_id, const char *name,
                            uint16_t arg_count, ...) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_custom_t *c = tcelm_arena_alloc(arena,
        sizeof(tcelm_custom_t) + arg_count * sizeof(tcelm_value_t *));

    c->ctor_id = ctor_id;
    c->ctor_name = name;
    c->arg_count = arg_count;

    va_list args;
    va_start(args, arg_count);
    for (uint16_t i = 0; i < arg_count; i++) {
        c->args[i] = va_arg(args, tcelm_value_t *);
    }
    va_end(args);

    v->tag = TCELM_TAG_CUSTOM;
    v->data.custom = c;
    return v;
}

uint16_t tcelm_custom_ctor(tcelm_value_t *value) {
    return value->data.custom->ctor_id;
}

tcelm_value_t *tcelm_custom_arg(tcelm_value_t *value, int index) {
    return value->data.custom->args[index];
}
