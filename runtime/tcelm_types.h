/*
 * tcelm_types.h - Elm type representations in C
 *
 * Elm values are represented as tagged unions. Each value has a tag
 * indicating its type, followed by type-specific data.
 *
 * Memory layout:
 *   - All values are allocated from the arena
 *   - Values are immutable (new values created on modification)
 *   - No reference counting or GC needed due to arena allocation
 */

#ifndef TCELM_TYPES_H
#define TCELM_TYPES_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include "tcelm_arena.h"

/* Forward declarations */
typedef struct tcelm_value tcelm_value_t;
typedef struct tcelm_closure tcelm_closure_t;
typedef struct tcelm_list tcelm_list_t;
typedef struct tcelm_record tcelm_record_t;
typedef struct tcelm_custom tcelm_custom_t;

/* Value tags - what kind of value is this? */
typedef enum tcelm_tag {
    /* Primitives */
    TCELM_TAG_INT,          /* Int (64-bit signed) */
    TCELM_TAG_FLOAT,        /* Float (64-bit double) */
    TCELM_TAG_CHAR,         /* Char (Unicode codepoint) */
    TCELM_TAG_STRING,       /* String (UTF-8) */
    TCELM_TAG_BOOL,         /* Bool (True/False) */

    /* Unit */
    TCELM_TAG_UNIT,         /* () */

    /* Containers */
    TCELM_TAG_LIST,         /* List a */
    TCELM_TAG_TUPLE2,       /* (a, b) */
    TCELM_TAG_TUPLE3,       /* (a, b, c) */
    TCELM_TAG_RECORD,       /* { field1 : a, field2 : b, ... } */

    /* Functions */
    TCELM_TAG_CLOSURE,      /* Partially applied function */

    /* Custom types (union types) */
    TCELM_TAG_CUSTOM,       /* type Foo = Bar Int | Baz String */

    /* Special */
    TCELM_TAG_THUNK,        /* Lazy evaluation (not yet computed) */
} tcelm_tag_t;

/* String representation */
typedef struct tcelm_string {
    size_t length;          /* Length in bytes (not chars) */
    size_t char_count;      /* Number of Unicode characters */
    char data[];            /* UTF-8 encoded data (null-terminated) */
} tcelm_string_t;

/* List cell (cons cell) */
struct tcelm_list {
    tcelm_value_t *head;    /* First element */
    tcelm_list_t *tail;     /* Rest of list (NULL for empty) */
};

/* Tuple (2 elements) */
typedef struct tcelm_tuple2 {
    tcelm_value_t *first;
    tcelm_value_t *second;
} tcelm_tuple2_t;

/* Tuple (3 elements) */
typedef struct tcelm_tuple3 {
    tcelm_value_t *first;
    tcelm_value_t *second;
    tcelm_value_t *third;
} tcelm_tuple3_t;

/* Record field */
typedef struct tcelm_field {
    const char *name;       /* Field name (interned string) */
    tcelm_value_t *value;   /* Field value */
} tcelm_field_t;

/* Record */
struct tcelm_record {
    size_t field_count;
    tcelm_field_t fields[]; /* Sorted by field name for fast lookup */
};

/* Closure (partially applied function) */
typedef tcelm_value_t *(*tcelm_fn_ptr)(tcelm_arena_t *, tcelm_value_t **args);

struct tcelm_closure {
    tcelm_fn_ptr fn;        /* Function pointer */
    uint8_t arity;          /* Total arguments needed */
    uint8_t applied;        /* Arguments already applied */
    tcelm_value_t *args[];  /* Applied arguments */
};

/* Custom type (union type) */
struct tcelm_custom {
    uint16_t ctor_id;       /* Constructor ID within the type */
    uint16_t arg_count;     /* Number of constructor arguments */
    const char *ctor_name;  /* Constructor name (for debugging) */
    tcelm_value_t *args[];  /* Constructor arguments */
};

/* Thunk for lazy evaluation */
typedef struct tcelm_thunk {
    tcelm_fn_ptr compute;   /* Function to compute value */
    tcelm_value_t *result;  /* Cached result (NULL if not computed) */
    tcelm_value_t *env[];   /* Captured environment */
} tcelm_thunk_t;

/* The universal value type */
struct tcelm_value {
    tcelm_tag_t tag;
    union {
        int64_t i;              /* TCELM_TAG_INT */
        double f;               /* TCELM_TAG_FLOAT */
        uint32_t c;             /* TCELM_TAG_CHAR (Unicode codepoint) */
        bool b;                 /* TCELM_TAG_BOOL */
        tcelm_string_t *s;      /* TCELM_TAG_STRING */
        tcelm_list_t *list;     /* TCELM_TAG_LIST */
        tcelm_tuple2_t *tuple2; /* TCELM_TAG_TUPLE2 */
        tcelm_tuple3_t *tuple3; /* TCELM_TAG_TUPLE3 */
        tcelm_record_t *record; /* TCELM_TAG_RECORD */
        tcelm_closure_t *closure; /* TCELM_TAG_CLOSURE */
        tcelm_custom_t *custom; /* TCELM_TAG_CUSTOM */
        tcelm_thunk_t *thunk;   /* TCELM_TAG_THUNK */
    } data;
};

/*
 * Value constructors
 * All return pointers to arena-allocated values
 */

/* Primitives */
tcelm_value_t *tcelm_int(tcelm_arena_t *arena, int64_t n);
tcelm_value_t *tcelm_float(tcelm_arena_t *arena, double f);
tcelm_value_t *tcelm_char(tcelm_arena_t *arena, uint32_t c);
tcelm_value_t *tcelm_bool(tcelm_arena_t *arena, bool b);
tcelm_value_t *tcelm_string(tcelm_arena_t *arena, const char *s);
tcelm_value_t *tcelm_string_len(tcelm_arena_t *arena, const char *s, size_t len);
tcelm_value_t *tcelm_unit(tcelm_arena_t *arena);

/* List operations */
tcelm_value_t *tcelm_nil(tcelm_arena_t *arena);
tcelm_value_t *tcelm_cons(tcelm_arena_t *arena, tcelm_value_t *head, tcelm_value_t *tail);
bool tcelm_is_nil(tcelm_value_t *list);
tcelm_value_t *tcelm_list_head(tcelm_value_t *list);
tcelm_value_t *tcelm_list_tail(tcelm_value_t *list);

/* Tuple operations */
tcelm_value_t *tcelm_tuple2(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_tuple3(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b, tcelm_value_t *c);
tcelm_value_t *tcelm_tuple2_first(tcelm_value_t *tuple);
tcelm_value_t *tcelm_tuple2_second(tcelm_value_t *tuple);
tcelm_value_t *tcelm_tuple3_first(tcelm_value_t *tuple);
tcelm_value_t *tcelm_tuple3_second(tcelm_value_t *tuple);
tcelm_value_t *tcelm_tuple3_third(tcelm_value_t *tuple);

/* Record operations */
tcelm_value_t *tcelm_record(tcelm_arena_t *arena, size_t field_count, ...);
tcelm_value_t *tcelm_record_get(tcelm_value_t *record, const char *field);
tcelm_value_t *tcelm_record_update(tcelm_arena_t *arena, tcelm_value_t *record,
                                    const char *field, tcelm_value_t *value);

/* Closure operations */
tcelm_value_t *tcelm_closure(tcelm_arena_t *arena, tcelm_fn_ptr fn, uint8_t arity);
tcelm_value_t *tcelm_apply(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *arg);
tcelm_value_t *tcelm_apply_n(tcelm_arena_t *arena, tcelm_value_t *fn, int n, ...);

/* Custom type operations */
tcelm_value_t *tcelm_custom(tcelm_arena_t *arena, uint16_t ctor_id, const char *name,
                            uint16_t arg_count, ...);
uint16_t tcelm_custom_ctor(tcelm_value_t *value);
tcelm_value_t *tcelm_custom_arg(tcelm_value_t *value, int index);

/* Type checking macros */
#define TCELM_IS_INT(v)     ((v)->tag == TCELM_TAG_INT)
#define TCELM_IS_FLOAT(v)   ((v)->tag == TCELM_TAG_FLOAT)
#define TCELM_IS_CHAR(v)    ((v)->tag == TCELM_TAG_CHAR)
#define TCELM_IS_STRING(v)  ((v)->tag == TCELM_TAG_STRING)
#define TCELM_IS_BOOL(v)    ((v)->tag == TCELM_TAG_BOOL)
#define TCELM_IS_LIST(v)    ((v)->tag == TCELM_TAG_LIST)
#define TCELM_IS_TUPLE2(v)  ((v)->tag == TCELM_TAG_TUPLE2)
#define TCELM_IS_TUPLE3(v)  ((v)->tag == TCELM_TAG_TUPLE3)
#define TCELM_IS_RECORD(v)  ((v)->tag == TCELM_TAG_RECORD)
#define TCELM_IS_CLOSURE(v) ((v)->tag == TCELM_TAG_CLOSURE)
#define TCELM_IS_CUSTOM(v)  ((v)->tag == TCELM_TAG_CUSTOM)

/* Value extraction macros */
#define TCELM_AS_INT(v)     ((v)->data.i)
#define TCELM_AS_FLOAT(v)   ((v)->data.f)
#define TCELM_AS_CHAR(v)    ((v)->data.c)
#define TCELM_AS_BOOL(v)    ((v)->data.b)
#define TCELM_AS_STRING(v)  ((v)->data.s)

/* Pre-allocated constants */
extern tcelm_value_t tcelm_true_val;
extern tcelm_value_t tcelm_false_val;
extern tcelm_value_t tcelm_unit_val;
extern tcelm_value_t tcelm_nil_val;

#define TCELM_TRUE  (&tcelm_true_val)
#define TCELM_FALSE (&tcelm_false_val)
#define TCELM_UNIT  (&tcelm_unit_val)
#define TCELM_NIL   (&tcelm_nil_val)

#endif /* TCELM_TYPES_H */

/* Predefined constructor IDs for built-in types */
#define TCELM_CTOR_TRUE 0
#define TCELM_CTOR_FALSE 1
#define TCELM_CTOR_NOTHING 0
#define TCELM_CTOR_JUST 1
#define TCELM_CTOR_OK 0
#define TCELM_CTOR_ERR 1
#define TCELM_CTOR_LT 0
#define TCELM_CTOR_EQ 1
#define TCELM_CTOR_GT 2

/* AST types (for self-hosting) */
#define TCELM_CTOR_AT 0

/* Pattern constructors */
#define TCELM_CTOR_PANYTHING 0
#define TCELM_CTOR_PVAR 1
#define TCELM_CTOR_PRECORD 2
#define TCELM_CTOR_PALIAS 3
#define TCELM_CTOR_PUNIT 4
#define TCELM_CTOR_PTUPLE 5
#define TCELM_CTOR_PCTOR 6
#define TCELM_CTOR_PCTORQUAL 7
#define TCELM_CTOR_PLIST 8
#define TCELM_CTOR_PCONS 9
#define TCELM_CTOR_PCHR 10
#define TCELM_CTOR_PSTR 11
#define TCELM_CTOR_PINT 12

/* Expr constructors */
#define TCELM_CTOR_INT 0
#define TCELM_CTOR_FLOAT 1
#define TCELM_CTOR_CHR 2
#define TCELM_CTOR_STR 3
#define TCELM_CTOR_VAR 4
#define TCELM_CTOR_VARQUAL 5
#define TCELM_CTOR_LIST 6
#define TCELM_CTOR_OP 7
#define TCELM_CTOR_NEGATE 8
#define TCELM_CTOR_BINOPS 9
#define TCELM_CTOR_LAMBDA 10
#define TCELM_CTOR_CALL 11
#define TCELM_CTOR_IF 12
#define TCELM_CTOR_LET 13
#define TCELM_CTOR_CASE 14
#define TCELM_CTOR_ACCESSOR 15
#define TCELM_CTOR_ACCESS 16
#define TCELM_CTOR_UPDATE 17
#define TCELM_CTOR_RECORD 18
#define TCELM_CTOR_UNIT 19
#define TCELM_CTOR_TUPLE 20

/* VarType constructors */
#define TCELM_CTOR_LOWVAR 0
#define TCELM_CTOR_CAPVAR 1

/* Def constructors */
#define TCELM_CTOR_DEFINE 0
#define TCELM_CTOR_DESTRUCT 1

/* Type constructors */
#define TCELM_CTOR_TLAMBDA 0
#define TCELM_CTOR_TVAR 1
#define TCELM_CTOR_TTYPE 2
#define TCELM_CTOR_TTYPEQUAL 3
#define TCELM_CTOR_TRECORD 4
#define TCELM_CTOR_TUNIT 5
#define TCELM_CTOR_TTUPLE 6
