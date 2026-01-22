/*
 * qemu_runtime_test.c - Comprehensive tcelm runtime tests for QEMU
 *
 * Tests the core runtime components in bare-metal QEMU:
 *   - Arena allocator
 *   - Type system (int, float, bool, char, string)
 *   - Lists and list operations
 *   - Tuples
 *   - Records
 *   - Custom types (Maybe, Result)
 *   - Closures and function application
 *
 * Compile with TCC:
 *   tcc -m32 -nostdlib -nostdinc qemu_runtime_test.c -o qemu_runtime_test.o
 *
 * Link:
 *   ld -m elf_i386 -T ../tcc-nuc-bsp/linkcmds.qemu \
 *      ../tcc-nuc-bsp/build/multiboot.o qemu_runtime_test.o -o qemu_runtime_test.elf
 *
 * Run:
 *   qemu-system-i386 -kernel qemu_runtime_test.elf -nographic -no-reboot
 */

#include "qemu_libc.h"

/* ========================================================================
 * ARENA ALLOCATOR (inlined from tcelm_arena.c)
 * ======================================================================== */

typedef struct tcelm_arena_block {
    struct tcelm_arena_block *next;
    size_t size;
    size_t used;
    char data[];
} tcelm_arena_block_t;

typedef struct tcelm_arena {
    tcelm_arena_block_t *first;
    tcelm_arena_block_t *current;
    size_t block_size;
    size_t total_allocated;
    size_t total_used;
} tcelm_arena_t;

TCELM_TLS tcelm_arena_t *tcelm_current_arena = NULL;

#define ARENA_ALIGNMENT 8
#define ALIGN_UP(x, a) (((x) + ((a) - 1)) & ~((a) - 1))
#define MIN_BLOCK_SIZE 4096

static tcelm_arena_block_t *arena_block_create(size_t size) {
    size_t total = sizeof(tcelm_arena_block_t) + size;
    tcelm_arena_block_t *block = (tcelm_arena_block_t *)malloc(total);
    if (!block) return NULL;
    block->next = NULL;
    block->size = size;
    block->used = 0;
    return block;
}

static int tcelm_arena_init(tcelm_arena_t *arena, size_t block_size) {
    if (!arena) return -1;
    if (block_size < MIN_BLOCK_SIZE) block_size = MIN_BLOCK_SIZE;
    arena->block_size = block_size;
    arena->total_allocated = 0;
    arena->total_used = 0;
    arena->first = arena_block_create(block_size);
    if (!arena->first) return -1;
    arena->current = arena->first;
    arena->total_allocated = block_size;
    return 0;
}

static tcelm_arena_t *tcelm_arena_create(size_t block_size) {
    tcelm_arena_t *arena = (tcelm_arena_t *)malloc(sizeof(tcelm_arena_t));
    if (!arena) return NULL;
    if (tcelm_arena_init(arena, block_size) != 0) {
        return NULL;
    }
    return arena;
}

static void *tcelm_arena_alloc(tcelm_arena_t *arena, size_t size) {
    if (!arena || size == 0) return NULL;
    size = ALIGN_UP(size, ARENA_ALIGNMENT);

    tcelm_arena_block_t *block = arena->current;
    while (block) {
        size_t aligned_used = ALIGN_UP(block->used, ARENA_ALIGNMENT);
        if (aligned_used + size <= block->size) {
            void *ptr = block->data + aligned_used;
            block->used = aligned_used + size;
            arena->total_used += size;
            return ptr;
        }
        if (block->next) {
            block = block->next;
            arena->current = block;
        } else {
            break;
        }
    }

    /* Need a new block */
    size_t new_block_size = arena->block_size;
    if (size > new_block_size) new_block_size = size;

    tcelm_arena_block_t *new_block = arena_block_create(new_block_size);
    if (!new_block) return NULL;

    if (block) block->next = new_block;
    arena->current = new_block;
    arena->total_allocated += new_block_size;

    void *ptr = new_block->data;
    new_block->used = size;
    arena->total_used += size;
    return ptr;
}

/* ========================================================================
 * TYPE SYSTEM (inlined from tcelm_types.c)
 * ======================================================================== */

typedef enum tcelm_tag {
    TCELM_TAG_INT,
    TCELM_TAG_FLOAT,
    TCELM_TAG_CHAR,
    TCELM_TAG_STRING,
    TCELM_TAG_BOOL,
    TCELM_TAG_UNIT,
    TCELM_TAG_LIST,
    TCELM_TAG_TUPLE2,
    TCELM_TAG_TUPLE3,
    TCELM_TAG_RECORD,
    TCELM_TAG_CLOSURE,
    TCELM_TAG_CUSTOM,
} tcelm_tag_t;

typedef struct tcelm_value tcelm_value_t;
typedef struct tcelm_list tcelm_list_t;

typedef struct tcelm_string {
    size_t length;
    size_t char_count;
    char data[];
} tcelm_string_t;

struct tcelm_list {
    tcelm_value_t *head;
    tcelm_list_t *tail;
};

typedef struct {
    tcelm_value_t *first;
    tcelm_value_t *second;
} tcelm_tuple2_t;

typedef struct {
    tcelm_value_t *first;
    tcelm_value_t *second;
    tcelm_value_t *third;
} tcelm_tuple3_t;

typedef struct {
    const char *name;
    tcelm_value_t *value;
} tcelm_field_t;

typedef struct {
    size_t field_count;
    tcelm_field_t fields[];
} tcelm_record_t;

typedef tcelm_value_t *(*tcelm_fn_ptr)(tcelm_arena_t *, tcelm_value_t **args);

typedef struct {
    tcelm_fn_ptr fn;
    uint8_t arity;
    uint8_t applied;
    tcelm_value_t *args[];
} tcelm_closure_t;

typedef struct {
    uint16_t ctor_id;
    uint16_t arg_count;
    const char *ctor_name;
    tcelm_value_t *args[];
} tcelm_custom_t;

struct tcelm_value {
    tcelm_tag_t tag;
    union {
        int64_t i;
        double f;
        uint32_t c;
        bool b;
        tcelm_string_t *s;
        tcelm_list_t *list;
        tcelm_tuple2_t *tuple2;
        tcelm_tuple3_t *tuple3;
        tcelm_record_t *record;
        tcelm_closure_t *closure;
        tcelm_custom_t *custom;
    } data;
};

/* Pre-allocated constants */
static tcelm_value_t tcelm_true_val = { .tag = TCELM_TAG_BOOL, .data.b = true };
static tcelm_value_t tcelm_false_val = { .tag = TCELM_TAG_BOOL, .data.b = false };
static tcelm_value_t tcelm_unit_val = { .tag = TCELM_TAG_UNIT };
static tcelm_value_t tcelm_nil_val = { .tag = TCELM_TAG_LIST, .data.list = NULL };

#define TCELM_TRUE  (&tcelm_true_val)
#define TCELM_FALSE (&tcelm_false_val)
#define TCELM_UNIT  (&tcelm_unit_val)
#define TCELM_NIL   (&tcelm_nil_val)

#define TCELM_AS_INT(v)     ((v)->data.i)
#define TCELM_AS_FLOAT(v)   ((v)->data.f)
#define TCELM_AS_CHAR(v)    ((v)->data.c)
#define TCELM_AS_BOOL(v)    ((v)->data.b)
#define TCELM_AS_STRING(v)  ((v)->data.s)

#define TCELM_CTOR_NOTHING 0
#define TCELM_CTOR_JUST 1

/* Value constructors */
static tcelm_value_t *tcelm_int(tcelm_arena_t *arena, int64_t n) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    v->tag = TCELM_TAG_INT;
    v->data.i = n;
    return v;
}

static tcelm_value_t *tcelm_bool(tcelm_arena_t *arena, bool b) {
    (void)arena;
    return b ? TCELM_TRUE : TCELM_FALSE;
}

static tcelm_value_t *tcelm_char(tcelm_arena_t *arena, uint32_t c) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    v->tag = TCELM_TAG_CHAR;
    v->data.c = c;
    return v;
}

static tcelm_value_t *tcelm_string(tcelm_arena_t *arena, const char *s) {
    size_t len = strlen(s);
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_string_t *str = tcelm_arena_alloc(arena, sizeof(tcelm_string_t) + len + 1);
    str->length = len;
    str->char_count = len;  /* Simplified: assume ASCII */
    memcpy(str->data, s, len);
    str->data[len] = '\0';
    v->tag = TCELM_TAG_STRING;
    v->data.s = str;
    return v;
}

static tcelm_value_t *tcelm_unit(tcelm_arena_t *arena) {
    (void)arena;
    return TCELM_UNIT;
}

static tcelm_value_t *tcelm_nil(tcelm_arena_t *arena) {
    (void)arena;
    return TCELM_NIL;
}

static tcelm_value_t *tcelm_cons(tcelm_arena_t *arena, tcelm_value_t *head, tcelm_value_t *tail) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_list_t *cell = tcelm_arena_alloc(arena, sizeof(tcelm_list_t));
    cell->head = head;
    cell->tail = (tail->tag == TCELM_TAG_LIST) ? tail->data.list : NULL;
    v->tag = TCELM_TAG_LIST;
    v->data.list = cell;
    return v;
}

static bool tcelm_is_nil(tcelm_value_t *list) {
    return list->tag == TCELM_TAG_LIST && list->data.list == NULL;
}

static tcelm_value_t *tcelm_list_head(tcelm_value_t *list) {
    if (list->data.list == NULL) return NULL;
    return list->data.list->head;
}

static tcelm_value_t *tcelm_tuple2(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_tuple2_t *t = tcelm_arena_alloc(arena, sizeof(tcelm_tuple2_t));
    t->first = a;
    t->second = b;
    v->tag = TCELM_TAG_TUPLE2;
    v->data.tuple2 = t;
    return v;
}

static tcelm_value_t *tcelm_tuple3(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b, tcelm_value_t *c) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_tuple3_t *t = tcelm_arena_alloc(arena, sizeof(tcelm_tuple3_t));
    t->first = a;
    t->second = b;
    t->third = c;
    v->tag = TCELM_TAG_TUPLE3;
    v->data.tuple3 = t;
    return v;
}

static tcelm_value_t *tcelm_record2(tcelm_arena_t *arena,
                                     const char *n1, tcelm_value_t *v1,
                                     const char *n2, tcelm_value_t *v2) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_record_t *rec = tcelm_arena_alloc(arena, sizeof(tcelm_record_t) + 2 * sizeof(tcelm_field_t));
    rec->field_count = 2;
    rec->fields[0].name = n1;
    rec->fields[0].value = v1;
    rec->fields[1].name = n2;
    rec->fields[1].value = v2;
    v->tag = TCELM_TAG_RECORD;
    v->data.record = rec;
    return v;
}

static tcelm_value_t *tcelm_record_get(tcelm_value_t *record, const char *field) {
    tcelm_record_t *rec = record->data.record;
    for (size_t i = 0; i < rec->field_count; i++) {
        if (strcmp(rec->fields[i].name, field) == 0) {
            return rec->fields[i].value;
        }
    }
    return NULL;
}

static tcelm_value_t *tcelm_custom0(tcelm_arena_t *arena, uint16_t ctor_id, const char *name) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_custom_t *c = tcelm_arena_alloc(arena, sizeof(tcelm_custom_t));
    c->ctor_id = ctor_id;
    c->ctor_name = name;
    c->arg_count = 0;
    v->tag = TCELM_TAG_CUSTOM;
    v->data.custom = c;
    return v;
}

static tcelm_value_t *tcelm_custom1(tcelm_arena_t *arena, uint16_t ctor_id, const char *name, tcelm_value_t *arg) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_custom_t *c = tcelm_arena_alloc(arena, sizeof(tcelm_custom_t) + sizeof(tcelm_value_t *));
    c->ctor_id = ctor_id;
    c->ctor_name = name;
    c->arg_count = 1;
    c->args[0] = arg;
    v->tag = TCELM_TAG_CUSTOM;
    v->data.custom = c;
    return v;
}

static tcelm_value_t *tcelm_closure(tcelm_arena_t *arena, tcelm_fn_ptr fn, uint8_t arity) {
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_closure_t *cl = tcelm_arena_alloc(arena, sizeof(tcelm_closure_t) + arity * sizeof(tcelm_value_t *));
    cl->fn = fn;
    cl->arity = arity;
    cl->applied = 0;
    v->tag = TCELM_TAG_CLOSURE;
    v->data.closure = cl;
    return v;
}

static tcelm_value_t *tcelm_apply(tcelm_arena_t *arena, tcelm_value_t *fn, tcelm_value_t *arg) {
    tcelm_closure_t *cl = fn->data.closure;

    if (cl->applied + 1 < cl->arity) {
        /* Need more arguments */
        tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
        tcelm_closure_t *new_cl = tcelm_arena_alloc(arena,
            sizeof(tcelm_closure_t) + cl->arity * sizeof(tcelm_value_t *));
        new_cl->fn = cl->fn;
        new_cl->arity = cl->arity;
        new_cl->applied = cl->applied + 1;
        for (uint8_t i = 0; i < cl->applied; i++) {
            new_cl->args[i] = cl->args[i];
        }
        new_cl->args[cl->applied] = arg;
        v->tag = TCELM_TAG_CLOSURE;
        v->data.closure = new_cl;
        return v;
    } else {
        /* All arguments provided - call function */
        tcelm_value_t *args[16];
        for (uint8_t i = 0; i < cl->applied; i++) {
            args[i] = cl->args[i];
        }
        args[cl->applied] = arg;
        return cl->fn(arena, args);
    }
}

/* ========================================================================
 * TEST FUNCTIONS
 * ======================================================================== */

static tcelm_arena_t *arena;

static void test_arena_basic(void) {
    TEST("arena allocation")
    void *p1 = tcelm_arena_alloc(arena, 32);
    void *p2 = tcelm_arena_alloc(arena, 64);
    ASSERT(p1 != NULL)
    ASSERT(p2 != NULL)
    ASSERT(p1 != p2)
    PASS()
}

static void test_int_creation(void) {
    TEST("int creation")
    tcelm_value_t *v = tcelm_int(arena, 42);
    ASSERT(v != NULL)
    ASSERT(v->tag == TCELM_TAG_INT)
    ASSERT_EQ(TCELM_AS_INT(v), 42)
    PASS()
}

static void test_int_negative(void) {
    TEST("int negative")
    tcelm_value_t *v = tcelm_int(arena, -123);
    ASSERT_EQ(TCELM_AS_INT(v), -123)
    PASS()
}

static void test_int_large(void) {
    TEST("int large")
    tcelm_value_t *v = tcelm_int(arena, 1000000000);
    ASSERT_EQ(TCELM_AS_INT(v), 1000000000)
    PASS()
}

static void test_bool_true(void) {
    TEST("bool true")
    tcelm_value_t *v = tcelm_bool(arena, true);
    ASSERT(v->tag == TCELM_TAG_BOOL)
    ASSERT(TCELM_AS_BOOL(v) == true)
    PASS()
}

static void test_bool_false(void) {
    TEST("bool false")
    tcelm_value_t *v = tcelm_bool(arena, false);
    ASSERT(TCELM_AS_BOOL(v) == false)
    PASS()
}

static void test_char_ascii(void) {
    TEST("char ascii")
    tcelm_value_t *v = tcelm_char(arena, 'A');
    ASSERT(v->tag == TCELM_TAG_CHAR)
    ASSERT_EQ(TCELM_AS_CHAR(v), 'A')
    PASS()
}

static void test_string_empty(void) {
    TEST("string empty")
    tcelm_value_t *v = tcelm_string(arena, "");
    ASSERT(v->tag == TCELM_TAG_STRING)
    ASSERT_EQ(TCELM_AS_STRING(v)->length, 0)
    PASS()
}

static void test_string_hello(void) {
    TEST("string hello")
    tcelm_value_t *v = tcelm_string(arena, "hello");
    ASSERT_EQ(TCELM_AS_STRING(v)->length, 5)
    ASSERT_STR_EQ(TCELM_AS_STRING(v)->data, "hello")
    PASS()
}

static void test_unit(void) {
    TEST("unit")
    tcelm_value_t *v = tcelm_unit(arena);
    ASSERT(v->tag == TCELM_TAG_UNIT)
    ASSERT(v == TCELM_UNIT)
    PASS()
}

static void test_list_nil(void) {
    TEST("list nil")
    tcelm_value_t *v = tcelm_nil(arena);
    ASSERT(v->tag == TCELM_TAG_LIST)
    ASSERT(tcelm_is_nil(v))
    PASS()
}

static void test_list_single(void) {
    TEST("list single")
    tcelm_value_t *list = tcelm_cons(arena, tcelm_int(arena, 1), tcelm_nil(arena));
    ASSERT(!tcelm_is_nil(list))
    ASSERT_EQ(TCELM_AS_INT(tcelm_list_head(list)), 1)
    PASS()
}

static void test_list_multi(void) {
    TEST("list multi")
    tcelm_value_t *list = tcelm_nil(arena);
    list = tcelm_cons(arena, tcelm_int(arena, 3), list);
    list = tcelm_cons(arena, tcelm_int(arena, 2), list);
    list = tcelm_cons(arena, tcelm_int(arena, 1), list);
    ASSERT_EQ(TCELM_AS_INT(tcelm_list_head(list)), 1)
    PASS()
}

static void test_tuple2(void) {
    TEST("tuple2")
    tcelm_value_t *t = tcelm_tuple2(arena, tcelm_int(arena, 10), tcelm_int(arena, 20));
    ASSERT(t->tag == TCELM_TAG_TUPLE2)
    ASSERT_EQ(TCELM_AS_INT(t->data.tuple2->first), 10)
    ASSERT_EQ(TCELM_AS_INT(t->data.tuple2->second), 20)
    PASS()
}

static void test_tuple3(void) {
    TEST("tuple3")
    tcelm_value_t *t = tcelm_tuple3(arena,
        tcelm_int(arena, 1),
        tcelm_int(arena, 2),
        tcelm_int(arena, 3));
    ASSERT(t->tag == TCELM_TAG_TUPLE3)
    ASSERT_EQ(TCELM_AS_INT(t->data.tuple3->first), 1)
    ASSERT_EQ(TCELM_AS_INT(t->data.tuple3->second), 2)
    ASSERT_EQ(TCELM_AS_INT(t->data.tuple3->third), 3)
    PASS()
}

static void test_record(void) {
    TEST("record")
    tcelm_value_t *r = tcelm_record2(arena,
        "x", tcelm_int(arena, 100),
        "y", tcelm_int(arena, 200));
    ASSERT(r->tag == TCELM_TAG_RECORD)
    tcelm_value_t *x = tcelm_record_get(r, "x");
    tcelm_value_t *y = tcelm_record_get(r, "y");
    ASSERT_EQ(TCELM_AS_INT(x), 100)
    ASSERT_EQ(TCELM_AS_INT(y), 200)
    PASS()
}

static void test_custom_nothing(void) {
    TEST("Maybe Nothing")
    tcelm_value_t *v = tcelm_custom0(arena, TCELM_CTOR_NOTHING, "Nothing");
    ASSERT(v->tag == TCELM_TAG_CUSTOM)
    ASSERT_EQ(v->data.custom->ctor_id, TCELM_CTOR_NOTHING)
    PASS()
}

static void test_custom_just(void) {
    TEST("Maybe Just")
    tcelm_value_t *v = tcelm_custom1(arena, TCELM_CTOR_JUST, "Just", tcelm_int(arena, 42));
    ASSERT_EQ(v->data.custom->ctor_id, TCELM_CTOR_JUST)
    ASSERT_EQ(TCELM_AS_INT(v->data.custom->args[0]), 42)
    PASS()
}

/* Test function: add two ints */
static tcelm_value_t *fn_add(tcelm_arena_t *a, tcelm_value_t **args) {
    return tcelm_int(a, TCELM_AS_INT(args[0]) + TCELM_AS_INT(args[1]));
}

static void test_closure_immediate(void) {
    TEST("closure immediate")
    tcelm_value_t *add = tcelm_closure(arena, fn_add, 2);
    tcelm_value_t *result = tcelm_apply(arena,
        tcelm_apply(arena, add, tcelm_int(arena, 3)),
        tcelm_int(arena, 4));
    ASSERT_EQ(TCELM_AS_INT(result), 7)
    PASS()
}

static void test_closure_partial(void) {
    TEST("closure partial")
    tcelm_value_t *add = tcelm_closure(arena, fn_add, 2);
    tcelm_value_t *add3 = tcelm_apply(arena, add, tcelm_int(arena, 3));
    ASSERT(add3->tag == TCELM_TAG_CLOSURE)
    tcelm_value_t *result = tcelm_apply(arena, add3, tcelm_int(arena, 10));
    ASSERT_EQ(TCELM_AS_INT(result), 13)
    PASS()
}

/* Test function: multiply three ints */
static tcelm_value_t *fn_mul3(tcelm_arena_t *a, tcelm_value_t **args) {
    return tcelm_int(a, TCELM_AS_INT(args[0]) * TCELM_AS_INT(args[1]) * TCELM_AS_INT(args[2]));
}

static void test_closure_arity3(void) {
    TEST("closure arity 3")
    tcelm_value_t *mul = tcelm_closure(arena, fn_mul3, 3);
    tcelm_value_t *mul2 = tcelm_apply(arena, mul, tcelm_int(arena, 2));
    tcelm_value_t *mul2_3 = tcelm_apply(arena, mul2, tcelm_int(arena, 3));
    tcelm_value_t *result = tcelm_apply(arena, mul2_3, tcelm_int(arena, 4));
    ASSERT_EQ(TCELM_AS_INT(result), 24)
    PASS()
}

static void test_mixed_list(void) {
    TEST("mixed types list")
    /* List of tuples */
    tcelm_value_t *list = tcelm_nil(arena);
    list = tcelm_cons(arena,
        tcelm_tuple2(arena, tcelm_string(arena, "c"), tcelm_int(arena, 3)),
        list);
    list = tcelm_cons(arena,
        tcelm_tuple2(arena, tcelm_string(arena, "b"), tcelm_int(arena, 2)),
        list);
    list = tcelm_cons(arena,
        tcelm_tuple2(arena, tcelm_string(arena, "a"), tcelm_int(arena, 1)),
        list);

    tcelm_value_t *first = tcelm_list_head(list);
    ASSERT(first->tag == TCELM_TAG_TUPLE2)
    ASSERT_STR_EQ(TCELM_AS_STRING(first->data.tuple2->first)->data, "a")
    ASSERT_EQ(TCELM_AS_INT(first->data.tuple2->second), 1)
    PASS()
}

static void test_nested_record(void) {
    TEST("nested record")
    tcelm_value_t *inner = tcelm_record2(arena,
        "a", tcelm_int(arena, 1),
        "b", tcelm_int(arena, 2));
    tcelm_value_t *outer = tcelm_record2(arena,
        "inner", inner,
        "name", tcelm_string(arena, "test"));

    tcelm_value_t *inner_ref = tcelm_record_get(outer, "inner");
    ASSERT(inner_ref->tag == TCELM_TAG_RECORD)
    ASSERT_EQ(TCELM_AS_INT(tcelm_record_get(inner_ref, "a")), 1)
    PASS()
}

/* ========================================================================
 * MAIN
 * ======================================================================== */

void kernel_main(unsigned int magic, void *info) {
    (void)magic;
    (void)info;

    serial_init();

    serial_puts("\n");
    serial_puts("================================\n");
    serial_puts("tcelm Runtime QEMU Tests\n");
    serial_puts("================================\n\n");

    /* Create arena */
    arena = tcelm_arena_create(64 * 1024);
    if (!arena) {
        serial_puts("Failed to create arena!\n");
        outb(0xf4, 0x01);
        return;
    }

    serial_puts("[Arena Tests]\n");
    test_arena_basic();

    serial_puts("\n[Int Tests]\n");
    test_int_creation();
    test_int_negative();
    test_int_large();

    serial_puts("\n[Bool Tests]\n");
    test_bool_true();
    test_bool_false();

    serial_puts("\n[Char Tests]\n");
    test_char_ascii();

    serial_puts("\n[String Tests]\n");
    test_string_empty();
    test_string_hello();

    serial_puts("\n[Unit Tests]\n");
    test_unit();

    serial_puts("\n[List Tests]\n");
    test_list_nil();
    test_list_single();
    test_list_multi();
    test_mixed_list();

    serial_puts("\n[Tuple Tests]\n");
    test_tuple2();
    test_tuple3();

    serial_puts("\n[Record Tests]\n");
    test_record();
    test_nested_record();

    serial_puts("\n[Custom Type Tests]\n");
    test_custom_nothing();
    test_custom_just();

    serial_puts("\n[Closure Tests]\n");
    test_closure_immediate();
    test_closure_partial();
    test_closure_arity3();

    serial_puts("\n================================\n");
    serial_puts("Tests: ");
    serial_putint(test_passed);
    serial_puts("/");
    serial_putint(test_passed + test_failed);
    serial_puts(" passed\n");
    serial_puts("================================\n");

    serial_puts("\nTest complete. Halting.\n");
    outb(0xf4, test_failed == 0 ? 0x00 : 0x01);
}
