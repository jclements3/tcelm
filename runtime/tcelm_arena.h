/*
 * tcelm_arena.h - Arena allocator for tcelm runtime
 *
 * Arena allocation provides deterministic memory management suitable
 * for hard real-time systems like RTEMS. Memory is allocated from
 * fixed-size arenas and freed in bulk.
 *
 * Usage pattern for 240 Hz frame loop:
 *   tcelm_arena_t frame_arena;
 *   tcelm_arena_init(&frame_arena, 1024 * 1024);  // 1 MB per frame
 *
 *   while (running) {
 *       // All allocations go to frame_arena
 *       tcelm_arena_reset(&frame_arena);
 *       result = elm_main(&frame_arena, input);
 *       output_result(result);
 *       // No individual frees needed - reset clears everything
 *   }
 *
 *   tcelm_arena_destroy(&frame_arena);
 */

#ifndef TCELM_ARENA_H
#define TCELM_ARENA_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

/* Arena block - linked list of memory chunks */
typedef struct tcelm_arena_block {
    struct tcelm_arena_block *next;
    size_t size;
    size_t used;
    uint8_t data[];  /* Flexible array member */
} tcelm_arena_block_t;

/* Arena allocator */
typedef struct tcelm_arena {
    tcelm_arena_block_t *current;    /* Current block for allocations */
    tcelm_arena_block_t *first;      /* First block (for reset) */
    size_t block_size;               /* Default block size */
    size_t total_allocated;          /* Total bytes allocated */
    size_t total_used;               /* Total bytes used */
} tcelm_arena_t;

/* Initialize arena with given block size */
int tcelm_arena_init(tcelm_arena_t *arena, size_t block_size);

/* Create and initialize a new arena (heap-allocated) */
tcelm_arena_t *tcelm_arena_create(size_t block_size);

/* Destroy arena and free all memory (for stack-allocated arenas) */
void tcelm_arena_destroy(tcelm_arena_t *arena);

/* Free a heap-allocated arena (created with tcelm_arena_create) */
void tcelm_arena_free(tcelm_arena_t *arena);

/* Reset arena for reuse (keeps allocated blocks) */
void tcelm_arena_reset(tcelm_arena_t *arena);

/* Allocate memory from arena (never fails if arena has space) */
void *tcelm_arena_alloc(tcelm_arena_t *arena, size_t size);

/* Allocate aligned memory from arena */
void *tcelm_arena_alloc_aligned(tcelm_arena_t *arena, size_t size, size_t align);

/* Allocate and zero memory */
void *tcelm_arena_calloc(tcelm_arena_t *arena, size_t count, size_t size);

/* Duplicate a string into arena */
char *tcelm_arena_strdup(tcelm_arena_t *arena, const char *str);

/* Get arena statistics */
typedef struct tcelm_arena_stats {
    size_t total_allocated;  /* Total memory from system */
    size_t total_used;       /* Memory used by allocations */
    size_t block_count;      /* Number of blocks */
    size_t largest_alloc;    /* Largest single allocation */
} tcelm_arena_stats_t;

void tcelm_arena_get_stats(tcelm_arena_t *arena, tcelm_arena_stats_t *stats);

/* Thread-local arena for current computation */
#ifdef TCELM_THREAD_LOCAL
    #define TCELM_TLS _Thread_local
#else
    #define TCELM_TLS
#endif

extern TCELM_TLS tcelm_arena_t *tcelm_current_arena;

/* Convenience macro for allocation */
#define TCELM_ALLOC(type) \
    ((type *)tcelm_arena_alloc(tcelm_current_arena, sizeof(type)))

#define TCELM_ALLOC_ARRAY(type, count) \
    ((type *)tcelm_arena_alloc(tcelm_current_arena, sizeof(type) * (count)))

#endif /* TCELM_ARENA_H */
