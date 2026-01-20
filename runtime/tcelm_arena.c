/*
 * tcelm_arena.c - Arena allocator implementation
 */

#include "tcelm_arena.h"
#include <stdlib.h>
#include <string.h>

/* Thread-local current arena */
TCELM_TLS tcelm_arena_t *tcelm_current_arena = NULL;

/* Alignment for all allocations (8 bytes for 64-bit) */
#define ARENA_ALIGNMENT 8
#define ALIGN_UP(x, a) (((x) + ((a) - 1)) & ~((a) - 1))

/* Minimum block size */
#define MIN_BLOCK_SIZE 4096

/* Create a new block */
static tcelm_arena_block_t *arena_block_create(size_t size) {
    size_t total = sizeof(tcelm_arena_block_t) + size;
    tcelm_arena_block_t *block = (tcelm_arena_block_t *)malloc(total);
    if (!block) return NULL;

    block->next = NULL;
    block->size = size;
    block->used = 0;
    return block;
}

int tcelm_arena_init(tcelm_arena_t *arena, size_t block_size) {
    if (!arena) return -1;

    if (block_size < MIN_BLOCK_SIZE) {
        block_size = MIN_BLOCK_SIZE;
    }

    arena->block_size = block_size;
    arena->total_allocated = 0;
    arena->total_used = 0;

    /* Allocate first block */
    arena->first = arena_block_create(block_size);
    if (!arena->first) return -1;

    arena->current = arena->first;
    arena->total_allocated = block_size;

    return 0;
}

void tcelm_arena_destroy(tcelm_arena_t *arena) {
    if (!arena) return;

    tcelm_arena_block_t *block = arena->first;
    while (block) {
        tcelm_arena_block_t *next = block->next;
        free(block);
        block = next;
    }

    arena->first = NULL;
    arena->current = NULL;
    arena->total_allocated = 0;
    arena->total_used = 0;
}

void tcelm_arena_reset(tcelm_arena_t *arena) {
    if (!arena) return;

    /* Reset all blocks to empty but keep them allocated */
    tcelm_arena_block_t *block = arena->first;
    while (block) {
        block->used = 0;
        block = block->next;
    }

    arena->current = arena->first;
    arena->total_used = 0;
}

void *tcelm_arena_alloc(tcelm_arena_t *arena, size_t size) {
    return tcelm_arena_alloc_aligned(arena, size, ARENA_ALIGNMENT);
}

void *tcelm_arena_alloc_aligned(tcelm_arena_t *arena, size_t size, size_t align) {
    if (!arena || size == 0) return NULL;

    /* Align size */
    size = ALIGN_UP(size, align);

    tcelm_arena_block_t *block = arena->current;

    /* Find a block with enough space */
    while (block) {
        size_t aligned_used = ALIGN_UP(block->used, align);
        if (aligned_used + size <= block->size) {
            void *ptr = block->data + aligned_used;
            block->used = aligned_used + size;
            arena->total_used += size;
            return ptr;
        }

        /* Move to next block if available */
        if (block->next) {
            block = block->next;
            arena->current = block;
        } else {
            break;
        }
    }

    /* Need a new block */
    size_t new_block_size = arena->block_size;
    if (size > new_block_size) {
        new_block_size = size;  /* Large allocation gets its own block */
    }

    tcelm_arena_block_t *new_block = arena_block_create(new_block_size);
    if (!new_block) return NULL;

    /* Link new block */
    if (block) {
        block->next = new_block;
    }
    arena->current = new_block;
    arena->total_allocated += new_block_size;

    /* Allocate from new block */
    void *ptr = new_block->data;
    new_block->used = size;
    arena->total_used += size;

    return ptr;
}

void *tcelm_arena_calloc(tcelm_arena_t *arena, size_t count, size_t size) {
    size_t total = count * size;
    void *ptr = tcelm_arena_alloc(arena, total);
    if (ptr) {
        memset(ptr, 0, total);
    }
    return ptr;
}

char *tcelm_arena_strdup(tcelm_arena_t *arena, const char *str) {
    if (!str) return NULL;

    size_t len = strlen(str) + 1;
    char *copy = (char *)tcelm_arena_alloc(arena, len);
    if (copy) {
        memcpy(copy, str, len);
    }
    return copy;
}

void tcelm_arena_get_stats(tcelm_arena_t *arena, tcelm_arena_stats_t *stats) {
    if (!arena || !stats) return;

    stats->total_allocated = arena->total_allocated;
    stats->total_used = arena->total_used;
    stats->block_count = 0;
    stats->largest_alloc = 0;

    tcelm_arena_block_t *block = arena->first;
    while (block) {
        stats->block_count++;
        if (block->used > stats->largest_alloc) {
            stats->largest_alloc = block->used;
        }
        block = block->next;
    }
}
