/*
 * Test harness for LambdaTest.c
 */

#include <stdio.h>
#include <string.h>
#include "../runtime/tcelm_arena.h"
#include "../runtime/tcelm_types.h"
#include "../runtime/tcelm_basics.h"

/* Include the generated code */
#include "LambdaTest.c"

int main(void) {
    printf("=== Lambda Test ===\n\n");

    /* Create arena */
    tcelm_arena_t arena;
    tcelm_arena_init(&arena, 64 * 1024);

    /* Test addOne: should return 6 (5 + 1) */
    printf("Testing addOne(5)... ");
    tcelm_value_t *result1 = elm_addOne(&arena, tcelm_int(&arena, 5));
    if (TCELM_AS_INT(result1) == 6) {
        printf("[PASS] got %ld\n", (long)TCELM_AS_INT(result1));
    } else {
        printf("[FAIL] expected 6, got %ld\n", (long)TCELM_AS_INT(result1));
    }

    /* Test pipeline: should return 6 (3 * 2) */
    printf("Testing pipeline(3)... ");
    tcelm_value_t *result2 = elm_pipeline(&arena, tcelm_int(&arena, 3));
    if (TCELM_AS_INT(result2) == 6) {
        printf("[PASS] got %ld\n", (long)TCELM_AS_INT(result2));
    } else {
        printf("[FAIL] expected 6, got %ld\n", (long)TCELM_AS_INT(result2));
    }

    /* Test main: should return 12 (6 + 6) */
    printf("Testing main... ");
    tcelm_value_t *result3 = elm_main(&arena);
    if (TCELM_AS_INT(result3) == 12) {
        printf("[PASS] got %ld\n", (long)TCELM_AS_INT(result3));
    } else {
        printf("[FAIL] expected 12, got %ld\n", (long)TCELM_AS_INT(result3));
    }

    printf("\n=== All tests completed ===\n");

    tcelm_arena_destroy(&arena);
    return 0;
}
