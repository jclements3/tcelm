/*
 * test_worker.c - Test Platform.worker runtime
 *
 * Tests the simple worker implementation used for self-hosting tcelm.
 */

#include "tcelm_types.h"
#include "tcelm_arena.h"
#include "tcelm_worker.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

static int tests_passed = 0;
static int tests_total = 0;

#define TEST(name, cond) do { \
    tests_total++; \
    if (cond) { \
        printf("  %s [PASS]\n", name); \
        tests_passed++; \
    } else { \
        printf("  %s [FAIL]\n", name); \
    } \
} while(0)

/*
 * Simple echo worker for testing
 * Takes input and outputs it back
 */
static tcelm_value_t *echo_init(tcelm_arena_t *arena, tcelm_value_t *flags) {
    /* Extract source from flags */
    tcelm_value_t *source = tcelm_record_get(flags, "source");

    /* Model is just the source */
    tcelm_value_t *model = source;

    /* Command is to output the source and exit */
    tcelm_value_t *cmd = tcelm_worker_cmd_batch(arena,
        tcelm_cons(arena,
            tcelm_worker_cmd_output(arena, source),
            tcelm_cons(arena,
                tcelm_worker_cmd_exit(arena, 0),
                tcelm_nil(arena)
            )
        )
    );

    return tcelm_tuple2(arena, model, cmd);
}

static tcelm_value_t *echo_update(tcelm_arena_t *arena, tcelm_value_t *msg, tcelm_value_t *model) {
    (void)msg;
    return tcelm_tuple2(arena, model, tcelm_worker_cmd_none(arena));
}

static const tcelm_worker_program_t echo_program = {
    .name = "Echo",
    .init = echo_init,
    .update = echo_update,
    .subscriptions = NULL
};

/*
 * Transform worker for testing
 * Takes input and transforms it to uppercase
 */
static char *to_upper(const char *s, size_t len) {
    char *result = malloc(len + 1);
    for (size_t i = 0; i < len; i++) {
        char c = s[i];
        if (c >= 'a' && c <= 'z') {
            result[i] = c - 'a' + 'A';
        } else {
            result[i] = c;
        }
    }
    result[len] = '\0';
    return result;
}

static tcelm_value_t *transform_init(tcelm_arena_t *arena, tcelm_value_t *flags) {
    tcelm_value_t *source = tcelm_record_get(flags, "source");

    /* Transform to uppercase */
    const char *input = TCELM_AS_STRING(source)->data;
    size_t len = strlen(input);
    char *upper = to_upper(input, len);
    tcelm_value_t *output = tcelm_string_len(arena, upper, len);
    free(upper);

    /* Model is the output */
    tcelm_value_t *model = output;

    /* Command is to output and exit */
    tcelm_value_t *cmd = tcelm_worker_cmd_batch(arena,
        tcelm_cons(arena,
            tcelm_worker_cmd_output(arena, output),
            tcelm_cons(arena,
                tcelm_worker_cmd_exit(arena, 0),
                tcelm_nil(arena)
            )
        )
    );

    return tcelm_tuple2(arena, model, cmd);
}

static const tcelm_worker_program_t transform_program = {
    .name = "Transform",
    .init = transform_init,
    .update = echo_update,
    .subscriptions = NULL
};

/*
 * Test file I/O helpers
 */
void test_file_io(void) {
    printf("\n=== File I/O Tests ===\n");

    tcelm_arena_t *arena = tcelm_arena_create(64 * 1024);

    /* Write test file */
    const char *test_content = "Hello, tcelm worker!";
    int result = tcelm_worker_write_file("/tmp/test_worker.txt", test_content);
    TEST("write_file returns 0", result == 0);

    /* Read test file */
    tcelm_value_t *read_val = tcelm_worker_read_file(arena, "/tmp/test_worker.txt");
    TEST("read_file returns non-NULL", read_val != NULL);
    TEST("read_file returns string", TCELM_IS_STRING(read_val));

    const char *read_str = TCELM_AS_STRING(read_val)->data;
    TEST("read_file content matches", strcmp(read_str, test_content) == 0);

    /* Read non-existent file */
    tcelm_value_t *bad_read = tcelm_worker_read_file(arena, "/tmp/nonexistent_file_xyz.txt");
    TEST("read nonexistent file returns NULL", bad_read == NULL);

    tcelm_arena_destroy(arena);
}

/*
 * Test command creation
 */
void test_commands(void) {
    printf("\n=== Command Tests ===\n");

    tcelm_arena_t *arena = tcelm_arena_create(64 * 1024);

    /* Test Cmd.none */
    tcelm_value_t *cmd_none = tcelm_worker_cmd_none(arena);
    TEST("cmd_none is custom", TCELM_IS_CUSTOM(cmd_none));
    TEST("cmd_none has tag 0", tcelm_custom_ctor(cmd_none) == 0);

    /* Test output command */
    tcelm_value_t *str = tcelm_string(arena, "output");
    tcelm_value_t *cmd_output = tcelm_worker_cmd_output(arena, str);
    TEST("cmd_output is custom", TCELM_IS_CUSTOM(cmd_output));
    TEST("cmd_output has tag 1", tcelm_custom_ctor(cmd_output) == 1);

    /* Test exit command */
    tcelm_value_t *cmd_exit = tcelm_worker_cmd_exit(arena, 42);
    TEST("cmd_exit is custom", TCELM_IS_CUSTOM(cmd_exit));
    TEST("cmd_exit has tag 2", tcelm_custom_ctor(cmd_exit) == 2);

    /* Test batch command */
    tcelm_value_t *list = tcelm_cons(arena, cmd_none, tcelm_nil(arena));
    tcelm_value_t *cmd_batch = tcelm_worker_cmd_batch(arena, list);
    TEST("cmd_batch is custom", TCELM_IS_CUSTOM(cmd_batch));
    TEST("cmd_batch has tag 3", tcelm_custom_ctor(cmd_batch) == 3);

    tcelm_arena_destroy(arena);
}

/*
 * Test worker program execution
 */
void test_worker_run(void) {
    printf("\n=== Worker Run Tests ===\n");

    /* Create test input file */
    const char *input = "hello world";
    tcelm_worker_write_file("/tmp/test_input.txt", input);

    /* Test echo worker */
    tcelm_worker_config_t config = TCELM_WORKER_DEFAULT_CONFIG;
    config.input_file = "/tmp/test_input.txt";
    config.output_file = "/tmp/test_output.txt";

    char *argv[] = { "test", "/tmp/test_input.txt", "-o", "/tmp/test_output.txt" };
    int result = tcelm_worker_run(&echo_program, &config, 4, argv);
    TEST("echo worker returns 0", result == 0);

    /* Read output */
    tcelm_arena_t *arena = tcelm_arena_create(64 * 1024);
    tcelm_value_t *output = tcelm_worker_read_file(arena, "/tmp/test_output.txt");
    TEST("output file created", output != NULL);
    TEST("output matches input", strcmp(TCELM_AS_STRING(output)->data, input) == 0);

    /* Test transform worker */
    result = tcelm_worker_run(&transform_program, &config, 4, argv);
    TEST("transform worker returns 0", result == 0);

    output = tcelm_worker_read_file(arena, "/tmp/test_output.txt");
    TEST("transformed output is uppercase", strcmp(TCELM_AS_STRING(output)->data, "HELLO WORLD") == 0);

    tcelm_arena_destroy(arena);
}

int main(void) {
    printf("=== Platform.worker Tests ===\n");

    test_file_io();
    test_commands();
    test_worker_run();

    printf("\n=== Summary ===\n");
    printf("Tests: %d/%d passed\n", tests_passed, tests_total);

    return (tests_passed == tests_total) ? 0 : 1;
}
