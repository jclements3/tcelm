/*
 * tcelm_worker.h - Platform.worker runtime for native/TCC builds
 *
 * This provides a simple Platform.worker implementation for self-hosting
 * tcelm. Instead of ports, it uses file I/O:
 *   - Input: stdin or file
 *   - Output: stdout or file
 *
 * For self-hosting the compiler, this allows tcelm to read Elm source
 * and output C code without needing full port infrastructure.
 */

#ifndef TCELM_WORKER_H
#define TCELM_WORKER_H

#include "tcelm_types.h"
#include "tcelm_arena.h"
#include <stdbool.h>

/*
 * Worker program definition
 * Mirrors the structure of Platform.worker in Elm
 */
typedef struct tcelm_worker_program {
    /* Name of the worker */
    const char *name;

    /* Flags type for initialization (passed via command line) */
    /* For simplicity, we pass the entire input as a string flag */

    /* init : Flags -> (Model, Cmd Msg) */
    tcelm_value_t *(*init)(tcelm_arena_t *arena, tcelm_value_t *flags);

    /* update : Msg -> Model -> (Model, Cmd Msg) */
    tcelm_value_t *(*update)(tcelm_arena_t *arena, tcelm_value_t *msg, tcelm_value_t *model);

    /* subscriptions : Model -> Sub Msg (optional for batch mode) */
    tcelm_value_t *(*subscriptions)(tcelm_arena_t *arena, tcelm_value_t *model);
} tcelm_worker_program_t;

/*
 * Worker configuration
 */
typedef struct tcelm_worker_config {
    /* Arena size for worker (default 1MB) */
    size_t arena_size;

    /* Input file (NULL = stdin) */
    const char *input_file;

    /* Output file (NULL = stdout) */
    const char *output_file;

    /* Run in batch mode (process all input, then exit) */
    bool batch_mode;

    /* JSON encode output (default true for compatibility) */
    bool json_output;
} tcelm_worker_config_t;

/* Default configuration */
extern const tcelm_worker_config_t TCELM_WORKER_DEFAULT_CONFIG;

/*
 * Run a Platform.worker program
 *
 * In batch mode (for self-hosting compiler):
 *   1. Read all input (source file content)
 *   2. Call init with flags containing input
 *   3. Process any commands from init
 *   4. Exit
 *
 * Returns: 0 on success, non-zero on error
 */
int tcelm_worker_run(
    const tcelm_worker_program_t *program,
    const tcelm_worker_config_t *config,
    int argc,
    char **argv
);

/*
 * Simplified run: use defaults, just pass the program
 */
int tcelm_worker_run_simple(const tcelm_worker_program_t *program);

/*
 * Command execution for worker
 * These are the Cmd types that the worker can handle
 */
typedef enum tcelm_worker_cmd_type {
    TCELM_WORKER_CMD_NONE = 0,
    TCELM_WORKER_CMD_OUTPUT,     /* Output a string (to stdout or file) */
    TCELM_WORKER_CMD_EXIT,       /* Exit with status code */
    TCELM_WORKER_CMD_BATCH       /* Batch of commands */
} tcelm_worker_cmd_type_t;

/*
 * Create worker commands
 */
tcelm_value_t *tcelm_worker_cmd_none(tcelm_arena_t *arena);
tcelm_value_t *tcelm_worker_cmd_output(tcelm_arena_t *arena, tcelm_value_t *string);
tcelm_value_t *tcelm_worker_cmd_exit(tcelm_arena_t *arena, int code);
tcelm_value_t *tcelm_worker_cmd_batch(tcelm_arena_t *arena, tcelm_value_t *cmds);

/*
 * Read entire file into a string value
 */
tcelm_value_t *tcelm_worker_read_file(tcelm_arena_t *arena, const char *path);

/*
 * Read stdin until EOF into a string value
 */
tcelm_value_t *tcelm_worker_read_stdin(tcelm_arena_t *arena);

/*
 * Write string to file
 */
int tcelm_worker_write_file(const char *path, const char *content);

/*
 * Write string to stdout
 */
int tcelm_worker_write_stdout(const char *content);

/*
 * Helper macro for defining a Platform.worker main
 */
#define TCELM_WORKER_MAIN(program) \
    int main(int argc, char **argv) { \
        return tcelm_worker_run(&(program), NULL, argc, argv); \
    }

#endif /* TCELM_WORKER_H */
