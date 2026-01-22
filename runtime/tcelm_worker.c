/*
 * tcelm_worker.c - Platform.worker runtime for native/TCC builds
 *
 * Simple implementation for self-hosting tcelm compiler.
 */

#include "tcelm_worker.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Default configuration */
const tcelm_worker_config_t TCELM_WORKER_DEFAULT_CONFIG = {
    .arena_size = 1024 * 1024,  /* 1MB */
    .input_file = NULL,         /* stdin */
    .output_file = NULL,        /* stdout */
    .batch_mode = true,
    .json_output = false
};

/*
 * Command type tag values
 */
#define CMD_NONE_TAG    0
#define CMD_OUTPUT_TAG  1
#define CMD_EXIT_TAG    2
#define CMD_BATCH_TAG   3

/*
 * Read entire file into memory
 */
static char *read_file_contents(const char *path, size_t *size_out) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    if (size < 0) {
        fclose(f);
        return NULL;
    }

    char *buf = malloc((size_t)size + 1);
    if (!buf) {
        fclose(f);
        return NULL;
    }

    size_t read = fread(buf, 1, (size_t)size, f);
    fclose(f);

    buf[read] = '\0';
    if (size_out) *size_out = read;

    return buf;
}

/*
 * Read stdin until EOF
 */
static char *read_stdin_contents(size_t *size_out) {
    size_t capacity = 4096;
    size_t size = 0;
    char *buf = malloc(capacity);
    if (!buf) return NULL;

    int c;
    while ((c = getchar()) != EOF) {
        if (size + 1 >= capacity) {
            capacity *= 2;
            char *newbuf = realloc(buf, capacity);
            if (!newbuf) {
                free(buf);
                return NULL;
            }
            buf = newbuf;
        }
        buf[size++] = (char)c;
    }

    buf[size] = '\0';
    if (size_out) *size_out = size;

    return buf;
}

/*
 * Read file into tcelm string value
 */
tcelm_value_t *tcelm_worker_read_file(tcelm_arena_t *arena, const char *path) {
    size_t size;
    char *contents = read_file_contents(path, &size);
    if (!contents) return NULL;

    tcelm_value_t *str = tcelm_string_len(arena, contents, size);
    free(contents);
    return str;
}

/*
 * Read stdin into tcelm string value
 */
tcelm_value_t *tcelm_worker_read_stdin(tcelm_arena_t *arena) {
    size_t size;
    char *contents = read_stdin_contents(&size);
    if (!contents) return NULL;

    tcelm_value_t *str = tcelm_string_len(arena, contents, size);
    free(contents);
    return str;
}

/*
 * Write to file
 */
int tcelm_worker_write_file(const char *path, const char *content) {
    FILE *f = fopen(path, "wb");
    if (!f) return -1;

    size_t len = strlen(content);
    size_t written = fwrite(content, 1, len, f);
    fclose(f);

    return (written == len) ? 0 : -1;
}

/*
 * Write to stdout
 */
int tcelm_worker_write_stdout(const char *content) {
    return fputs(content, stdout) >= 0 ? 0 : -1;
}

/*
 * Create Cmd.none
 */
tcelm_value_t *tcelm_worker_cmd_none(tcelm_arena_t *arena) {
    return tcelm_custom(arena, CMD_NONE_TAG, "CmdNone", 0);
}

/*
 * Create output command
 */
tcelm_value_t *tcelm_worker_cmd_output(tcelm_arena_t *arena, tcelm_value_t *string) {
    return tcelm_custom(arena, CMD_OUTPUT_TAG, "CmdOutput", 1, string);
}

/*
 * Create exit command
 */
tcelm_value_t *tcelm_worker_cmd_exit(tcelm_arena_t *arena, int code) {
    return tcelm_custom(arena, CMD_EXIT_TAG, "CmdExit", 1, tcelm_int(arena, code));
}

/*
 * Create batch command
 */
tcelm_value_t *tcelm_worker_cmd_batch(tcelm_arena_t *arena, tcelm_value_t *cmds) {
    return tcelm_custom(arena, CMD_BATCH_TAG, "CmdBatch", 1, cmds);
}

/*
 * Execute a command, returns exit code or -1 to continue
 */
static int execute_cmd(
    tcelm_value_t *cmd,
    const tcelm_worker_config_t *config
) {
    if (!cmd || !TCELM_IS_CUSTOM(cmd)) {
        return -1;  /* Continue */
    }

    int tag = tcelm_custom_ctor(cmd);

    switch (tag) {
        case CMD_NONE_TAG:
            return -1;  /* Continue */

        case CMD_OUTPUT_TAG: {
            tcelm_value_t *str = tcelm_custom_arg(cmd, 0);
            if (str && TCELM_IS_STRING(str)) {
                const char *content = TCELM_AS_STRING(str)->data;
                if (config->output_file) {
                    tcelm_worker_write_file(config->output_file, content);
                } else {
                    tcelm_worker_write_stdout(content);
                }
            }
            return -1;  /* Continue */
        }

        case CMD_EXIT_TAG: {
            tcelm_value_t *code = tcelm_custom_arg(cmd, 0);
            if (code && TCELM_IS_INT(code)) {
                return (int)TCELM_AS_INT(code);
            }
            return 0;
        }

        case CMD_BATCH_TAG: {
            tcelm_value_t *cmds = tcelm_custom_arg(cmd, 0);
            /* Execute each command in the list */
            while (cmds && cmds != TCELM_NIL) {
                tcelm_value_t *head = tcelm_list_head(cmds);
                int result = execute_cmd(head, config);
                if (result >= 0) {
                    return result;  /* Exit requested */
                }
                cmds = tcelm_list_tail(cmds);
            }
            return -1;  /* Continue */
        }

        default:
            return -1;  /* Unknown command, continue */
    }
}

/*
 * Extract model and cmd from (model, cmd) tuple returned by init/update
 */
static void extract_model_cmd(
    tcelm_value_t *result,
    tcelm_value_t **model_out,
    tcelm_value_t **cmd_out
) {
    if (!result || !TCELM_IS_TUPLE2(result)) {
        *model_out = NULL;
        *cmd_out = NULL;
        return;
    }

    *model_out = tcelm_tuple2_first(result);
    *cmd_out = tcelm_tuple2_second(result);
}

/*
 * Run the worker program
 */
int tcelm_worker_run(
    const tcelm_worker_program_t *program,
    const tcelm_worker_config_t *config,
    int argc,
    char **argv
) {
    if (!program || !program->init) {
        fprintf(stderr, "Error: Invalid worker program\n");
        return 1;
    }

    /* Use default config if none provided */
    tcelm_worker_config_t cfg;
    if (config) {
        cfg = *config;
    } else {
        cfg = TCELM_WORKER_DEFAULT_CONFIG;
    }

    /* Parse command line for input/output files */
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            cfg.output_file = argv[++i];
        } else if (argv[i][0] != '-') {
            cfg.input_file = argv[i];
        }
    }

    /* Create arena */
    tcelm_arena_t *arena = tcelm_arena_create(cfg.arena_size);
    if (!arena) {
        fprintf(stderr, "Error: Failed to create arena\n");
        return 1;
    }

    /* Set as current arena for list operations */
    extern tcelm_arena_t *tcelm_current_arena;
    tcelm_current_arena = arena;

    /* Read input */
    tcelm_value_t *input;
    if (cfg.input_file) {
        input = tcelm_worker_read_file(arena, cfg.input_file);
        if (!input) {
            fprintf(stderr, "Error: Failed to read file: %s\n", cfg.input_file);
            tcelm_arena_destroy(arena);
            return 1;
        }
    } else {
        input = tcelm_worker_read_stdin(arena);
        if (!input) {
            fprintf(stderr, "Error: Failed to read stdin\n");
            tcelm_arena_destroy(arena);
            return 1;
        }
    }

    /* Create flags record containing input */
    tcelm_value_t *flags = tcelm_record(arena, 1, "source", input);

    /* Call init */
    tcelm_value_t *result = program->init(arena, flags);
    if (!result) {
        fprintf(stderr, "Error: init returned NULL\n");
        tcelm_arena_destroy(arena);
        return 1;
    }

    tcelm_value_t *model;
    tcelm_value_t *cmd;
    extract_model_cmd(result, &model, &cmd);

    /* Execute initial command */
    int exit_code = execute_cmd(cmd, &cfg);

    /* In batch mode, we're done after init */
    if (cfg.batch_mode || exit_code >= 0) {
        tcelm_arena_destroy(arena);
        return (exit_code >= 0) ? exit_code : 0;
    }

    /* Interactive mode: not implemented yet */
    /* Would need message loop with subscriptions */

    tcelm_arena_destroy(arena);
    return 0;
}

/*
 * Simplified run
 */
int tcelm_worker_run_simple(const tcelm_worker_program_t *program) {
    return tcelm_worker_run(program, NULL, 0, NULL);
}
