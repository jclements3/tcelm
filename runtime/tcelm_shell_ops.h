/*
 * tcelm_shell_ops.h - Shell operations for tcelm
 *
 * Provides Turtle/Shelly-style shell scripting primitives.
 * Works on both RTEMS (with filesystem) and POSIX hosts.
 */

#ifndef TCELM_SHELL_OPS_H
#define TCELM_SHELL_OPS_H

#include "tcelm_types.h"
#include "tcelm_arena.h"
#include <stdbool.h>

/*
 * ============================================================================
 * FILE PATHS
 * ============================================================================
 */

/* Path operations (pure, no I/O) */
tcelm_value_t *tcelm_path_join(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b);
tcelm_value_t *tcelm_path_parent(tcelm_arena_t *arena, tcelm_value_t *path);
tcelm_value_t *tcelm_path_filename(tcelm_arena_t *arena, tcelm_value_t *path);
tcelm_value_t *tcelm_path_extension(tcelm_arena_t *arena, tcelm_value_t *path);
tcelm_value_t *tcelm_path_drop_extension(tcelm_arena_t *arena, tcelm_value_t *path);
tcelm_value_t *tcelm_path_add_extension(tcelm_arena_t *arena, tcelm_value_t *path, tcelm_value_t *ext);

/*
 * ============================================================================
 * DIRECTORY OPERATIONS
 * ============================================================================
 */

/* Get current working directory */
tcelm_value_t *tcelm_shell_pwd(tcelm_arena_t *arena);

/* Change directory */
tcelm_value_t *tcelm_shell_cd(tcelm_arena_t *arena, tcelm_value_t *path);

/* Get home directory */
tcelm_value_t *tcelm_shell_home(tcelm_arena_t *arena);

/* List directory - returns list of paths */
tcelm_value_t *tcelm_shell_ls(tcelm_arena_t *arena, tcelm_value_t *path);

/* List directory tree recursively */
tcelm_value_t *tcelm_shell_lstree(tcelm_arena_t *arena, tcelm_value_t *path);

/* Create directory */
tcelm_value_t *tcelm_shell_mkdir(tcelm_arena_t *arena, tcelm_value_t *path);

/* Create directory tree (mkdir -p) */
tcelm_value_t *tcelm_shell_mkdirtree(tcelm_arena_t *arena, tcelm_value_t *path);

/* Remove empty directory */
tcelm_value_t *tcelm_shell_rmdir(tcelm_arena_t *arena, tcelm_value_t *path);

/* Remove directory tree */
tcelm_value_t *tcelm_shell_rmtree(tcelm_arena_t *arena, tcelm_value_t *path);

/*
 * ============================================================================
 * FILE OPERATIONS
 * ============================================================================
 */

/* Touch file (create or update timestamp) */
tcelm_value_t *tcelm_shell_touch(tcelm_arena_t *arena, tcelm_value_t *path);

/* Copy file */
tcelm_value_t *tcelm_shell_cp(tcelm_arena_t *arena, tcelm_value_t *src, tcelm_value_t *dst);

/* Move/rename file */
tcelm_value_t *tcelm_shell_mv(tcelm_arena_t *arena, tcelm_value_t *src, tcelm_value_t *dst);

/* Remove file */
tcelm_value_t *tcelm_shell_rm(tcelm_arena_t *arena, tcelm_value_t *path);

/* Read file as list of lines */
tcelm_value_t *tcelm_shell_cat(tcelm_arena_t *arena, tcelm_value_t *path);

/* Read entire file as string */
tcelm_value_t *tcelm_shell_read_file(tcelm_arena_t *arena, tcelm_value_t *path);

/* Write string to file */
tcelm_value_t *tcelm_shell_write_file(tcelm_arena_t *arena, tcelm_value_t *path, tcelm_value_t *content);

/* Append string to file */
tcelm_value_t *tcelm_shell_append_file(tcelm_arena_t *arena, tcelm_value_t *path, tcelm_value_t *content);

/* Check if file exists */
tcelm_value_t *tcelm_shell_file_exists(tcelm_arena_t *arena, tcelm_value_t *path);

/* Check if directory exists */
tcelm_value_t *tcelm_shell_dir_exists(tcelm_arena_t *arena, tcelm_value_t *path);

/* Check if path is regular file */
tcelm_value_t *tcelm_shell_is_file(tcelm_arena_t *arena, tcelm_value_t *path);

/* Check if path is directory */
tcelm_value_t *tcelm_shell_is_dir(tcelm_arena_t *arena, tcelm_value_t *path);

/* Get file size */
tcelm_value_t *tcelm_shell_file_size(tcelm_arena_t *arena, tcelm_value_t *path);

/*
 * ============================================================================
 * TEXT PROCESSING
 * ============================================================================
 */

/* Read lines from stdin */
tcelm_value_t *tcelm_shell_stdin(tcelm_arena_t *arena);

/* Write line to stdout */
tcelm_value_t *tcelm_shell_stdout(tcelm_arena_t *arena, tcelm_value_t *line);

/* Write line to stderr */
tcelm_value_t *tcelm_shell_stderr(tcelm_arena_t *arena, tcelm_value_t *line);

/* Echo text to stdout */
tcelm_value_t *tcelm_shell_echo(tcelm_arena_t *arena, tcelm_value_t *text);

/* Printf-style output */
tcelm_value_t *tcelm_shell_printf(tcelm_arena_t *arena, tcelm_value_t *fmt, tcelm_value_t *args);

/*
 * ============================================================================
 * PROCESS EXECUTION
 * ============================================================================
 */

/* Run process with arguments (fire and forget) */
tcelm_value_t *tcelm_shell_proc(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *args);

/* Run shell command string */
tcelm_value_t *tcelm_shell_shell(tcelm_arena_t *arena, tcelm_value_t *cmd);

/* Run process and capture stdout as list of lines */
tcelm_value_t *tcelm_shell_inproc(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *args, tcelm_value_t *stdin_lines);

/* Run shell command and capture stdout */
tcelm_value_t *tcelm_shell_inshell(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *stdin_lines);

/* Run process and get exit code */
tcelm_value_t *tcelm_shell_proc_exit(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *args);

/* Run process and get stdout as string */
tcelm_value_t *tcelm_shell_proc_strict(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *args);

/* Run process and get stdout, stderr, and exit code */
tcelm_value_t *tcelm_shell_proc_strict_with_err(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *args);

/*
 * ============================================================================
 * GLOB PATTERNS
 * ============================================================================
 */

/* Match a path against a glob pattern */
tcelm_value_t *tcelm_shell_glob_match(tcelm_arena_t *arena, tcelm_value_t *pattern, tcelm_value_t *path);

/* Find files matching pattern in directory */
tcelm_value_t *tcelm_shell_find(tcelm_arena_t *arena, tcelm_value_t *pattern, tcelm_value_t *dir);

/*
 * ============================================================================
 * UTILITIES
 * ============================================================================
 */

/* Get current date/time as string */
tcelm_value_t *tcelm_shell_date(tcelm_arena_t *arena);

/* Sleep for milliseconds */
tcelm_value_t *tcelm_shell_sleep(tcelm_arena_t *arena, tcelm_value_t *ms);

/* Get environment variable */
tcelm_value_t *tcelm_shell_env(tcelm_arena_t *arena, tcelm_value_t *name);

/* Set environment variable */
tcelm_value_t *tcelm_shell_export(tcelm_arena_t *arena, tcelm_value_t *name, tcelm_value_t *value);

/* Find executable in PATH */
tcelm_value_t *tcelm_shell_which(tcelm_arena_t *arena, tcelm_value_t *name);

/* Get hostname */
tcelm_value_t *tcelm_shell_hostname(tcelm_arena_t *arena);

#endif /* TCELM_SHELL_OPS_H */
