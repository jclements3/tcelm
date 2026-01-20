/*
 * tcelm_shell.h - RTEMS Shell integration for tcelm
 *
 * Provides interactive shell commands for:
 * - Evaluating Elm expressions
 * - Loading Elm modules
 * - Managing Elm tasks
 * - Runtime inspection and debugging
 */

#ifndef TCELM_SHELL_H
#define TCELM_SHELL_H

#include "tcelm_rtems.h"

/*
 * Initialize tcelm shell commands
 * Call after rtems_shell_init() to register tcelm commands
 */
int tcelm_shell_init(void);

/*
 * Shell command handlers (can also be called directly)
 */

/* elm - Evaluate an Elm expression */
int tcelm_shell_cmd_elm(int argc, char **argv);

/* elm-tasks - List all Elm tasks */
int tcelm_shell_cmd_tasks(int argc, char **argv);

/* elm-stats - Show runtime statistics */
int tcelm_shell_cmd_stats(int argc, char **argv);

/* elm-send <task> <msg> - Send message to task */
int tcelm_shell_cmd_send(int argc, char **argv);

/* elm-spawn <module> - Spawn a new Elm task */
int tcelm_shell_cmd_spawn(int argc, char **argv);

/* elm-kill <task> - Kill an Elm task */
int tcelm_shell_cmd_kill(int argc, char **argv);

/* elm-channels - List all channels */
int tcelm_shell_cmd_channels(int argc, char **argv);

/* elm-arena - Show arena statistics */
int tcelm_shell_cmd_arena(int argc, char **argv);

/* elm-help - Show tcelm help */
int tcelm_shell_cmd_help(int argc, char **argv);

/*
 * Module registry for dynamically spawning tasks
 */
typedef struct tcelm_module_entry {
    const char *name;
    const tcelm_task_def_t *def;
} tcelm_module_entry_t;

/* Register a module for shell spawning */
int tcelm_shell_register_module(const char *name, const tcelm_task_def_t *def);

/* Find a registered module */
const tcelm_task_def_t *tcelm_shell_find_module(const char *name);

#endif /* TCELM_SHELL_H */
