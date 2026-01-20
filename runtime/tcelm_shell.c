/*
 * tcelm_shell.c - RTEMS Shell integration for tcelm
 */

#include "tcelm_shell.h"
#include "tcelm_basics.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef __rtems__
#include <rtems/shell.h>
#endif

/*
 * Module registry
 */
#define MAX_MODULES 64

static struct {
    tcelm_module_entry_t entries[MAX_MODULES];
    size_t count;
} module_registry;

/*
 * Task name registry (for shell commands)
 */
static struct {
    const char *names[32];
    tcelm_task_t *tasks[32];
    size_t count;
} task_registry;

/*
 * REPL arena for expression evaluation
 */
static tcelm_arena_t repl_arena;
static bool repl_initialized = false;

/*
 * ============================================================================
 * MODULE REGISTRY
 * ============================================================================
 */

int tcelm_shell_register_module(const char *name, const tcelm_task_def_t *def) {
    if (module_registry.count >= MAX_MODULES) return -1;

    module_registry.entries[module_registry.count].name = name;
    module_registry.entries[module_registry.count].def = def;
    module_registry.count++;

    return 0;
}

const tcelm_task_def_t *tcelm_shell_find_module(const char *name) {
    for (size_t i = 0; i < module_registry.count; i++) {
        if (strcmp(module_registry.entries[i].name, name) == 0) {
            return module_registry.entries[i].def;
        }
    }
    return NULL;
}

/*
 * ============================================================================
 * SHELL COMMANDS
 * ============================================================================
 */

int tcelm_shell_cmd_help(int argc, char **argv) {
    (void)argc;
    (void)argv;

    printf("\n");
    printf("  _            _           \n");
    printf(" | |_ ___ ___ | |_____ ___ \n");
    printf(" |  _/ __/ _ \\| |  _ \\/ -_)\n");
    printf(" |_| \\__\\___/|_|_| |_\\___|\n");
    printf("\n");
    printf(" Elm for RTEMS - Real-time functional programming\n");
    printf("\n");
    printf("Commands:\n");
    printf("  elm <expr>          Evaluate an Elm expression\n");
    printf("  elm-tasks           List all running Elm tasks\n");
    printf("  elm-stats           Show runtime statistics\n");
    printf("  elm-send <t> <msg>  Send message to task\n");
    printf("  elm-spawn <module>  Spawn a new Elm task from module\n");
    printf("  elm-kill <task>     Kill an Elm task\n");
    printf("  elm-channels        List all pub/sub channels\n");
    printf("  elm-arena           Show arena memory statistics\n");
    printf("  elm-modules         List registered modules\n");
    printf("  elm-help            Show this help\n");
    printf("\n");
    printf("Examples:\n");
    printf("  elm 2 + 2\n");
    printf("  elm List.map (\\x -> x * 2) [1, 2, 3]\n");
    printf("  elm-spawn Counter\n");
    printf("  elm-send counter (Increment 5)\n");
    printf("\n");

    return 0;
}

int tcelm_shell_cmd_elm(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: elm <expression>\n");
        printf("Example: elm 2 + 2\n");
        return 1;
    }

    /* Initialize REPL arena if needed */
    if (!repl_initialized) {
        if (tcelm_arena_init(&repl_arena, 64 * 1024) != 0) {
            printf("Error: Failed to initialize REPL arena\n");
            return 1;
        }
        repl_initialized = true;
    }

    /* Reset arena for this evaluation */
    tcelm_arena_reset(&repl_arena);

    /* Concatenate all arguments into expression */
    char expr[1024];
    expr[0] = '\0';
    for (int i = 1; i < argc; i++) {
        if (i > 1) strcat(expr, " ");
        strcat(expr, argv[i]);
    }

    /* For now, parse simple expressions */
    /* TODO: integrate with full Elm parser */

    /* Try to parse as integer */
    char *endptr;
    long val = strtol(expr, &endptr, 10);
    if (*endptr == '\0') {
        tcelm_value_t *result = tcelm_int(&repl_arena, val);
        printf("%ld : Int\n", (long)TCELM_AS_INT(result));
        return 0;
    }

    /* Try to parse as float */
    double fval = strtod(expr, &endptr);
    if (*endptr == '\0') {
        tcelm_value_t *result = tcelm_float(&repl_arena, fval);
        printf("%g : Float\n", TCELM_AS_FLOAT(result));
        return 0;
    }

    /* Try to parse as simple arithmetic: a + b, a - b, etc */
    int a, b;
    char op;
    if (sscanf(expr, "%d %c %d", &a, &op, &b) == 3) {
        tcelm_value_t *va = tcelm_int(&repl_arena, a);
        tcelm_value_t *vb = tcelm_int(&repl_arena, b);
        tcelm_value_t *result = NULL;

        switch (op) {
            case '+':
                result = tcelm_add_int(&repl_arena, va, vb);
                break;
            case '-':
                result = tcelm_sub_int(&repl_arena, va, vb);
                break;
            case '*':
                result = tcelm_mul_int(&repl_arena, va, vb);
                break;
            case '/':
                result = tcelm_div_int(&repl_arena, va, vb);
                break;
            case '%':
                result = tcelm_mod_int(&repl_arena, va, vb);
                break;
            default:
                printf("Unknown operator: %c\n", op);
                return 1;
        }

        if (result) {
            printf("%ld : Int\n", (long)TCELM_AS_INT(result));
            return 0;
        }
    }

    /* Try boolean literals */
    if (strcmp(expr, "True") == 0) {
        printf("True : Bool\n");
        return 0;
    }
    if (strcmp(expr, "False") == 0) {
        printf("False : Bool\n");
        return 0;
    }

    /* String literal */
    if (expr[0] == '"') {
        size_t len = strlen(expr);
        if (len > 1 && expr[len-1] == '"') {
            expr[len-1] = '\0';
            tcelm_value_t *result = tcelm_string(&repl_arena, expr + 1);
            printf("\"%s\" : String\n", TCELM_AS_STRING(result)->data);
            return 0;
        }
    }

    printf("Parse error: Could not evaluate '%s'\n", expr);
    printf("(Full Elm parser integration pending)\n");
    return 1;
}

int tcelm_shell_cmd_tasks(int argc, char **argv) {
    (void)argc;
    (void)argv;

    printf("\nElm Tasks:\n");
    printf("%-20s %-10s %-15s %-10s\n", "Name", "Status", "Messages", "Updates");
    printf("%-20s %-10s %-15s %-10s\n", "----", "------", "--------", "-------");

    if (task_registry.count == 0) {
        printf("(no tasks running)\n");
    }

    for (size_t i = 0; i < task_registry.count; i++) {
        tcelm_task_stats_t stats;
        if (tcelm_task_get_stats(task_registry.tasks[i], &stats) == 0) {
            printf("%-20s %-10s %-15lu %-10lu\n",
                   task_registry.names[i],
                   "running",
                   (unsigned long)stats.messages_processed,
                   (unsigned long)stats.update_calls);
        }
    }

    printf("\n");
    return 0;
}

int tcelm_shell_cmd_stats(int argc, char **argv) {
    (void)argc;
    (void)argv;

    tcelm_rtems_print_stats();
    return 0;
}

int tcelm_shell_cmd_send(int argc, char **argv) {
    if (argc < 3) {
        printf("Usage: elm-send <task-name> <message>\n");
        printf("Example: elm-send counter Increment\n");
        return 1;
    }

    const char *task_name = argv[1];

    /* Find task by name */
    tcelm_task_t *task = NULL;
    for (size_t i = 0; i < task_registry.count; i++) {
        if (strcmp(task_registry.names[i], task_name) == 0) {
            task = task_registry.tasks[i];
            break;
        }
    }

    if (!task) {
        printf("Error: Task '%s' not found\n", task_name);
        return 1;
    }

    /* Parse message - for now just send as string */
    /* TODO: parse Elm values */
    if (!repl_initialized) {
        tcelm_arena_init(&repl_arena, 64 * 1024);
        repl_initialized = true;
    }

    /* Concatenate remaining args as message */
    char msg_str[512];
    msg_str[0] = '\0';
    for (int i = 2; i < argc; i++) {
        if (i > 2) strcat(msg_str, " ");
        strcat(msg_str, argv[i]);
    }

    tcelm_value_t *msg = tcelm_string(&repl_arena, msg_str);
    if (tcelm_task_send(task, msg) == 0) {
        printf("Message sent to '%s'\n", task_name);
        return 0;
    } else {
        printf("Error: Failed to send message\n");
        return 1;
    }
}

int tcelm_shell_cmd_spawn(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: elm-spawn <module-name> [task-name]\n");
        printf("Example: elm-spawn Counter myCounter\n");
        return 1;
    }

    const char *module_name = argv[1];
    const char *task_name = argc > 2 ? argv[2] : module_name;

    const tcelm_task_def_t *def = tcelm_shell_find_module(module_name);
    if (!def) {
        printf("Error: Module '%s' not found\n", module_name);
        printf("Use 'elm-modules' to list available modules\n");
        return 1;
    }

    /* Create modified def with custom name if provided */
    tcelm_task_def_t custom_def = *def;
    custom_def.name = task_name;

    tcelm_task_t *task = tcelm_task_spawn(&custom_def);
    if (!task) {
        printf("Error: Failed to spawn task\n");
        return 1;
    }

    /* Register in task registry */
    if (task_registry.count < 32) {
        task_registry.names[task_registry.count] = task_name;
        task_registry.tasks[task_registry.count] = task;
        task_registry.count++;
    }

    printf("Spawned Elm task '%s' from module '%s'\n", task_name, module_name);
    return 0;
}

int tcelm_shell_cmd_kill(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: elm-kill <task-name>\n");
        return 1;
    }

    const char *task_name = argv[1];

    /* Find and remove from registry */
    for (size_t i = 0; i < task_registry.count; i++) {
        if (strcmp(task_registry.names[i], task_name) == 0) {
            tcelm_task_delete(task_registry.tasks[i]);

            /* Shift remaining entries */
            for (size_t j = i; j < task_registry.count - 1; j++) {
                task_registry.names[j] = task_registry.names[j + 1];
                task_registry.tasks[j] = task_registry.tasks[j + 1];
            }
            task_registry.count--;

            printf("Killed task '%s'\n", task_name);
            return 0;
        }
    }

    printf("Error: Task '%s' not found\n", task_name);
    return 1;
}

int tcelm_shell_cmd_channels(int argc, char **argv) {
    (void)argc;
    (void)argv;

    tcelm_rtems_stats_t stats;
    tcelm_rtems_get_stats(&stats);

    printf("\nPub/Sub Channels: %zu active\n", stats.active_channels);
    /* TODO: list channel names and subscriber counts */
    printf("(detailed channel info not yet implemented)\n\n");

    return 0;
}

int tcelm_shell_cmd_arena(int argc, char **argv) {
    (void)argc;
    (void)argv;

    printf("\nArena Memory Statistics:\n\n");

    if (repl_initialized) {
        tcelm_arena_stats_t stats;
        tcelm_arena_get_stats(&repl_arena, &stats);
        printf("REPL Arena:\n");
        printf("  Allocated: %zu bytes\n", stats.total_allocated);
        printf("  Used:      %zu bytes\n", stats.total_used);
        printf("  Blocks:    %zu\n", stats.block_count);
        printf("\n");
    }

    printf("Per-task arenas:\n");
    for (size_t i = 0; i < task_registry.count; i++) {
        tcelm_task_stats_t stats;
        if (tcelm_task_get_stats(task_registry.tasks[i], &stats) == 0) {
            printf("  %s: peak %zu bytes, %lu resets\n",
                   task_registry.names[i],
                   stats.arena_peak_usage,
                   (unsigned long)stats.arena_resets);
        }
    }

    printf("\n");
    return 0;
}

static int tcelm_shell_cmd_modules(int argc, char **argv) {
    (void)argc;
    (void)argv;

    printf("\nRegistered Elm Modules:\n");
    if (module_registry.count == 0) {
        printf("(no modules registered)\n");
    }

    for (size_t i = 0; i < module_registry.count; i++) {
        printf("  %s\n", module_registry.entries[i].name);
    }

    printf("\nUse 'elm-spawn <module>' to create a task from a module.\n\n");
    return 0;
}

/*
 * ============================================================================
 * SHELL INITIALIZATION
 * ============================================================================
 */

int tcelm_shell_init(void) {
#ifdef __rtems__
    /* Register all tcelm commands */
    rtems_shell_add_cmd("elm", "tcelm",
        "elm <expr> - Evaluate an Elm expression",
        tcelm_shell_cmd_elm);

    rtems_shell_add_cmd("elm-tasks", "tcelm",
        "List all running Elm tasks",
        tcelm_shell_cmd_tasks);

    rtems_shell_add_cmd("elm-stats", "tcelm",
        "Show tcelm runtime statistics",
        tcelm_shell_cmd_stats);

    rtems_shell_add_cmd("elm-send", "tcelm",
        "elm-send <task> <msg> - Send message to Elm task",
        tcelm_shell_cmd_send);

    rtems_shell_add_cmd("elm-spawn", "tcelm",
        "elm-spawn <module> [name] - Spawn Elm task from module",
        tcelm_shell_cmd_spawn);

    rtems_shell_add_cmd("elm-kill", "tcelm",
        "elm-kill <task> - Kill an Elm task",
        tcelm_shell_cmd_kill);

    rtems_shell_add_cmd("elm-channels", "tcelm",
        "List all pub/sub channels",
        tcelm_shell_cmd_channels);

    rtems_shell_add_cmd("elm-arena", "tcelm",
        "Show arena memory statistics",
        tcelm_shell_cmd_arena);

    rtems_shell_add_cmd("elm-modules", "tcelm",
        "List registered Elm modules",
        tcelm_shell_cmd_modules);

    rtems_shell_add_cmd("elm-help", "tcelm",
        "Show tcelm help",
        tcelm_shell_cmd_help);

    printf("tcelm shell commands registered. Type 'elm-help' for usage.\n");
#else
    printf("RTEMS shell not available (host build)\n");
#endif

    return 0;
}
