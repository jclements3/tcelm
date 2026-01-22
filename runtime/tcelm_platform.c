/*
 * tcelm_platform.c - Platform module stubs for Elm standard library
 *
 * Provides Cmd.none, Sub.none, and Platform.worker support.
 */

#include "tcelm_platform.h"
#include <string.h>

/* Global singleton values */
tcelm_value_t *elm_Cmd_none = NULL;
tcelm_value_t *elm_Sub_none = NULL;

/* Static storage for singletons (for arenaless initialization) */
static tcelm_value_t cmd_none_val;
static tcelm_value_t sub_none_val;
static int platform_initialized = 0;

/*
 * Initialize Platform module globals
 */
void tcelm_platform_init(tcelm_arena_t *arena) {
    if (platform_initialized) return;

    /* Create Cmd.none as a custom type with tag 0 */
    if (arena) {
        elm_Cmd_none = tcelm_custom(arena, 0, "Cmd.none", 0);
        elm_Sub_none = tcelm_custom(arena, 0, "Sub.none", 0);
    } else {
        /* Arena-less initialization using static storage */
        memset(&cmd_none_val, 0, sizeof(cmd_none_val));
        cmd_none_val.tag = TCELM_TAG_CUSTOM;
        elm_Cmd_none = &cmd_none_val;

        memset(&sub_none_val, 0, sizeof(sub_none_val));
        sub_none_val.tag = TCELM_TAG_CUSTOM;
        elm_Sub_none = &sub_none_val;
    }

    platform_initialized = 1;
}

/*
 * Platform.worker - Create a headless worker program
 *
 * The config record contains init, update, and subscriptions functions.
 * This just returns the config - actual execution happens via tcelm_worker_run().
 */
tcelm_value_t *elm_Platform_worker(tcelm_arena_t *arena, tcelm_value_t *config) {
    /* Ensure platform is initialized */
    if (!platform_initialized) {
        tcelm_platform_init(arena);
    }

    /* Return the config record - the worker runtime will use it */
    return config;
}

/*
 * Closure-compatible implementation of Platform.worker
 */
tcelm_value_t *elm_Platform_worker_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return elm_Platform_worker(arena, args[0]);
}
