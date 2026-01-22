/*
 * tcelm_platform.h - Platform module stubs for Elm standard library
 *
 * Provides the core Platform.worker support for generated Elm code.
 */

#ifndef TCELM_PLATFORM_H
#define TCELM_PLATFORM_H

#include "tcelm_types.h"
#include "tcelm_arena.h"

/*
 * Cmd.none - The "do nothing" command
 * This is a global singleton value
 */
extern tcelm_value_t *elm_Cmd_none;

/*
 * Sub.none - The "no subscriptions" subscription
 * This is a global singleton value
 */
extern tcelm_value_t *elm_Sub_none;

/*
 * Platform.worker - Create a headless worker program
 *
 * This is a placeholder that returns the config record.
 * The actual worker execution happens via tcelm_worker_run().
 */
tcelm_value_t *elm_Platform_worker(tcelm_arena_t *arena, tcelm_value_t *config);

/*
 * Initialize Platform module globals
 * Call this before using any Platform functions
 */
void tcelm_platform_init(tcelm_arena_t *arena);

#endif /* TCELM_PLATFORM_H */
