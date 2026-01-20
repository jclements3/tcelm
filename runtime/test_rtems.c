/*
 * test_rtems.c - Test tcelm RTEMS integration (host build)
 *
 * Compile with: gcc -o test_rtems test_rtems.c tcelm_arena.c tcelm_types.c \
 *                   tcelm_basics.c tcelm_rtems.c tcelm_shell.c -lm
 */

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "tcelm_arena.h"
#include "tcelm_types.h"
#include "tcelm_basics.h"
#include "tcelm_rtems.h"
#include "tcelm_shell.h"

/*
 * Example Elm task: Counter
 *
 * Model = Int
 * Msg = Increment | Decrement | Reset
 */

/* Constructor IDs for Msg type */
#define MSG_INCREMENT 0
#define MSG_DECREMENT 1
#define MSG_RESET     2

/* init : Model */
static tcelm_value_t *counter_init(tcelm_arena_t *arena) {
    return tcelm_int(arena, 0);
}

/* update : Msg -> Model -> (Model, Cmd Msg) */
static tcelm_value_t *counter_update(
        tcelm_arena_t *arena,
        tcelm_value_t *msg,
        tcelm_value_t *model) {

    int64_t count = TCELM_AS_INT(model);
    tcelm_value_t *newModel;

    if (TCELM_IS_CUSTOM(msg)) {
        uint16_t ctor = tcelm_custom_ctor(msg);
        switch (ctor) {
            case MSG_INCREMENT:
                newModel = tcelm_int(arena, count + 1);
                printf("Counter: %ld -> %ld (increment)\n",
                       (long)count, (long)(count + 1));
                break;

            case MSG_DECREMENT:
                newModel = tcelm_int(arena, count - 1);
                printf("Counter: %ld -> %ld (decrement)\n",
                       (long)count, (long)(count - 1));
                break;

            case MSG_RESET:
                newModel = tcelm_int(arena, 0);
                printf("Counter: %ld -> 0 (reset)\n", (long)count);
                break;

            default:
                newModel = model;
                printf("Counter: unknown message ctor %d\n", ctor);
        }
    } else if (TCELM_IS_INT(msg)) {
        /* Direct int message - increment by that amount */
        int64_t delta = TCELM_AS_INT(msg);
        newModel = tcelm_int(arena, count + delta);
        printf("Counter: %ld -> %ld (+%ld)\n",
               (long)count, (long)(count + delta), (long)delta);
    } else {
        newModel = model;
        printf("Counter: unhandled message type\n");
    }

    /* Return (newModel, Cmd.none) */
    return tcelm_tuple2(arena, newModel, tcelm_cmd_none(arena));
}

/* subscriptions : Model -> Sub Msg */
static tcelm_value_t *counter_subscriptions(
        tcelm_arena_t *arena,
        tcelm_value_t *model) {
    (void)model;
    return tcelm_sub_none(arena);
}

/* Task definition */
static const tcelm_task_def_t counter_task_def = {
    .name = "Counter",
    .init = counter_init,
    .update = counter_update,
    .subscriptions = counter_subscriptions,
    .view = NULL,
    .priority = 0,
    .stack_size = 0
};

/*
 * Test the RTEMS integration
 */
int main(void) {
    printf("\n=== tcelm RTEMS Integration Test ===\n\n");

    /* Initialize runtime */
    printf("Initializing tcelm RTEMS runtime...\n");
    int rc = tcelm_rtems_init(NULL);
    assert(rc == 0);
    printf("  Runtime initialized with default config\n\n");

    /* Initialize shell (host mode - just prints message) */
    printf("Initializing shell commands...\n");
    tcelm_shell_init();
    printf("\n");

    /* Register counter module */
    printf("Registering Counter module...\n");
    tcelm_shell_register_module("Counter", &counter_task_def);
    printf("  Module registered\n\n");

    /* Test finding module */
    printf("Finding Counter module...\n");
    const tcelm_task_def_t *found = tcelm_shell_find_module("Counter");
    assert(found != NULL);
    assert(strcmp(found->name, "Counter") == 0);
    printf("  Found: %s\n\n", found->name);

    /* Spawn counter task */
    printf("Spawning Counter task...\n");
    tcelm_task_t *counter = tcelm_task_spawn(&counter_task_def);
    assert(counter != NULL);
    printf("  Task spawned\n\n");

    /* Create an arena for messages */
    tcelm_arena_t msg_arena;
    tcelm_arena_init(&msg_arena, 4096);
    tcelm_current_arena = &msg_arena;

    /* Send some messages */
    printf("Sending messages to counter...\n\n");

    /* Send Increment */
    tcelm_value_t *inc_msg = tcelm_custom(&msg_arena, MSG_INCREMENT, "Increment", 0);
    tcelm_task_send(counter, inc_msg);

    /* Send Increment */
    tcelm_task_send(counter, inc_msg);

    /* Send Increment */
    tcelm_task_send(counter, inc_msg);

    /* Send Decrement */
    tcelm_value_t *dec_msg = tcelm_custom(&msg_arena, MSG_DECREMENT, "Decrement", 0);
    tcelm_task_send(counter, dec_msg);

    /* Send direct int */
    tcelm_task_send_int(counter, 10);

    /* Send Reset */
    tcelm_value_t *reset_msg = tcelm_custom(&msg_arena, MSG_RESET, "Reset", 0);
    tcelm_task_send(counter, reset_msg);

    printf("\n");

    /* Test timing functions */
    printf("Testing timing functions...\n");
    tcelm_value_t *time_ms = tcelm_rtems_time_ms(&msg_arena);
    tcelm_value_t *ticks = tcelm_rtems_tick_get(&msg_arena);
    tcelm_value_t *tps = tcelm_rtems_ticks_per_second(&msg_arena);
    printf("  Time (ms):          %ld\n", (long)TCELM_AS_INT(time_ms));
    printf("  Ticks:              %ld\n", (long)TCELM_AS_INT(ticks));
    printf("  Ticks per second:   %ld\n", (long)TCELM_AS_INT(tps));
    printf("\n");

    /* Get task stats */
    printf("Task statistics:\n");
    tcelm_task_stats_t stats;
    tcelm_task_get_stats(counter, &stats);
    printf("  Name:               %s\n", stats.name);
    printf("  Messages received:  %lu\n", (unsigned long)stats.messages_received);
    printf("  Messages processed: %lu\n", (unsigned long)stats.messages_processed);
    printf("  Update calls:       %lu\n", (unsigned long)stats.update_calls);
    printf("\n");

    /* Print runtime stats */
    tcelm_rtems_print_stats();

    /* Test shell commands */
    printf("\n=== Testing Shell Commands ===\n\n");

    /* Test elm command */
    printf("Testing 'elm' command:\n");
    char *elm_args1[] = {"elm", "42"};
    tcelm_shell_cmd_elm(2, elm_args1);

    char *elm_args2[] = {"elm", "10", "+", "5"};
    tcelm_shell_cmd_elm(4, elm_args2);

    char *elm_args3[] = {"elm", "\"hello\""};
    tcelm_shell_cmd_elm(2, elm_args3);
    printf("\n");

    /* Test elm-help */
    printf("Testing 'elm-help' command:\n");
    char *help_args[] = {"elm-help"};
    tcelm_shell_cmd_help(1, help_args);

    /* Clean up */
    printf("Cleaning up...\n");
    tcelm_task_delete(counter);
    tcelm_arena_destroy(&msg_arena);
    tcelm_rtems_shutdown();
    printf("  Done\n\n");

    printf("=== All tests passed! ===\n\n");
    return 0;
}
