/*
 * simple_test.c - Quick sanity test of tcelm runtime
 */

#include "tcelm_runtime.h"
#include <stdio.h>

static tcelm_channel_t *g_channel = NULL;

static tcelm_value_t *producer_task(tcelm_arena_t *arena, tcelm_value_t *arg) {
    int id = TCELM_AS_INT(arg);
    printf("[Producer %d] Starting\n", id);
    fflush(stdout);

    for (int i = 0; i < 5; i++) {
        tcelm_value_t *msg = tcelm_int(arena, id * 100 + i);
        printf("[Producer %d] Sending %d\n", id, id * 100 + i);
        fflush(stdout);
        tcelm_channel_send(g_channel, msg);
        tcelm_task_sleep(50);
    }

    printf("[Producer %d] Done\n", id);
    fflush(stdout);
    return tcelm_task_succeed(arena, tcelm_unit(arena));
}

static tcelm_value_t *consumer_task(tcelm_arena_t *arena, tcelm_value_t *arg) {
    (void)arg;
    printf("[Consumer] Starting\n");
    fflush(stdout);

    for (int i = 0; i < 10; i++) {
        tcelm_value_t *result = tcelm_channel_receive_timeout(arena, g_channel, 200);

        if (tcelm_custom_ctor(result) == TCELM_CTOR_NOTHING) {
            printf("[Consumer] Timeout\n");
            fflush(stdout);
        } else {
            tcelm_value_t *msg = tcelm_custom_arg(result, 0);
            printf("[Consumer] Received: %lld\n", (long long)TCELM_AS_INT(msg));
            fflush(stdout);
        }
    }

    printf("[Consumer] Done\n");
    fflush(stdout);
    return tcelm_task_succeed(arena, tcelm_unit(arena));
}

int main(void) {
    printf("=== Simple Test Starting ===\n");
    fflush(stdout);

    tcelm_task_init();
    tcelm_channel_init();
    tcelm_mvar_init();

    tcelm_arena_t *arena = tcelm_arena_create(64 * 1024);
    printf("Arena created\n");
    fflush(stdout);

    tcelm_channel_config_t config = { .capacity = 8, .message_size = 64, .name = "TEST" };
    g_channel = tcelm_channel_create(arena, &config);
    printf("Channel created\n");
    fflush(stdout);

    printf("Spawning tasks...\n");
    fflush(stdout);

    tcelm_task_handle_t *prod1 = tcelm_task_spawn_default(arena, producer_task, tcelm_int(arena, 1));
    tcelm_task_handle_t *prod2 = tcelm_task_spawn_default(arena, producer_task, tcelm_int(arena, 2));
    tcelm_task_handle_t *cons = tcelm_task_spawn_default(arena, consumer_task, tcelm_unit(arena));

    printf("Waiting for completion...\n");
    fflush(stdout);

    tcelm_task_await(arena, prod1);
    printf("Producer 1 complete\n");
    fflush(stdout);

    tcelm_task_await(arena, prod2);
    printf("Producer 2 complete\n");
    fflush(stdout);

    tcelm_task_await(arena, cons);
    printf("Consumer complete\n");
    fflush(stdout);

    printf("=== Test Complete ===\n");

    tcelm_channel_close(g_channel);
    tcelm_arena_free(arena);

    return 0;
}
