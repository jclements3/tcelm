#include <stdio.h>
#include <assert.h>
#include "tcelm_arena.h"
#include "tcelm_types.h"
#include "tcelm_channel.h"

int main(void) {
    printf("Creating arena...\n");
    tcelm_arena_t *arena = tcelm_arena_create(64 * 1024);
    assert(arena != NULL);
    printf("Arena created at %p\n", (void*)arena);

    printf("Creating channel...\n");
    tcelm_channel_t *chan = tcelm_channel_create_default(arena, 10);
    printf("Channel created at %p\n", (void*)chan);
    
    if (chan) {
        printf("Channel queue_id = %u\n", chan->queue_id);
    }

    printf("Creating message...\n");
    tcelm_value_t *msg = tcelm_int(arena, 42);
    printf("Message at %p\n", (void*)msg);

    printf("Sending message...\n");
    int result = tcelm_channel_send(chan, msg);
    printf("Send result: %d\n", result);

    printf("Done!\n");
    return 0;
}
