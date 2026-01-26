/*
 * sensor_monitor.c - Generated C code for SensorMonitor.elm
 *
 * Demonstrates tcelm runtime: tasks, channels, MVars, IO
 * Compile: make sensor_monitor
 * Run: ./sensor_monitor
 */

#include "tcelm_runtime.h"
#include <stdio.h>
#include <string.h>

/* Type: SensorReading */
typedef struct {
    int sensor_id;
    double value;
    int timestamp;
} sensor_reading_t;

/* Type: SystemState */
typedef struct {
    int total_readings;
    double average_temp;
    double max_temp;
    double min_temp;
} system_state_t;

/* Wrap sensor_reading in tcelm_value_t */
static tcelm_value_t *wrap_reading(tcelm_arena_t *arena, sensor_reading_t *reading) {
    sensor_reading_t *copy = tcelm_arena_alloc(arena, sizeof(sensor_reading_t));
    *copy = *reading;
    return tcelm_record(arena, 3,
        "sensorId", tcelm_int(arena, copy->sensor_id),
        "value", tcelm_float(arena, copy->value),
        "timestamp", tcelm_int(arena, copy->timestamp)
    );
}

/* Unwrap sensor_reading from tcelm_value_t */
static sensor_reading_t unwrap_reading(tcelm_value_t *val) {
    sensor_reading_t r;
    r.sensor_id = TCELM_AS_INT(tcelm_record_get(val, "sensorId"));
    r.value = TCELM_AS_FLOAT(tcelm_record_get(val, "value"));
    r.timestamp = TCELM_AS_INT(tcelm_record_get(val, "timestamp"));
    return r;
}

/* Wrap system_state in tcelm_value_t */
static tcelm_value_t *wrap_state(tcelm_arena_t *arena, system_state_t *state) {
    return tcelm_record(arena, 4,
        "totalReadings", tcelm_int(arena, state->total_readings),
        "averageTemp", tcelm_float(arena, state->average_temp),
        "maxTemp", tcelm_float(arena, state->max_temp),
        "minTemp", tcelm_float(arena, state->min_temp)
    );
}

/* Unwrap system_state from tcelm_value_t */
static system_state_t unwrap_state(tcelm_value_t *val) {
    system_state_t s;
    s.total_readings = TCELM_AS_INT(tcelm_record_get(val, "totalReadings"));
    s.average_temp = TCELM_AS_FLOAT(tcelm_record_get(val, "averageTemp"));
    s.max_temp = TCELM_AS_FLOAT(tcelm_record_get(val, "maxTemp"));
    s.min_temp = TCELM_AS_FLOAT(tcelm_record_get(val, "minTemp"));
    return s;
}

/* Global channel and MVar (set by main, used by tasks) */
static tcelm_channel_t *g_channel = NULL;
static tcelm_mvar_t *g_state_mvar = NULL;

/*
 * Sensor task: generates readings and sends to channel
 */
static tcelm_value_t *sensor_task(tcelm_arena_t *arena, tcelm_value_t *arg) {
    int sensor_id = TCELM_AS_INT(arg);

    printf("[Sensor %d] Starting\n", sensor_id);

    for (int count = 0; count < 10; count++) {
        /* Simulate sensor reading (would be real hardware on NUC) */
        sensor_reading_t reading = {
            .sensor_id = sensor_id,
            .value = 20.0 + (double)((count * 7 + sensor_id * 13) % 100) / 10.0,
            .timestamp = count * 100
        };

        /* Send to channel */
        tcelm_value_t *msg = wrap_reading(arena, &reading);
        tcelm_channel_send(g_channel, msg);

        /* Sleep between readings */
        tcelm_task_sleep(50);
    }

    printf("[Sensor %d] Done\n", sensor_id);
    return tcelm_task_succeed(arena, tcelm_unit(arena));
}

/*
 * Aggregator task: receives readings, updates shared state
 */
static tcelm_value_t *aggregator_task(tcelm_arena_t *arena, tcelm_value_t *arg) {
    (void)arg;

    printf("[Aggregator] Starting\n");

    int timeout_count = 0;
    int msg_count = 0;

    while (timeout_count < 2) {
        tcelm_value_t *result = tcelm_channel_receive_timeout(arena, g_channel, 300);

        if (tcelm_custom_ctor(result) == TCELM_CTOR_NOTHING) {
            timeout_count++;
            continue;
        }

        timeout_count = 0;
        msg_count++;

        /* Unwrap Just value */
        tcelm_value_t *msg = tcelm_custom_arg(result, 0);
        sensor_reading_t reading = unwrap_reading(msg);

        /* Update shared state atomically */
        tcelm_value_t *state_val = tcelm_mvar_take(arena, g_state_mvar);
        system_state_t state = unwrap_state(state_val);

        int new_total = state.total_readings + 1;
        double new_avg = (state.average_temp * state.total_readings + reading.value) / new_total;
        double new_max = (reading.value > state.max_temp) ? reading.value : state.max_temp;
        double new_min = (reading.value < state.min_temp) ? reading.value : state.min_temp;

        state.total_readings = new_total;
        state.average_temp = new_avg;
        state.max_temp = new_max;
        state.min_temp = new_min;

        tcelm_mvar_put(g_state_mvar, wrap_state(arena, &state));
    }

    printf("[Aggregator] Done (%d messages)\n", msg_count);
    return tcelm_task_succeed(arena, tcelm_unit(arena));
}

/*
 * Logger task: periodically prints system state
 */
static tcelm_value_t *logger_task(tcelm_arena_t *arena, tcelm_value_t *arg) {
    (void)arg;

    printf("[Logger] Starting\n");

    for (int i = 0; i < 5; i++) {
        tcelm_task_sleep(200);

        /* Read state (non-destructive) */
        tcelm_value_t *state_val = tcelm_mvar_read(arena, g_state_mvar);
        system_state_t state = unwrap_state(state_val);

        printf("[Logger] readings=%d avg=%.2f max=%.2f min=%.2f\n",
               state.total_readings,
               state.average_temp,
               state.max_temp,
               state.min_temp);
    }

    printf("[Logger] Done\n");
    return tcelm_task_succeed(arena, tcelm_unit(arena));
}

/*
 * Main entry point
 */
int main(void) {
    /* Disable buffering for real-time output */
    setbuf(stdout, NULL);

    /* Initialize runtime */
    tcelm_task_init();
    tcelm_channel_init();
    tcelm_mvar_init();

    /* Create main arena */
    tcelm_arena_t *arena = tcelm_arena_create(256 * 1024);

    printf("=== Sensor Monitor ===\n");
    printf("Platform: %s\n\n",
#ifdef __rtems__
           "RTEMS (real-time)"
#else
           "Native (pthreads)"
#endif
    );

    /* Create channel for sensor readings */
    tcelm_channel_config_t chan_config = {
        .capacity = 32,
        .message_size = sizeof(tcelm_value_t *),
        .name = "SENS"
    };
    g_channel = tcelm_channel_create(arena, &chan_config);

    /* Create MVar with initial state */
    system_state_t initial_state = {
        .total_readings = 0,
        .average_temp = 0.0,
        .max_temp = -1000.0,
        .min_temp = 1000.0
    };
    g_state_mvar = tcelm_mvar_new(arena, wrap_state(arena, &initial_state));

    /* Spawn tasks */
    tcelm_task_handle_t *sensor1 = tcelm_task_spawn_default(arena, sensor_task, tcelm_int(arena, 1));
    tcelm_task_handle_t *sensor2 = tcelm_task_spawn_default(arena, sensor_task, tcelm_int(arena, 2));
    tcelm_task_handle_t *sensor3 = tcelm_task_spawn_default(arena, sensor_task, tcelm_int(arena, 3));
    tcelm_task_handle_t *aggregator = tcelm_task_spawn_default(arena, aggregator_task, tcelm_unit(arena));
    tcelm_task_handle_t *logger = tcelm_task_spawn_default(arena, logger_task, tcelm_unit(arena));

    /* Wait for all tasks */
    tcelm_task_await(arena, sensor1);
    tcelm_task_await(arena, sensor2);
    tcelm_task_await(arena, sensor3);
    tcelm_task_await(arena, aggregator);
    tcelm_task_await(arena, logger);

    /* Print final state */
    tcelm_value_t *final_state_val = tcelm_mvar_read(arena, g_state_mvar);
    system_state_t final_state = unwrap_state(final_state_val);

    printf("\n=== Final Results ===\n");
    printf("Total readings: %d\n", final_state.total_readings);
    printf("Average temp:   %.2f\n", final_state.average_temp);
    printf("Max temp:       %.2f\n", final_state.max_temp);
    printf("Min temp:       %.2f\n", final_state.min_temp);

    /* Cleanup */
    tcelm_channel_close(g_channel);
    tcelm_mvar_delete(g_state_mvar);
    tcelm_arena_free(arena);

    tcelm_mvar_shutdown();
    tcelm_channel_shutdown();
    tcelm_task_shutdown();

    return 0;
}
