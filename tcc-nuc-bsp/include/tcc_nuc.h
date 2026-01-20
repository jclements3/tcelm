/*
 * tcc_nuc.h - TCC helper definitions for Intel NUC RTEMS target
 *
 * Include this in TCC-compiled applications for the NUC cluster.
 */

#ifndef TCC_NUC_H
#define TCC_NUC_H

#include <stdint.h>
#include <stddef.h>

/* Mark as TCC-NUC target */
#define __TCC_NUC__ 1
#define __i386__ 1

/*
 * HWIL cluster parameters
 */
#define HWIL_NODE_COUNT         60
#define HWIL_FRAME_RATE_HZ      240
#define HWIL_FRAME_TIME_US      4167    /* ~4.167 ms */

/* Network ports */
#define HWIL_CONTROL_PORT       7000
#define HWIL_DOWNLOAD_PORT_BASE 8001
#define HWIL_TELEMETRY_PORT_BASE 9000

/*
 * Telemetry frame - matches JEMINI protocol
 * 80 bytes at 240 Hz per node
 */
typedef struct __attribute__((packed)) {
    uint32_t frame_id;          /* Frame counter */
    uint64_t timestamp_ns;      /* Nanosecond timestamp */
    double position[3];         /* X, Y, Z position (m) */
    double velocity[3];         /* Vx, Vy, Vz velocity (m/s) */
    double temperature;         /* Sensor temperature (K) */
    uint16_t peak_intensity;    /* Pixel peak value */
    float centroid_x;           /* Centroid X position */
    float centroid_y;           /* Centroid Y position */
    uint8_t deadline_miss;      /* Deadline miss flag */
    uint8_t padding;            /* Alignment padding */
} hwil_telemetry_t;

/*
 * Static assert to verify telemetry size
 */
_Static_assert(sizeof(hwil_telemetry_t) == 80,
               "hwil_telemetry_t must be exactly 80 bytes");

/*
 * RTEMS task priorities for HWIL
 */
#define HWIL_PRIORITY_TELEMETRY     10  /* Highest - 240 Hz receive */
#define HWIL_PRIORITY_CONTROL       20  /* High - command processing */
#define HWIL_PRIORITY_DISPLAY       50  /* Medium - console updates */
#define HWIL_PRIORITY_IDLE         200  /* Low - background tasks */

/*
 * Arena allocation helpers
 * (Use tcelm_arena.h for full API)
 */
#define HWIL_ARENA_SIZE     (16 * 1024 * 1024)  /* 16 MB per task */

/*
 * Utility macros
 */
#define HWIL_UNUSED(x)      ((void)(x))
#define HWIL_ARRAY_SIZE(a)  (sizeof(a) / sizeof((a)[0]))

/*
 * Inline nanosecond timing (uses RTEMS clock)
 */
static inline uint64_t hwil_clock_ns(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}

#endif /* TCC_NUC_H */
