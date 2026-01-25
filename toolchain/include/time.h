/*
 * time.h - Minimal time for tcelm bare-metal
 */

#ifndef _TIME_H
#define _TIME_H

#include <stdint.h>

typedef int64_t time_t;
typedef int clockid_t;

struct timespec {
    time_t tv_sec;
    long   tv_nsec;
};

/* Clock types */
#define CLOCK_REALTIME  0
#define CLOCK_MONOTONIC 1

/* Get current time in ms */
time_t time(time_t *t);

/* Get time with nanosecond precision */
int clock_gettime(clockid_t clk_id, struct timespec *tp);

/* Sleep */
int nanosleep(const struct timespec *req, struct timespec *rem);

#endif /* _TIME_H */
