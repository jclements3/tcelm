/*
 * tcelm_clock.h - Wall-clock time for RTEMS
 *
 * Provides real-time clock (RTC) access and tick conversion.
 */

#ifndef TCELM_CLOCK_H
#define TCELM_CLOCK_H

#include "tcelm_types.h"
#include <stdint.h>
#include <stdbool.h>

#ifdef __rtems__
#include <rtems.h>
#endif

/*
 * Time of day structure
 */
typedef struct tcelm_time_of_day {
    uint32_t year;          /* 1970-2099 */
    uint32_t month;         /* 1-12 */
    uint32_t day;           /* 1-31 */
    uint32_t hour;          /* 0-23 */
    uint32_t minute;        /* 0-59 */
    uint32_t second;        /* 0-59 */
    uint32_t ticks;         /* Sub-second ticks */
} tcelm_time_of_day_t;

/*
 * Uptime structure
 */
typedef struct tcelm_uptime {
    uint64_t seconds;
    uint32_t nanoseconds;
} tcelm_uptime_t;

/*
 * Initialize clock subsystem
 */
int tcelm_clock_init(void);

/*
 * Get current time of day
 * Returns 0 on success, -1 if clock not set
 */
int tcelm_clock_get_tod(tcelm_time_of_day_t *tod);

/*
 * Set time of day
 * Returns 0 on success
 */
int tcelm_clock_set_tod(const tcelm_time_of_day_t *tod);

/*
 * Check if clock has been set
 */
bool tcelm_clock_is_set(void);

/*
 * Get system uptime in seconds
 */
uint64_t tcelm_clock_get_uptime_seconds(void);

/*
 * Get system uptime in nanoseconds
 */
uint64_t tcelm_clock_get_uptime_ns(void);

/*
 * Get system uptime (full precision)
 */
int tcelm_clock_get_uptime(tcelm_uptime_t *uptime);

/*
 * Get ticks per second
 */
uint32_t tcelm_clock_get_ticks_per_second(void);

/*
 * Get current tick count
 */
uint32_t tcelm_clock_get_ticks(void);

/*
 * Convert ticks to milliseconds
 */
uint32_t tcelm_clock_ticks_to_ms(uint32_t ticks);

/*
 * Convert milliseconds to ticks
 */
uint32_t tcelm_clock_ms_to_ticks(uint32_t ms);

/*
 * Convert ticks to nanoseconds
 */
uint64_t tcelm_clock_ticks_to_ns(uint32_t ticks);

/*
 * Date/time utilities
 */
bool tcelm_clock_is_leap_year(uint32_t year);
uint32_t tcelm_clock_days_in_month(uint32_t year, uint32_t month);
uint32_t tcelm_clock_day_of_week(uint32_t year, uint32_t month, uint32_t day);

/*
 * Unix timestamp conversions
 */
uint64_t tcelm_clock_tod_to_unix(const tcelm_time_of_day_t *tod);
void tcelm_clock_unix_to_tod(uint64_t unix_time, tcelm_time_of_day_t *tod);

#endif /* TCELM_CLOCK_H */
