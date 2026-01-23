/*
 * tcelm_clock.c - Wall-clock time for RTEMS
 *
 * Provides real-time clock access and tick conversion utilities.
 */

#include "tcelm_clock.h"
#include <string.h>
#include <stdio.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems/clock.h>
#else
#include <time.h>
#include <sys/time.h>
#endif

/* Days in each month (non-leap year) */
static const uint32_t days_per_month[] = {
    0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};

/* Days before each month (non-leap year) */
static const uint32_t days_before_month[] = {
    0, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
};

/*
 * Initialize clock subsystem
 */
int tcelm_clock_init(void) {
    /* Nothing special needed */
    return 0;
}

#ifdef __rtems__

/*
 * RTEMS implementation
 */

int tcelm_clock_get_tod(tcelm_time_of_day_t *tod) {
    if (!tod) return -1;

    rtems_time_of_day rtems_tod;
    rtems_status_code status = rtems_clock_get_tod(&rtems_tod);

    if (status != RTEMS_SUCCESSFUL) {
        return -1;
    }

    tod->year = rtems_tod.year;
    tod->month = rtems_tod.month;
    tod->day = rtems_tod.day;
    tod->hour = rtems_tod.hour;
    tod->minute = rtems_tod.minute;
    tod->second = rtems_tod.second;
    tod->ticks = rtems_tod.ticks;

    return 0;
}

int tcelm_clock_set_tod(const tcelm_time_of_day_t *tod) {
    if (!tod) return -1;

    rtems_time_of_day rtems_tod = {
        .year = tod->year,
        .month = tod->month,
        .day = tod->day,
        .hour = tod->hour,
        .minute = tod->minute,
        .second = tod->second,
        .ticks = tod->ticks
    };

    rtems_status_code status = rtems_clock_set(&rtems_tod);
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
}

bool tcelm_clock_is_set(void) {
    rtems_time_of_day tod;
    return rtems_clock_get_tod(&tod) == RTEMS_SUCCESSFUL;
}

uint64_t tcelm_clock_get_uptime_seconds(void) {
    struct timespec ts;
    rtems_clock_get_uptime(&ts);
    return ts.tv_sec;
}

uint64_t tcelm_clock_get_uptime_ns(void) {
    struct timespec ts;
    rtems_clock_get_uptime(&ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + ts.tv_nsec;
}

int tcelm_clock_get_uptime(tcelm_uptime_t *uptime) {
    if (!uptime) return -1;

    struct timespec ts;
    rtems_clock_get_uptime(&ts);

    uptime->seconds = ts.tv_sec;
    uptime->nanoseconds = ts.tv_nsec;

    return 0;
}

uint32_t tcelm_clock_get_ticks_per_second(void) {
    return rtems_clock_get_ticks_per_second();
}

uint32_t tcelm_clock_get_ticks(void) {
    return rtems_clock_get_ticks_since_boot();
}

uint32_t tcelm_clock_ticks_to_ms(uint32_t ticks) {
    uint32_t tps = rtems_clock_get_ticks_per_second();
    return (ticks * 1000) / tps;
}

uint32_t tcelm_clock_ms_to_ticks(uint32_t ms) {
    uint32_t tps = rtems_clock_get_ticks_per_second();
    return (ms * tps) / 1000;
}

uint64_t tcelm_clock_ticks_to_ns(uint32_t ticks) {
    uint32_t tps = rtems_clock_get_ticks_per_second();
    return ((uint64_t)ticks * 1000000000ULL) / tps;
}

#else

/*
 * Native (POSIX) implementation
 */

static struct timespec boot_time;
static bool boot_time_set = false;

static void init_boot_time(void) {
    if (!boot_time_set) {
        clock_gettime(CLOCK_MONOTONIC, &boot_time);
        boot_time_set = true;
    }
}

int tcelm_clock_get_tod(tcelm_time_of_day_t *tod) {
    if (!tod) return -1;

    time_t now = time(NULL);
    struct tm *tm = localtime(&now);

    if (!tm) return -1;

    tod->year = tm->tm_year + 1900;
    tod->month = tm->tm_mon + 1;
    tod->day = tm->tm_mday;
    tod->hour = tm->tm_hour;
    tod->minute = tm->tm_min;
    tod->second = tm->tm_sec;
    tod->ticks = 0;

    return 0;
}

int tcelm_clock_set_tod(const tcelm_time_of_day_t *tod) {
    (void)tod;
    /* Setting system time requires root privileges on most systems */
    fprintf(stderr, "Warning: tcelm_clock_set_tod not implemented on this platform\n");
    return -1;
}

bool tcelm_clock_is_set(void) {
    return true;  /* Always "set" on POSIX */
}

uint64_t tcelm_clock_get_uptime_seconds(void) {
    init_boot_time();

    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);

    return now.tv_sec - boot_time.tv_sec;
}

uint64_t tcelm_clock_get_uptime_ns(void) {
    init_boot_time();

    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);

    int64_t sec_diff = now.tv_sec - boot_time.tv_sec;
    int64_t ns_diff = now.tv_nsec - boot_time.tv_nsec;

    return sec_diff * 1000000000LL + ns_diff;
}

int tcelm_clock_get_uptime(tcelm_uptime_t *uptime) {
    if (!uptime) return -1;

    init_boot_time();

    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);

    uptime->seconds = now.tv_sec - boot_time.tv_sec;
    uptime->nanoseconds = now.tv_nsec;

    return 0;
}

uint32_t tcelm_clock_get_ticks_per_second(void) {
    return 1000;  /* Simulate 1kHz tick rate */
}

uint32_t tcelm_clock_get_ticks(void) {
    return (uint32_t)(tcelm_clock_get_uptime_ns() / 1000000);
}

uint32_t tcelm_clock_ticks_to_ms(uint32_t ticks) {
    return ticks;  /* 1ms ticks */
}

uint32_t tcelm_clock_ms_to_ticks(uint32_t ms) {
    return ms;  /* 1ms ticks */
}

uint64_t tcelm_clock_ticks_to_ns(uint32_t ticks) {
    return (uint64_t)ticks * 1000000;
}

#endif /* __rtems__ */

/*
 * Common date/time utilities
 */

bool tcelm_clock_is_leap_year(uint32_t year) {
    return (year % 4 == 0) && ((year % 100 != 0) || (year % 400 == 0));
}

uint32_t tcelm_clock_days_in_month(uint32_t year, uint32_t month) {
    if (month < 1 || month > 12) return 0;

    if (month == 2 && tcelm_clock_is_leap_year(year)) {
        return 29;
    }
    return days_per_month[month];
}

uint32_t tcelm_clock_day_of_week(uint32_t year, uint32_t month, uint32_t day) {
    /* Zeller's formula */
    if (month < 3) {
        month += 12;
        year--;
    }

    uint32_t k = year % 100;
    uint32_t j = year / 100;

    uint32_t h = (day + (13 * (month + 1)) / 5 + k + k / 4 + j / 4 + 5 * j) % 7;

    /* Convert from Zeller (0=Sat) to standard (0=Sun) */
    return (h + 6) % 7;
}

/*
 * Unix timestamp conversions
 */

/* Seconds from 1970-01-01 to given date */
static uint64_t days_since_epoch(uint32_t year, uint32_t month, uint32_t day) {
    uint64_t days = 0;

    /* Days from years */
    for (uint32_t y = 1970; y < year; y++) {
        days += tcelm_clock_is_leap_year(y) ? 366 : 365;
    }

    /* Days from months */
    days += days_before_month[month];
    if (month > 2 && tcelm_clock_is_leap_year(year)) {
        days++;
    }

    /* Days in current month */
    days += day - 1;

    return days;
}

uint64_t tcelm_clock_tod_to_unix(const tcelm_time_of_day_t *tod) {
    if (!tod) return 0;

    uint64_t days = days_since_epoch(tod->year, tod->month, tod->day);
    uint64_t seconds = days * 86400ULL +
                       tod->hour * 3600ULL +
                       tod->minute * 60ULL +
                       tod->second;

    return seconds;
}

void tcelm_clock_unix_to_tod(uint64_t unix_time, tcelm_time_of_day_t *tod) {
    if (!tod) return;

    /* Extract time of day */
    uint32_t seconds_in_day = unix_time % 86400;
    tod->hour = seconds_in_day / 3600;
    tod->minute = (seconds_in_day % 3600) / 60;
    tod->second = seconds_in_day % 60;
    tod->ticks = 0;

    /* Calculate date */
    uint32_t days = unix_time / 86400;
    uint32_t year = 1970;

    while (1) {
        uint32_t days_in_year = tcelm_clock_is_leap_year(year) ? 366 : 365;
        if (days < days_in_year) break;
        days -= days_in_year;
        year++;
    }

    tod->year = year;

    /* Find month */
    uint32_t month = 1;
    while (month <= 12) {
        uint32_t dim = tcelm_clock_days_in_month(year, month);
        if (days < dim) break;
        days -= dim;
        month++;
    }

    tod->month = month;
    tod->day = days + 1;
}
