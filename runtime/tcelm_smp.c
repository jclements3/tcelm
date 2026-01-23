/*
 * tcelm_smp.c - SMP support for RTEMS
 *
 * Provides CPU affinity control for multi-core systems.
 */

#include "tcelm_smp.h"
#include <string.h>
#include <stdio.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/score/smp.h>
#include <rtems/score/processormask.h>
#include <rtems/rtems/tasks.h>
#elif defined(__linux__)
#define _GNU_SOURCE
#include <sched.h>
#include <pthread.h>
#include <unistd.h>
#elif defined(__APPLE__)
#include <sys/sysctl.h>
#include <pthread.h>
#include <mach/mach.h>
#include <mach/thread_policy.h>
#endif

/*
 * Initialize SMP subsystem
 */
int tcelm_smp_init(void) {
    return 0;
}

#ifdef __rtems__

/*
 * RTEMS SMP implementation
 */

uint32_t tcelm_smp_processor_count(void) {
    return rtems_scheduler_get_processor_maximum();
}

uint32_t tcelm_smp_current_processor(void) {
    return rtems_scheduler_get_processor();
}

bool tcelm_smp_is_enabled(void) {
#if defined(RTEMS_SMP)
    return tcelm_smp_processor_count() > 1;
#else
    return false;
#endif
}

tcelm_cpu_state_t tcelm_smp_get_processor_state(uint32_t cpu_index) {
    if (cpu_index >= tcelm_smp_processor_count()) {
        return TCELM_CPU_OFFLINE;
    }

#if defined(RTEMS_SMP)
    /* Check if processor is online */
    /* RTEMS 5+ has rtems_scheduler_get_processor_set */
    return TCELM_CPU_ONLINE;  /* Assume online if index is valid */
#else
    return (cpu_index == 0) ? TCELM_CPU_ONLINE : TCELM_CPU_OFFLINE;
#endif
}

tcelm_cpu_set_t tcelm_smp_get_affinity(uint32_t task_id) {
    rtems_id id = (task_id == 0) ? RTEMS_SELF : (rtems_id)task_id;

#if defined(RTEMS_SMP)
    cpu_set_t cpuset;
    size_t cpusetsize = sizeof(cpuset);

    rtems_status_code status = rtems_task_get_affinity(id, cpusetsize, &cpuset);
    if (status != RTEMS_SUCCESSFUL) {
        return TCELM_CPU_SET_ALL;
    }

    /* Convert cpu_set_t to our bitmask */
    tcelm_cpu_set_t result = 0;
    uint32_t max_cpus = tcelm_smp_processor_count();
    for (uint32_t i = 0; i < max_cpus && i < 32; i++) {
        if (CPU_ISSET(i, &cpuset)) {
            result |= (1u << i);
        }
    }
    return result;
#else
    (void)id;
    return TCELM_CPU_SET_CPU(0);
#endif
}

int tcelm_smp_set_affinity(uint32_t task_id, tcelm_cpu_set_t cpu_set) {
    rtems_id id = (task_id == 0) ? RTEMS_SELF : (rtems_id)task_id;

#if defined(RTEMS_SMP)
    cpu_set_t cpuset;
    CPU_ZERO(&cpuset);

    uint32_t max_cpus = tcelm_smp_processor_count();
    for (uint32_t i = 0; i < max_cpus && i < 32; i++) {
        if (cpu_set & (1u << i)) {
            CPU_SET(i, &cpuset);
        }
    }

    rtems_status_code status = rtems_task_set_affinity(id, sizeof(cpuset), &cpuset);
    return (status == RTEMS_SUCCESSFUL) ? 0 : -1;
#else
    (void)id;
    (void)cpu_set;
    return 0;  /* No-op on uniprocessor */
#endif
}

tcelm_cpu_set_t tcelm_smp_get_affinity_default(void) {
#if defined(RTEMS_SMP)
    uint32_t count = tcelm_smp_processor_count();
    if (count >= 32) {
        return TCELM_CPU_SET_ALL;
    }
    return (1u << count) - 1;
#else
    return TCELM_CPU_SET_CPU(0);
#endif
}

#elif defined(__linux__)

/*
 * Linux implementation
 */

uint32_t tcelm_smp_processor_count(void) {
    long nprocs = sysconf(_SC_NPROCESSORS_ONLN);
    return (nprocs > 0) ? (uint32_t)nprocs : 1;
}

uint32_t tcelm_smp_current_processor(void) {
    int cpu = sched_getcpu();
    return (cpu >= 0) ? (uint32_t)cpu : 0;
}

bool tcelm_smp_is_enabled(void) {
    return tcelm_smp_processor_count() > 1;
}

tcelm_cpu_state_t tcelm_smp_get_processor_state(uint32_t cpu_index) {
    if (cpu_index >= tcelm_smp_processor_count()) {
        return TCELM_CPU_OFFLINE;
    }

    /* Check /sys/devices/system/cpu/cpuN/online */
    char path[64];
    snprintf(path, sizeof(path), "/sys/devices/system/cpu/cpu%u/online", cpu_index);

    FILE *f = fopen(path, "r");
    if (!f) {
        /* CPU 0 typically doesn't have online file, assume online */
        return (cpu_index == 0) ? TCELM_CPU_ONLINE : TCELM_CPU_OFFLINE;
    }

    int online = 0;
    fscanf(f, "%d", &online);
    fclose(f);

    return online ? TCELM_CPU_ONLINE : TCELM_CPU_OFFLINE;
}

tcelm_cpu_set_t tcelm_smp_get_affinity(uint32_t task_id) {
    cpu_set_t cpuset;
    CPU_ZERO(&cpuset);

    pid_t pid = (task_id == 0) ? 0 : (pid_t)task_id;

    if (sched_getaffinity(pid, sizeof(cpuset), &cpuset) != 0) {
        return TCELM_CPU_SET_ALL;
    }

    tcelm_cpu_set_t result = 0;
    uint32_t max_cpus = tcelm_smp_processor_count();
    for (uint32_t i = 0; i < max_cpus && i < 32; i++) {
        if (CPU_ISSET(i, &cpuset)) {
            result |= (1u << i);
        }
    }
    return result;
}

int tcelm_smp_set_affinity(uint32_t task_id, tcelm_cpu_set_t cpu_set) {
    cpu_set_t cpuset;
    CPU_ZERO(&cpuset);

    uint32_t max_cpus = tcelm_smp_processor_count();
    for (uint32_t i = 0; i < max_cpus && i < 32; i++) {
        if (cpu_set & (1u << i)) {
            CPU_SET(i, &cpuset);
        }
    }

    pid_t pid = (task_id == 0) ? 0 : (pid_t)task_id;
    return sched_setaffinity(pid, sizeof(cpuset), &cpuset);
}

tcelm_cpu_set_t tcelm_smp_get_affinity_default(void) {
    uint32_t count = tcelm_smp_processor_count();
    if (count >= 32) {
        return TCELM_CPU_SET_ALL;
    }
    return (1u << count) - 1;
}

#elif defined(__APPLE__)

/*
 * macOS implementation
 * Note: macOS has limited affinity support
 */

uint32_t tcelm_smp_processor_count(void) {
    int count;
    size_t size = sizeof(count);
    if (sysctlbyname("hw.ncpu", &count, &size, NULL, 0) == 0) {
        return (uint32_t)count;
    }
    return 1;
}

uint32_t tcelm_smp_current_processor(void) {
    /* macOS doesn't expose this directly */
    return 0;
}

bool tcelm_smp_is_enabled(void) {
    return tcelm_smp_processor_count() > 1;
}

tcelm_cpu_state_t tcelm_smp_get_processor_state(uint32_t cpu_index) {
    if (cpu_index >= tcelm_smp_processor_count()) {
        return TCELM_CPU_OFFLINE;
    }
    return TCELM_CPU_ONLINE;  /* Assume all online */
}

tcelm_cpu_set_t tcelm_smp_get_affinity(uint32_t task_id) {
    (void)task_id;
    /* macOS doesn't support getting thread affinity */
    return tcelm_smp_get_affinity_default();
}

int tcelm_smp_set_affinity(uint32_t task_id, tcelm_cpu_set_t cpu_set) {
    (void)task_id;

    /* macOS thread affinity is advisory via thread_policy_set */
    /* This hints at which CPU to prefer but doesn't guarantee */
    if (tcelm_cpu_set_count(cpu_set) == 1) {
        /* Find which CPU */
        uint32_t cpu = 0;
        while ((cpu_set & (1u << cpu)) == 0) cpu++;

        thread_affinity_policy_data_t policy = { cpu };
        thread_port_t thread = mach_thread_self();
        thread_policy_set(thread, THREAD_AFFINITY_POLICY,
                          (thread_policy_t)&policy, 1);
        mach_port_deallocate(mach_task_self(), thread);
    }

    return 0;
}

tcelm_cpu_set_t tcelm_smp_get_affinity_default(void) {
    uint32_t count = tcelm_smp_processor_count();
    if (count >= 32) {
        return TCELM_CPU_SET_ALL;
    }
    return (1u << count) - 1;
}

#else

/*
 * Stub implementation for unsupported platforms
 */

uint32_t tcelm_smp_processor_count(void) { return 1; }
uint32_t tcelm_smp_current_processor(void) { return 0; }
bool tcelm_smp_is_enabled(void) { return false; }
tcelm_cpu_state_t tcelm_smp_get_processor_state(uint32_t cpu_index) {
    return (cpu_index == 0) ? TCELM_CPU_ONLINE : TCELM_CPU_OFFLINE;
}
tcelm_cpu_set_t tcelm_smp_get_affinity(uint32_t task_id) {
    (void)task_id;
    return TCELM_CPU_SET_CPU(0);
}
int tcelm_smp_set_affinity(uint32_t task_id, tcelm_cpu_set_t cpu_set) {
    (void)task_id;
    (void)cpu_set;
    return 0;
}
tcelm_cpu_set_t tcelm_smp_get_affinity_default(void) {
    return TCELM_CPU_SET_CPU(0);
}

#endif
