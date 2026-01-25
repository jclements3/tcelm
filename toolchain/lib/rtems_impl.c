/*
 * rtems_impl.c - RTEMS API implementation for tcelm/NUC toolchain
 *
 * This provides a cooperative multitasking implementation of RTEMS APIs
 * that can run on bare-metal x86 (QEMU or real hardware).
 *
 * Features:
 *   - SMP support (up to 4 cores)
 *   - Cooperative task scheduling with CPU affinity
 *   - Binary and counting semaphores
 *   - Message queues
 *   - Software timers
 *   - Event flags
 */

#include <rtems.h>
#include <string.h>
#include <stdlib.h>
#include <sys/cpuset.h>
#include <smp.h>

/* Configuration limits */
#define MAX_TASKS          32
#define MAX_SEMAPHORES     32
#define MAX_MESSAGE_QUEUES 16
#define MAX_TIMERS         32
#define MAX_RATE_MONOTONIC 16

#define TASK_STACK_SIZE    8192

/* Task states */
typedef enum {
    TASK_STATE_FREE = 0,
    TASK_STATE_READY,
    TASK_STATE_RUNNING,
    TASK_STATE_BLOCKED,
    TASK_STATE_SUSPENDED,
    TASK_STATE_DORMANT
} task_state_t;

/* Task control block */
typedef struct {
    rtems_id            id;
    rtems_name          name;
    task_state_t        state;
    rtems_task_priority priority;
    rtems_mode          mode;
    rtems_attribute     attributes;
    rtems_task_entry    entry;
    rtems_task_argument argument;
    void               *stack;
    void               *sp;              /* Saved stack pointer */
    rtems_event_set     pending_events;
    rtems_event_set     wanted_events;
    rtems_option        event_option;
    rtems_interval      wake_tick;       /* Tick to wake at (0 = no timeout) */
    rtems_id            blocked_on;      /* Resource blocking on */
    cpu_set_t           affinity;        /* CPU affinity mask */
    uint32_t            running_on_cpu;  /* Which CPU task is running on (-1 if not running) */
} task_tcb_t;

/* Semaphore control block */
typedef struct {
    rtems_id            id;
    rtems_name          name;
    uint32_t            count;
    uint32_t            max_count;
    rtems_attribute     attributes;
    rtems_task_priority ceiling;
    rtems_id            holder;          /* Task holding (for binary) */
} semaphore_t;

/* Message queue entry */
typedef struct msg_entry {
    struct msg_entry   *next;
    size_t              size;
    char                data[];
} msg_entry_t;

/* Message queue control block */
typedef struct {
    rtems_id            id;
    rtems_name          name;
    uint32_t            max_pending;
    size_t              max_message_size;
    rtems_attribute     attributes;
    uint32_t            pending_count;
    msg_entry_t        *head;
    msg_entry_t        *tail;
} message_queue_t;

/* Timer control block */
typedef struct {
    rtems_id                    id;
    rtems_name                  name;
    rtems_interval              interval;
    rtems_interval              fire_tick;
    rtems_timer_service_routine routine;
    void                       *user_data;
    bool                        active;
    bool                        repeat;     /* For rate monotonic */
} timer_t;

/* Global state */
static task_tcb_t      tasks[MAX_TASKS];
static semaphore_t     semaphores[MAX_SEMAPHORES];
static message_queue_t message_queues[MAX_MESSAGE_QUEUES];
static timer_t         timers[MAX_TIMERS];
static timer_t         rate_monotonic[MAX_RATE_MONOTONIC];

/* Per-CPU current task (indexed by CPU ID) */
static rtems_id        current_task_id[SMP_MAX_CPUS] = {0};
static rtems_interval  system_ticks = 0;
static rtems_interval  ticks_per_second = 1000;  /* 1ms tick */

static uint32_t        next_task_id = 1;
static uint32_t        next_sem_id = 1;
static uint32_t        next_mq_id = 1;
static uint32_t        next_timer_id = 1;
static uint32_t        next_rm_id = 1;

static bool            scheduler_running = false;

/* SMP locks for protecting shared data */
static spinlock_t      tasks_lock = SPINLOCK_INITIALIZER;
static spinlock_t      sem_lock = SPINLOCK_INITIALIZER;
static spinlock_t      mq_lock = SPINLOCK_INITIALIZER;
static spinlock_t      timer_lock = SPINLOCK_INITIALIZER;
static spinlock_t      id_lock = SPINLOCK_INITIALIZER;

/* Forward declarations */
static void scheduler_tick(void);
static void schedule(void);
static task_tcb_t *find_task(rtems_id id);
static task_tcb_t *get_current_task(void);

/*
 * Initialization
 */

void rtems_initialize_executive(void) {
    memset(tasks, 0, sizeof(tasks));
    memset(semaphores, 0, sizeof(semaphores));
    memset(message_queues, 0, sizeof(message_queues));
    memset(timers, 0, sizeof(timers));
    memset(rate_monotonic, 0, sizeof(rate_monotonic));
    memset(current_task_id, 0, sizeof(current_task_id));

    /* Initialize SMP subsystem */
    smp_init();

    scheduler_running = true;
}

/* Get current CPU's task ID */
static inline rtems_id get_current_task_id(void) {
    uint32_t cpu = smp_cpu_id();
    return current_task_id[cpu < SMP_MAX_CPUS ? cpu : 0];
}

/* Set current CPU's task ID */
static inline void set_current_task_id(rtems_id id) {
    uint32_t cpu = smp_cpu_id();
    if (cpu < SMP_MAX_CPUS) {
        current_task_id[cpu] = id;
    }
}

/*
 * Clock Functions
 */

rtems_interval rtems_clock_get_ticks_since_boot(void) {
    return system_ticks;
}

rtems_interval rtems_clock_get_ticks_per_second(void) {
    return ticks_per_second;
}

rtems_status_code rtems_clock_get_tod(rtems_time_of_day *time_buffer) {
    if (!time_buffer) return RTEMS_INVALID_ADDRESS;
    /* Simplified: just return ticks as seconds */
    memset(time_buffer, 0, sizeof(*time_buffer));
    time_buffer->second = system_ticks / ticks_per_second;
    time_buffer->ticks = system_ticks % ticks_per_second;
    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_clock_set(const rtems_time_of_day *time_buffer) {
    (void)time_buffer;
    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_clock_get_uptime(struct timespec *uptime) {
    if (!uptime) return RTEMS_INVALID_ADDRESS;

    /* Convert ticks to timespec */
    rtems_interval ticks = system_ticks;
    rtems_interval tps = ticks_per_second;

    uptime->tv_sec = ticks / tps;
    /* Calculate nanoseconds from remaining ticks */
    rtems_interval remaining_ticks = ticks % tps;
    uptime->tv_nsec = (remaining_ticks * 1000000000L) / tps;

    return RTEMS_SUCCESSFUL;
}

/* Get uptime in ticks - used by libc time functions */
uint32_t rtems_clock_get_uptime_ticks(void) {
    return system_ticks;
}

/* Called by timer interrupt or main loop */
static void scheduler_tick(void) {
    system_ticks++;

    /* Check timers */
    for (int i = 0; i < MAX_TIMERS; i++) {
        if (timers[i].active && timers[i].fire_tick <= system_ticks) {
            timers[i].active = false;
            if (timers[i].routine) {
                timers[i].routine(timers[i].id, timers[i].user_data);
            }
        }
    }

    /* Wake sleeping tasks */
    for (int i = 0; i < MAX_TASKS; i++) {
        if (tasks[i].state == TASK_STATE_BLOCKED &&
            tasks[i].wake_tick > 0 &&
            tasks[i].wake_tick <= system_ticks) {
            tasks[i].state = TASK_STATE_READY;
            tasks[i].wake_tick = 0;
        }
    }
}

/*
 * Task Management
 */

static task_tcb_t *find_task(rtems_id id) {
    for (int i = 0; i < MAX_TASKS; i++) {
        if (tasks[i].id == id && tasks[i].state != TASK_STATE_FREE) {
            return &tasks[i];
        }
    }
    return NULL;
}

static task_tcb_t *get_current_task(void) {
    return find_task(get_current_task_id());
}

rtems_status_code rtems_task_create(
    rtems_name           name,
    rtems_task_priority  initial_priority,
    size_t               stack_size,
    rtems_mode           initial_modes,
    rtems_attribute      attribute_set,
    rtems_id            *id
) {
    if (!id) return RTEMS_INVALID_ADDRESS;
    if (initial_priority > RTEMS_MAXIMUM_PRIORITY) return RTEMS_INVALID_PRIORITY;

    /* Find free slot */
    task_tcb_t *task = NULL;
    for (int i = 0; i < MAX_TASKS; i++) {
        if (tasks[i].state == TASK_STATE_FREE) {
            task = &tasks[i];
            break;
        }
    }
    if (!task) return RTEMS_TOO_MANY;

    /* Allocate stack */
    if (stack_size < RTEMS_MINIMUM_STACK_SIZE) {
        stack_size = RTEMS_MINIMUM_STACK_SIZE;
    }
    task->stack = malloc(stack_size);
    if (!task->stack) return RTEMS_NO_MEMORY;

    /* Initialize TCB */
    task->id = next_task_id++;
    task->name = name;
    task->state = TASK_STATE_DORMANT;
    task->priority = initial_priority;
    task->mode = initial_modes;
    task->attributes = attribute_set;
    task->entry = NULL;
    task->argument = 0;
    task->sp = (char *)task->stack + stack_size;
    task->pending_events = 0;
    task->wanted_events = 0;
    task->wake_tick = 0;
    task->blocked_on = 0;

    /* Default affinity: can run on all CPUs (CPU 0 for single-core) */
    CPU_ZERO(&task->affinity);
    CPU_SET(0, &task->affinity);

    *id = task->id;
    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_task_start(
    rtems_id            id,
    rtems_task_entry    entry_point,
    rtems_task_argument argument
) {
    task_tcb_t *task = find_task(id);
    if (!task) return RTEMS_INVALID_ID;
    if (task->state != TASK_STATE_DORMANT) return RTEMS_INCORRECT_STATE;
    if (!entry_point) return RTEMS_INVALID_ADDRESS;

    task->entry = entry_point;
    task->argument = argument;
    task->state = TASK_STATE_READY;

    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_task_delete(rtems_id id) {
    rtems_id my_task = get_current_task_id();
    if (id == RTEMS_SELF) id = my_task;

    task_tcb_t *task = find_task(id);
    if (!task) return RTEMS_INVALID_ID;

    spinlock_acquire(&tasks_lock);
    if (task->stack) {
        free(task->stack);
        task->stack = NULL;
    }
    task->state = TASK_STATE_FREE;
    task->id = 0;
    spinlock_release(&tasks_lock);

    if (id == my_task) {
        set_current_task_id(0);
        schedule();
    }

    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_task_suspend(rtems_id id) {
    rtems_id my_task = get_current_task_id();
    if (id == RTEMS_SELF) id = my_task;

    task_tcb_t *task = find_task(id);
    if (!task) return RTEMS_INVALID_ID;
    if (task->state == TASK_STATE_SUSPENDED) return RTEMS_ALREADY_SUSPENDED;

    spinlock_acquire(&tasks_lock);
    task->state = TASK_STATE_SUSPENDED;
    spinlock_release(&tasks_lock);

    if (id == my_task) {
        schedule();
    }

    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_task_resume(rtems_id id) {
    task_tcb_t *task = find_task(id);
    if (!task) return RTEMS_INVALID_ID;
    if (task->state != TASK_STATE_SUSPENDED) return RTEMS_INCORRECT_STATE;

    task->state = TASK_STATE_READY;
    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_task_set_priority(
    rtems_id             id,
    rtems_task_priority  new_priority,
    rtems_task_priority *old_priority
) {
    if (id == RTEMS_SELF) id = get_current_task_id();

    task_tcb_t *task = find_task(id);
    if (!task) return RTEMS_INVALID_ID;

    spinlock_acquire(&tasks_lock);
    if (old_priority) *old_priority = task->priority;

    if (new_priority != RTEMS_CURRENT_PRIORITY) {
        if (new_priority > RTEMS_MAXIMUM_PRIORITY) {
            spinlock_release(&tasks_lock);
            return RTEMS_INVALID_PRIORITY;
        }
        task->priority = new_priority;
    }
    spinlock_release(&tasks_lock);

    return RTEMS_SUCCESSFUL;
}

rtems_id rtems_task_self(void) {
    return get_current_task_id();
}

rtems_status_code rtems_task_wake_after(rtems_interval ticks) {
    task_tcb_t *task = get_current_task();
    if (!task) return RTEMS_INCORRECT_STATE;

    if (ticks == 0) {
        /* Yield */
        schedule();
    } else {
        task->wake_tick = system_ticks + ticks;
        task->state = TASK_STATE_BLOCKED;
        schedule();
    }

    return RTEMS_SUCCESSFUL;
}

void rtems_task_exit(void) {
    rtems_task_delete(RTEMS_SELF);
}

/*
 * CPU Affinity
 */

rtems_status_code rtems_task_set_affinity(
    rtems_id         id,
    size_t           cpusetsize,
    const cpu_set_t *cpuset
) {
    if (id == RTEMS_SELF) id = get_current_task_id();
    if (!cpuset) return RTEMS_INVALID_ADDRESS;
    if (cpusetsize < sizeof(cpu_set_t)) return RTEMS_INVALID_SIZE;

    task_tcb_t *task = find_task(id);
    if (!task) return RTEMS_INVALID_ID;

    spinlock_acquire(&tasks_lock);
    /* Copy the affinity mask - scheduler will respect this for SMP */
    task->affinity = *cpuset;
    spinlock_release(&tasks_lock);

    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_task_get_affinity(
    rtems_id   id,
    size_t     cpusetsize,
    cpu_set_t *cpuset
) {
    if (id == RTEMS_SELF) id = get_current_task_id();
    if (!cpuset) return RTEMS_INVALID_ADDRESS;
    if (cpusetsize < sizeof(cpu_set_t)) return RTEMS_INVALID_SIZE;

    task_tcb_t *task = find_task(id);
    if (!task) return RTEMS_INVALID_ID;

    spinlock_acquire(&tasks_lock);
    /* Return the affinity mask */
    *cpuset = task->affinity;
    spinlock_release(&tasks_lock);

    return RTEMS_SUCCESSFUL;
}

/*
 * Semaphore Management
 */

static semaphore_t *find_semaphore(rtems_id id) {
    for (int i = 0; i < MAX_SEMAPHORES; i++) {
        if (semaphores[i].id == id) {
            return &semaphores[i];
        }
    }
    return NULL;
}

rtems_status_code rtems_semaphore_create(
    rtems_name       name,
    uint32_t         count,
    rtems_attribute  attribute_set,
    rtems_task_priority priority_ceiling,
    rtems_id        *id
) {
    if (!id) return RTEMS_INVALID_ADDRESS;

    semaphore_t *sem = NULL;
    for (int i = 0; i < MAX_SEMAPHORES; i++) {
        if (semaphores[i].id == 0) {
            sem = &semaphores[i];
            break;
        }
    }
    if (!sem) return RTEMS_TOO_MANY;

    sem->id = next_sem_id++;
    sem->name = name;
    sem->count = count;
    sem->max_count = (attribute_set & RTEMS_BINARY_SEMAPHORE) ? 1 : UINT32_MAX;
    sem->attributes = attribute_set;
    sem->ceiling = priority_ceiling;
    sem->holder = 0;

    *id = sem->id;
    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_semaphore_delete(rtems_id id) {
    semaphore_t *sem = find_semaphore(id);
    if (!sem) return RTEMS_INVALID_ID;

    /* Wake any waiting tasks */
    for (int i = 0; i < MAX_TASKS; i++) {
        if (tasks[i].state == TASK_STATE_BLOCKED && tasks[i].blocked_on == id) {
            tasks[i].state = TASK_STATE_READY;
            tasks[i].blocked_on = 0;
        }
    }

    sem->id = 0;
    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_semaphore_obtain(
    rtems_id       id,
    rtems_option   option_set,
    rtems_interval timeout
) {
    semaphore_t *sem = find_semaphore(id);
    if (!sem) return RTEMS_INVALID_ID;

    spinlock_acquire(&sem_lock);
    if (sem->count > 0) {
        sem->count--;
        if (sem->attributes & RTEMS_BINARY_SEMAPHORE) {
            sem->holder = get_current_task_id();
        }
        spinlock_release(&sem_lock);
        return RTEMS_SUCCESSFUL;
    }
    spinlock_release(&sem_lock);

    if (option_set & RTEMS_NO_WAIT) {
        return RTEMS_UNSATISFIED;
    }

    /* Block */
    task_tcb_t *task = get_current_task();
    if (!task) return RTEMS_INCORRECT_STATE;

    spinlock_acquire(&tasks_lock);
    task->state = TASK_STATE_BLOCKED;
    task->blocked_on = id;
    task->wake_tick = (timeout == RTEMS_NO_TIMEOUT) ? 0 : system_ticks + timeout;
    spinlock_release(&tasks_lock);

    schedule();

    /* Check if we got it or timed out */
    if (task->blocked_on == id) {
        task->blocked_on = 0;
        return RTEMS_TIMEOUT;
    }

    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_semaphore_release(rtems_id id) {
    semaphore_t *sem = find_semaphore(id);
    if (!sem) return RTEMS_INVALID_ID;

    /* Check for waiting tasks */
    for (int i = 0; i < MAX_TASKS; i++) {
        if (tasks[i].state == TASK_STATE_BLOCKED && tasks[i].blocked_on == id) {
            tasks[i].state = TASK_STATE_READY;
            tasks[i].blocked_on = 0;
            if (sem->attributes & RTEMS_BINARY_SEMAPHORE) {
                sem->holder = tasks[i].id;
            }
            return RTEMS_SUCCESSFUL;
        }
    }

    /* No waiters, increment count */
    if (sem->count < sem->max_count) {
        sem->count++;
    }
    sem->holder = 0;

    return RTEMS_SUCCESSFUL;
}

/*
 * Message Queue Management
 */

static message_queue_t *find_mq(rtems_id id) {
    for (int i = 0; i < MAX_MESSAGE_QUEUES; i++) {
        if (message_queues[i].id == id) {
            return &message_queues[i];
        }
    }
    return NULL;
}

rtems_status_code rtems_message_queue_create(
    rtems_name       name,
    uint32_t         count,
    size_t           max_message_size,
    rtems_attribute  attribute_set,
    rtems_id        *id
) {
    if (!id) return RTEMS_INVALID_ADDRESS;

    message_queue_t *mq = NULL;
    for (int i = 0; i < MAX_MESSAGE_QUEUES; i++) {
        if (message_queues[i].id == 0) {
            mq = &message_queues[i];
            break;
        }
    }
    if (!mq) return RTEMS_TOO_MANY;

    mq->id = next_mq_id++;
    mq->name = name;
    mq->max_pending = count;
    mq->max_message_size = max_message_size;
    mq->attributes = attribute_set;
    mq->pending_count = 0;
    mq->head = NULL;
    mq->tail = NULL;

    *id = mq->id;
    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_message_queue_delete(rtems_id id) {
    message_queue_t *mq = find_mq(id);
    if (!mq) return RTEMS_INVALID_ID;

    /* Free all messages */
    msg_entry_t *msg = mq->head;
    while (msg) {
        msg_entry_t *next = msg->next;
        free(msg);
        msg = next;
    }

    /* Wake waiting tasks */
    for (int i = 0; i < MAX_TASKS; i++) {
        if (tasks[i].state == TASK_STATE_BLOCKED && tasks[i].blocked_on == id) {
            tasks[i].state = TASK_STATE_READY;
            tasks[i].blocked_on = 0;
        }
    }

    mq->id = 0;
    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_message_queue_send(
    rtems_id    id,
    const void *buffer,
    size_t      size
) {
    message_queue_t *mq = find_mq(id);
    if (!mq) return RTEMS_INVALID_ID;
    if (!buffer) return RTEMS_INVALID_ADDRESS;
    if (size > mq->max_message_size) return RTEMS_INVALID_SIZE;

    if (mq->pending_count >= mq->max_pending) {
        return RTEMS_TOO_MANY;
    }

    msg_entry_t *msg = malloc(sizeof(msg_entry_t) + size);
    if (!msg) return RTEMS_NO_MEMORY;

    msg->next = NULL;
    msg->size = size;
    memcpy(msg->data, buffer, size);

    if (mq->tail) {
        mq->tail->next = msg;
    } else {
        mq->head = msg;
    }
    mq->tail = msg;
    mq->pending_count++;

    /* Wake first waiting receiver */
    for (int i = 0; i < MAX_TASKS; i++) {
        if (tasks[i].state == TASK_STATE_BLOCKED && tasks[i].blocked_on == id) {
            tasks[i].state = TASK_STATE_READY;
            tasks[i].blocked_on = 0;
            break;
        }
    }

    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_message_queue_receive(
    rtems_id       id,
    void          *buffer,
    size_t        *size,
    rtems_option   option_set,
    rtems_interval timeout
) {
    message_queue_t *mq = find_mq(id);
    if (!mq) return RTEMS_INVALID_ID;
    if (!buffer || !size) return RTEMS_INVALID_ADDRESS;

retry:
    if (mq->head) {
        msg_entry_t *msg = mq->head;
        mq->head = msg->next;
        if (!mq->head) mq->tail = NULL;
        mq->pending_count--;

        if (msg->size > *size) {
            *size = msg->size;
            free(msg);
            return RTEMS_INVALID_SIZE;
        }

        *size = msg->size;
        memcpy(buffer, msg->data, msg->size);
        free(msg);
        return RTEMS_SUCCESSFUL;
    }

    if (option_set & RTEMS_NO_WAIT) {
        return RTEMS_UNSATISFIED;
    }

    /* Block */
    task_tcb_t *task = get_current_task();
    if (!task) return RTEMS_INCORRECT_STATE;

    task->state = TASK_STATE_BLOCKED;
    task->blocked_on = id;
    task->wake_tick = (timeout == RTEMS_NO_TIMEOUT) ? 0 : system_ticks + timeout;

    schedule();

    if (task->blocked_on == id) {
        task->blocked_on = 0;
        return RTEMS_TIMEOUT;
    }

    goto retry;
}

rtems_status_code rtems_message_queue_get_number_pending(
    rtems_id  id,
    uint32_t *count
) {
    message_queue_t *mq = find_mq(id);
    if (!mq) return RTEMS_INVALID_ID;
    if (!count) return RTEMS_INVALID_ADDRESS;

    *count = mq->pending_count;
    return RTEMS_SUCCESSFUL;
}

/*
 * Timer Management
 */

static timer_t *find_timer(rtems_id id) {
    for (int i = 0; i < MAX_TIMERS; i++) {
        if (timers[i].id == id) {
            return &timers[i];
        }
    }
    return NULL;
}

rtems_status_code rtems_timer_create(rtems_name name, rtems_id *id) {
    if (!id) return RTEMS_INVALID_ADDRESS;

    timer_t *timer = NULL;
    for (int i = 0; i < MAX_TIMERS; i++) {
        if (timers[i].id == 0) {
            timer = &timers[i];
            break;
        }
    }
    if (!timer) return RTEMS_TOO_MANY;

    timer->id = next_timer_id++;
    timer->name = name;
    timer->active = false;

    *id = timer->id;
    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_timer_delete(rtems_id id) {
    timer_t *timer = find_timer(id);
    if (!timer) return RTEMS_INVALID_ID;

    timer->id = 0;
    timer->active = false;
    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_timer_fire_after(
    rtems_id                     id,
    rtems_interval               ticks,
    rtems_timer_service_routine  routine,
    void                        *user_data
) {
    timer_t *timer = find_timer(id);
    if (!timer) return RTEMS_INVALID_ID;

    timer->interval = ticks;
    timer->fire_tick = system_ticks + ticks;
    timer->routine = routine;
    timer->user_data = user_data;
    timer->active = true;
    timer->repeat = false;

    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_timer_cancel(rtems_id id) {
    timer_t *timer = find_timer(id);
    if (!timer) return RTEMS_INVALID_ID;

    timer->active = false;
    return RTEMS_SUCCESSFUL;
}

/*
 * Rate Monotonic Management
 */

static timer_t *find_rm(rtems_id id) {
    for (int i = 0; i < MAX_RATE_MONOTONIC; i++) {
        if (rate_monotonic[i].id == id) {
            return &rate_monotonic[i];
        }
    }
    return NULL;
}

rtems_status_code rtems_rate_monotonic_create(rtems_name name, rtems_id *id) {
    if (!id) return RTEMS_INVALID_ADDRESS;

    timer_t *rm = NULL;
    for (int i = 0; i < MAX_RATE_MONOTONIC; i++) {
        if (rate_monotonic[i].id == 0) {
            rm = &rate_monotonic[i];
            break;
        }
    }
    if (!rm) return RTEMS_TOO_MANY;

    rm->id = next_rm_id++;
    rm->name = name;
    rm->active = false;

    *id = rm->id;
    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_rate_monotonic_delete(rtems_id id) {
    timer_t *rm = find_rm(id);
    if (!rm) return RTEMS_INVALID_ID;

    rm->id = 0;
    rm->active = false;
    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_rate_monotonic_period(rtems_id id, rtems_interval length) {
    timer_t *rm = find_rm(id);
    if (!rm) return RTEMS_INVALID_ID;

    if (!rm->active) {
        /* First call - start period */
        rm->interval = length;
        rm->fire_tick = system_ticks + length;
        rm->active = true;
        return RTEMS_SUCCESSFUL;
    }

    /* Subsequent call - wait for period end */
    if (system_ticks < rm->fire_tick) {
        task_tcb_t *task = get_current_task();
        if (task) {
            task->wake_tick = rm->fire_tick;
            task->state = TASK_STATE_BLOCKED;
            schedule();
        }
    }

    /* Check for deadline miss */
    rtems_status_code status = RTEMS_SUCCESSFUL;
    if (system_ticks > rm->fire_tick) {
        status = RTEMS_TIMEOUT;  /* Deadline missed */
    }

    /* Set next period */
    rm->fire_tick += rm->interval;

    return status;
}

rtems_status_code rtems_rate_monotonic_cancel(rtems_id id) {
    timer_t *rm = find_rm(id);
    if (!rm) return RTEMS_INVALID_ID;

    rm->active = false;
    return RTEMS_SUCCESSFUL;
}

/*
 * Event Management
 */

rtems_status_code rtems_event_send(rtems_id id, rtems_event_set event_in) {
    task_tcb_t *task = find_task(id);
    if (!task) return RTEMS_INVALID_ID;

    task->pending_events |= event_in;

    /* Check if task is waiting for these events */
    if (task->state == TASK_STATE_BLOCKED && task->wanted_events) {
        rtems_event_set match;
        if (task->event_option & RTEMS_EVENT_ANY) {
            match = task->pending_events & task->wanted_events;
        } else {
            match = (task->pending_events & task->wanted_events) == task->wanted_events
                    ? task->wanted_events : 0;
        }
        if (match) {
            task->state = TASK_STATE_READY;
        }
    }

    return RTEMS_SUCCESSFUL;
}

rtems_status_code rtems_event_receive(
    rtems_event_set  event_in,
    rtems_option     option_set,
    rtems_interval   ticks,
    rtems_event_set *event_out
) {
    if (!event_out) return RTEMS_INVALID_ADDRESS;

    task_tcb_t *task = get_current_task();
    if (!task) return RTEMS_INCORRECT_STATE;

retry:
    {
        rtems_event_set match;
        if (option_set & RTEMS_EVENT_ANY) {
            match = task->pending_events & event_in;
        } else {
            match = (task->pending_events & event_in) == event_in ? event_in : 0;
        }

        if (match) {
            *event_out = match;
            task->pending_events &= ~match;
            return RTEMS_SUCCESSFUL;
        }
    }

    if (option_set & RTEMS_NO_WAIT) {
        return RTEMS_UNSATISFIED;
    }

    /* Block waiting for events */
    task->wanted_events = event_in;
    task->event_option = option_set;
    task->state = TASK_STATE_BLOCKED;
    task->wake_tick = (ticks == RTEMS_NO_TIMEOUT) ? 0 : system_ticks + ticks;

    schedule();

    task->wanted_events = 0;

    if (task->wake_tick && system_ticks >= task->wake_tick) {
        return RTEMS_TIMEOUT;
    }

    goto retry;
}

/*
 * Scheduler (SMP-aware priority-based)
 */

static void schedule(void) {
    uint32_t cpu = smp_cpu_id();

    spinlock_acquire(&tasks_lock);

    /* Find highest priority ready task that can run on this CPU */
    task_tcb_t *best = NULL;

    for (int i = 0; i < MAX_TASKS; i++) {
        if (tasks[i].state == TASK_STATE_READY) {
            /* Check CPU affinity */
            if (CPU_ISSET(cpu, &tasks[i].affinity)) {
                if (!best || tasks[i].priority < best->priority) {
                    best = &tasks[i];
                }
            }
        }
    }

    if (best) {
        task_tcb_t *prev = get_current_task();
        if (prev && prev->state == TASK_STATE_RUNNING) {
            prev->state = TASK_STATE_READY;
            prev->running_on_cpu = (uint32_t)-1;
        }

        best->state = TASK_STATE_RUNNING;
        best->running_on_cpu = cpu;
        set_current_task_id(best->id);
    }

    spinlock_release(&tasks_lock);
}

/*
 * SMP-aware cooperative scheduler main loop
 * Call this from your main() to run all tasks
 * Each CPU runs its own instance of this loop
 */
void rtems_scheduler_run(void) {
    uint32_t cpu = smp_cpu_id();

    while (scheduler_running) {
        /* Only BSP (CPU 0) handles system tick */
        if (cpu == 0) {
            scheduler_tick();
        }

        spinlock_acquire(&tasks_lock);

        /* Find a ready task that can run on this CPU */
        task_tcb_t *task = NULL;
        for (int i = 0; i < MAX_TASKS; i++) {
            if (tasks[i].state == TASK_STATE_READY && CPU_ISSET(cpu, &tasks[i].affinity)) {
                task = &tasks[i];
                break;
            }
        }

        if (task) {
            task->state = TASK_STATE_RUNNING;
            task->running_on_cpu = cpu;
            set_current_task_id(task->id);
            spinlock_release(&tasks_lock);

            if (task->entry) {
                task->entry(task->argument);
            }

            /* Task returned - delete it */
            rtems_task_delete(task->id);
        } else {
            spinlock_release(&tasks_lock);
            /* No work - yield to allow other CPUs/tasks */
            __asm__ volatile ("pause");
        }
    }
}

/*
 * Get number of available processors
 */
uint32_t rtems_get_processor_count(void) {
    /* Return configured CPU count (detected via CPUID) */
    extern uint32_t smp_get_configured_cpus(void);
    return smp_get_configured_cpus();
}

/*
 * Get current processor index
 */
uint32_t rtems_get_current_processor(void) {
    return smp_cpu_id();
}

/*
 * Interrupt Management (stubs for bare-metal)
 */

static rtems_interrupt_level interrupt_level = 0;

rtems_interrupt_level _rtems_interrupt_disable(void) {
    rtems_interrupt_level old = interrupt_level;
    interrupt_level = 1;
    __asm__ volatile ("cli" ::: "memory");
    return old;
}

void _rtems_interrupt_enable(rtems_interrupt_level level) {
    interrupt_level = level;
    if (level == 0) {
        __asm__ volatile ("sti" ::: "memory");
    }
}

bool rtems_interrupt_is_in_progress(void) {
    return false;  /* No interrupt support in cooperative mode */
}

/*
 * Workspace (simple malloc wrapper)
 */

void *rtems_workspace_allocate(size_t size) {
    return malloc(size);
}

bool rtems_workspace_free(void *ptr) {
    free(ptr);
    return true;
}

/*
 * Fatal Error
 */

void rtems_fatal(uint32_t fatal_source, uint32_t error_code) {
    (void)fatal_source;
    (void)error_code;
    /* Halt */
    __asm__ volatile ("cli; hlt");
    while(1);
}

void rtems_fatal_error_occurred(uint32_t error_code) {
    rtems_fatal(RTEMS_FATAL_SOURCE_APPLICATION, error_code);
}

/*
 * Status text
 */

const char *rtems_status_text(rtems_status_code code) {
    static const char *texts[] = {
        "RTEMS_SUCCESSFUL",
        "RTEMS_TASK_EXITTED",
        "RTEMS_MP_NOT_CONFIGURED",
        "RTEMS_INVALID_NAME",
        "RTEMS_INVALID_ID",
        "RTEMS_TOO_MANY",
        "RTEMS_TIMEOUT",
        "RTEMS_OBJECT_WAS_DELETED",
        "RTEMS_INVALID_SIZE",
        "RTEMS_INVALID_ADDRESS",
        "RTEMS_INVALID_NUMBER",
        "RTEMS_NOT_DEFINED",
        "RTEMS_RESOURCE_IN_USE",
        "RTEMS_UNSATISFIED",
        "RTEMS_INCORRECT_STATE",
        "RTEMS_ALREADY_SUSPENDED",
        "RTEMS_ILLEGAL_ON_SELF",
        "RTEMS_ILLEGAL_ON_REMOTE_OBJECT",
        "RTEMS_CALLED_FROM_ISR",
        "RTEMS_INVALID_PRIORITY",
    };
    if (code < sizeof(texts)/sizeof(texts[0])) {
        return texts[code];
    }
    return "RTEMS_UNKNOWN";
}
