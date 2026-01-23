module Target.RTEMS exposing
    ( generateCode
    , runtimePreamble
    , -- Modular preambles
      coreTypes
    , taskModule
    , clockModule
    , semaphoreModule
    , messageModule
    , timerModule
    , eventModule
    , rmsModule
    , interruptModule
    , -- Elm runtime helpers
      elmStringHelpers
    , elmUnionTypes
    , elmListHelpers
    )

{-| RTEMS target code generation.

This module handles generation of C code for the RTEMS real-time
operating system target.

## RTEMS Modules

RTEMS has a modular design where you include only what you need:

  - **coreTypes** - Basic RTEMS types (always required)
  - **taskModule** - Task management (usually required)
  - **clockModule** - Clock and time services
  - **semaphoreModule** - Semaphores for synchronization
  - **messageModule** - Message queues for IPC
  - **timerModule** - Software timers
  - **eventModule** - Event flags
  - **rmsModule** - Rate Monotonic Scheduler (for periodic tasks)
  - **interruptModule** - Interrupt management

See <https://docs.rtems.org/branches/master/c-user/config/index.html>
for full configuration documentation.

-}

import AST.Source as Src


{-| Generate RTEMS-compatible C code for a module.
Currently delegates to Cli.generateRtemsCode.
-}
generateCode : Src.Module -> String
generateCode ast =
    -- This is a placeholder - the actual implementation remains in Cli.elm
    -- for now, to avoid breaking the build. Full migration will happen in
    -- subsequent refactoring.
    "/* RTEMS target - see Cli.generateRtemsCode */"


{-| Complete runtime preamble for typical RTEMS applications.
Includes core types, task module, clock module, and Elm helpers.
-}
runtimePreamble : String
runtimePreamble =
    String.join "\n"
        [ coreTypes
        , taskModule
        , clockModule
        , elmStringHelpers
        , elmUnionTypes
        , elmListHelpers
        ]



-- RTEMS MODULES


{-| Core RTEMS types - always required.
Includes rtems\_id, rtems\_status\_code, rtems\_name, etc.
-}
coreTypes : String
coreTypes =
    String.join "\n"
        [ "/* RTEMS Core Types */"
        , "#include <stdint.h>"
        , "#include <stdbool.h>"
        , "#include <stddef.h>"
        , ""
        , "typedef uint32_t rtems_id;"
        , "typedef uint32_t rtems_status_code;"
        , "typedef uint32_t rtems_name;"
        , "typedef uint32_t rtems_interval;"
        , "typedef uint32_t rtems_task_priority;"
        , "typedef uint32_t rtems_mode;"
        , "typedef uint32_t rtems_attribute;"
        , "typedef uint32_t rtems_option;"
        , ""
        , "#define RTEMS_SUCCESSFUL 0"
        , "#define RTEMS_TIMEOUT 6"
        , "#define RTEMS_SELF 0"
        , "#define RTEMS_NO_TIMEOUT 0"
        , "#define RTEMS_WAIT 0"
        , "#define RTEMS_NO_WAIT 1"
        , ""
        , "#define rtems_build_name(c1,c2,c3,c4) \\"
        , "    ((uint32_t)(c1)<<24|(uint32_t)(c2)<<16|(uint32_t)(c3)<<8|(uint32_t)(c4))"
        ]


{-| Task management module.
Provides rtems\_task\_create, rtems\_task\_start, rtems\_task\_delete, etc.
-}
taskModule : String
taskModule =
    String.join "\n"
        [ ""
        , "/* RTEMS Task Management */"
        , "typedef uintptr_t rtems_task_argument;"
        , "typedef void (*rtems_task_entry)(rtems_task_argument);"
        , ""
        , "#define RTEMS_MINIMUM_STACK_SIZE 4096"
        , "#define RTEMS_DEFAULT_MODES 0"
        , "#define RTEMS_DEFAULT_ATTRIBUTES 0"
        , ""
        , "extern rtems_status_code rtems_task_create("
        , "    rtems_name name, rtems_task_priority priority,"
        , "    size_t stack_size, rtems_mode modes,"
        , "    rtems_attribute attributes, rtems_id *id);"
        , "extern rtems_status_code rtems_task_start("
        , "    rtems_id id, rtems_task_entry entry, rtems_task_argument arg);"
        , "extern rtems_status_code rtems_task_delete(rtems_id id);"
        , "extern rtems_status_code rtems_task_suspend(rtems_id id);"
        , "extern rtems_status_code rtems_task_resume(rtems_id id);"
        , "extern rtems_status_code rtems_task_set_priority("
        , "    rtems_id id, rtems_task_priority new_priority,"
        , "    rtems_task_priority *old_priority);"
        , "extern rtems_status_code rtems_task_wake_after(rtems_interval ticks);"
        , "extern rtems_id rtems_task_self(void);"
        ]


{-| Clock and time module.
Provides rtems\_clock\_get\_ticks\_per\_second, rtems\_clock\_get\_uptime, etc.
-}
clockModule : String
clockModule =
    String.join "\n"
        [ ""
        , "/* RTEMS Clock Services */"
        , "typedef long time_t;"
        , "struct timespec { time_t tv_sec; long tv_nsec; };"
        , ""
        , "extern rtems_interval rtems_clock_get_ticks_since_boot(void);"
        , "extern rtems_interval rtems_clock_get_ticks_per_second(void);"
        , "extern rtems_status_code rtems_clock_get_uptime(struct timespec *uptime);"
        , ""
        , "#define RTEMS_MILLISECONDS_TO_TICKS(ms) \\"
        , "    ((ms) * rtems_clock_get_ticks_per_second() / 1000)"
        ]


{-| Semaphore module.
Provides rtems\_semaphore\_create, rtems\_semaphore\_obtain, etc.
-}
semaphoreModule : String
semaphoreModule =
    String.join "\n"
        [ ""
        , "/* RTEMS Semaphores */"
        , "#define RTEMS_BINARY_SEMAPHORE 0x10"
        , "#define RTEMS_COUNTING_SEMAPHORE 0"
        , "#define RTEMS_PRIORITY 0x04"
        , "#define RTEMS_FIFO 0"
        , "#define RTEMS_INHERIT_PRIORITY 0x40"
        , ""
        , "extern rtems_status_code rtems_semaphore_create("
        , "    rtems_name name, uint32_t count, rtems_attribute attrs,"
        , "    rtems_task_priority ceiling, rtems_id *id);"
        , "extern rtems_status_code rtems_semaphore_delete(rtems_id id);"
        , "extern rtems_status_code rtems_semaphore_obtain("
        , "    rtems_id id, rtems_option options, rtems_interval timeout);"
        , "extern rtems_status_code rtems_semaphore_release(rtems_id id);"
        ]


{-| Message queue module.
Provides rtems\_message\_queue\_create, rtems\_message\_queue\_send, etc.
-}
messageModule : String
messageModule =
    String.join "\n"
        [ ""
        , "/* RTEMS Message Queues */"
        , "extern rtems_status_code rtems_message_queue_create("
        , "    rtems_name name, uint32_t count, size_t max_size,"
        , "    rtems_attribute attrs, rtems_id *id);"
        , "extern rtems_status_code rtems_message_queue_delete(rtems_id id);"
        , "extern rtems_status_code rtems_message_queue_send("
        , "    rtems_id id, const void *buffer, size_t size);"
        , "extern rtems_status_code rtems_message_queue_receive("
        , "    rtems_id id, void *buffer, size_t *size,"
        , "    rtems_option options, rtems_interval timeout);"
        , "extern rtems_status_code rtems_message_queue_broadcast("
        , "    rtems_id id, const void *buffer, size_t size, uint32_t *count);"
        ]


{-| Timer module.
Provides rtems\_timer\_create, rtems\_timer\_fire\_after, etc.
-}
timerModule : String
timerModule =
    String.join "\n"
        [ ""
        , "/* RTEMS Timers */"
        , "typedef void (*rtems_timer_service_routine)(rtems_id, void *);"
        , ""
        , "extern rtems_status_code rtems_timer_create("
        , "    rtems_name name, rtems_id *id);"
        , "extern rtems_status_code rtems_timer_delete(rtems_id id);"
        , "extern rtems_status_code rtems_timer_fire_after("
        , "    rtems_id id, rtems_interval ticks,"
        , "    rtems_timer_service_routine routine, void *user_data);"
        , "extern rtems_status_code rtems_timer_fire_when("
        , "    rtems_id id, rtems_time_of_day *wall_time,"
        , "    rtems_timer_service_routine routine, void *user_data);"
        , "extern rtems_status_code rtems_timer_cancel(rtems_id id);"
        , "extern rtems_status_code rtems_timer_reset(rtems_id id);"
        ]


{-| Event flags module.
Provides rtems\_event\_send, rtems\_event\_receive, etc.
-}
eventModule : String
eventModule =
    String.join "\n"
        [ ""
        , "/* RTEMS Events */"
        , "typedef uint32_t rtems_event_set;"
        , ""
        , "#define RTEMS_EVENT_0  0x00000001"
        , "#define RTEMS_EVENT_1  0x00000002"
        , "#define RTEMS_EVENT_2  0x00000004"
        , "#define RTEMS_EVENT_3  0x00000008"
        , "#define RTEMS_EVENT_ALL 0x00000000"
        , "#define RTEMS_EVENT_ANY 0x00000002"
        , ""
        , "extern rtems_status_code rtems_event_send("
        , "    rtems_id id, rtems_event_set event_in);"
        , "extern rtems_status_code rtems_event_receive("
        , "    rtems_event_set event_in, rtems_option options,"
        , "    rtems_interval timeout, rtems_event_set *event_out);"
        ]


{-| Rate Monotonic Scheduler module.
For periodic real-time tasks with deadline monitoring.
-}
rmsModule : String
rmsModule =
    String.join "\n"
        [ ""
        , "/* RTEMS Rate Monotonic Scheduler */"
        , "typedef struct {"
        , "    uint32_t count;"
        , "    uint32_t missed_count;"
        , "    uint32_t min_cpu_time;"
        , "    uint32_t max_cpu_time;"
        , "    uint32_t total_cpu_time;"
        , "} rtems_rate_monotonic_period_statistics;"
        , ""
        , "extern rtems_status_code rtems_rate_monotonic_create("
        , "    rtems_name name, rtems_id *id);"
        , "extern rtems_status_code rtems_rate_monotonic_delete(rtems_id id);"
        , "extern rtems_status_code rtems_rate_monotonic_period("
        , "    rtems_id id, rtems_interval length);"
        , "extern rtems_status_code rtems_rate_monotonic_cancel(rtems_id id);"
        , "extern rtems_status_code rtems_rate_monotonic_get_status("
        , "    rtems_id id, rtems_rate_monotonic_period_status *status);"
        , "extern rtems_status_code rtems_rate_monotonic_get_statistics("
        , "    rtems_id id, rtems_rate_monotonic_period_statistics *stats);"
        ]


{-| Interrupt management module.
-}
interruptModule : String
interruptModule =
    String.join "\n"
        [ ""
        , "/* RTEMS Interrupt Management */"
        , "typedef uint32_t rtems_interrupt_level;"
        , ""
        , "extern rtems_interrupt_level _rtems_interrupt_disable(void);"
        , "extern void _rtems_interrupt_enable(rtems_interrupt_level level);"
        , "extern bool rtems_interrupt_is_in_progress(void);"
        , ""
        , "#define rtems_interrupt_disable(level) \\"
        , "    do { (level) = _rtems_interrupt_disable(); } while(0)"
        , "#define rtems_interrupt_enable(level) \\"
        , "    _rtems_interrupt_enable(level)"
        ]



-- ELM RUNTIME HELPERS


{-| String helper functions for Elm.
Single-buffer versions (suitable for embedded with limited memory).
-}
elmStringHelpers : String
elmStringHelpers =
    String.join "\n"
        [ ""
        , "/* Elm String Helpers */"
        , "static int strcmp(const char *a, const char *b) {"
        , "    while (*a && *a == *b) { a++; b++; }"
        , "    return (unsigned char)*a - (unsigned char)*b;"
        , "}"
        , ""
        , "static double elm_strlen(const char *s) {"
        , "    int len = 0; while (*s++) len++;"
        , "    return len;"
        , "}"
        , ""
        , "static char __elm_fromint_buf[32];"
        , "static const char *elm_from_int(int n) {"
        , "    char tmp[32]; int i = 0, j = 0, neg = 0;"
        , "    if (n < 0) { neg = 1; n = -n; }"
        , "    if (n == 0) { __elm_fromint_buf[0] = '0'; __elm_fromint_buf[1] = 0; return __elm_fromint_buf; }"
        , "    while (n > 0) { tmp[i++] = '0' + (n % 10); n /= 10; }"
        , "    if (neg) __elm_fromint_buf[j++] = '-';"
        , "    while (i > 0) __elm_fromint_buf[j++] = tmp[--i];"
        , "    __elm_fromint_buf[j] = 0;"
        , "    return __elm_fromint_buf;"
        , "}"
        , ""
        , "static char __elm_append_buf[512];"
        , "static const char *elm_str_append(const char *a, const char *b) {"
        , "    int i = 0, j = 0;"
        , "    while (a[i] && i < 255) { __elm_append_buf[i] = a[i]; i++; }"
        , "    while (b[j] && i + j < 511) { __elm_append_buf[i + j] = b[j]; j++; }"
        , "    __elm_append_buf[i + j] = 0;"
        , "    return __elm_append_buf;"
        , "}"
        ]


{-| Union type support for Elm custom types (Maybe, Result, etc).
-}
elmUnionTypes : String
elmUnionTypes =
    String.join "\n"
        [ ""
        , "/* Elm Union Types */"
        , "struct elm_union_s;"
        , "typedef struct elm_union_s {"
        , "    int tag;"
        , "    union { double num; struct elm_union_s *child; const char *str; void *ptr; } data;"
        , "    struct elm_union_s *data2;"
        , "} elm_union_t;"
        , ""
        , "static elm_union_t *elm_alloc_union(elm_union_t val) {"
        , "    elm_union_t *p = (elm_union_t*)malloc(sizeof(elm_union_t));"
        , "    *p = val;"
        , "    return p;"
        , "}"
        , ""
        , "/* Built-in type tags */"
        , "#define TAG_Nothing 0"
        , "#define TAG_Just 1"
        , "#define TAG_Err 0"
        , "#define TAG_Ok 1"
        , "#define TAG_LT 0"
        , "#define TAG_EQ 1"
        , "#define TAG_GT 2"
        , ""
        , "static elm_union_t elm_Nothing(void) {"
        , "    elm_union_t r = { .tag = TAG_Nothing, .data = {.num = 0}, .data2 = 0 };"
        , "    return r;"
        , "}"
        , "static elm_union_t elm_Just(elm_union_t v) {"
        , "    elm_union_t r = { .tag = TAG_Just, .data = {.child = elm_alloc_union(v)}, .data2 = 0 };"
        , "    return r;"
        , "}"
        , "static elm_union_t elm_Ok(elm_union_t v) {"
        , "    elm_union_t r = { .tag = TAG_Ok, .data = {.child = elm_alloc_union(v)}, .data2 = 0 };"
        , "    return r;"
        , "}"
        , "static elm_union_t elm_Err(elm_union_t v) {"
        , "    elm_union_t r = { .tag = TAG_Err, .data = {.child = elm_alloc_union(v)}, .data2 = 0 };"
        , "    return r;"
        , "}"
        ]


{-| List type support for Elm.
Fixed-size array implementation suitable for embedded.
-}
elmListHelpers : String
elmListHelpers =
    String.join "\n"
        [ ""
        , "/* Elm List Type */"
        , "#define ELM_LIST_MAX 16"
        , "typedef struct { int length; int data[ELM_LIST_MAX]; } elm_list_t;"
        , ""
        , "static elm_union_t elm_List_head(elm_list_t lst) {"
        , "    if (lst.length > 0) {"
        , "        elm_union_t inner = { .tag = 0, .data = {.num = lst.data[0]}, .data2 = 0 };"
        , "        return elm_Just(inner);"
        , "    }"
        , "    return elm_Nothing();"
        , "}"
        ]
