/*
 * rtems_config.h - RTEMS configuration for tcelm applications
 *
 * Include this file in your main.c before #include <rtems.h>
 */

#ifndef TCELM_RTEMS_CONFIG_H
#define TCELM_RTEMS_CONFIG_H

/*
 * ============================================================================
 * CONSOLE CONFIGURATION
 * ============================================================================
 *
 * Options:
 *   TCELM_CONSOLE_SERIAL  - Serial console (default, most compatible)
 *   TCELM_CONSOLE_VGA     - VGA text mode console
 *   TCELM_CONSOLE_FB      - Framebuffer console (requires BSP support)
 */

#ifndef TCELM_CONSOLE_MODE
#define TCELM_CONSOLE_MODE TCELM_CONSOLE_SERIAL
#endif

#define TCELM_CONSOLE_SERIAL 0
#define TCELM_CONSOLE_VGA    1
#define TCELM_CONSOLE_FB     2

/*
 * ============================================================================
 * VIDEO/DISPLAY CONFIGURATION
 * ============================================================================
 *
 * For framebuffer mode (TCELM_CONSOLE_FB), set resolution:
 *   TCELM_VIDEO_640x480   - VGA
 *   TCELM_VIDEO_800x600   - SVGA
 *   TCELM_VIDEO_1024x768  - XGA
 *   TCELM_VIDEO_1280x1024 - SXGA
 */

#ifndef TCELM_VIDEO_WIDTH
#define TCELM_VIDEO_WIDTH  1280
#endif

#ifndef TCELM_VIDEO_HEIGHT
#define TCELM_VIDEO_HEIGHT 1024
#endif

#ifndef TCELM_VIDEO_DEPTH
#define TCELM_VIDEO_DEPTH  32   /* bits per pixel */
#endif

/*
 * ============================================================================
 * FILESYSTEM CONFIGURATION
 * ============================================================================
 *
 * TCELM_FS_IMFS    - In-memory filesystem (RAM-based, volatile)
 * TCELM_FS_DOSFS   - FAT filesystem (for disk/SD card)
 * TCELM_FS_JFFS2   - Journaling Flash File System 2
 * TCELM_FS_NFS     - Network File System
 */

#ifndef TCELM_FS_TYPE
#define TCELM_FS_TYPE TCELM_FS_IMFS
#endif

#define TCELM_FS_IMFS  0
#define TCELM_FS_DOSFS 1
#define TCELM_FS_JFFS2 2
#define TCELM_FS_NFS   3

/* IMFS size (in-memory filesystem) */
#ifndef TCELM_IMFS_SIZE
#define TCELM_IMFS_SIZE (16 * 1024 * 1024)  /* 16 MB default */
#endif

/*
 * ============================================================================
 * SHELL CONFIGURATION
 * ============================================================================
 */

#ifndef TCELM_SHELL_STACK_SIZE
#define TCELM_SHELL_STACK_SIZE (32 * 1024)  /* 32 KB */
#endif

#ifndef TCELM_SHELL_PRIORITY
#define TCELM_SHELL_PRIORITY 100
#endif

/* Enable shell login prompt */
#ifndef TCELM_SHELL_LOGIN
#define TCELM_SHELL_LOGIN 0
#endif

/* Shell prompt */
#ifndef TCELM_SHELL_PROMPT
#define TCELM_SHELL_PROMPT "tcelm> "
#endif

/*
 * ============================================================================
 * MEMORY CONFIGURATION
 * ============================================================================
 */

/* Workspace size for RTEMS */
#ifndef TCELM_WORKSPACE_SIZE
#define TCELM_WORKSPACE_SIZE (512 * 1024)  /* 512 KB */
#endif

/* Maximum tasks */
#ifndef TCELM_MAX_TASKS
#define TCELM_MAX_TASKS 32
#endif

/* Maximum semaphores */
#ifndef TCELM_MAX_SEMAPHORES
#define TCELM_MAX_SEMAPHORES 64
#endif

/* Maximum message queues */
#ifndef TCELM_MAX_MESSAGE_QUEUES
#define TCELM_MAX_MESSAGE_QUEUES 32
#endif

/* Maximum timers */
#ifndef TCELM_MAX_TIMERS
#define TCELM_MAX_TIMERS 32
#endif

/*
 * ============================================================================
 * NETWORKING (Optional)
 * ============================================================================
 */

#ifdef TCELM_ENABLE_NETWORK

#ifndef TCELM_IP_ADDRESS
#define TCELM_IP_ADDRESS "192.168.1.100"
#endif

#ifndef TCELM_IP_NETMASK
#define TCELM_IP_NETMASK "255.255.255.0"
#endif

#ifndef TCELM_IP_GATEWAY
#define TCELM_IP_GATEWAY "192.168.1.1"
#endif

#endif /* TCELM_ENABLE_NETWORK */

/*
 * ============================================================================
 * RTEMS CONFIGURATION MACROS
 * ============================================================================
 * These set up the RTEMS configuration tables.
 */

#define CONFIGURE_APPLICATION_NEEDS_CONSOLE_DRIVER
#define CONFIGURE_APPLICATION_NEEDS_CLOCK_DRIVER

#ifdef TCELM_ENABLE_NETWORK
#define CONFIGURE_APPLICATION_NEEDS_LIBBLOCK
#endif

#define CONFIGURE_MAXIMUM_TASKS            TCELM_MAX_TASKS
#define CONFIGURE_MAXIMUM_SEMAPHORES       TCELM_MAX_SEMAPHORES
#define CONFIGURE_MAXIMUM_MESSAGE_QUEUES   TCELM_MAX_MESSAGE_QUEUES
#define CONFIGURE_MAXIMUM_TIMERS           TCELM_MAX_TIMERS

#define CONFIGURE_RTEMS_INIT_TASKS_TABLE
#define CONFIGURE_INIT_TASK_STACK_SIZE     (16 * 1024)

/* Filesystem support */
#if TCELM_FS_TYPE == TCELM_FS_IMFS
#define CONFIGURE_APPLICATION_NEEDS_LIBBLOCK
#define CONFIGURE_IMFS_MEMFILE_BYTES_PER_BLOCK 512
#define CONFIGURE_MAXIMUM_FILE_DESCRIPTORS 64
#define CONFIGURE_USE_IMFS_AS_BASE_FILESYSTEM
#endif

#if TCELM_FS_TYPE == TCELM_FS_DOSFS
#define CONFIGURE_APPLICATION_NEEDS_LIBBLOCK
#define CONFIGURE_FILESYSTEM_DOSFS
#define CONFIGURE_MAXIMUM_FILE_DESCRIPTORS 64
#endif

/* Shell support */
#ifdef TCELM_ENABLE_SHELL
#define CONFIGURE_APPLICATION_NEEDS_LIBBLOCK
#define CONFIGURE_SHELL_COMMANDS_INIT
#define CONFIGURE_SHELL_COMMANDS_ALL
#include <rtems/shellconfig.h>
#endif

/* Microseconds per tick - for timing precision */
#ifndef CONFIGURE_MICROSECONDS_PER_TICK
#define CONFIGURE_MICROSECONDS_PER_TICK 1000  /* 1ms = 1000 Hz */
#endif

/* Enable unlimited objects for flexibility */
#define CONFIGURE_UNLIMITED_OBJECTS

/* Init */
#define CONFIGURE_INIT

#endif /* TCELM_RTEMS_CONFIG_H */
