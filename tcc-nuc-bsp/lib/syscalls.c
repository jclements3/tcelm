/*
 * syscalls.c - RTEMS syscall stubs for TCC-NUC-BSP
 *
 * These stubs provide the minimal C library interface that TCC expects.
 * They are compiled with the target RTEMS headers at build time.
 *
 * This file should be compiled with:
 *   tcc -c -I<rtems-include> syscalls.c
 *
 * Or for standalone testing, compile with -DSTANDALONE
 */

#ifdef STANDALONE
/* Minimal types for standalone compilation */
typedef long ptrdiff_t;
typedef unsigned int uint32_t;
#define RTEMS_SELF 0
#define ENOMEM 12
#define EINVAL 22
#define S_IFCHR 0020000

struct stat {
    unsigned int st_mode;
    char _padding[60];
};

/* Forward declarations */
extern int write(int, const char*, int);
extern int read(int, char*, int);
extern int close(int);
extern int lseek(int, int, int);
extern void rtems_task_delete(int);
extern int rtems_task_ident(int, int, uint32_t*);

/* Global errno variable (for standalone builds) */
int errno = 0;

#else
/* Real RTEMS build */
#include <rtems.h>
#include <unistd.h>
#include <errno.h>
#include <sys/stat.h>
#include <string.h>
#endif

/*
 * _exit - Terminate the RTEMS task
 */
void _exit(int status)
{
    (void)status;
    rtems_task_delete(RTEMS_SELF);
    /* Should not return, but just in case */
    while (1) {
        __asm__ volatile("hlt");
    }
}

/*
 * _sbrk - Memory allocation (not used - we use arena allocator)
 *
 * TCC might call this for malloc. We return -1 to indicate failure
 * since tcelm uses arena allocation instead.
 */
void *_sbrk(ptrdiff_t incr)
{
    (void)incr;
    errno = ENOMEM;
    return (void *)-1;
}

/*
 * _write - Write to file descriptor
 */
int _write(int fd, const char *buf, int len)
{
    return write(fd, buf, len);
}

/*
 * _read - Read from file descriptor
 */
int _read(int fd, char *buf, int len)
{
    return read(fd, buf, len);
}

/*
 * _close - Close file descriptor
 */
int _close(int fd)
{
    return close(fd);
}

/*
 * _lseek - Seek in file
 */
int _lseek(int fd, int offset, int whence)
{
    return lseek(fd, offset, whence);
}

/*
 * _fstat - Get file status
 */
int _fstat(int fd, struct stat *st)
{
    (void)fd;
#ifdef STANDALONE
    st->st_mode = S_IFCHR;
#else
    memset(st, 0, sizeof(*st));
    st->st_mode = S_IFCHR;  /* Character device (console) */
#endif
    return 0;
}

/*
 * _isatty - Check if fd is a terminal
 */
int _isatty(int fd)
{
    return (fd >= 0 && fd <= 2);  /* stdin, stdout, stderr are ttys */
}

/*
 * _kill - Send signal (minimal stub)
 */
int _kill(int pid, int sig)
{
    (void)pid;
    (void)sig;
    errno = EINVAL;
    return -1;
}

/*
 * _getpid - Get process ID
 */
int _getpid(void)
{
#ifdef STANDALONE
    return 1;
#else
    rtems_id id;
    rtems_task_ident(RTEMS_SELF, RTEMS_SEARCH_LOCAL_NODE, &id);
    return (int)id;
#endif
}

/*
 * abort - Abort execution
 */
void abort(void)
{
    _exit(1);
}
