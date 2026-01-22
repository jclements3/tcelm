/*
 * tcelm_io.c - IO monad runtime for RTEMS
 *
 * Implementation of I/O operations using RTEMS I/O manager and POSIX APIs.
 */

#include "tcelm_io.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <errno.h>

#ifdef __rtems__
#include <rtems.h>
#include <termios.h>
#include <sys/ioctl.h>
#else
#include <termios.h>
#include <sys/ioctl.h>
#endif

/* Default serial configuration */
const tcelm_serial_config_t TCELM_SERIAL_DEFAULT_CONFIG = {
    .baud_rate = 115200,
    .data_bits = 8,
    .stop_bits = 1,
    .parity = 'N',
    .timeout_ds = 0
};

/* Stored command line arguments */
static int stored_argc = 0;
static char **stored_argv = NULL;

/*
 * Initialize IO subsystem
 */
int tcelm_io_init(void) {
    return 0;
}

/*
 * Shutdown IO subsystem
 */
void tcelm_io_shutdown(void) {
    /* Nothing to do */
}

/*
 * Store command line arguments (call from main)
 */
void tcelm_io_set_args(int argc, char **argv) {
    stored_argc = argc;
    stored_argv = argv;
}

/*
 * ============================================================================
 * CONSOLE I/O
 * ============================================================================
 */

void tcelm_io_print(const char *str) {
    if (str) {
        fputs(str, stdout);
        fflush(stdout);
    }
}

void tcelm_io_println(const char *str) {
    if (str) {
        puts(str);
    } else {
        putchar('\n');
    }
    fflush(stdout);
}

void tcelm_io_printf(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
    fflush(stdout);
}

tcelm_value_t *tcelm_io_readline(tcelm_arena_t *arena) {
    char buffer[4096];

    if (fgets(buffer, sizeof(buffer), stdin) == NULL) {
        return tcelm_string(arena, "");
    }

    /* Remove trailing newline */
    size_t len = strlen(buffer);
    if (len > 0 && buffer[len - 1] == '\n') {
        buffer[len - 1] = '\0';
    }

    return tcelm_string(arena, buffer);
}

tcelm_value_t *tcelm_io_readline_prompt(tcelm_arena_t *arena, const char *prompt) {
    tcelm_io_print(prompt);
    return tcelm_io_readline(arena);
}

bool tcelm_io_input_available(void) {
#ifdef __rtems__
    int n;
    if (ioctl(STDIN_FILENO, FIONREAD, &n) == 0) {
        return n > 0;
    }
    return false;
#else
    fd_set fds;
    struct timeval tv = {0, 0};
    FD_ZERO(&fds);
    FD_SET(STDIN_FILENO, &fds);
    return select(STDIN_FILENO + 1, &fds, NULL, NULL, &tv) > 0;
#endif
}

/*
 * ============================================================================
 * FILE I/O
 * ============================================================================
 */

tcelm_file_t *tcelm_file_open(
    tcelm_arena_t *arena,
    const char *path,
    tcelm_file_mode_t mode
) {
    int flags;
    int fd;

    switch (mode) {
        case TCELM_FILE_READ:
            flags = O_RDONLY;
            break;
        case TCELM_FILE_WRITE:
            flags = O_WRONLY | O_CREAT | O_TRUNC;
            break;
        case TCELM_FILE_APPEND:
            flags = O_WRONLY | O_CREAT | O_APPEND;
            break;
        case TCELM_FILE_READWRITE:
            flags = O_RDWR | O_CREAT;
            break;
        default:
            return NULL;
    }

    fd = open(path, flags, 0644);
    if (fd < 0) {
        return NULL;
    }

    tcelm_file_t *file = tcelm_arena_alloc(arena, sizeof(tcelm_file_t));
    if (!file) {
        close(fd);
        return NULL;
    }

    file->fd = fd;
    file->path = path;
    file->is_open = true;

    return file;
}

int tcelm_file_close(tcelm_file_t *file) {
    if (!file || !file->is_open) {
        return -1;
    }

    int result = close(file->fd);
    file->is_open = false;
    file->fd = -1;

    return result;
}

int tcelm_file_read(tcelm_file_t *file, void *buffer, size_t size) {
    if (!file || !file->is_open) {
        return -1;
    }
    return (int)read(file->fd, buffer, size);
}

tcelm_value_t *tcelm_file_read_all(tcelm_arena_t *arena, const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) {
        return tcelm_custom(arena, TCELM_CTOR_ERR, "Err", 1,
                          tcelm_string(arena, "File not found"));
    }

    int fd = open(path, O_RDONLY);
    if (fd < 0) {
        return tcelm_custom(arena, TCELM_CTOR_ERR, "Err", 1,
                          tcelm_string(arena, "Cannot open file"));
    }

    size_t size = (size_t)st.st_size;
    char *buffer = tcelm_arena_alloc(arena, size + 1);
    if (!buffer) {
        close(fd);
        return tcelm_custom(arena, TCELM_CTOR_ERR, "Err", 1,
                          tcelm_string(arena, "Out of memory"));
    }

    ssize_t bytes_read = read(fd, buffer, size);
    close(fd);

    if (bytes_read < 0) {
        return tcelm_custom(arena, TCELM_CTOR_ERR, "Err", 1,
                          tcelm_string(arena, "Read error"));
    }

    buffer[bytes_read] = '\0';

    return tcelm_custom(arena, TCELM_CTOR_OK, "Ok", 1,
                       tcelm_string(arena, buffer));
}

tcelm_value_t *tcelm_file_read_line(tcelm_arena_t *arena, tcelm_file_t *file) {
    if (!file || !file->is_open) {
        return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
    }

    char buffer[4096];
    size_t pos = 0;
    char c;

    while (pos < sizeof(buffer) - 1) {
        ssize_t n = read(file->fd, &c, 1);
        if (n <= 0) {
            if (pos == 0) {
                return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
            }
            break;
        }
        if (c == '\n') {
            break;
        }
        buffer[pos++] = c;
    }

    buffer[pos] = '\0';
    return tcelm_custom(arena, TCELM_CTOR_JUST, "Just", 1,
                       tcelm_string(arena, buffer));
}

int tcelm_file_write(tcelm_file_t *file, const void *buffer, size_t size) {
    if (!file || !file->is_open) {
        return -1;
    }
    return (int)write(file->fd, buffer, size);
}

int tcelm_file_write_string(tcelm_file_t *file, const char *str) {
    if (!str) return 0;
    return tcelm_file_write(file, str, strlen(str));
}

int tcelm_file_write_all(const char *path, const char *content) {
    int fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (fd < 0) {
        return -1;
    }

    size_t len = strlen(content);
    ssize_t written = write(fd, content, len);
    close(fd);

    return (written == (ssize_t)len) ? 0 : -1;
}

int tcelm_file_seek(tcelm_file_t *file, long offset, int whence) {
    if (!file || !file->is_open) {
        return -1;
    }
    return (lseek(file->fd, offset, whence) >= 0) ? 0 : -1;
}

long tcelm_file_tell(tcelm_file_t *file) {
    if (!file || !file->is_open) {
        return -1;
    }
    return lseek(file->fd, 0, SEEK_CUR);
}

int tcelm_file_flush(tcelm_file_t *file) {
    if (!file || !file->is_open) {
        return -1;
    }
    return fsync(file->fd);
}

bool tcelm_file_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0;
}

long tcelm_file_size(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) {
        return -1;
    }
    return (long)st.st_size;
}

int tcelm_file_delete(const char *path) {
    return unlink(path);
}

/*
 * ============================================================================
 * SERIAL PORT I/O
 * ============================================================================
 */

static speed_t baud_to_speed(uint32_t baud) {
    switch (baud) {
        case 9600:   return B9600;
        case 19200:  return B19200;
        case 38400:  return B38400;
        case 57600:  return B57600;
        case 115200: return B115200;
#ifdef B230400
        case 230400: return B230400;
#endif
#ifdef B460800
        case 460800: return B460800;
#endif
#ifdef B921600
        case 921600: return B921600;
#endif
        default:     return B115200;
    }
}

tcelm_serial_t *tcelm_serial_open(
    tcelm_arena_t *arena,
    int port_num,
    const tcelm_serial_config_t *config
) {
    char device[32];
#ifdef __rtems__
    snprintf(device, sizeof(device), "/dev/ttyS%d", port_num);
#else
    snprintf(device, sizeof(device), "/dev/ttyUSB%d", port_num);
#endif

    int fd = open(device, O_RDWR | O_NOCTTY);
    if (fd < 0) {
        /* Try alternative device names */
        snprintf(device, sizeof(device), "/dev/ttyS%d", port_num);
        fd = open(device, O_RDWR | O_NOCTTY);
        if (fd < 0) {
            return NULL;
        }
    }

    /* Configure serial port */
    struct termios tty;
    if (tcgetattr(fd, &tty) != 0) {
        close(fd);
        return NULL;
    }

    /* Set baud rate */
    speed_t speed = baud_to_speed(config->baud_rate);
    cfsetispeed(&tty, speed);
    cfsetospeed(&tty, speed);

    /* Set data bits */
    tty.c_cflag &= ~CSIZE;
    switch (config->data_bits) {
        case 5: tty.c_cflag |= CS5; break;
        case 6: tty.c_cflag |= CS6; break;
        case 7: tty.c_cflag |= CS7; break;
        default: tty.c_cflag |= CS8; break;
    }

    /* Set stop bits */
    if (config->stop_bits == 2) {
        tty.c_cflag |= CSTOPB;
    } else {
        tty.c_cflag &= ~CSTOPB;
    }

    /* Set parity */
    switch (config->parity) {
        case 'O':
        case 'o':
            tty.c_cflag |= PARENB | PARODD;
            break;
        case 'E':
        case 'e':
            tty.c_cflag |= PARENB;
            tty.c_cflag &= ~PARODD;
            break;
        default:
            tty.c_cflag &= ~PARENB;
            break;
    }

    /* Raw mode */
    tty.c_cflag |= CLOCAL | CREAD;
    tty.c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG);
    tty.c_iflag &= ~(IXON | IXOFF | IXANY | ICRNL | INLCR);
    tty.c_oflag &= ~OPOST;

    /* Timeout */
    tty.c_cc[VMIN] = config->timeout_ds == 0 ? 1 : 0;
    tty.c_cc[VTIME] = config->timeout_ds;

    if (tcsetattr(fd, TCSANOW, &tty) != 0) {
        close(fd);
        return NULL;
    }

    /* Flush buffers */
    tcflush(fd, TCIOFLUSH);

    tcelm_serial_t *serial = tcelm_arena_alloc(arena, sizeof(tcelm_serial_t));
    if (!serial) {
        close(fd);
        return NULL;
    }

    serial->fd = fd;
    serial->port_num = port_num;
    serial->baud_rate = config->baud_rate;
    serial->is_open = true;

    return serial;
}

int tcelm_serial_close(tcelm_serial_t *serial) {
    if (!serial || !serial->is_open) {
        return -1;
    }

    int result = close(serial->fd);
    serial->is_open = false;
    serial->fd = -1;

    return result;
}

int tcelm_serial_write(tcelm_serial_t *serial, const void *buffer, size_t size) {
    if (!serial || !serial->is_open) {
        return -1;
    }
    return (int)write(serial->fd, buffer, size);
}

int tcelm_serial_write_string(tcelm_serial_t *serial, const char *str) {
    if (!str) return 0;
    return tcelm_serial_write(serial, str, strlen(str));
}

int tcelm_serial_read(tcelm_serial_t *serial, void *buffer, size_t size) {
    if (!serial || !serial->is_open) {
        return -1;
    }
    return (int)read(serial->fd, buffer, size);
}

tcelm_value_t *tcelm_serial_read_line(
    tcelm_arena_t *arena,
    tcelm_serial_t *serial,
    size_t max_size
) {
    if (!serial || !serial->is_open) {
        return tcelm_custom(arena, TCELM_CTOR_ERR, "Err", 1,
                          tcelm_string(arena, "Port not open"));
    }

    char *buffer = tcelm_arena_alloc(arena, max_size + 1);
    if (!buffer) {
        return tcelm_custom(arena, TCELM_CTOR_ERR, "Err", 1,
                          tcelm_string(arena, "Out of memory"));
    }

    size_t pos = 0;
    char c;

    while (pos < max_size) {
        ssize_t n = read(serial->fd, &c, 1);
        if (n <= 0) {
            if (n < 0 && errno != EAGAIN) {
                return tcelm_custom(arena, TCELM_CTOR_ERR, "Err", 1,
                                  tcelm_string(arena, "Read error"));
            }
            break;
        }
        if (c == '\n' || c == '\r') {
            break;
        }
        buffer[pos++] = c;
    }

    buffer[pos] = '\0';
    return tcelm_custom(arena, TCELM_CTOR_OK, "Ok", 1,
                       tcelm_string(arena, buffer));
}

int tcelm_serial_flush(tcelm_serial_t *serial) {
    if (!serial || !serial->is_open) {
        return -1;
    }
    return tcflush(serial->fd, TCIOFLUSH);
}

int tcelm_serial_available(tcelm_serial_t *serial) {
    if (!serial || !serial->is_open) {
        return -1;
    }
    int bytes;
    if (ioctl(serial->fd, FIONREAD, &bytes) < 0) {
        return -1;
    }
    return bytes;
}

int tcelm_serial_set_timeout(tcelm_serial_t *serial, uint16_t timeout_ds) {
    if (!serial || !serial->is_open) {
        return -1;
    }

    struct termios tty;
    if (tcgetattr(serial->fd, &tty) != 0) {
        return -1;
    }

    tty.c_cc[VMIN] = timeout_ds == 0 ? 1 : 0;
    tty.c_cc[VTIME] = timeout_ds;

    return tcsetattr(serial->fd, TCSANOW, &tty);
}

/*
 * ============================================================================
 * SYSTEM
 * ============================================================================
 */

tcelm_value_t *tcelm_io_get_args(tcelm_arena_t *arena) {
    tcelm_value_t *list = tcelm_nil(arena);

    for (int i = stored_argc - 1; i >= 0; i--) {
        tcelm_value_t *arg = tcelm_string(arena, stored_argv[i]);
        list = tcelm_cons(arena, arg, list);
    }

    return list;
}

tcelm_value_t *tcelm_io_get_env(tcelm_arena_t *arena, const char *name) {
    const char *value = getenv(name);

    if (!value) {
        return tcelm_custom(arena, TCELM_CTOR_NOTHING, "Nothing", 0);
    }

    return tcelm_custom(arena, TCELM_CTOR_JUST, "Just", 1,
                       tcelm_string(arena, value));
}

void tcelm_io_exit(int code) {
#ifdef __rtems__
    rtems_shutdown_executive(code);
#else
    exit(code);
#endif
}
