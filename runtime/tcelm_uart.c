/*
 * tcelm_uart.c - UART driver for RTEMS
 *
 * Provides UART read/write with interrupt-driven receive support.
 * Supports RTEMS termios and POSIX implementations.
 */

#include "tcelm_uart.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/termiostypes.h>
#include <termios.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#elif defined(__linux__) || defined(__APPLE__)
#include <termios.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <pthread.h>
#include <errno.h>
#endif

/* Default configuration */
const tcelm_uart_config_t TCELM_UART_DEFAULT_CONFIG = {
    .baud_rate = 115200,
    .data_bits = 8,
    .parity = TCELM_UART_PARITY_NONE,
    .stop_bits = TCELM_UART_STOP_1,
    .flow_control = TCELM_UART_FLOW_NONE,
    .rx_buffer_size = 256,
    .tx_buffer_size = 256,
    .timeout_ms = 0  /* Blocking */
};

/* Maximum number of open ports */
#define MAX_UART_PORTS 8

/* Receive buffer size */
#define RX_BUFFER_SIZE 1024

/* Line buffer size */
#define LINE_BUFFER_SIZE 256

/*
 * UART port structure
 */
struct tcelm_uart_port {
    int fd;
    uint32_t port_num;
    char device_name[64];
    tcelm_uart_config_t config;
    bool open;

    /* Receive buffer (circular) */
    uint8_t rx_buffer[RX_BUFFER_SIZE];
    volatile size_t rx_head;
    volatile size_t rx_tail;

    /* Line buffer */
    char line_buffer[LINE_BUFFER_SIZE];
    size_t line_pos;

    /* Callbacks */
    tcelm_uart_rx_callback_t rx_callback;
    void *rx_user_data;
    tcelm_uart_line_callback_t line_callback;
    void *line_user_data;

    /* Statistics */
    tcelm_uart_stats_t stats;

#if defined(__linux__) || defined(__APPLE__)
    /* Receive thread */
    pthread_t rx_thread;
    bool rx_thread_running;
    pthread_mutex_t rx_mutex;
#endif

#ifdef __rtems__
    rtems_id rx_task_id;
    bool rx_task_running;
#endif
};

/* Global state */
static struct {
    bool initialized;
    tcelm_uart_port_t ports[MAX_UART_PORTS];
    tcelm_uart_port_t *console;
} uart_state;

/*
 * Initialize UART subsystem
 */
int tcelm_uart_init(void) {
    if (uart_state.initialized) {
        return 0;
    }

    memset(&uart_state, 0, sizeof(uart_state));
    uart_state.initialized = true;

    return 0;
}

/*
 * Shutdown UART subsystem
 */
void tcelm_uart_shutdown(void) {
    if (!uart_state.initialized) {
        return;
    }

    for (int i = 0; i < MAX_UART_PORTS; i++) {
        if (uart_state.ports[i].open) {
            tcelm_uart_close(&uart_state.ports[i]);
        }
    }

    uart_state.initialized = false;
}

/*
 * Convert baud rate to termios constant
 */
static speed_t baud_to_speed(uint32_t baud) {
    switch (baud) {
        case 300:    return B300;
        case 1200:   return B1200;
        case 2400:   return B2400;
        case 4800:   return B4800;
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

/*
 * Apply configuration to termios
 */
static int apply_config(int fd, const tcelm_uart_config_t *config) {
    struct termios tty;

    if (tcgetattr(fd, &tty) != 0) {
        return -1;
    }

    /* Set baud rate */
    speed_t speed = baud_to_speed(config->baud_rate);
    cfsetispeed(&tty, speed);
    cfsetospeed(&tty, speed);

    /* Data bits */
    tty.c_cflag &= ~CSIZE;
    switch (config->data_bits) {
        case 5: tty.c_cflag |= CS5; break;
        case 6: tty.c_cflag |= CS6; break;
        case 7: tty.c_cflag |= CS7; break;
        case 8:
        default: tty.c_cflag |= CS8; break;
    }

    /* Parity */
    tty.c_cflag &= ~(PARENB | PARODD);
    switch (config->parity) {
        case TCELM_UART_PARITY_ODD:
            tty.c_cflag |= PARENB | PARODD;
            break;
        case TCELM_UART_PARITY_EVEN:
            tty.c_cflag |= PARENB;
            break;
        case TCELM_UART_PARITY_NONE:
        default:
            break;
    }

    /* Stop bits */
    if (config->stop_bits == TCELM_UART_STOP_2) {
        tty.c_cflag |= CSTOPB;
    } else {
        tty.c_cflag &= ~CSTOPB;
    }

    /* Flow control */
#ifdef CRTSCTS
    tty.c_cflag &= ~CRTSCTS;
    if (config->flow_control == TCELM_UART_FLOW_HARDWARE) {
        tty.c_cflag |= CRTSCTS;
    }
#endif

    tty.c_iflag &= ~(IXON | IXOFF | IXANY);
    if (config->flow_control == TCELM_UART_FLOW_SOFTWARE) {
        tty.c_iflag |= IXON | IXOFF;
    }

    /* Raw mode */
    tty.c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG);
    tty.c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL);
    tty.c_oflag &= ~OPOST;

    /* Enable receiver, local mode */
    tty.c_cflag |= CREAD | CLOCAL;

    /* Timeout */
    if (config->timeout_ms == 0) {
        /* Blocking read */
        tty.c_cc[VMIN] = 1;
        tty.c_cc[VTIME] = 0;
    } else {
        /* Timeout in deciseconds */
        tty.c_cc[VMIN] = 0;
        tty.c_cc[VTIME] = (config->timeout_ms + 99) / 100;
    }

    if (tcsetattr(fd, TCSANOW, &tty) != 0) {
        return -1;
    }

    return 0;
}

#if defined(__linux__) || defined(__APPLE__)

/*
 * Receive thread for interrupt-driven operation
 */
static void *uart_rx_thread(void *arg) {
    tcelm_uart_port_t *port = (tcelm_uart_port_t *)arg;
    uint8_t buf[64];

    while (port->rx_thread_running) {
        int n = read(port->fd, buf, sizeof(buf));
        if (n > 0) {
            pthread_mutex_lock(&port->rx_mutex);

            /* Store in circular buffer */
            for (int i = 0; i < n; i++) {
                size_t next = (port->rx_head + 1) % RX_BUFFER_SIZE;
                if (next != port->rx_tail) {
                    port->rx_buffer[port->rx_head] = buf[i];
                    port->rx_head = next;
                } else {
                    port->stats.rx_overruns++;
                }

                /* Check for line callback */
                if (port->line_callback) {
                    if (buf[i] == '\n' || buf[i] == '\r') {
                        if (port->line_pos > 0) {
                            port->line_buffer[port->line_pos] = '\0';
                            port->line_callback(port, port->line_buffer, port->line_user_data);
                            port->line_pos = 0;
                        }
                    } else if (port->line_pos < LINE_BUFFER_SIZE - 1) {
                        port->line_buffer[port->line_pos++] = buf[i];
                    }
                }
            }

            port->stats.bytes_received += n;

            /* Call RX callback */
            if (port->rx_callback) {
                port->rx_callback(port, buf, n, port->rx_user_data);
            }

            pthread_mutex_unlock(&port->rx_mutex);
        } else if (n < 0 && errno != EAGAIN && errno != EWOULDBLOCK) {
            /* Error */
            break;
        }
    }

    return NULL;
}

#endif /* __linux__ || __APPLE__ */

/*
 * Open a UART port by number
 */
tcelm_uart_port_t *tcelm_uart_open(
    uint32_t port_num,
    const tcelm_uart_config_t *config
) {
    char device[64];

#ifdef __rtems__
    snprintf(device, sizeof(device), "/dev/ttyS%u", port_num);
#elif defined(__linux__)
    snprintf(device, sizeof(device), "/dev/ttyS%u", port_num);
#elif defined(__APPLE__)
    snprintf(device, sizeof(device), "/dev/tty.usbserial%u", port_num);
#else
    snprintf(device, sizeof(device), "/dev/ttyS%u", port_num);
#endif

    tcelm_uart_port_t *port = tcelm_uart_open_device(device, config);
    if (port) {
        port->port_num = port_num;
    }
    return port;
}

/*
 * Open UART by device name
 */
tcelm_uart_port_t *tcelm_uart_open_device(
    const char *device,
    const tcelm_uart_config_t *config
) {
    if (!uart_state.initialized) {
        tcelm_uart_init();
    }

    /* Find free slot */
    tcelm_uart_port_t *port = NULL;
    for (int i = 0; i < MAX_UART_PORTS; i++) {
        if (!uart_state.ports[i].open) {
            port = &uart_state.ports[i];
            break;
        }
    }

    if (!port) {
        fprintf(stderr, "No free UART slots\n");
        return NULL;
    }

    /* Open device */
    int fd = open(device, O_RDWR | O_NOCTTY | O_NONBLOCK);
    if (fd < 0) {
        fprintf(stderr, "Failed to open %s: %s\n", device, strerror(errno));
        return NULL;
    }

    /* Clear non-blocking for now */
    int flags = fcntl(fd, F_GETFL, 0);
    fcntl(fd, F_SETFL, flags & ~O_NONBLOCK);

    /* Initialize port structure */
    memset(port, 0, sizeof(*port));
    port->fd = fd;
    strncpy(port->device_name, device, sizeof(port->device_name) - 1);
    port->config = config ? *config : TCELM_UART_DEFAULT_CONFIG;
    port->open = true;

#if defined(__linux__) || defined(__APPLE__)
    pthread_mutex_init(&port->rx_mutex, NULL);
#endif

    /* Apply configuration */
    if (apply_config(fd, &port->config) != 0) {
        fprintf(stderr, "Failed to configure UART\n");
        close(fd);
        port->open = false;
        return NULL;
    }

    /* Flush buffers */
    tcflush(fd, TCIOFLUSH);

    return port;
}

/*
 * Close a UART port
 */
void tcelm_uart_close(tcelm_uart_port_t *port) {
    if (!port || !port->open) {
        return;
    }

#if defined(__linux__) || defined(__APPLE__)
    if (port->rx_thread_running) {
        port->rx_thread_running = false;
        pthread_join(port->rx_thread, NULL);
    }
    pthread_mutex_destroy(&port->rx_mutex);
#endif

    if (port->fd >= 0) {
        close(port->fd);
    }

    port->open = false;
    port->fd = -1;
}

/*
 * Configure UART port
 */
int tcelm_uart_configure(tcelm_uart_port_t *port, const tcelm_uart_config_t *config) {
    if (!port || !port->open || !config) {
        return -1;
    }

    port->config = *config;
    return apply_config(port->fd, config);
}

/*
 * Write data to UART
 */
int tcelm_uart_write(tcelm_uart_port_t *port, const uint8_t *data, size_t length) {
    if (!port || !port->open || !data) {
        return -1;
    }

    int written = write(port->fd, data, length);
    if (written > 0) {
        port->stats.bytes_sent += written;
    }
    return written;
}

/*
 * Write string to UART
 */
int tcelm_uart_write_string(tcelm_uart_port_t *port, const char *str) {
    if (!str) return -1;
    return tcelm_uart_write(port, (const uint8_t *)str, strlen(str));
}

/*
 * Write string with newline
 */
int tcelm_uart_write_line(tcelm_uart_port_t *port, const char *str) {
    int n = tcelm_uart_write_string(port, str);
    if (n >= 0) {
        n += tcelm_uart_write(port, (const uint8_t *)"\r\n", 2);
    }
    return n;
}

/*
 * Read data from UART
 */
int tcelm_uart_read(tcelm_uart_port_t *port, uint8_t *buffer, size_t max_length) {
    if (!port || !port->open || !buffer) {
        return -1;
    }

#if defined(__linux__) || defined(__APPLE__)
    /* If RX thread is running, read from buffer */
    if (port->rx_thread_running) {
        pthread_mutex_lock(&port->rx_mutex);

        size_t count = 0;
        while (count < max_length && port->rx_tail != port->rx_head) {
            buffer[count++] = port->rx_buffer[port->rx_tail];
            port->rx_tail = (port->rx_tail + 1) % RX_BUFFER_SIZE;
        }

        pthread_mutex_unlock(&port->rx_mutex);
        return count;
    }
#endif

    /* Direct read */
    return read(port->fd, buffer, max_length);
}

/*
 * Read up to newline
 */
char *tcelm_uart_read_line(tcelm_uart_port_t *port) {
    if (!port || !port->open) {
        return NULL;
    }

    char *line = malloc(LINE_BUFFER_SIZE);
    if (!line) return NULL;

    int n = tcelm_uart_read_line_buf(port, line, LINE_BUFFER_SIZE);
    if (n <= 0) {
        free(line);
        return NULL;
    }

    return line;
}

/*
 * Read line into buffer
 */
int tcelm_uart_read_line_buf(
    tcelm_uart_port_t *port,
    char *buffer,
    size_t max_length
) {
    if (!port || !port->open || !buffer || max_length == 0) {
        return -1;
    }

    size_t pos = 0;
    uint8_t c;

    while (pos < max_length - 1) {
        int n = tcelm_uart_read(port, &c, 1);
        if (n <= 0) {
            if (pos == 0) return n;
            break;
        }

        if (c == '\n' || c == '\r') {
            /* Skip empty lines */
            if (pos == 0) continue;
            break;
        }

        buffer[pos++] = c;
    }

    buffer[pos] = '\0';
    return pos;
}

/*
 * Non-blocking read
 */
int tcelm_uart_read_nonblock(tcelm_uart_port_t *port, uint8_t *buffer, size_t max_length) {
    if (!port || !port->open || !buffer) {
        return -1;
    }

#if defined(__linux__) || defined(__APPLE__)
    /* If RX thread is running, read from buffer */
    if (port->rx_thread_running) {
        return tcelm_uart_read(port, buffer, max_length);
    }

    /* Set non-blocking */
    int flags = fcntl(port->fd, F_GETFL, 0);
    fcntl(port->fd, F_SETFL, flags | O_NONBLOCK);

    int n = read(port->fd, buffer, max_length);

    /* Restore blocking */
    fcntl(port->fd, F_SETFL, flags);

    if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
        return 0;
    }
    return n;
#else
    return read(port->fd, buffer, max_length);
#endif
}

/*
 * Check how many bytes are available
 */
size_t tcelm_uart_available(tcelm_uart_port_t *port) {
    if (!port || !port->open) {
        return 0;
    }

#if defined(__linux__) || defined(__APPLE__)
    if (port->rx_thread_running) {
        pthread_mutex_lock(&port->rx_mutex);
        size_t avail = (port->rx_head - port->rx_tail + RX_BUFFER_SIZE) % RX_BUFFER_SIZE;
        pthread_mutex_unlock(&port->rx_mutex);
        return avail;
    }

    int avail = 0;
    ioctl(port->fd, FIONREAD, &avail);
    return avail > 0 ? avail : 0;
#else
    return 0;
#endif
}

/*
 * Flush receive buffer
 */
void tcelm_uart_flush_rx(tcelm_uart_port_t *port) {
    if (!port || !port->open) return;

#if defined(__linux__) || defined(__APPLE__)
    pthread_mutex_lock(&port->rx_mutex);
    port->rx_head = port->rx_tail = 0;
    pthread_mutex_unlock(&port->rx_mutex);
#endif

    tcflush(port->fd, TCIFLUSH);
}

/*
 * Flush transmit buffer
 */
void tcelm_uart_flush_tx(tcelm_uart_port_t *port) {
    if (!port || !port->open) return;
    tcdrain(port->fd);
}

/*
 * Set receive callback
 */
int tcelm_uart_set_rx_callback(
    tcelm_uart_port_t *port,
    tcelm_uart_rx_callback_t callback,
    void *user_data
) {
    if (!port || !port->open) return -1;

    port->rx_callback = callback;
    port->rx_user_data = user_data;

#if defined(__linux__) || defined(__APPLE__)
    /* Start RX thread if not running */
    if (callback && !port->rx_thread_running) {
        port->rx_thread_running = true;

        /* Set fd to non-blocking for thread */
        int flags = fcntl(port->fd, F_GETFL, 0);
        fcntl(port->fd, F_SETFL, flags | O_NONBLOCK);

        pthread_create(&port->rx_thread, NULL, uart_rx_thread, port);
    }
#endif

    return 0;
}

/*
 * Set line callback
 */
int tcelm_uart_set_line_callback(
    tcelm_uart_port_t *port,
    tcelm_uart_line_callback_t callback,
    void *user_data
) {
    if (!port || !port->open) return -1;

    port->line_callback = callback;
    port->line_user_data = user_data;
    port->line_pos = 0;

#if defined(__linux__) || defined(__APPLE__)
    /* Start RX thread if not running */
    if (callback && !port->rx_thread_running) {
        port->rx_thread_running = true;

        int flags = fcntl(port->fd, F_GETFL, 0);
        fcntl(port->fd, F_SETFL, flags | O_NONBLOCK);

        pthread_create(&port->rx_thread, NULL, uart_rx_thread, port);
    }
#endif

    return 0;
}

/*
 * Clear callbacks
 */
void tcelm_uart_clear_callbacks(tcelm_uart_port_t *port) {
    if (!port) return;

    port->rx_callback = NULL;
    port->line_callback = NULL;

#if defined(__linux__) || defined(__APPLE__)
    if (port->rx_thread_running) {
        port->rx_thread_running = false;
        pthread_join(port->rx_thread, NULL);

        /* Restore blocking mode */
        int flags = fcntl(port->fd, F_GETFL, 0);
        fcntl(port->fd, F_SETFL, flags & ~O_NONBLOCK);
    }
#endif
}

/*
 * Get statistics
 */
int tcelm_uart_get_stats(tcelm_uart_port_t *port, tcelm_uart_stats_t *stats) {
    if (!port || !stats) return -1;
    *stats = port->stats;
    return 0;
}

/*
 * Reset statistics
 */
void tcelm_uart_reset_stats(tcelm_uart_port_t *port) {
    if (!port) return;
    memset(&port->stats, 0, sizeof(port->stats));
}

/*
 * Get console port
 */
tcelm_uart_port_t *tcelm_uart_console(void) {
    if (!uart_state.console) {
        /* Open console - use stdout as fallback */
        uart_state.console = tcelm_uart_open(0, &TCELM_UART_DEFAULT_CONFIG);
    }
    return uart_state.console;
}

/*
 * Printf to UART port
 */
int tcelm_uart_printf(tcelm_uart_port_t *port, const char *format, ...) {
    char buf[512];
    va_list args;

    va_start(args, format);
    int n = vsnprintf(buf, sizeof(buf), format, args);
    va_end(args);

    if (n > 0) {
        return tcelm_uart_write_string(port, buf);
    }
    return n;
}
