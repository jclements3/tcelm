/*
 * tcelm_uart.h - UART driver for RTEMS
 *
 * Provides UART read/write with interrupt-driven receive support.
 */

#ifndef TCELM_UART_H
#define TCELM_UART_H

#include "tcelm_types.h"
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __rtems__
#include <rtems.h>
#endif

/*
 * UART parity modes
 */
typedef enum {
    TCELM_UART_PARITY_NONE,
    TCELM_UART_PARITY_ODD,
    TCELM_UART_PARITY_EVEN
} tcelm_uart_parity_t;

/*
 * UART stop bits
 */
typedef enum {
    TCELM_UART_STOP_1,
    TCELM_UART_STOP_2
} tcelm_uart_stop_t;

/*
 * UART flow control
 */
typedef enum {
    TCELM_UART_FLOW_NONE,
    TCELM_UART_FLOW_HARDWARE,  /* RTS/CTS */
    TCELM_UART_FLOW_SOFTWARE   /* XON/XOFF */
} tcelm_uart_flow_t;

/*
 * UART configuration
 */
typedef struct tcelm_uart_config {
    uint32_t baud_rate;         /* e.g., 9600, 115200 */
    uint8_t data_bits;          /* 5, 6, 7, or 8 */
    tcelm_uart_parity_t parity;
    tcelm_uart_stop_t stop_bits;
    tcelm_uart_flow_t flow_control;
    uint32_t rx_buffer_size;    /* Receive buffer size (default 256) */
    uint32_t tx_buffer_size;    /* Transmit buffer size (default 256) */
    uint32_t timeout_ms;        /* Read timeout (0 = blocking) */
} tcelm_uart_config_t;

/* Default configuration: 115200 8N1 */
extern const tcelm_uart_config_t TCELM_UART_DEFAULT_CONFIG;

/*
 * UART port handle
 */
typedef struct tcelm_uart_port tcelm_uart_port_t;

/*
 * Receive callback for interrupt-driven operation
 */
typedef void (*tcelm_uart_rx_callback_t)(
    tcelm_uart_port_t *port,
    const uint8_t *data,
    size_t length,
    void *user_data
);

/*
 * Line callback (called when newline received)
 */
typedef void (*tcelm_uart_line_callback_t)(
    tcelm_uart_port_t *port,
    const char *line,
    void *user_data
);

/*
 * Initialize UART subsystem
 */
int tcelm_uart_init(void);

/*
 * Shutdown UART subsystem
 */
void tcelm_uart_shutdown(void);

/*
 * Open a UART port
 * port_num: Port number (0 = /dev/ttyS0, 1 = /dev/ttyS1, etc.)
 * config: Configuration, or NULL for defaults
 * Returns port handle or NULL on failure
 */
tcelm_uart_port_t *tcelm_uart_open(
    uint32_t port_num,
    const tcelm_uart_config_t *config
);

/*
 * Open UART by device name
 * device: Device path (e.g., "/dev/ttyUSB0")
 */
tcelm_uart_port_t *tcelm_uart_open_device(
    const char *device,
    const tcelm_uart_config_t *config
);

/*
 * Close a UART port
 */
void tcelm_uart_close(tcelm_uart_port_t *port);

/*
 * Configure UART port
 */
int tcelm_uart_configure(tcelm_uart_port_t *port, const tcelm_uart_config_t *config);

/*
 * Write data to UART
 * Returns number of bytes written, or -1 on error
 */
int tcelm_uart_write(tcelm_uart_port_t *port, const uint8_t *data, size_t length);

/*
 * Write string to UART
 */
int tcelm_uart_write_string(tcelm_uart_port_t *port, const char *str);

/*
 * Write string with newline
 */
int tcelm_uart_write_line(tcelm_uart_port_t *port, const char *str);

/*
 * Read data from UART (blocking or with configured timeout)
 * Returns number of bytes read, 0 on timeout, -1 on error
 */
int tcelm_uart_read(tcelm_uart_port_t *port, uint8_t *buffer, size_t max_length);

/*
 * Read up to newline (blocking)
 * Returns line without newline, or NULL on error/timeout
 * Caller must free returned string
 */
char *tcelm_uart_read_line(tcelm_uart_port_t *port);

/*
 * Read line into buffer (no allocation)
 * Returns length of line, 0 on timeout, -1 on error
 */
int tcelm_uart_read_line_buf(
    tcelm_uart_port_t *port,
    char *buffer,
    size_t max_length
);

/*
 * Non-blocking read
 * Returns number of bytes available and read, 0 if none available
 */
int tcelm_uart_read_nonblock(tcelm_uart_port_t *port, uint8_t *buffer, size_t max_length);

/*
 * Check how many bytes are available to read
 */
size_t tcelm_uart_available(tcelm_uart_port_t *port);

/*
 * Flush receive buffer
 */
void tcelm_uart_flush_rx(tcelm_uart_port_t *port);

/*
 * Flush transmit buffer (wait for pending data to be sent)
 */
void tcelm_uart_flush_tx(tcelm_uart_port_t *port);

/*
 * Register receive callback (interrupt-driven)
 * callback: Called when data is received
 * user_data: Passed to callback
 */
int tcelm_uart_set_rx_callback(
    tcelm_uart_port_t *port,
    tcelm_uart_rx_callback_t callback,
    void *user_data
);

/*
 * Register line callback (called when newline received)
 */
int tcelm_uart_set_line_callback(
    tcelm_uart_port_t *port,
    tcelm_uart_line_callback_t callback,
    void *user_data
);

/*
 * Clear callbacks
 */
void tcelm_uart_clear_callbacks(tcelm_uart_port_t *port);

/*
 * Get port statistics
 */
typedef struct {
    uint64_t bytes_sent;
    uint64_t bytes_received;
    uint32_t rx_overruns;
    uint32_t framing_errors;
    uint32_t parity_errors;
} tcelm_uart_stats_t;

int tcelm_uart_get_stats(tcelm_uart_port_t *port, tcelm_uart_stats_t *stats);

/*
 * Reset statistics
 */
void tcelm_uart_reset_stats(tcelm_uart_port_t *port);

/*
 * Get console port (for debug output)
 */
tcelm_uart_port_t *tcelm_uart_console(void);

/*
 * Printf to UART port
 */
int tcelm_uart_printf(tcelm_uart_port_t *port, const char *format, ...);

#endif /* TCELM_UART_H */
