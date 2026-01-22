/*
 * tcelm_io.h - IO monad runtime for RTEMS
 *
 * Provides console I/O, file operations, and serial port access.
 * On RTEMS, uses the RTEMS I/O manager and POSIX file APIs.
 */

#ifndef TCELM_IO_H
#define TCELM_IO_H

#include "tcelm_types.h"
#include "tcelm_arena.h"
#include <stddef.h>

/*
 * Initialize IO subsystem
 */
int tcelm_io_init(void);

/*
 * Shutdown IO subsystem
 */
void tcelm_io_shutdown(void);

/*
 * ============================================================================
 * CONSOLE I/O
 * ============================================================================
 */

/*
 * Print string to stdout (no newline)
 */
void tcelm_io_print(const char *str);

/*
 * Print string to stdout with newline
 */
void tcelm_io_println(const char *str);

/*
 * Print formatted string (printf-style)
 */
void tcelm_io_printf(const char *fmt, ...);

/*
 * Read a line from stdin
 * Returns arena-allocated string
 */
tcelm_value_t *tcelm_io_readline(tcelm_arena_t *arena);

/*
 * Read a line with prompt
 */
tcelm_value_t *tcelm_io_readline_prompt(tcelm_arena_t *arena, const char *prompt);

/*
 * Check if input is available (non-blocking)
 */
bool tcelm_io_input_available(void);

/*
 * ============================================================================
 * FILE I/O
 * ============================================================================
 */

/*
 * File handle
 */
typedef struct tcelm_file {
    int fd;                     /* File descriptor */
    const char *path;           /* File path */
    bool is_open;               /* Is file open? */
} tcelm_file_t;

/*
 * File open modes
 */
typedef enum tcelm_file_mode {
    TCELM_FILE_READ,            /* Read only */
    TCELM_FILE_WRITE,           /* Write only (create/truncate) */
    TCELM_FILE_APPEND,          /* Append (create if not exists) */
    TCELM_FILE_READWRITE        /* Read and write */
} tcelm_file_mode_t;

/*
 * Open a file
 * Returns file handle or NULL on error
 */
tcelm_file_t *tcelm_file_open(
    tcelm_arena_t *arena,
    const char *path,
    tcelm_file_mode_t mode
);

/*
 * Close a file
 */
int tcelm_file_close(tcelm_file_t *file);

/*
 * Read from file
 * Returns bytes read, or -1 on error
 */
int tcelm_file_read(tcelm_file_t *file, void *buffer, size_t size);

/*
 * Read entire file contents as string
 */
tcelm_value_t *tcelm_file_read_all(tcelm_arena_t *arena, const char *path);

/*
 * Read file line by line
 * Returns Nothing at EOF, Just line otherwise
 */
tcelm_value_t *tcelm_file_read_line(tcelm_arena_t *arena, tcelm_file_t *file);

/*
 * Write to file
 * Returns bytes written, or -1 on error
 */
int tcelm_file_write(tcelm_file_t *file, const void *buffer, size_t size);

/*
 * Write string to file
 */
int tcelm_file_write_string(tcelm_file_t *file, const char *str);

/*
 * Write entire string to file (convenience)
 */
int tcelm_file_write_all(const char *path, const char *content);

/*
 * Seek to position
 */
int tcelm_file_seek(tcelm_file_t *file, long offset, int whence);

/*
 * Get current position
 */
long tcelm_file_tell(tcelm_file_t *file);

/*
 * Flush file buffers
 */
int tcelm_file_flush(tcelm_file_t *file);

/*
 * Check if file exists
 */
bool tcelm_file_exists(const char *path);

/*
 * Get file size
 */
long tcelm_file_size(const char *path);

/*
 * Delete a file
 */
int tcelm_file_delete(const char *path);

/*
 * ============================================================================
 * SERIAL PORT I/O
 * ============================================================================
 */

/*
 * Serial port handle
 */
typedef struct tcelm_serial {
    int fd;                     /* File descriptor */
    int port_num;               /* Port number (0 = /dev/ttyS0) */
    uint32_t baud_rate;         /* Current baud rate */
    bool is_open;               /* Is port open? */
} tcelm_serial_t;

/*
 * Baud rate constants
 */
#define TCELM_BAUD_9600     9600
#define TCELM_BAUD_19200    19200
#define TCELM_BAUD_38400    38400
#define TCELM_BAUD_57600    57600
#define TCELM_BAUD_115200   115200
#define TCELM_BAUD_230400   230400
#define TCELM_BAUD_460800   460800
#define TCELM_BAUD_921600   921600

/*
 * Serial port configuration
 */
typedef struct tcelm_serial_config {
    uint32_t baud_rate;         /* Baud rate */
    uint8_t data_bits;          /* Data bits (5-8) */
    uint8_t stop_bits;          /* Stop bits (1-2) */
    char parity;                /* 'N' = none, 'O' = odd, 'E' = even */
    uint16_t timeout_ds;        /* Read timeout in deciseconds (0 = blocking) */
} tcelm_serial_config_t;

/* Default serial configuration: 115200 8N1 */
extern const tcelm_serial_config_t TCELM_SERIAL_DEFAULT_CONFIG;

/*
 * Open a serial port
 */
tcelm_serial_t *tcelm_serial_open(
    tcelm_arena_t *arena,
    int port_num,
    const tcelm_serial_config_t *config
);

/*
 * Close serial port
 */
int tcelm_serial_close(tcelm_serial_t *serial);

/*
 * Write to serial port
 * Returns bytes written, or -1 on error
 */
int tcelm_serial_write(tcelm_serial_t *serial, const void *buffer, size_t size);

/*
 * Write string to serial port
 */
int tcelm_serial_write_string(tcelm_serial_t *serial, const char *str);

/*
 * Read from serial port
 * Returns bytes read, or -1 on error
 */
int tcelm_serial_read(tcelm_serial_t *serial, void *buffer, size_t size);

/*
 * Read line from serial port (until \n or max_size)
 */
tcelm_value_t *tcelm_serial_read_line(
    tcelm_arena_t *arena,
    tcelm_serial_t *serial,
    size_t max_size
);

/*
 * Flush serial port buffers
 */
int tcelm_serial_flush(tcelm_serial_t *serial);

/*
 * Check bytes available to read
 */
int tcelm_serial_available(tcelm_serial_t *serial);

/*
 * Set serial port timeout
 */
int tcelm_serial_set_timeout(tcelm_serial_t *serial, uint16_t timeout_ds);

/*
 * ============================================================================
 * SYSTEM
 * ============================================================================
 */

/*
 * Get command line arguments
 * Returns list of strings
 */
tcelm_value_t *tcelm_io_get_args(tcelm_arena_t *arena);

/*
 * Get environment variable
 * Returns Nothing if not set, Just value otherwise
 */
tcelm_value_t *tcelm_io_get_env(tcelm_arena_t *arena, const char *name);

/*
 * Exit program
 */
void tcelm_io_exit(int code);

#endif /* TCELM_IO_H */
