/*
 * tcelm_i2c.h - I2C (Inter-Integrated Circuit) for RTEMS
 *
 * Two-wire serial bus for sensors, EEPROMs, RTCs, and other peripherals.
 */

#ifndef TCELM_I2C_H
#define TCELM_I2C_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __rtems__
#include <rtems.h>
#endif

/*
 * I2C bus speeds
 */
typedef enum {
    TCELM_I2C_STANDARD   = 100000,   /* 100 kHz */
    TCELM_I2C_FAST       = 400000,   /* 400 kHz */
    TCELM_I2C_FAST_PLUS  = 1000000,  /* 1 MHz */
    TCELM_I2C_HIGH_SPEED = 3400000   /* 3.4 MHz */
} tcelm_i2c_speed_t;

/*
 * I2C configuration
 */
typedef struct {
    tcelm_i2c_speed_t speed;    /* Bus speed */
    bool ten_bit_addr;          /* Use 10-bit addressing */
    uint32_t timeout_ms;        /* Timeout in milliseconds */
} tcelm_i2c_config_t;

/*
 * Default configuration: Fast mode (400kHz), 7-bit addressing
 */
extern const tcelm_i2c_config_t TCELM_I2C_DEFAULT_CONFIG;

/*
 * Initialize I2C subsystem
 */
int tcelm_i2c_init(void);

/*
 * Shutdown I2C subsystem
 */
void tcelm_i2c_shutdown(void);

/*
 * Open an I2C bus
 * bus_num: Bus number (0, 1, etc.)
 * Returns: 0 on success, -1 on error
 */
int tcelm_i2c_open(int bus_num);

/*
 * Close an I2C bus
 */
int tcelm_i2c_close(int bus_num);

/*
 * Configure I2C bus
 */
int tcelm_i2c_configure(int bus_num, const tcelm_i2c_config_t *config);

/*
 * Write data to a device
 * address: 7-bit I2C address
 * data: Data to write
 * len: Number of bytes
 * Returns: Number of bytes written, -1 on error
 */
int tcelm_i2c_write(int bus_num, int address,
                    const uint8_t *data, size_t len);

/*
 * Read data from a device
 * address: 7-bit I2C address
 * data: Buffer for received data
 * len: Number of bytes to read
 * Returns: Number of bytes read, -1 on error
 */
int tcelm_i2c_read(int bus_num, int address,
                   uint8_t *data, size_t len);

/*
 * Combined write-read (repeated start)
 * Common for register reads
 */
int tcelm_i2c_write_read(int bus_num, int address,
                         const uint8_t *tx_data, size_t tx_len,
                         uint8_t *rx_data, size_t rx_len);

/*
 * Write to a single register
 */
int tcelm_i2c_write_reg(int bus_num, int address,
                        uint8_t reg, uint8_t value);

/*
 * Read from a single register
 */
int tcelm_i2c_read_reg(int bus_num, int address,
                       uint8_t reg, uint8_t *value);

/*
 * Read multiple consecutive registers
 */
int tcelm_i2c_read_regs(int bus_num, int address,
                        uint8_t start_reg, uint8_t *data, size_t len);

/*
 * Probe for a device at an address
 * Returns: true if device ACKs, false otherwise
 */
bool tcelm_i2c_probe(int bus_num, int address);

/*
 * Scan bus for all responding devices
 * addresses: Buffer for found addresses
 * max_addresses: Size of buffer
 * Returns: Number of devices found
 */
int tcelm_i2c_scan(int bus_num, int *addresses, size_t max_addresses);

#endif /* TCELM_I2C_H */
