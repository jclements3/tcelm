/*
 * tcelm_spi.h - SPI (Serial Peripheral Interface) for RTEMS
 *
 * Provides SPI bus access for sensors, displays, SD cards, and other peripherals.
 */

#ifndef TCELM_SPI_H
#define TCELM_SPI_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __rtems__
#include <rtems.h>
#endif

/*
 * SPI modes (CPOL/CPHA)
 */
typedef enum {
    TCELM_SPI_MODE0 = 0,  /* Clock idle low, sample on rising edge */
    TCELM_SPI_MODE1 = 1,  /* Clock idle low, sample on falling edge */
    TCELM_SPI_MODE2 = 2,  /* Clock idle high, sample on falling edge */
    TCELM_SPI_MODE3 = 3   /* Clock idle high, sample on rising edge */
} tcelm_spi_mode_t;

/*
 * SPI bit order
 */
typedef enum {
    TCELM_SPI_MSB_FIRST = 0,
    TCELM_SPI_LSB_FIRST = 1
} tcelm_spi_bit_order_t;

/*
 * SPI configuration
 */
typedef struct {
    uint32_t clock_hz;          /* Clock frequency in Hz */
    tcelm_spi_mode_t mode;      /* Clock mode */
    tcelm_spi_bit_order_t bit_order;  /* Bit transmission order */
    uint8_t bits_per_word;      /* Bits per word (usually 8) */
    bool cs_active_high;        /* CS polarity */
} tcelm_spi_config_t;

/*
 * Default configuration: 1MHz, Mode 0, MSB first, 8 bits
 */
extern const tcelm_spi_config_t TCELM_SPI_DEFAULT_CONFIG;

/*
 * Initialize SPI subsystem
 */
int tcelm_spi_init(void);

/*
 * Shutdown SPI subsystem
 */
void tcelm_spi_shutdown(void);

/*
 * Open an SPI bus
 * bus_num: Bus number (0, 1, etc.)
 * Returns: 0 on success, -1 on error
 */
int tcelm_spi_open(int bus_num);

/*
 * Close an SPI bus
 */
int tcelm_spi_close(int bus_num);

/*
 * Configure SPI bus
 */
int tcelm_spi_configure(int bus_num, const tcelm_spi_config_t *config);

/*
 * Full-duplex transfer
 * tx_data: Data to send
 * rx_data: Buffer for received data
 * len: Number of bytes to transfer
 * Returns: Number of bytes transferred, -1 on error
 */
int tcelm_spi_transfer(int bus_num, int cs_pin,
                       const uint8_t *tx_data, uint8_t *rx_data, size_t len);

/*
 * Write-only transfer (ignore received data)
 */
int tcelm_spi_write(int bus_num, int cs_pin,
                    const uint8_t *data, size_t len);

/*
 * Read-only transfer (send zeros)
 */
int tcelm_spi_read(int bus_num, int cs_pin,
                   uint8_t *data, size_t len);

/*
 * Write then read (half-duplex)
 */
int tcelm_spi_write_read(int bus_num, int cs_pin,
                         const uint8_t *tx_data, size_t tx_len,
                         uint8_t *rx_data, size_t rx_len);

/*
 * Manually assert chip select
 */
int tcelm_spi_select(int bus_num, int cs_pin);

/*
 * Manually deassert chip select
 */
int tcelm_spi_deselect(int bus_num, int cs_pin);

#endif /* TCELM_SPI_H */
