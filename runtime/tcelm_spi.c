/*
 * tcelm_spi.c - SPI runtime implementation
 *
 * Provides SPI bus access with RTEMS and Linux backends.
 */

#include "tcelm_spi.h"
#include <string.h>
#include <stdio.h>

#ifdef __rtems__
#include <rtems.h>
#include <bsp.h>
/* RTEMS SPI driver headers - BSP specific */
#else
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#ifdef __linux__
#include <linux/spi/spidev.h>
#endif
#endif

#define MAX_SPI_BUSES 4

const tcelm_spi_config_t TCELM_SPI_DEFAULT_CONFIG = {
    .clock_hz = 1000000,
    .mode = TCELM_SPI_MODE0,
    .bit_order = TCELM_SPI_MSB_FIRST,
    .bits_per_word = 8,
    .cs_active_high = false
};

typedef struct {
    bool is_open;
    int fd;                     /* File descriptor (Linux) or handle (RTEMS) */
    tcelm_spi_config_t config;
} spi_bus_state_t;

static spi_bus_state_t spi_buses[MAX_SPI_BUSES];
static bool spi_initialized = false;

int tcelm_spi_init(void) {
    if (spi_initialized) return 0;

    memset(spi_buses, 0, sizeof(spi_buses));
    spi_initialized = true;
    return 0;
}

void tcelm_spi_shutdown(void) {
    for (int i = 0; i < MAX_SPI_BUSES; i++) {
        if (spi_buses[i].is_open) {
            tcelm_spi_close(i);
        }
    }
    spi_initialized = false;
}

#ifdef __rtems__

int tcelm_spi_open(int bus_num) {
    if (bus_num < 0 || bus_num >= MAX_SPI_BUSES) return -1;
    if (spi_buses[bus_num].is_open) return 0;

    /* RTEMS: Open SPI bus through BSP driver */
    char path[32];
    snprintf(path, sizeof(path), "/dev/spi%d", bus_num);

    int fd = open(path, O_RDWR);
    if (fd < 0) {
        fprintf(stderr, "tcelm_spi_open: Failed to open %s\n", path);
        return -1;
    }

    spi_buses[bus_num].fd = fd;
    spi_buses[bus_num].is_open = true;
    spi_buses[bus_num].config = TCELM_SPI_DEFAULT_CONFIG;
    return 0;
}

int tcelm_spi_close(int bus_num) {
    if (bus_num < 0 || bus_num >= MAX_SPI_BUSES) return -1;
    if (!spi_buses[bus_num].is_open) return 0;

    close(spi_buses[bus_num].fd);
    spi_buses[bus_num].is_open = false;
    return 0;
}

int tcelm_spi_configure(int bus_num, const tcelm_spi_config_t *config) {
    if (bus_num < 0 || bus_num >= MAX_SPI_BUSES) return -1;
    if (!spi_buses[bus_num].is_open) return -1;
    if (!config) return -1;

    spi_buses[bus_num].config = *config;

    /* BSP-specific SPI configuration via ioctl */
    /* This would use RTEMS SPI framework ioctls */
    return 0;
}

int tcelm_spi_transfer(int bus_num, int cs_pin,
                       const uint8_t *tx_data, uint8_t *rx_data, size_t len) {
    if (bus_num < 0 || bus_num >= MAX_SPI_BUSES) return -1;
    if (!spi_buses[bus_num].is_open) return -1;

    /* RTEMS: Use SPI_IOC_MESSAGE or BSP-specific transfer */
    /* For now, simple read/write implementation */
    (void)cs_pin;

    if (tx_data && rx_data) {
        /* Full duplex - would use ioctl SPI_IOC_MESSAGE */
        memcpy(rx_data, tx_data, len);  /* Placeholder */
    }

    return (int)len;
}

int tcelm_spi_write(int bus_num, int cs_pin,
                    const uint8_t *data, size_t len) {
    if (bus_num < 0 || bus_num >= MAX_SPI_BUSES) return -1;
    if (!spi_buses[bus_num].is_open) return -1;
    (void)cs_pin;

    return (int)write(spi_buses[bus_num].fd, data, len);
}

int tcelm_spi_read(int bus_num, int cs_pin,
                   uint8_t *data, size_t len) {
    if (bus_num < 0 || bus_num >= MAX_SPI_BUSES) return -1;
    if (!spi_buses[bus_num].is_open) return -1;
    (void)cs_pin;

    return (int)read(spi_buses[bus_num].fd, data, len);
}

int tcelm_spi_write_read(int bus_num, int cs_pin,
                         const uint8_t *tx_data, size_t tx_len,
                         uint8_t *rx_data, size_t rx_len) {
    if (tcelm_spi_write(bus_num, cs_pin, tx_data, tx_len) < 0) return -1;
    return tcelm_spi_read(bus_num, cs_pin, rx_data, rx_len);
}

int tcelm_spi_select(int bus_num, int cs_pin) {
    (void)bus_num;
    (void)cs_pin;
    /* BSP-specific CS control via GPIO */
    return 0;
}

int tcelm_spi_deselect(int bus_num, int cs_pin) {
    (void)bus_num;
    (void)cs_pin;
    /* BSP-specific CS control via GPIO */
    return 0;
}

#else /* Native Linux implementation */

int tcelm_spi_open(int bus_num) {
    if (bus_num < 0 || bus_num >= MAX_SPI_BUSES) return -1;
    if (spi_buses[bus_num].is_open) return 0;

#ifdef __linux__
    char path[32];
    snprintf(path, sizeof(path), "/dev/spidev%d.0", bus_num);

    int fd = open(path, O_RDWR);
    if (fd < 0) {
        fprintf(stderr, "tcelm_spi_open: Failed to open %s\n", path);
        return -1;
    }

    spi_buses[bus_num].fd = fd;
    spi_buses[bus_num].is_open = true;
    spi_buses[bus_num].config = TCELM_SPI_DEFAULT_CONFIG;
    return 0;
#else
    /* Stub for other platforms */
    spi_buses[bus_num].fd = -1;
    spi_buses[bus_num].is_open = true;
    spi_buses[bus_num].config = TCELM_SPI_DEFAULT_CONFIG;
    return 0;
#endif
}

int tcelm_spi_close(int bus_num) {
    if (bus_num < 0 || bus_num >= MAX_SPI_BUSES) return -1;
    if (!spi_buses[bus_num].is_open) return 0;

    if (spi_buses[bus_num].fd >= 0) {
        close(spi_buses[bus_num].fd);
    }
    spi_buses[bus_num].is_open = false;
    return 0;
}

int tcelm_spi_configure(int bus_num, const tcelm_spi_config_t *config) {
    if (bus_num < 0 || bus_num >= MAX_SPI_BUSES) return -1;
    if (!spi_buses[bus_num].is_open) return -1;
    if (!config) return -1;

    spi_buses[bus_num].config = *config;

#ifdef __linux__
    int fd = spi_buses[bus_num].fd;
    if (fd < 0) return 0;

    uint8_t mode = (uint8_t)config->mode;
    if (config->bit_order == TCELM_SPI_LSB_FIRST) {
        mode |= SPI_LSB_FIRST;
    }

    ioctl(fd, SPI_IOC_WR_MODE, &mode);
    ioctl(fd, SPI_IOC_WR_BITS_PER_WORD, &config->bits_per_word);
    ioctl(fd, SPI_IOC_WR_MAX_SPEED_HZ, &config->clock_hz);
#endif

    return 0;
}

int tcelm_spi_transfer(int bus_num, int cs_pin,
                       const uint8_t *tx_data, uint8_t *rx_data, size_t len) {
    if (bus_num < 0 || bus_num >= MAX_SPI_BUSES) return -1;
    if (!spi_buses[bus_num].is_open) return -1;
    (void)cs_pin;

#ifdef __linux__
    int fd = spi_buses[bus_num].fd;
    if (fd < 0) {
        if (rx_data && tx_data) memcpy(rx_data, tx_data, len);
        return (int)len;
    }

    struct spi_ioc_transfer tr = {
        .tx_buf = (unsigned long)tx_data,
        .rx_buf = (unsigned long)rx_data,
        .len = len,
        .speed_hz = spi_buses[bus_num].config.clock_hz,
        .bits_per_word = spi_buses[bus_num].config.bits_per_word,
    };

    int ret = ioctl(fd, SPI_IOC_MESSAGE(1), &tr);
    return ret < 0 ? -1 : (int)len;
#else
    if (rx_data && tx_data) memcpy(rx_data, tx_data, len);
    return (int)len;
#endif
}

int tcelm_spi_write(int bus_num, int cs_pin,
                    const uint8_t *data, size_t len) {
    return tcelm_spi_transfer(bus_num, cs_pin, data, NULL, len);
}

int tcelm_spi_read(int bus_num, int cs_pin,
                   uint8_t *data, size_t len) {
    return tcelm_spi_transfer(bus_num, cs_pin, NULL, data, len);
}

int tcelm_spi_write_read(int bus_num, int cs_pin,
                         const uint8_t *tx_data, size_t tx_len,
                         uint8_t *rx_data, size_t rx_len) {
    if (tcelm_spi_write(bus_num, cs_pin, tx_data, tx_len) < 0) return -1;
    return tcelm_spi_read(bus_num, cs_pin, rx_data, rx_len);
}

int tcelm_spi_select(int bus_num, int cs_pin) {
    (void)bus_num;
    (void)cs_pin;
    /* Would use GPIO to control CS on Linux */
    return 0;
}

int tcelm_spi_deselect(int bus_num, int cs_pin) {
    (void)bus_num;
    (void)cs_pin;
    return 0;
}

#endif /* __rtems__ */
