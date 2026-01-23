/*
 * tcelm_i2c.c - I2C runtime implementation
 *
 * Provides I2C bus access with RTEMS and Linux backends.
 */

#include "tcelm_i2c.h"
#include <string.h>
#include <stdio.h>

#ifdef __rtems__
#include <rtems.h>
#include <bsp.h>
/* RTEMS I2C driver headers - BSP specific */
#else
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#ifdef __linux__
#include <linux/i2c.h>
#include <linux/i2c-dev.h>
#endif
#endif

#define MAX_I2C_BUSES 4

const tcelm_i2c_config_t TCELM_I2C_DEFAULT_CONFIG = {
    .speed = TCELM_I2C_FAST,
    .ten_bit_addr = false,
    .timeout_ms = 1000
};

typedef struct {
    bool is_open;
    int fd;
    tcelm_i2c_config_t config;
} i2c_bus_state_t;

static i2c_bus_state_t i2c_buses[MAX_I2C_BUSES];
static bool i2c_initialized = false;

int tcelm_i2c_init(void) {
    if (i2c_initialized) return 0;

    memset(i2c_buses, 0, sizeof(i2c_buses));
    i2c_initialized = true;
    return 0;
}

void tcelm_i2c_shutdown(void) {
    for (int i = 0; i < MAX_I2C_BUSES; i++) {
        if (i2c_buses[i].is_open) {
            tcelm_i2c_close(i);
        }
    }
    i2c_initialized = false;
}

#ifdef __rtems__

int tcelm_i2c_open(int bus_num) {
    if (bus_num < 0 || bus_num >= MAX_I2C_BUSES) return -1;
    if (i2c_buses[bus_num].is_open) return 0;

    char path[32];
    snprintf(path, sizeof(path), "/dev/i2c%d", bus_num);

    int fd = open(path, O_RDWR);
    if (fd < 0) {
        fprintf(stderr, "tcelm_i2c_open: Failed to open %s\n", path);
        return -1;
    }

    i2c_buses[bus_num].fd = fd;
    i2c_buses[bus_num].is_open = true;
    i2c_buses[bus_num].config = TCELM_I2C_DEFAULT_CONFIG;
    return 0;
}

int tcelm_i2c_close(int bus_num) {
    if (bus_num < 0 || bus_num >= MAX_I2C_BUSES) return -1;
    if (!i2c_buses[bus_num].is_open) return 0;

    close(i2c_buses[bus_num].fd);
    i2c_buses[bus_num].is_open = false;
    return 0;
}

int tcelm_i2c_configure(int bus_num, const tcelm_i2c_config_t *config) {
    if (bus_num < 0 || bus_num >= MAX_I2C_BUSES) return -1;
    if (!i2c_buses[bus_num].is_open) return -1;
    if (!config) return -1;

    i2c_buses[bus_num].config = *config;
    /* RTEMS: BSP-specific configuration via ioctl */
    return 0;
}

int tcelm_i2c_write(int bus_num, int address,
                    const uint8_t *data, size_t len) {
    if (bus_num < 0 || bus_num >= MAX_I2C_BUSES) return -1;
    if (!i2c_buses[bus_num].is_open) return -1;

    /* Set slave address and write */
    int fd = i2c_buses[bus_num].fd;
    ioctl(fd, 0x0703 /* I2C_SLAVE */, address);
    return (int)write(fd, data, len);
}

int tcelm_i2c_read(int bus_num, int address,
                   uint8_t *data, size_t len) {
    if (bus_num < 0 || bus_num >= MAX_I2C_BUSES) return -1;
    if (!i2c_buses[bus_num].is_open) return -1;

    int fd = i2c_buses[bus_num].fd;
    ioctl(fd, 0x0703 /* I2C_SLAVE */, address);
    return (int)read(fd, data, len);
}

int tcelm_i2c_write_read(int bus_num, int address,
                         const uint8_t *tx_data, size_t tx_len,
                         uint8_t *rx_data, size_t rx_len) {
    if (tcelm_i2c_write(bus_num, address, tx_data, tx_len) < 0) return -1;
    return tcelm_i2c_read(bus_num, address, rx_data, rx_len);
}

#else /* Native Linux implementation */

int tcelm_i2c_open(int bus_num) {
    if (bus_num < 0 || bus_num >= MAX_I2C_BUSES) return -1;
    if (i2c_buses[bus_num].is_open) return 0;

#ifdef __linux__
    char path[32];
    snprintf(path, sizeof(path), "/dev/i2c-%d", bus_num);

    int fd = open(path, O_RDWR);
    if (fd < 0) {
        fprintf(stderr, "tcelm_i2c_open: Failed to open %s\n", path);
        return -1;
    }

    i2c_buses[bus_num].fd = fd;
    i2c_buses[bus_num].is_open = true;
    i2c_buses[bus_num].config = TCELM_I2C_DEFAULT_CONFIG;
    return 0;
#else
    /* Stub for other platforms */
    i2c_buses[bus_num].fd = -1;
    i2c_buses[bus_num].is_open = true;
    i2c_buses[bus_num].config = TCELM_I2C_DEFAULT_CONFIG;
    return 0;
#endif
}

int tcelm_i2c_close(int bus_num) {
    if (bus_num < 0 || bus_num >= MAX_I2C_BUSES) return -1;
    if (!i2c_buses[bus_num].is_open) return 0;

    if (i2c_buses[bus_num].fd >= 0) {
        close(i2c_buses[bus_num].fd);
    }
    i2c_buses[bus_num].is_open = false;
    return 0;
}

int tcelm_i2c_configure(int bus_num, const tcelm_i2c_config_t *config) {
    if (bus_num < 0 || bus_num >= MAX_I2C_BUSES) return -1;
    if (!i2c_buses[bus_num].is_open) return -1;
    if (!config) return -1;

    i2c_buses[bus_num].config = *config;
    return 0;
}

int tcelm_i2c_write(int bus_num, int address,
                    const uint8_t *data, size_t len) {
    if (bus_num < 0 || bus_num >= MAX_I2C_BUSES) return -1;
    if (!i2c_buses[bus_num].is_open) return -1;

#ifdef __linux__
    int fd = i2c_buses[bus_num].fd;
    if (fd < 0) return (int)len;

    ioctl(fd, I2C_SLAVE, address);
    return (int)write(fd, data, len);
#else
    (void)address;
    (void)data;
    return (int)len;
#endif
}

int tcelm_i2c_read(int bus_num, int address,
                   uint8_t *data, size_t len) {
    if (bus_num < 0 || bus_num >= MAX_I2C_BUSES) return -1;
    if (!i2c_buses[bus_num].is_open) return -1;

#ifdef __linux__
    int fd = i2c_buses[bus_num].fd;
    if (fd < 0) {
        memset(data, 0, len);
        return (int)len;
    }

    ioctl(fd, I2C_SLAVE, address);
    return (int)read(fd, data, len);
#else
    (void)address;
    memset(data, 0, len);
    return (int)len;
#endif
}

int tcelm_i2c_write_read(int bus_num, int address,
                         const uint8_t *tx_data, size_t tx_len,
                         uint8_t *rx_data, size_t rx_len) {
#ifdef __linux__
    int fd = i2c_buses[bus_num].fd;
    if (fd >= 0) {
        struct i2c_msg msgs[2] = {
            { .addr = address, .flags = 0, .len = tx_len, .buf = (uint8_t *)tx_data },
            { .addr = address, .flags = I2C_M_RD, .len = rx_len, .buf = rx_data }
        };
        struct i2c_rdwr_ioctl_data rdwr = { .msgs = msgs, .nmsgs = 2 };

        if (ioctl(fd, I2C_RDWR, &rdwr) < 0) {
            return -1;
        }
        return (int)rx_len;
    }
#endif

    if (tcelm_i2c_write(bus_num, address, tx_data, tx_len) < 0) return -1;
    return tcelm_i2c_read(bus_num, address, rx_data, rx_len);
}

#endif /* __rtems__ */

/* Common implementations */

int tcelm_i2c_write_reg(int bus_num, int address,
                        uint8_t reg, uint8_t value) {
    uint8_t data[2] = { reg, value };
    return tcelm_i2c_write(bus_num, address, data, 2);
}

int tcelm_i2c_read_reg(int bus_num, int address,
                       uint8_t reg, uint8_t *value) {
    return tcelm_i2c_write_read(bus_num, address, &reg, 1, value, 1);
}

int tcelm_i2c_read_regs(int bus_num, int address,
                        uint8_t start_reg, uint8_t *data, size_t len) {
    return tcelm_i2c_write_read(bus_num, address, &start_reg, 1, data, len);
}

bool tcelm_i2c_probe(int bus_num, int address) {
    uint8_t dummy;
    return tcelm_i2c_read(bus_num, address, &dummy, 0) >= 0;
}

int tcelm_i2c_scan(int bus_num, int *addresses, size_t max_addresses) {
    int count = 0;

    /* Scan addresses 0x08 to 0x77 (valid 7-bit addresses) */
    for (int addr = 0x08; addr <= 0x77 && (size_t)count < max_addresses; addr++) {
        if (tcelm_i2c_probe(bus_num, addr)) {
            addresses[count++] = addr;
        }
    }

    return count;
}
