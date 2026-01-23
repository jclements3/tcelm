/*
 * tcelm_adc.c - ADC runtime implementation
 *
 * Provides ADC access with RTEMS and Linux backends.
 */

#include "tcelm_adc.h"
#include <string.h>
#include <stdio.h>
#include <math.h>

#ifdef __rtems__
#include <rtems.h>
#include <bsp.h>
#else
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#endif

#define MAX_ADC_DEVICES 4
#define MAX_ADC_CHANNELS 16

const tcelm_adc_config_t TCELM_ADC_DEFAULT_CONFIG = {
    .resolution = 12,
    .reference = TCELM_ADC_REF_VCC,
    .reference_voltage = 3.3f,
    .sample_rate = 1000,
    .averaging = 1
};

typedef struct {
    bool continuous_active;
    tcelm_adc_callback_t callback;
    void *user_data;
    tcelm_adc_sample_t last_sample;
} adc_channel_state_t;

typedef struct {
    bool is_open;
    int fd;
    tcelm_adc_config_t config;
    adc_channel_state_t channels[MAX_ADC_CHANNELS];
} adc_device_state_t;

static adc_device_state_t adc_devices[MAX_ADC_DEVICES];
static bool adc_initialized = false;

int tcelm_adc_init(void) {
    if (adc_initialized) return 0;

    memset(adc_devices, 0, sizeof(adc_devices));
    adc_initialized = true;
    return 0;
}

void tcelm_adc_shutdown(void) {
    for (int i = 0; i < MAX_ADC_DEVICES; i++) {
        if (adc_devices[i].is_open) {
            tcelm_adc_close(i);
        }
    }
    adc_initialized = false;
}

#ifdef __rtems__

int tcelm_adc_open(int device_num) {
    if (device_num < 0 || device_num >= MAX_ADC_DEVICES) return -1;
    if (adc_devices[device_num].is_open) return 0;

    char path[32];
    snprintf(path, sizeof(path), "/dev/adc%d", device_num);

    int fd = open(path, O_RDWR);
    if (fd < 0) {
        fprintf(stderr, "tcelm_adc_open: Failed to open %s\n", path);
        return -1;
    }

    adc_devices[device_num].fd = fd;
    adc_devices[device_num].is_open = true;
    adc_devices[device_num].config = TCELM_ADC_DEFAULT_CONFIG;
    return 0;
}

int tcelm_adc_close(int device_num) {
    if (device_num < 0 || device_num >= MAX_ADC_DEVICES) return -1;
    if (!adc_devices[device_num].is_open) return 0;

    close(adc_devices[device_num].fd);
    adc_devices[device_num].is_open = false;
    return 0;
}

int tcelm_adc_configure(int device_num, const tcelm_adc_config_t *config) {
    if (device_num < 0 || device_num >= MAX_ADC_DEVICES) return -1;
    if (!adc_devices[device_num].is_open) return -1;
    if (!config) return -1;

    adc_devices[device_num].config = *config;
    /* BSP-specific configuration via ioctl */
    return 0;
}

int tcelm_adc_read(int device_num, int channel) {
    if (device_num < 0 || device_num >= MAX_ADC_DEVICES) return -1;
    if (!adc_devices[device_num].is_open) return -1;
    if (channel < 0 || channel >= MAX_ADC_CHANNELS) return -1;

    /* BSP-specific ADC read via ioctl or read() */
    /* Placeholder: return mid-scale value */
    int resolution = adc_devices[device_num].config.resolution;
    return (1 << (resolution - 1));
}

#else /* Native Linux implementation */

int tcelm_adc_open(int device_num) {
    if (device_num < 0 || device_num >= MAX_ADC_DEVICES) return -1;
    if (adc_devices[device_num].is_open) return 0;

#ifdef __linux__
    /* Linux IIO subsystem */
    char path[64];
    snprintf(path, sizeof(path), "/sys/bus/iio/devices/iio:device%d", device_num);

    /* Check if device exists */
    int fd = open(path, O_RDONLY);
    if (fd < 0) {
        /* Device doesn't exist, use stub mode */
        adc_devices[device_num].fd = -1;
    } else {
        close(fd);
        adc_devices[device_num].fd = device_num;  /* Use device_num as handle */
    }
#else
    adc_devices[device_num].fd = -1;
#endif

    adc_devices[device_num].is_open = true;
    adc_devices[device_num].config = TCELM_ADC_DEFAULT_CONFIG;
    return 0;
}

int tcelm_adc_close(int device_num) {
    if (device_num < 0 || device_num >= MAX_ADC_DEVICES) return -1;
    if (!adc_devices[device_num].is_open) return 0;

    adc_devices[device_num].is_open = false;
    return 0;
}

int tcelm_adc_configure(int device_num, const tcelm_adc_config_t *config) {
    if (device_num < 0 || device_num >= MAX_ADC_DEVICES) return -1;
    if (!adc_devices[device_num].is_open) return -1;
    if (!config) return -1;

    adc_devices[device_num].config = *config;
    return 0;
}

int tcelm_adc_read(int device_num, int channel) {
    if (device_num < 0 || device_num >= MAX_ADC_DEVICES) return -1;
    if (!adc_devices[device_num].is_open) return -1;
    if (channel < 0 || channel >= MAX_ADC_CHANNELS) return -1;

#ifdef __linux__
    if (adc_devices[device_num].fd >= 0) {
        /* Read from IIO sysfs */
        char path[128];
        snprintf(path, sizeof(path),
                 "/sys/bus/iio/devices/iio:device%d/in_voltage%d_raw",
                 device_num, channel);

        int fd = open(path, O_RDONLY);
        if (fd >= 0) {
            char buf[32];
            ssize_t n = read(fd, buf, sizeof(buf) - 1);
            close(fd);

            if (n > 0) {
                buf[n] = '\0';
                return atoi(buf);
            }
        }
    }
#endif

    /* Stub: return random-ish value */
    int resolution = adc_devices[device_num].config.resolution;
    int max_val = (1 << resolution) - 1;
    return (max_val / 2) + ((channel * 123) % (max_val / 4));
}

#endif /* __rtems__ */

/* Common implementations */

float tcelm_adc_read_voltage(int device_num, int channel) {
    int raw = tcelm_adc_read(device_num, channel);
    if (raw < 0) return NAN;

    tcelm_adc_config_t *config = &adc_devices[device_num].config;
    int max_val = (1 << config->resolution) - 1;
    return ((float)raw * config->reference_voltage) / (float)max_val;
}

int tcelm_adc_read_multiple(int device_num, const int *channels,
                            int *values, size_t count) {
    for (size_t i = 0; i < count; i++) {
        values[i] = tcelm_adc_read(device_num, channels[i]);
        if (values[i] < 0) return -1;
    }
    return 0;
}

int tcelm_adc_start_continuous(int device_num, int channel, uint32_t sample_rate) {
    if (device_num < 0 || device_num >= MAX_ADC_DEVICES) return -1;
    if (channel < 0 || channel >= MAX_ADC_CHANNELS) return -1;
    if (!adc_devices[device_num].is_open) return -1;

    adc_devices[device_num].channels[channel].continuous_active = true;
    (void)sample_rate;
    /* Would start DMA or timer-based sampling on real hardware */
    return 0;
}

int tcelm_adc_stop_continuous(int device_num, int channel) {
    if (device_num < 0 || device_num >= MAX_ADC_DEVICES) return -1;
    if (channel < 0 || channel >= MAX_ADC_CHANNELS) return -1;
    if (!adc_devices[device_num].is_open) return -1;

    adc_devices[device_num].channels[channel].continuous_active = false;
    return 0;
}

int tcelm_adc_set_callback(int device_num, int channel,
                           tcelm_adc_callback_t callback, void *user_data) {
    if (device_num < 0 || device_num >= MAX_ADC_DEVICES) return -1;
    if (channel < 0 || channel >= MAX_ADC_CHANNELS) return -1;

    adc_devices[device_num].channels[channel].callback = callback;
    adc_devices[device_num].channels[channel].user_data = user_data;
    return 0;
}

int tcelm_adc_get_sample(int device_num, int channel, tcelm_adc_sample_t *sample) {
    if (device_num < 0 || device_num >= MAX_ADC_DEVICES) return -1;
    if (channel < 0 || channel >= MAX_ADC_CHANNELS) return -1;
    if (!sample) return -1;

    /* Read current value and create sample */
    int raw = tcelm_adc_read(device_num, channel);
    if (raw < 0) return -1;

    sample->channel = (uint8_t)channel;
    sample->value = (uint16_t)raw;
    sample->timestamp_us = 0;  /* Would use real timestamp */

    return 0;
}
