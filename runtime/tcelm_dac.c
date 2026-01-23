/*
 * tcelm_dac.c - DAC runtime implementation
 *
 * Provides DAC access with RTEMS and Linux backends.
 */

#include "tcelm_dac.h"
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

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#define MAX_DAC_DEVICES 4
#define MAX_DAC_CHANNELS 8

const tcelm_dac_config_t TCELM_DAC_DEFAULT_CONFIG = {
    .resolution = 12,
    .reference_voltage = 3.3f,
    .buffered = true
};

typedef struct {
    bool waveform_active;
    tcelm_dac_waveform_config_t waveform_config;
    uint16_t current_value;
} dac_channel_state_t;

typedef struct {
    bool is_open;
    int fd;
    tcelm_dac_config_t config;
    dac_channel_state_t channels[MAX_DAC_CHANNELS];
} dac_device_state_t;

static dac_device_state_t dac_devices[MAX_DAC_DEVICES];
static bool dac_initialized = false;

int tcelm_dac_init(void) {
    if (dac_initialized) return 0;

    memset(dac_devices, 0, sizeof(dac_devices));
    dac_initialized = true;
    return 0;
}

void tcelm_dac_shutdown(void) {
    for (int i = 0; i < MAX_DAC_DEVICES; i++) {
        if (dac_devices[i].is_open) {
            tcelm_dac_close(i);
        }
    }
    dac_initialized = false;
}

#ifdef __rtems__

int tcelm_dac_open(int device_num) {
    if (device_num < 0 || device_num >= MAX_DAC_DEVICES) return -1;
    if (dac_devices[device_num].is_open) return 0;

    char path[32];
    snprintf(path, sizeof(path), "/dev/dac%d", device_num);

    int fd = open(path, O_RDWR);
    if (fd < 0) {
        fprintf(stderr, "tcelm_dac_open: Failed to open %s\n", path);
        return -1;
    }

    dac_devices[device_num].fd = fd;
    dac_devices[device_num].is_open = true;
    dac_devices[device_num].config = TCELM_DAC_DEFAULT_CONFIG;
    return 0;
}

int tcelm_dac_close(int device_num) {
    if (device_num < 0 || device_num >= MAX_DAC_DEVICES) return -1;
    if (!dac_devices[device_num].is_open) return 0;

    /* Stop any active waveforms */
    for (int ch = 0; ch < MAX_DAC_CHANNELS; ch++) {
        if (dac_devices[device_num].channels[ch].waveform_active) {
            tcelm_dac_stop_waveform(device_num, ch);
        }
    }

    close(dac_devices[device_num].fd);
    dac_devices[device_num].is_open = false;
    return 0;
}

int tcelm_dac_configure(int device_num, const tcelm_dac_config_t *config) {
    if (device_num < 0 || device_num >= MAX_DAC_DEVICES) return -1;
    if (!dac_devices[device_num].is_open) return -1;
    if (!config) return -1;

    dac_devices[device_num].config = *config;
    /* BSP-specific configuration via ioctl */
    return 0;
}

int tcelm_dac_write(int device_num, int channel, uint16_t value) {
    if (device_num < 0 || device_num >= MAX_DAC_DEVICES) return -1;
    if (!dac_devices[device_num].is_open) return -1;
    if (channel < 0 || channel >= MAX_DAC_CHANNELS) return -1;

    dac_devices[device_num].channels[channel].current_value = value;

    /* BSP-specific DAC write via ioctl or write() */
    return 0;
}

#else /* Native Linux implementation */

int tcelm_dac_open(int device_num) {
    if (device_num < 0 || device_num >= MAX_DAC_DEVICES) return -1;
    if (dac_devices[device_num].is_open) return 0;

#ifdef __linux__
    /* Linux IIO subsystem for DAC */
    char path[64];
    snprintf(path, sizeof(path), "/sys/bus/iio/devices/iio:device%d", device_num);

    int fd = open(path, O_RDONLY);
    if (fd < 0) {
        dac_devices[device_num].fd = -1;
    } else {
        close(fd);
        dac_devices[device_num].fd = device_num;
    }
#else
    dac_devices[device_num].fd = -1;
#endif

    dac_devices[device_num].is_open = true;
    dac_devices[device_num].config = TCELM_DAC_DEFAULT_CONFIG;
    return 0;
}

int tcelm_dac_close(int device_num) {
    if (device_num < 0 || device_num >= MAX_DAC_DEVICES) return -1;
    if (!dac_devices[device_num].is_open) return 0;

    for (int ch = 0; ch < MAX_DAC_CHANNELS; ch++) {
        if (dac_devices[device_num].channels[ch].waveform_active) {
            tcelm_dac_stop_waveform(device_num, ch);
        }
    }

    dac_devices[device_num].is_open = false;
    return 0;
}

int tcelm_dac_configure(int device_num, const tcelm_dac_config_t *config) {
    if (device_num < 0 || device_num >= MAX_DAC_DEVICES) return -1;
    if (!dac_devices[device_num].is_open) return -1;
    if (!config) return -1;

    dac_devices[device_num].config = *config;
    return 0;
}

int tcelm_dac_write(int device_num, int channel, uint16_t value) {
    if (device_num < 0 || device_num >= MAX_DAC_DEVICES) return -1;
    if (!dac_devices[device_num].is_open) return -1;
    if (channel < 0 || channel >= MAX_DAC_CHANNELS) return -1;

    dac_devices[device_num].channels[channel].current_value = value;

#ifdef __linux__
    if (dac_devices[device_num].fd >= 0) {
        char path[128];
        snprintf(path, sizeof(path),
                 "/sys/bus/iio/devices/iio:device%d/out_voltage%d_raw",
                 device_num, channel);

        int fd = open(path, O_WRONLY);
        if (fd >= 0) {
            char buf[32];
            int n = snprintf(buf, sizeof(buf), "%u", value);
            write(fd, buf, n);
            close(fd);
        }
    }
#endif

    return 0;
}

#endif /* __rtems__ */

/* Common implementations */

int tcelm_dac_write_voltage(int device_num, int channel, float voltage) {
    if (device_num < 0 || device_num >= MAX_DAC_DEVICES) return -1;
    if (!dac_devices[device_num].is_open) return -1;

    tcelm_dac_config_t *config = &dac_devices[device_num].config;

    /* Clamp voltage to valid range */
    if (voltage < 0.0f) voltage = 0.0f;
    if (voltage > config->reference_voltage) voltage = config->reference_voltage;

    /* Convert voltage to raw value */
    int max_val = (1 << config->resolution) - 1;
    uint16_t raw = (uint16_t)((voltage * max_val) / config->reference_voltage);

    return tcelm_dac_write(device_num, channel, raw);
}

int tcelm_dac_write_multiple(int device_num,
                             const int *channels, const uint16_t *values,
                             size_t count) {
    for (size_t i = 0; i < count; i++) {
        if (tcelm_dac_write(device_num, channels[i], values[i]) < 0) {
            return -1;
        }
    }
    return 0;
}

int tcelm_dac_start_waveform(int device_num, int channel,
                             const tcelm_dac_waveform_config_t *config) {
    if (device_num < 0 || device_num >= MAX_DAC_DEVICES) return -1;
    if (channel < 0 || channel >= MAX_DAC_CHANNELS) return -1;
    if (!dac_devices[device_num].is_open) return -1;
    if (!config) return -1;

    dac_channel_state_t *ch = &dac_devices[device_num].channels[channel];
    ch->waveform_config = *config;
    ch->waveform_active = true;

    /* On real hardware, this would configure a timer/DMA for waveform generation */
    /* For now, just store the config */

    return 0;
}

int tcelm_dac_stop_waveform(int device_num, int channel) {
    if (device_num < 0 || device_num >= MAX_DAC_DEVICES) return -1;
    if (channel < 0 || channel >= MAX_DAC_CHANNELS) return -1;
    if (!dac_devices[device_num].is_open) return -1;

    dac_devices[device_num].channels[channel].waveform_active = false;
    return 0;
}

bool tcelm_dac_is_waveform_active(int device_num, int channel) {
    if (device_num < 0 || device_num >= MAX_DAC_DEVICES) return false;
    if (channel < 0 || channel >= MAX_DAC_CHANNELS) return false;
    if (!dac_devices[device_num].is_open) return false;

    return dac_devices[device_num].channels[channel].waveform_active;
}

/*
 * Helper function to generate a waveform sample
 * phase: 0.0 to 1.0 (one complete cycle)
 */
uint16_t tcelm_dac_generate_sample(tcelm_dac_waveform_t type,
                                    float phase, float amplitude, float offset,
                                    int resolution) {
    float value = 0.0f;
    int max_val = (1 << resolution) - 1;

    switch (type) {
        case TCELM_DAC_WAVE_SINE:
            value = sinf(phase * 2.0f * M_PI);
            break;

        case TCELM_DAC_WAVE_SQUARE:
            value = (phase < 0.5f) ? 1.0f : -1.0f;
            break;

        case TCELM_DAC_WAVE_TRIANGLE:
            if (phase < 0.25f) {
                value = phase * 4.0f;
            } else if (phase < 0.75f) {
                value = 1.0f - (phase - 0.25f) * 4.0f;
            } else {
                value = -1.0f + (phase - 0.75f) * 4.0f;
            }
            break;

        case TCELM_DAC_WAVE_SAWTOOTH:
            value = 2.0f * phase - 1.0f;
            break;

        case TCELM_DAC_WAVE_NOISE:
            value = ((float)rand() / RAND_MAX) * 2.0f - 1.0f;
            break;

        default:
            value = 0.0f;
    }

    /* Apply amplitude and offset, convert to raw value */
    float normalized = (value * amplitude + offset + 1.0f) / 2.0f;
    if (normalized < 0.0f) normalized = 0.0f;
    if (normalized > 1.0f) normalized = 1.0f;

    return (uint16_t)(normalized * max_val);
}
