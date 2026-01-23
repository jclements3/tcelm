/*
 * tcelm_dac.h - DAC (Digital-to-Analog Converter) for RTEMS
 *
 * Converts digital values to analog voltages for actuators and control.
 */

#ifndef TCELM_DAC_H
#define TCELM_DAC_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __rtems__
#include <rtems.h>
#endif

/*
 * Waveform types for automatic generation
 */
typedef enum {
    TCELM_DAC_WAVE_SINE     = 0,
    TCELM_DAC_WAVE_SQUARE   = 1,
    TCELM_DAC_WAVE_TRIANGLE = 2,
    TCELM_DAC_WAVE_SAWTOOTH = 3,
    TCELM_DAC_WAVE_NOISE    = 4,
    TCELM_DAC_WAVE_CUSTOM   = 5
} tcelm_dac_waveform_t;

/*
 * DAC configuration
 */
typedef struct {
    uint8_t resolution;         /* Bits (8, 10, 12, 16) */
    float reference_voltage;    /* Output range in volts */
    bool buffered;              /* Use output buffer */
} tcelm_dac_config_t;

/*
 * Waveform configuration
 */
typedef struct {
    tcelm_dac_waveform_t type;
    float frequency;            /* Hz */
    float amplitude;            /* 0.0 - 1.0 */
    float offset;               /* DC offset (0.0 - 1.0) */
    const uint16_t *samples;    /* Custom waveform samples */
    size_t sample_count;        /* Number of custom samples */
    uint32_t sample_rate;       /* Custom sample rate */
} tcelm_dac_waveform_config_t;

/*
 * Default configuration: 12-bit, 3.3V reference, buffered
 */
extern const tcelm_dac_config_t TCELM_DAC_DEFAULT_CONFIG;

/*
 * Initialize DAC subsystem
 */
int tcelm_dac_init(void);

/*
 * Shutdown DAC subsystem
 */
void tcelm_dac_shutdown(void);

/*
 * Open a DAC device
 * device_num: Device number (0, 1, etc.)
 * Returns: 0 on success, -1 on error
 */
int tcelm_dac_open(int device_num);

/*
 * Close a DAC device
 */
int tcelm_dac_close(int device_num);

/*
 * Configure DAC device
 */
int tcelm_dac_configure(int device_num, const tcelm_dac_config_t *config);

/*
 * Write raw value to a channel
 * value: 0 to 2^resolution - 1
 * Returns: 0 on success, -1 on error
 */
int tcelm_dac_write(int device_num, int channel, uint16_t value);

/*
 * Write voltage to a channel
 * Returns: 0 on success, -1 on error
 */
int tcelm_dac_write_voltage(int device_num, int channel, float voltage);

/*
 * Write to multiple channels at once
 * Returns: 0 on success, -1 on error
 */
int tcelm_dac_write_multiple(int device_num,
                             const int *channels, const uint16_t *values,
                             size_t count);

/*
 * Start generating a waveform
 */
int tcelm_dac_start_waveform(int device_num, int channel,
                             const tcelm_dac_waveform_config_t *config);

/*
 * Stop waveform generation
 */
int tcelm_dac_stop_waveform(int device_num, int channel);

/*
 * Check if waveform is active
 */
bool tcelm_dac_is_waveform_active(int device_num, int channel);

#endif /* TCELM_DAC_H */
