/*
 * tcelm_adc.h - ADC (Analog-to-Digital Converter) for RTEMS
 *
 * Converts analog voltages to digital values for sensors and measurements.
 */

#ifndef TCELM_ADC_H
#define TCELM_ADC_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __rtems__
#include <rtems.h>
#endif

/*
 * ADC reference source
 */
typedef enum {
    TCELM_ADC_REF_INTERNAL = 0,  /* Internal reference */
    TCELM_ADC_REF_EXTERNAL = 1,  /* External VREF pin */
    TCELM_ADC_REF_VCC      = 2   /* Supply voltage */
} tcelm_adc_reference_t;

/*
 * ADC configuration
 */
typedef struct {
    uint8_t resolution;             /* Bits (8, 10, 12, 16) */
    tcelm_adc_reference_t reference; /* Reference source */
    float reference_voltage;        /* Reference voltage in volts */
    uint32_t sample_rate;           /* Samples per second */
    uint8_t averaging;              /* Number of samples to average */
} tcelm_adc_config_t;

/*
 * ADC sample with timestamp
 */
typedef struct {
    uint8_t channel;                /* Channel number */
    uint16_t value;                 /* Raw ADC value */
    uint32_t timestamp_us;          /* Microseconds since start */
} tcelm_adc_sample_t;

/*
 * Continuous sampling callback
 */
typedef void (*tcelm_adc_callback_t)(tcelm_adc_sample_t sample, void *user_data);

/*
 * Default configuration: 12-bit, 3.3V VCC reference
 */
extern const tcelm_adc_config_t TCELM_ADC_DEFAULT_CONFIG;

/*
 * Initialize ADC subsystem
 */
int tcelm_adc_init(void);

/*
 * Shutdown ADC subsystem
 */
void tcelm_adc_shutdown(void);

/*
 * Open an ADC device
 * device_num: Device number (0, 1, etc.)
 * Returns: 0 on success, -1 on error
 */
int tcelm_adc_open(int device_num);

/*
 * Close an ADC device
 */
int tcelm_adc_close(int device_num);

/*
 * Configure ADC device
 */
int tcelm_adc_configure(int device_num, const tcelm_adc_config_t *config);

/*
 * Read raw value from a channel
 * Returns: Raw ADC value (0 to 2^resolution - 1), -1 on error
 */
int tcelm_adc_read(int device_num, int channel);

/*
 * Read voltage from a channel
 * Returns: Voltage in volts, NaN on error
 */
float tcelm_adc_read_voltage(int device_num, int channel);

/*
 * Read multiple channels at once
 * channels: Array of channel numbers
 * values: Array for results
 * count: Number of channels
 * Returns: 0 on success, -1 on error
 */
int tcelm_adc_read_multiple(int device_num, const int *channels,
                            int *values, size_t count);

/*
 * Start continuous sampling on a channel
 */
int tcelm_adc_start_continuous(int device_num, int channel, uint32_t sample_rate);

/*
 * Stop continuous sampling
 */
int tcelm_adc_stop_continuous(int device_num, int channel);

/*
 * Register callback for continuous samples
 */
int tcelm_adc_set_callback(int device_num, int channel,
                           tcelm_adc_callback_t callback, void *user_data);

/*
 * Get last sample from continuous mode
 */
int tcelm_adc_get_sample(int device_num, int channel, tcelm_adc_sample_t *sample);

#endif /* TCELM_ADC_H */
