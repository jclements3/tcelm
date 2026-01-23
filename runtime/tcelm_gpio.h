/*
 * tcelm_gpio.h - GPIO driver for RTEMS
 *
 * Provides platform-agnostic GPIO interface with BSP-specific backends.
 */

#ifndef TCELM_GPIO_H
#define TCELM_GPIO_H

#include "tcelm_types.h"
#include <stdint.h>
#include <stdbool.h>

#ifdef __rtems__
#include <rtems.h>
#endif

/*
 * GPIO pin direction
 */
typedef enum {
    TCELM_GPIO_INPUT,
    TCELM_GPIO_OUTPUT,
    TCELM_GPIO_INPUT_PULLUP,
    TCELM_GPIO_INPUT_PULLDOWN,
    TCELM_GPIO_OUTPUT_OPEN_DRAIN
} tcelm_gpio_direction_t;

/*
 * GPIO edge trigger mode for interrupts
 */
typedef enum {
    TCELM_GPIO_EDGE_NONE,
    TCELM_GPIO_EDGE_RISING,
    TCELM_GPIO_EDGE_FALLING,
    TCELM_GPIO_EDGE_BOTH
} tcelm_gpio_edge_t;

/*
 * GPIO pin handle
 */
typedef struct tcelm_gpio_pin {
    uint32_t port;              /* GPIO port number */
    uint32_t pin;               /* Pin number within port */
    tcelm_gpio_direction_t dir; /* Current direction */
    tcelm_gpio_edge_t edge;     /* Edge trigger mode */
    void *bsp_data;             /* BSP-specific data */
    bool initialized;
} tcelm_gpio_pin_t;

/*
 * GPIO interrupt callback
 */
typedef void (*tcelm_gpio_callback_t)(uint32_t port, uint32_t pin, bool level, void *user_data);

/*
 * GPIO interrupt registration
 */
typedef struct tcelm_gpio_irq {
    tcelm_gpio_pin_t *pin;
    tcelm_gpio_callback_t callback;
    void *user_data;
    tcelm_gpio_edge_t edge;
    bool enabled;
} tcelm_gpio_irq_t;

/*
 * Initialize GPIO subsystem
 */
int tcelm_gpio_init(void);

/*
 * Shutdown GPIO subsystem
 */
void tcelm_gpio_shutdown(void);

/*
 * Open a GPIO pin
 * port: GPIO port number (0, 1, 2, ...)
 * pin: Pin number within port (0-31 typically)
 * dir: Initial direction
 * Returns pin handle or NULL on failure
 */
tcelm_gpio_pin_t *tcelm_gpio_open(
    uint32_t port,
    uint32_t pin,
    tcelm_gpio_direction_t dir
);

/*
 * Close a GPIO pin
 */
void tcelm_gpio_close(tcelm_gpio_pin_t *gpio);

/*
 * Set pin direction
 */
int tcelm_gpio_set_direction(tcelm_gpio_pin_t *gpio, tcelm_gpio_direction_t dir);

/*
 * Read pin value
 * Returns true if high, false if low
 */
bool tcelm_gpio_read(tcelm_gpio_pin_t *gpio);

/*
 * Write pin value
 * value: true for high, false for low
 */
int tcelm_gpio_write(tcelm_gpio_pin_t *gpio, bool value);

/*
 * Toggle pin value
 */
int tcelm_gpio_toggle(tcelm_gpio_pin_t *gpio);

/*
 * Register interrupt callback
 * edge: Which edge(s) to trigger on
 * callback: Function to call on interrupt
 * user_data: Passed to callback
 */
tcelm_gpio_irq_t *tcelm_gpio_register_irq(
    tcelm_gpio_pin_t *gpio,
    tcelm_gpio_edge_t edge,
    tcelm_gpio_callback_t callback,
    void *user_data
);

/*
 * Unregister interrupt callback
 */
void tcelm_gpio_unregister_irq(tcelm_gpio_irq_t *irq);

/*
 * Enable/disable interrupt
 */
int tcelm_gpio_irq_enable(tcelm_gpio_irq_t *irq);
int tcelm_gpio_irq_disable(tcelm_gpio_irq_t *irq);

/*
 * Read multiple pins as a value
 * pins: Array of pin handles
 * count: Number of pins
 * Returns value where bit 0 = first pin, bit 1 = second pin, etc.
 */
uint32_t tcelm_gpio_read_multi(tcelm_gpio_pin_t **pins, uint32_t count);

/*
 * Write multiple pins from a value
 * pins: Array of pin handles
 * count: Number of pins
 * value: Bits to write (bit 0 = first pin, etc.)
 */
int tcelm_gpio_write_multi(tcelm_gpio_pin_t **pins, uint32_t count, uint32_t value);

/*
 * BSP-specific initialization (implemented per-platform)
 */
int tcelm_gpio_bsp_init(void);
void tcelm_gpio_bsp_shutdown(void);

#endif /* TCELM_GPIO_H */
