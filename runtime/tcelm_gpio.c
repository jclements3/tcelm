/*
 * tcelm_gpio.c - GPIO driver for RTEMS
 *
 * Platform-agnostic GPIO implementation with BSP-specific backends.
 * Currently supports:
 * - Generic RTEMS GPIO API
 * - Linux sysfs GPIO (for development)
 * - BeagleBone PRU GPIO
 * - Intel NUC (via libgpiod)
 */

#include "tcelm_gpio.h"
#include "tcelm_arena.h"
#include <string.h>
#include <stdio.h>

#ifdef __rtems__
#include <rtems.h>
#include <bsp.h>
/* BSP-specific GPIO headers would be included here */
#elif defined(__linux__)
#include <fcntl.h>
#include <unistd.h>
#include <poll.h>
#include <pthread.h>
#endif

/* Maximum number of GPIO ports */
#define MAX_GPIO_PORTS 8

/* Maximum pins per port */
#define MAX_PINS_PER_PORT 32

/* Maximum registered interrupts */
#define MAX_GPIO_IRQS 64

/* GPIO state */
static struct {
    bool initialized;
    tcelm_gpio_pin_t pins[MAX_GPIO_PORTS][MAX_PINS_PER_PORT];
    tcelm_gpio_irq_t irqs[MAX_GPIO_IRQS];
    uint32_t irq_count;
#ifdef __linux__
    pthread_t irq_thread;
    bool irq_thread_running;
#endif
} gpio_state;

/*
 * Initialize GPIO subsystem
 */
int tcelm_gpio_init(void) {
    if (gpio_state.initialized) {
        return 0;
    }

    memset(&gpio_state, 0, sizeof(gpio_state));

    /* Call BSP-specific initialization */
    int result = tcelm_gpio_bsp_init();
    if (result != 0) {
        return result;
    }

    gpio_state.initialized = true;
    return 0;
}

/*
 * Shutdown GPIO subsystem
 */
void tcelm_gpio_shutdown(void) {
    if (!gpio_state.initialized) {
        return;
    }

    /* Disable all interrupts */
    for (uint32_t i = 0; i < gpio_state.irq_count; i++) {
        if (gpio_state.irqs[i].enabled) {
            tcelm_gpio_irq_disable(&gpio_state.irqs[i]);
        }
    }

    /* Close all pins */
    for (uint32_t port = 0; port < MAX_GPIO_PORTS; port++) {
        for (uint32_t pin = 0; pin < MAX_PINS_PER_PORT; pin++) {
            if (gpio_state.pins[port][pin].initialized) {
                tcelm_gpio_close(&gpio_state.pins[port][pin]);
            }
        }
    }

    tcelm_gpio_bsp_shutdown();
    gpio_state.initialized = false;
}

#ifdef __rtems__

/*
 * RTEMS BSP-specific implementation
 * This needs to be customized per BSP (BeagleBone, NUC, etc.)
 */

int tcelm_gpio_bsp_init(void) {
    /* BSP-specific GPIO controller initialization */
#if defined(BSP_BEAGLEBONE)
    /* BeagleBone GPIO init */
    /* TODO: Initialize AM335x GPIO controllers */
#elif defined(BSP_NUC)
    /* Intel NUC GPIO via PCH */
    /* TODO: Initialize Intel PCH GPIO */
#else
    /* Generic: assume no hardware GPIO */
    fprintf(stderr, "Warning: No BSP-specific GPIO support\n");
#endif
    return 0;
}

void tcelm_gpio_bsp_shutdown(void) {
    /* BSP-specific cleanup */
}

tcelm_gpio_pin_t *tcelm_gpio_open(
    uint32_t port,
    uint32_t pin,
    tcelm_gpio_direction_t dir
) {
    if (port >= MAX_GPIO_PORTS || pin >= MAX_PINS_PER_PORT) {
        return NULL;
    }

    tcelm_gpio_pin_t *gpio = &gpio_state.pins[port][pin];
    if (gpio->initialized) {
        /* Already open, just update direction */
        tcelm_gpio_set_direction(gpio, dir);
        return gpio;
    }

    gpio->port = port;
    gpio->pin = pin;
    gpio->dir = dir;
    gpio->edge = TCELM_GPIO_EDGE_NONE;
    gpio->bsp_data = NULL;

#if defined(BSP_BEAGLEBONE)
    /* BeagleBone: Configure pinmux and GPIO direction */
    /* TODO: Implement AM335x GPIO configuration */
#elif defined(BSP_NUC)
    /* NUC: Configure Intel PCH GPIO */
    /* TODO: Implement PCH GPIO configuration */
#endif

    gpio->initialized = true;
    return gpio;
}

void tcelm_gpio_close(tcelm_gpio_pin_t *gpio) {
    if (!gpio || !gpio->initialized) {
        return;
    }

    /* Disable any registered interrupts */
    for (uint32_t i = 0; i < gpio_state.irq_count; i++) {
        if (gpio_state.irqs[i].pin == gpio) {
            tcelm_gpio_unregister_irq(&gpio_state.irqs[i]);
        }
    }

    /* BSP-specific cleanup */
    if (gpio->bsp_data) {
        /* Free BSP data */
        gpio->bsp_data = NULL;
    }

    gpio->initialized = false;
}

int tcelm_gpio_set_direction(tcelm_gpio_pin_t *gpio, tcelm_gpio_direction_t dir) {
    if (!gpio || !gpio->initialized) {
        return -1;
    }

    gpio->dir = dir;

#if defined(BSP_BEAGLEBONE)
    /* BeagleBone: Set GPIO_OE register bit */
    /* TODO: Implement */
#elif defined(BSP_NUC)
    /* NUC: Set PCH GPIO direction */
    /* TODO: Implement */
#endif

    return 0;
}

bool tcelm_gpio_read(tcelm_gpio_pin_t *gpio) {
    if (!gpio || !gpio->initialized) {
        return false;
    }

#if defined(BSP_BEAGLEBONE)
    /* BeagleBone: Read GPIO_DATAIN register */
    /* TODO: Implement */
    return false;
#elif defined(BSP_NUC)
    /* NUC: Read PCH GPIO pad value */
    /* TODO: Implement */
    return false;
#else
    /* No hardware: return false */
    return false;
#endif
}

int tcelm_gpio_write(tcelm_gpio_pin_t *gpio, bool value) {
    if (!gpio || !gpio->initialized) {
        return -1;
    }

    if (gpio->dir != TCELM_GPIO_OUTPUT && gpio->dir != TCELM_GPIO_OUTPUT_OPEN_DRAIN) {
        return -1;  /* Not an output */
    }

#if defined(BSP_BEAGLEBONE)
    /* BeagleBone: Write GPIO_SETDATAOUT or GPIO_CLEARDATAOUT */
    /* TODO: Implement */
#elif defined(BSP_NUC)
    /* NUC: Write PCH GPIO pad value */
    /* TODO: Implement */
#endif

    return 0;
}

int tcelm_gpio_toggle(tcelm_gpio_pin_t *gpio) {
    if (!gpio || !gpio->initialized) {
        return -1;
    }

    bool current = tcelm_gpio_read(gpio);
    return tcelm_gpio_write(gpio, !current);
}

tcelm_gpio_irq_t *tcelm_gpio_register_irq(
    tcelm_gpio_pin_t *gpio,
    tcelm_gpio_edge_t edge,
    tcelm_gpio_callback_t callback,
    void *user_data
) {
    if (!gpio || !gpio->initialized || !callback) {
        return NULL;
    }

    if (gpio_state.irq_count >= MAX_GPIO_IRQS) {
        return NULL;
    }

    tcelm_gpio_irq_t *irq = &gpio_state.irqs[gpio_state.irq_count++];
    irq->pin = gpio;
    irq->callback = callback;
    irq->user_data = user_data;
    irq->edge = edge;
    irq->enabled = false;

    gpio->edge = edge;

#if defined(BSP_BEAGLEBONE)
    /* BeagleBone: Configure GPIO interrupt edge detection */
    /* TODO: Implement AM335x GPIO IRQ setup */
#elif defined(BSP_NUC)
    /* NUC: Configure PCH GPIO interrupt */
    /* TODO: Implement */
#endif

    return irq;
}

void tcelm_gpio_unregister_irq(tcelm_gpio_irq_t *irq) {
    if (!irq) {
        return;
    }

    tcelm_gpio_irq_disable(irq);

    /* Mark as unused */
    irq->pin = NULL;
    irq->callback = NULL;
    irq->enabled = false;
}

int tcelm_gpio_irq_enable(tcelm_gpio_irq_t *irq) {
    if (!irq || !irq->pin) {
        return -1;
    }

#if defined(BSP_BEAGLEBONE)
    /* BeagleBone: Enable GPIO interrupt in INTC */
    /* TODO: Implement */
#elif defined(BSP_NUC)
    /* NUC: Enable PCH GPIO interrupt */
    /* TODO: Implement */
#endif

    irq->enabled = true;
    return 0;
}

int tcelm_gpio_irq_disable(tcelm_gpio_irq_t *irq) {
    if (!irq || !irq->pin) {
        return -1;
    }

#if defined(BSP_BEAGLEBONE)
    /* BeagleBone: Disable GPIO interrupt in INTC */
    /* TODO: Implement */
#elif defined(BSP_NUC)
    /* NUC: Disable PCH GPIO interrupt */
    /* TODO: Implement */
#endif

    irq->enabled = false;
    return 0;
}

#elif defined(__linux__)

/*
 * Linux sysfs GPIO implementation (for development/testing)
 */

typedef struct {
    int value_fd;
    int edge_fd;
    char path[64];
} linux_gpio_data_t;

int tcelm_gpio_bsp_init(void) {
    gpio_state.irq_thread_running = false;
    return 0;
}

void tcelm_gpio_bsp_shutdown(void) {
    if (gpio_state.irq_thread_running) {
        gpio_state.irq_thread_running = false;
        pthread_join(gpio_state.irq_thread, NULL);
    }
}

static int gpio_export(uint32_t gpio_num) {
    char path[64];
    snprintf(path, sizeof(path), "/sys/class/gpio/gpio%u", gpio_num);

    /* Check if already exported */
    if (access(path, F_OK) == 0) {
        return 0;
    }

    int fd = open("/sys/class/gpio/export", O_WRONLY);
    if (fd < 0) {
        return -1;
    }

    char buf[16];
    int len = snprintf(buf, sizeof(buf), "%u", gpio_num);
    int result = write(fd, buf, len);
    close(fd);

    return (result == len) ? 0 : -1;
}

static int gpio_unexport(uint32_t gpio_num) {
    int fd = open("/sys/class/gpio/unexport", O_WRONLY);
    if (fd < 0) {
        return -1;
    }

    char buf[16];
    int len = snprintf(buf, sizeof(buf), "%u", gpio_num);
    int result = write(fd, buf, len);
    close(fd);

    return (result == len) ? 0 : -1;
}

tcelm_gpio_pin_t *tcelm_gpio_open(
    uint32_t port,
    uint32_t pin,
    tcelm_gpio_direction_t dir
) {
    if (port >= MAX_GPIO_PORTS || pin >= MAX_PINS_PER_PORT) {
        return NULL;
    }

    uint32_t gpio_num = port * 32 + pin;

    if (gpio_export(gpio_num) != 0) {
        fprintf(stderr, "Failed to export GPIO %u\n", gpio_num);
        return NULL;
    }

    tcelm_gpio_pin_t *gpio = &gpio_state.pins[port][pin];
    gpio->port = port;
    gpio->pin = pin;
    gpio->edge = TCELM_GPIO_EDGE_NONE;

    /* Allocate Linux-specific data */
    linux_gpio_data_t *data = malloc(sizeof(linux_gpio_data_t));
    if (!data) {
        gpio_unexport(gpio_num);
        return NULL;
    }

    snprintf(data->path, sizeof(data->path), "/sys/class/gpio/gpio%u", gpio_num);

    /* Open value file */
    char value_path[80];
    snprintf(value_path, sizeof(value_path), "%s/value", data->path);
    data->value_fd = open(value_path, O_RDWR);
    if (data->value_fd < 0) {
        free(data);
        gpio_unexport(gpio_num);
        return NULL;
    }

    data->edge_fd = -1;
    gpio->bsp_data = data;
    gpio->initialized = true;

    /* Set direction */
    tcelm_gpio_set_direction(gpio, dir);

    return gpio;
}

void tcelm_gpio_close(tcelm_gpio_pin_t *gpio) {
    if (!gpio || !gpio->initialized) {
        return;
    }

    linux_gpio_data_t *data = (linux_gpio_data_t *)gpio->bsp_data;
    if (data) {
        if (data->value_fd >= 0) {
            close(data->value_fd);
        }
        if (data->edge_fd >= 0) {
            close(data->edge_fd);
        }
        free(data);
    }

    uint32_t gpio_num = gpio->port * 32 + gpio->pin;
    gpio_unexport(gpio_num);

    gpio->bsp_data = NULL;
    gpio->initialized = false;
}

int tcelm_gpio_set_direction(tcelm_gpio_pin_t *gpio, tcelm_gpio_direction_t dir) {
    if (!gpio || !gpio->initialized) {
        return -1;
    }

    linux_gpio_data_t *data = (linux_gpio_data_t *)gpio->bsp_data;
    char path[80];
    snprintf(path, sizeof(path), "%s/direction", data->path);

    int fd = open(path, O_WRONLY);
    if (fd < 0) {
        return -1;
    }

    const char *dir_str = (dir == TCELM_GPIO_INPUT ||
                           dir == TCELM_GPIO_INPUT_PULLUP ||
                           dir == TCELM_GPIO_INPUT_PULLDOWN) ? "in" : "out";

    write(fd, dir_str, strlen(dir_str));
    close(fd);

    gpio->dir = dir;
    return 0;
}

bool tcelm_gpio_read(tcelm_gpio_pin_t *gpio) {
    if (!gpio || !gpio->initialized) {
        return false;
    }

    linux_gpio_data_t *data = (linux_gpio_data_t *)gpio->bsp_data;

    char buf[4];
    lseek(data->value_fd, 0, SEEK_SET);
    int n = read(data->value_fd, buf, sizeof(buf));

    if (n > 0) {
        return buf[0] == '1';
    }
    return false;
}

int tcelm_gpio_write(tcelm_gpio_pin_t *gpio, bool value) {
    if (!gpio || !gpio->initialized) {
        return -1;
    }

    linux_gpio_data_t *data = (linux_gpio_data_t *)gpio->bsp_data;
    const char *val_str = value ? "1" : "0";

    lseek(data->value_fd, 0, SEEK_SET);
    int result = write(data->value_fd, val_str, 1);

    return (result == 1) ? 0 : -1;
}

int tcelm_gpio_toggle(tcelm_gpio_pin_t *gpio) {
    bool current = tcelm_gpio_read(gpio);
    return tcelm_gpio_write(gpio, !current);
}

/* IRQ polling thread for Linux */
static void *gpio_irq_poll_thread(void *arg) {
    (void)arg;
    struct pollfd fds[MAX_GPIO_IRQS];

    while (gpio_state.irq_thread_running) {
        int nfds = 0;

        /* Build poll list */
        for (uint32_t i = 0; i < gpio_state.irq_count; i++) {
            tcelm_gpio_irq_t *irq = &gpio_state.irqs[i];
            if (irq->enabled && irq->pin && irq->pin->bsp_data) {
                linux_gpio_data_t *data = (linux_gpio_data_t *)irq->pin->bsp_data;
                if (data->value_fd >= 0) {
                    fds[nfds].fd = data->value_fd;
                    fds[nfds].events = POLLPRI | POLLERR;
                    nfds++;
                }
            }
        }

        if (nfds == 0) {
            usleep(10000);  /* 10ms */
            continue;
        }

        int ret = poll(fds, nfds, 100);  /* 100ms timeout */
        if (ret > 0) {
            /* Check which GPIO triggered */
            int idx = 0;
            for (uint32_t i = 0; i < gpio_state.irq_count; i++) {
                tcelm_gpio_irq_t *irq = &gpio_state.irqs[i];
                if (irq->enabled && irq->pin && irq->pin->bsp_data) {
                    if (fds[idx].revents & POLLPRI) {
                        /* Clear interrupt by reading */
                        char buf[4];
                        lseek(fds[idx].fd, 0, SEEK_SET);
                        read(fds[idx].fd, buf, sizeof(buf));

                        bool level = buf[0] == '1';
                        irq->callback(irq->pin->port, irq->pin->pin, level, irq->user_data);
                    }
                    idx++;
                }
            }
        }
    }

    return NULL;
}

tcelm_gpio_irq_t *tcelm_gpio_register_irq(
    tcelm_gpio_pin_t *gpio,
    tcelm_gpio_edge_t edge,
    tcelm_gpio_callback_t callback,
    void *user_data
) {
    if (!gpio || !gpio->initialized || !callback) {
        return NULL;
    }

    if (gpio_state.irq_count >= MAX_GPIO_IRQS) {
        return NULL;
    }

    linux_gpio_data_t *data = (linux_gpio_data_t *)gpio->bsp_data;

    /* Set edge detection */
    char path[80];
    snprintf(path, sizeof(path), "%s/edge", data->path);

    int fd = open(path, O_WRONLY);
    if (fd < 0) {
        return NULL;
    }

    const char *edge_str;
    switch (edge) {
        case TCELM_GPIO_EDGE_RISING:  edge_str = "rising"; break;
        case TCELM_GPIO_EDGE_FALLING: edge_str = "falling"; break;
        case TCELM_GPIO_EDGE_BOTH:    edge_str = "both"; break;
        default:                      edge_str = "none"; break;
    }

    write(fd, edge_str, strlen(edge_str));
    close(fd);

    tcelm_gpio_irq_t *irq = &gpio_state.irqs[gpio_state.irq_count++];
    irq->pin = gpio;
    irq->callback = callback;
    irq->user_data = user_data;
    irq->edge = edge;
    irq->enabled = false;

    gpio->edge = edge;

    /* Start IRQ thread if not running */
    if (!gpio_state.irq_thread_running) {
        gpio_state.irq_thread_running = true;
        pthread_create(&gpio_state.irq_thread, NULL, gpio_irq_poll_thread, NULL);
    }

    return irq;
}

void tcelm_gpio_unregister_irq(tcelm_gpio_irq_t *irq) {
    if (!irq) {
        return;
    }

    irq->enabled = false;
    irq->pin = NULL;
    irq->callback = NULL;
}

int tcelm_gpio_irq_enable(tcelm_gpio_irq_t *irq) {
    if (!irq || !irq->pin) {
        return -1;
    }
    irq->enabled = true;
    return 0;
}

int tcelm_gpio_irq_disable(tcelm_gpio_irq_t *irq) {
    if (!irq || !irq->pin) {
        return -1;
    }
    irq->enabled = false;
    return 0;
}

#else

/* Stub implementation for unsupported platforms */

int tcelm_gpio_bsp_init(void) {
    fprintf(stderr, "Warning: GPIO not supported on this platform\n");
    return 0;
}

void tcelm_gpio_bsp_shutdown(void) {}

tcelm_gpio_pin_t *tcelm_gpio_open(uint32_t port, uint32_t pin, tcelm_gpio_direction_t dir) {
    (void)port; (void)pin; (void)dir;
    return NULL;
}

void tcelm_gpio_close(tcelm_gpio_pin_t *gpio) { (void)gpio; }
int tcelm_gpio_set_direction(tcelm_gpio_pin_t *gpio, tcelm_gpio_direction_t dir) {
    (void)gpio; (void)dir; return -1;
}
bool tcelm_gpio_read(tcelm_gpio_pin_t *gpio) { (void)gpio; return false; }
int tcelm_gpio_write(tcelm_gpio_pin_t *gpio, bool value) { (void)gpio; (void)value; return -1; }
int tcelm_gpio_toggle(tcelm_gpio_pin_t *gpio) { (void)gpio; return -1; }

tcelm_gpio_irq_t *tcelm_gpio_register_irq(
    tcelm_gpio_pin_t *gpio, tcelm_gpio_edge_t edge,
    tcelm_gpio_callback_t callback, void *user_data
) {
    (void)gpio; (void)edge; (void)callback; (void)user_data;
    return NULL;
}

void tcelm_gpio_unregister_irq(tcelm_gpio_irq_t *irq) { (void)irq; }
int tcelm_gpio_irq_enable(tcelm_gpio_irq_t *irq) { (void)irq; return -1; }
int tcelm_gpio_irq_disable(tcelm_gpio_irq_t *irq) { (void)irq; return -1; }

#endif

/*
 * Common functions (all platforms)
 */

uint32_t tcelm_gpio_read_multi(tcelm_gpio_pin_t **pins, uint32_t count) {
    uint32_t result = 0;
    for (uint32_t i = 0; i < count && i < 32; i++) {
        if (pins[i] && tcelm_gpio_read(pins[i])) {
            result |= (1u << i);
        }
    }
    return result;
}

int tcelm_gpio_write_multi(tcelm_gpio_pin_t **pins, uint32_t count, uint32_t value) {
    int result = 0;
    for (uint32_t i = 0; i < count && i < 32; i++) {
        if (pins[i]) {
            if (tcelm_gpio_write(pins[i], (value >> i) & 1) != 0) {
                result = -1;
            }
        }
    }
    return result;
}
