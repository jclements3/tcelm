# Examples Makefile
# Builds example tcelm programs for native (localhost) or RTEMS

RUNTIME_DIR = ../runtime

# Native build (localhost with pthreads)
CC = gcc
CFLAGS = -Wall -Wextra -g -I$(RUNTIME_DIR)
LDFLAGS = -lpthread -lrt -lm

# Runtime sources needed
RUNTIME_SRCS = \
    $(RUNTIME_DIR)/tcelm_arena.c \
    $(RUNTIME_DIR)/tcelm_types.c \
    $(RUNTIME_DIR)/tcelm_task.c \
    $(RUNTIME_DIR)/tcelm_channel.c \
    $(RUNTIME_DIR)/tcelm_mvar.c \
    $(RUNTIME_DIR)/tcelm_timer.c \
    $(RUNTIME_DIR)/tcelm_io.c

# Examples
EXAMPLES = sensor_monitor

.PHONY: all clean run

all: $(EXAMPLES)

sensor_monitor: sensor_monitor.c $(RUNTIME_SRCS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

run: sensor_monitor
	./sensor_monitor

clean:
	rm -f $(EXAMPLES)

# RTEMS cross-compile target (for NUC deployment)
# Requires RTEMS toolchain installed
RTEMS_BSP ?= pc686
RTEMS_PREFIX ?= /opt/rtems/6

rtems: sensor_monitor_rtems

sensor_monitor_rtems: sensor_monitor.c $(RUNTIME_SRCS)
	$(RTEMS_PREFIX)/bin/i386-rtems6-gcc \
		-D__rtems__ \
		-I$(RTEMS_PREFIX)/i386-rtems6/$(RTEMS_BSP)/lib/include \
		-B$(RTEMS_PREFIX)/i386-rtems6/$(RTEMS_BSP)/lib \
		-specs bsp_specs \
		$(CFLAGS) \
		-o $@ $^ \
		-Wl,--gc-sections
