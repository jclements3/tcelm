# tcelm Runtime Makefile
#
# Targets:
#   make host      - Build for host (testing)
#   make rtems     - Build for RTEMS (cross-compile)
#   make test      - Build and run host tests
#   make clean     - Clean build artifacts
#

# Common sources
COMMON_SRCS = tcelm_arena.c tcelm_types.c tcelm_basics.c
# Standard library modules (for bootstrap)
STDLIB_SRCS = tcelm_string.c tcelm_list.c tcelm_char.c tcelm_maybe.c
# New RTEMS integration layer (Task, Channel, MVar, Semaphore, Barrier, Budget, Protected, Timer, IO, Worker, Platform)
NEW_INTEGRATION_SRCS = tcelm_task.c tcelm_channel.c tcelm_mvar.c tcelm_semaphore.c tcelm_barrier.c tcelm_budget.c tcelm_protected.c tcelm_timer.c tcelm_io.c tcelm_worker.c tcelm_platform.c
# Legacy RTEMS sources (kept separate to avoid conflicts)
RTEMS_SRCS = tcelm_rtems.c tcelm_shell.c tcelm_shell_ops.c tcelm_hwil.c

# All library sources (includes new integration layer)
LIB_SRCS = $(COMMON_SRCS) $(STDLIB_SRCS) $(NEW_INTEGRATION_SRCS) $(RTEMS_SRCS)
LIB_OBJS = $(LIB_SRCS:.c=.o)

# Test sources
TEST_RUNTIME_SRC = test_runtime.c
TEST_RTEMS_SRC = test_rtems.c
TEST_SHELL_OPS_SRC = test_shell_ops.c

# Local TCC path (use project's bundled TCC)
TCC_DIR ?= $(shell cd .. && pwd)/tcc

# Host compiler (use system GCC for better compatibility)
HOST_CC = gcc
HOST_CFLAGS = -Wall -Wextra -g -O0
# -lrt is needed on Linux for clock_gettime, not on macOS
ifeq ($(shell uname),Darwin)
HOST_LDFLAGS = -lm -lpthread
else
HOST_LDFLAGS = -lm -lpthread -lrt
endif

# TCC (Tiny C Compiler) - for fast iteration
# Use system TCC for host builds (x86_64), project TCC is for RTEMS cross-compilation
TCC_HOST = /usr/local/bin/tcc
TCC_RTEMS = $(TCC_DIR)/tcc
TCC = $(TCC_HOST)
TCC_FLAGS = -Wall

# RTEMS cross-compiler (RTEMS 7 for x86_64/amd64 - NUC)
RTEMS_PREFIX ?= $(HOME)/projects/hwil/rtems/7
RTEMS_BSP ?= amd64
RTEMS_BSP_DIR = $(RTEMS_PREFIX)/x86_64-rtems7/$(RTEMS_BSP)
RTEMS_CC = $(RTEMS_PREFIX)/bin/x86_64-rtems7-gcc
RTEMS_ABI_FLAGS = -mno-red-zone -mcmodel=large
RTEMS_CFLAGS = -Wall -Wextra -O2 -D__rtems__ \
    $(RTEMS_ABI_FLAGS) \
    -isystem $(RTEMS_BSP_DIR)/lib/include
RTEMS_LDFLAGS = $(RTEMS_ABI_FLAGS) -B$(RTEMS_BSP_DIR)/lib -qrtems -Wl,--gc-sections

# Default target
.PHONY: all
all: host

# Object files only (for linking with test suites)
CORE_OBJS = tcelm_arena.o tcelm_types.o tcelm_basics.o tcelm_string.o tcelm_list.o tcelm_char.o tcelm_maybe.o

.PHONY: objs
objs: $(CORE_OBJS)

%.o: %.c
	$(HOST_CC) $(HOST_CFLAGS) -c $< -o $@

# Host build
.PHONY: host
host: test_runtime test_rtems test_shell_ops test_pmap test_worker

test_runtime: $(TEST_RUNTIME_SRC) $(COMMON_SRCS) $(STDLIB_SRCS)
	$(HOST_CC) $(HOST_CFLAGS) -o $@ $^ $(HOST_LDFLAGS)

test_pmap: test_pmap.c $(COMMON_SRCS) $(STDLIB_SRCS)
	$(HOST_CC) $(HOST_CFLAGS) -o $@ $^ $(HOST_LDFLAGS)

test_rtems: $(TEST_RTEMS_SRC) $(LIB_SRCS)
	$(HOST_CC) $(HOST_CFLAGS) -o $@ $^ $(HOST_LDFLAGS)

test_shell_ops: $(TEST_SHELL_OPS_SRC) $(COMMON_SRCS) $(STDLIB_SRCS) tcelm_shell_ops.c
	$(HOST_CC) $(HOST_CFLAGS) -o $@ $^ $(HOST_LDFLAGS)

test_worker: test_worker.c $(COMMON_SRCS) $(STDLIB_SRCS) tcelm_worker.c
	$(HOST_CC) $(HOST_CFLAGS) -o $@ $^ $(HOST_LDFLAGS)

# TCC build (faster compilation for development)
.PHONY: tcc
tcc: test_runtime_tcc test_rtems_tcc test_worker_tcc

test_runtime_tcc: $(TEST_RUNTIME_SRC) $(COMMON_SRCS) $(STDLIB_SRCS)
	$(TCC) $(TCC_FLAGS) -o $@ $^ -lm

test_rtems_tcc: $(TEST_RTEMS_SRC) $(LIB_SRCS)
	$(TCC) $(TCC_FLAGS) -o $@ $^ -lm

test_worker_tcc: test_worker.c $(COMMON_SRCS) $(STDLIB_SRCS) tcelm_worker.c
	$(TCC) $(TCC_FLAGS) -o $@ $^ -lm

# RTEMS build (cross-compile)
.PHONY: rtems
rtems: libtcelm_rtems.a

libtcelm_rtems.a: $(LIB_SRCS:.c=.rtems.o)
	$(RTEMS_PREFIX)/bin/x86_64-rtems7-ar rcs $@ $^

%.rtems.o: %.c
	$(RTEMS_CC) $(RTEMS_CFLAGS) -c $< -o $@

# Run tests
.PHONY: test
test: test_runtime test_rtems test_shell_ops test_pmap test_worker
	@echo "=== Running runtime tests ==="
	./test_runtime
	@echo ""
	@echo "=== Running RTEMS integration tests ==="
	./test_rtems
	@echo ""
	@echo "=== Running Shell Ops tests (Turtle/Shelly style) ==="
	./test_shell_ops
	@echo ""
	@echo "=== Running pmap (parallel map) tests ==="
	./test_pmap
	@echo ""
	@echo "=== Running Platform.worker tests ==="
	./test_worker

.PHONY: test-tcc
test-tcc: test_runtime_tcc test_rtems_tcc test_worker_tcc
	@echo "=== Running runtime tests (TCC) ==="
	./test_runtime_tcc
	@echo ""
	@echo "=== Running RTEMS integration tests (TCC) ==="
	./test_rtems_tcc
	@echo ""
	@echo "=== Running Platform.worker tests (TCC) ==="
	./test_worker_tcc

# New integration layer build (Task, Channel, MVar, Timer, IO)
.PHONY: integration
integration: test_integration

test_integration: test_integration.c $(COMMON_SRCS) $(STDLIB_SRCS) $(NEW_INTEGRATION_SRCS)
	$(HOST_CC) $(HOST_CFLAGS) -o $@ $^ $(HOST_LDFLAGS)

# Build new integration as library
libtcelm_integration.a: $(COMMON_SRCS:.c=.o) $(STDLIB_SRCS:.c=.o) $(NEW_INTEGRATION_SRCS:.c=.o)
	ar rcs $@ $^

# Clean
.PHONY: clean
clean:
	rm -f test_runtime test_rtems test_shell_ops test_integration test_pmap test_worker
	rm -f test_runtime_tcc test_rtems_tcc test_shell_ops_tcc test_worker_tcc test_integration_tcc
	rm -f *.o *.rtems.o
	rm -f libtcelm_rtems.a libtcelm_integration.a

# Install (copy headers and library to a target directory)
.PHONY: install
install: libtcelm_rtems.a
	@echo "Install target not configured. Set DESTDIR to install."

# Help
.PHONY: help
help:
	@echo "tcelm Runtime Build System"
	@echo ""
	@echo "Targets:"
	@echo "  make host      - Build for host testing (default)"
	@echo "  make tcc       - Build with TCC (faster)"
	@echo "  make rtems     - Build for RTEMS (cross-compile)"
	@echo "  make test      - Build and run host tests"
	@echo "  make test-tcc  - Build and run tests with TCC"
	@echo "  make clean     - Clean build artifacts"
	@echo "  make help      - Show this help"
	@echo ""
	@echo "Environment variables:"
	@echo "  RTEMS_PREFIX   - RTEMS installation path (default: ~/projects/hwil/rtems/7)"
	@echo "  RTEMS_BSP      - RTEMS BSP (default: pc686)"
