#
# Makefile for tcelm tests
#
# Targets:
#   make test           - Run all tests
#   make test-unit      - Run unit tests only
#   make test-subsystem - Run subsystem tests only
#   make test-system    - Run system tests only
#   make test-qemu      - Run TCC/QEMU bare-metal tests
#   make clean          - Clean test artifacts
#

.PHONY: all test test-unit test-subsystem test-system test-parity test-qemu clean

PROJECT_ROOT := $(shell cd .. && pwd)
RUNTIME_DIR := $(PROJECT_ROOT)/runtime
TCC_DIR := $(PROJECT_ROOT)/tcc
TCC_NUC_BSP := $(PROJECT_ROOT)/tcc-nuc-bsp

# Compiler flags
# Native (host) tests use GCC for portability and system header access
# QEMU tests use project TCC (configured for RTEMS/bare-metal)
CC_NATIVE := gcc
CC_QEMU := $(TCC_DIR)/tcc
CFLAGS := -Wall -I$(RUNTIME_DIR)
LDFLAGS := -lm -lpthread

# QEMU test settings
QEMU := qemu-system-i386
QEMU_FLAGS := -nographic -no-reboot -serial mon:stdio
QEMU_TIMEOUT := 10

# Test directories
UNIT_DIR := unit/runtime
PROP_DIR := property

# Unit test sources
UNIT_TESTS := $(UNIT_DIR)/test_types \
              $(UNIT_DIR)/test_list \
              $(UNIT_DIR)/test_string

# Build runtime object files
$(RUNTIME_OBJS):
	$(MAKE) -C $(RUNTIME_DIR) objs

# Runtime objects - explicit list to avoid wildcard timing issues
RUNTIME_OBJS := $(RUNTIME_DIR)/tcelm_arena.o $(RUNTIME_DIR)/tcelm_types.o \
                $(RUNTIME_DIR)/tcelm_basics.o $(RUNTIME_DIR)/tcelm_string.o \
                $(RUNTIME_DIR)/tcelm_list.o $(RUNTIME_DIR)/tcelm_char.o \
                $(RUNTIME_DIR)/tcelm_maybe.o

# Build unit test executables (using system TCC for native tests)
$(UNIT_DIR)/test_types: $(UNIT_DIR)/test_types.c $(PROP_DIR)/proptest.h $(RUNTIME_OBJS)
	$(CC_NATIVE) $(CFLAGS) $< $(RUNTIME_OBJS) -o $@ $(LDFLAGS)

$(UNIT_DIR)/test_list: $(UNIT_DIR)/test_list.c $(PROP_DIR)/proptest.h $(RUNTIME_OBJS)
	$(CC_NATIVE) $(CFLAGS) $< $(RUNTIME_OBJS) -o $@ $(LDFLAGS)

$(UNIT_DIR)/test_string: $(UNIT_DIR)/test_string.c $(PROP_DIR)/proptest.h $(RUNTIME_OBJS)
	$(CC_NATIVE) $(CFLAGS) $< $(RUNTIME_OBJS) -o $@ $(LDFLAGS)

# Run all tests
all: test

test: test-unit test-subsystem test-system
	@echo ""
	@echo "All tests completed!"

# Run unit tests
test-unit: $(UNIT_TESTS)
	@echo ""
	@echo "Running unit tests..."
	@echo ""
	@for test in $(UNIT_TESTS); do \
		echo "Running $$test..."; \
		./$$test || exit 1; \
		echo ""; \
	done
	@echo "Unit tests passed!"

# Run subsystem tests
test-subsystem:
	@echo ""
	@echo "Running subsystem tests..."
	@echo ""
	@./subsystem/test_codegen.sh

# Run system tests
test-system:
	@echo ""
	@echo "Running system integration tests..."
	@echo ""
	@./system/test_integration.sh

# Run parity tests (host vs board output comparison)
test-parity:
	@echo ""
	@echo "Running host/board parity tests..."
	@echo ""
	@./parity/run_parity_tests.sh

# =============================================================================
# QEMU Testing (TCC-only, bare-metal)
# =============================================================================

# Build multiboot header object
$(TCC_NUC_BSP)/build/multiboot.o: $(TCC_NUC_BSP)/crt/multiboot.S
	@mkdir -p $(TCC_NUC_BSP)/build
	as --32 $< -o $@

# Build QEMU test objects with TCC
qemu_test.o: qemu_test.c
	$(CC) -m32 -nostdlib -nostdinc -c $< -o $@

qemu_runtime_test.o: qemu_runtime_test.c qemu_libc.h
	$(CC) -m32 -nostdlib -nostdinc -c $< -o $@

# Link QEMU test executables
qemu_test.elf: $(TCC_NUC_BSP)/build/multiboot.o qemu_test.o
	ld -m elf_i386 -T $(TCC_NUC_BSP)/linkcmds.qemu $^ -o $@

qemu_runtime_test.elf: $(TCC_NUC_BSP)/build/multiboot.o qemu_runtime_test.o
	ld -m elf_i386 -T $(TCC_NUC_BSP)/linkcmds.qemu $^ -o $@

# Run basic QEMU test
test-qemu-basic: qemu_test.elf
	@echo ""
	@echo "Running TCC/QEMU basic tests..."
	@echo ""
	@timeout $(QEMU_TIMEOUT) $(QEMU) -kernel $< $(QEMU_FLAGS) || \
		if [ $$? -eq 124 ]; then echo "QEMU timeout (expected)"; fi

# Run runtime QEMU test
test-qemu-runtime: qemu_runtime_test.elf
	@echo ""
	@echo "Running TCC/QEMU runtime tests..."
	@echo ""
	@timeout $(QEMU_TIMEOUT) $(QEMU) -kernel $< $(QEMU_FLAGS) || \
		if [ $$? -eq 124 ]; then echo "QEMU timeout (expected)"; fi

# Run all QEMU tests
test-qemu: test-qemu-basic test-qemu-runtime
	@echo ""
	@echo "All QEMU tests completed!"

# Build all QEMU tests without running
build-qemu: qemu_test.elf qemu_runtime_test.elf
	@echo "Built qemu_test.elf and qemu_runtime_test.elf"

# Run with specific seed for reproducibility
test-seed:
	@echo "Running tests with seed $(SEED)..."
	@for test in $(UNIT_TESTS); do \
		./$$test $(SEED) || exit 1; \
	done

# Clean test artifacts
clean:
	rm -f $(UNIT_DIR)/test_types $(UNIT_DIR)/test_list $(UNIT_DIR)/test_string
	rm -f $(UNIT_DIR)/*.o
	rm -rf /tmp/tcelm_*_tests
	rm -f qemu_test.o qemu_test.elf
	rm -f qemu_runtime_test.o qemu_runtime_test.elf

# Build unit tests without running
build-unit: $(UNIT_TESTS)

# Help
help:
	@echo "tcelm Test Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  test              - Run all tests (unit, subsystem, system)"
	@echo "  test-unit         - Run property-based unit tests for C runtime"
	@echo "  test-subsystem    - Run code generator subsystem tests"
	@echo "  test-system       - Run full system integration tests"
	@echo "  test-parity       - Run host/board output parity tests"
	@echo "  test-qemu         - Run all QEMU tests (basic + runtime)"
	@echo "  test-qemu-basic   - Run basic TCC/QEMU tests"
	@echo "  test-qemu-runtime - Run tcelm runtime tests in QEMU"
	@echo "  test-seed SEED=N  - Run tests with specific random seed"
	@echo "  build-unit        - Build unit test executables"
	@echo "  build-qemu        - Build QEMU test executables"
	@echo "  clean             - Remove test artifacts"
	@echo "  help              - Show this help"
	@echo ""
	@echo "QEMU Testing:"
	@echo "  Compiles C code with TCC and runs in QEMU bare-metal."
	@echo "  Tests: arena, types, lists, tuples, records, closures."
	@echo "  Note: RTEMS components require full RTEMS BSP (not TCC-only)."
