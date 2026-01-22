#
# Makefile for tcelm NUC toolchain
#
# This builds the runtime libraries and CRT for the toolchain.
#

TOOLCHAIN_DIR := $(shell pwd)
TCC := $(TOOLCHAIN_DIR)/../tcc/tcc
RUNTIME_DIR := $(TOOLCHAIN_DIR)/../runtime

# Compiler flags
CFLAGS := -m32 -nostdlib -nostdinc \
          -I$(TOOLCHAIN_DIR)/include \
          -I$(RUNTIME_DIR) \
          -D__rtems__ -DTCELM_BARE_METAL

# Assembler
AS := as
ASFLAGS := --32

# Linker
LD := ld
LDFLAGS := -m elf_i386 -T $(TOOLCHAIN_DIR)/scripts/nuc.ld

# Archiver
AR := ar

# Output directories
BUILD_DIR := $(TOOLCHAIN_DIR)/build
LIB_DIR := $(TOOLCHAIN_DIR)/lib

# QEMU
QEMU := qemu-system-i386
QEMU_FLAGS := -nographic -no-reboot -serial mon:stdio
QEMU_FLAGS_SMP := -nographic -no-reboot -serial mon:stdio -smp 4
QEMU_TIMEOUT := 15

# Sources
LIBC_SRC := lib/libc_impl.c
RTEMS_SRC := lib/rtems_impl.c
SMP_SRC := lib/smp_impl.c
CRT_SRC := crt/crt0.S

# Objects
LIBC_OBJ := $(BUILD_DIR)/libc_impl.o
RTEMS_OBJ := $(BUILD_DIR)/rtems_impl.o
SMP_OBJ := $(BUILD_DIR)/smp_impl.o
CRT_OBJ := $(BUILD_DIR)/crt0.o

# Libraries
LIBTCELM := $(LIB_DIR)/libtcelm.a

.PHONY: all clean install

all: $(BUILD_DIR) $(CRT_OBJ) $(LIBTCELM)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Compile libc
$(LIBC_OBJ): $(LIBC_SRC) | $(BUILD_DIR)
	$(TCC) $(CFLAGS) -c $< -o $@

# Compile RTEMS implementation
$(RTEMS_OBJ): $(RTEMS_SRC) | $(BUILD_DIR)
	$(TCC) $(CFLAGS) -c $< -o $@

# Compile SMP implementation
$(SMP_OBJ): $(SMP_SRC) | $(BUILD_DIR)
	$(TCC) $(CFLAGS) -c $< -o $@

# Assemble CRT
$(CRT_OBJ): $(CRT_SRC) | $(BUILD_DIR)
	$(AS) $(ASFLAGS) $< -o $@

# Create library
$(LIBTCELM): $(LIBC_OBJ) $(RTEMS_OBJ) $(SMP_OBJ)
	$(AR) rcs $@ $^

# Install (copy to lib directory)
install: all
	cp $(CRT_OBJ) $(LIB_DIR)/crt0.o
	@echo "Toolchain built successfully!"
	@echo ""
	@echo "Usage:"
	@echo "  tcelm-cc source.c -o output.o"
	@echo "  tcelm-ld crt0.o output.o -ltcelm -o program.elf"

clean:
	rm -rf $(BUILD_DIR)
	rm -f $(LIB_DIR)/libtcelm.a
	rm -f $(LIB_DIR)/crt0.o

# Make scripts executable
scripts:
	chmod +x bin/tcelm-cc bin/tcelm-ld

# =============================================================================
# tcelm Runtime Tests
# =============================================================================

# tcelm runtime sources
TCELM_RUNTIME_SRCS := $(RUNTIME_DIR)/tcelm_arena.c \
                      $(RUNTIME_DIR)/tcelm_types.c

# Runtime objects
TCELM_RUNTIME_OBJS := $(BUILD_DIR)/tcelm_arena.o \
                      $(BUILD_DIR)/tcelm_types.o

# Compile tcelm runtime
$(BUILD_DIR)/tcelm_arena.o: $(RUNTIME_DIR)/tcelm_arena.c | $(BUILD_DIR)
	$(TCC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/tcelm_types.o: $(RUNTIME_DIR)/tcelm_types.c | $(BUILD_DIR)
	$(TCC) $(CFLAGS) -c $< -o $@

# Test sources
$(BUILD_DIR)/tcelm_runtime_test.o: examples/tcelm_runtime_test.c | $(BUILD_DIR)
	$(TCC) $(CFLAGS) -c $< -o $@

# Link runtime test
$(BUILD_DIR)/tcelm_runtime_test.elf: $(CRT_OBJ) $(BUILD_DIR)/tcelm_runtime_test.o $(TCELM_RUNTIME_OBJS) $(LIBTCELM)
	$(LD) $(LDFLAGS) $^ -o $@

# Build and run tcelm runtime test
.PHONY: test-runtime
test-runtime: $(BUILD_DIR)/tcelm_runtime_test.elf
	@echo ""
	@echo "Running tcelm runtime tests in QEMU..."
	@echo ""
	@timeout $(QEMU_TIMEOUT) $(QEMU) -kernel $< $(QEMU_FLAGS) || \
		if [ $$? -eq 124 ]; then echo "QEMU timeout (expected)"; fi
	@echo ""

# Build runtime test without running
.PHONY: build-runtime-test
build-runtime-test: $(BUILD_DIR)/tcelm_runtime_test.elf
	@echo "Built: $<"

# =============================================================================
# SMP Tests
# =============================================================================

# SMP test sources
$(BUILD_DIR)/smp_test.o: examples/smp_test.c | $(BUILD_DIR)
	$(TCC) $(CFLAGS) -c $< -o $@

# Link SMP test
$(BUILD_DIR)/smp_test.elf: $(CRT_OBJ) $(BUILD_DIR)/smp_test.o $(LIBTCELM)
	$(LD) $(LDFLAGS) $^ -o $@

# Build and run SMP test with 4 cores
.PHONY: test-smp
test-smp: $(BUILD_DIR)/smp_test.elf
	@echo ""
	@echo "Running SMP tests in QEMU (4 cores)..."
	@echo ""
	@timeout $(QEMU_TIMEOUT) $(QEMU) -kernel $< $(QEMU_FLAGS_SMP) || \
		if [ $$? -eq 124 ]; then echo "QEMU timeout (expected)"; fi
	@echo ""

# Run runtime test with SMP enabled
.PHONY: test-runtime-smp
test-runtime-smp: $(BUILD_DIR)/tcelm_runtime_test.elf
	@echo ""
	@echo "Running tcelm runtime tests in QEMU (4 cores)..."
	@echo ""
	@timeout $(QEMU_TIMEOUT) $(QEMU) -kernel $< $(QEMU_FLAGS_SMP) || \
		if [ $$? -eq 124 ]; then echo "QEMU timeout (expected)"; fi
	@echo ""

# Multi-core test
$(BUILD_DIR)/multicore_test.o: examples/multicore_test.c | $(BUILD_DIR)
	$(TCC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/multicore_test.elf: $(CRT_OBJ) $(BUILD_DIR)/multicore_test.o $(LIBTCELM)
	$(LD) $(LDFLAGS) $^ -o $@

.PHONY: test-multicore
test-multicore: $(BUILD_DIR)/multicore_test.elf
	@echo ""
	@echo "Running multi-core test in QEMU (4 cores)..."
	@echo ""
	@timeout $(QEMU_TIMEOUT) $(QEMU) -kernel $< $(QEMU_FLAGS_SMP) || \
		if [ $$? -eq 124 ]; then echo "QEMU timeout (expected)"; fi
	@echo ""

# IPI (Inter-Processor Interrupt) test
$(BUILD_DIR)/ipi_test.o: examples/ipi_test.c | $(BUILD_DIR)
	$(TCC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/ipi_test.elf: $(CRT_OBJ) $(BUILD_DIR)/ipi_test.o $(LIBTCELM)
	$(LD) $(LDFLAGS) $^ -o $@

.PHONY: test-ipi
test-ipi: $(BUILD_DIR)/ipi_test.elf
	@echo ""
	@echo "Running IPI test in QEMU (4 cores)..."
	@echo ""
	@timeout $(QEMU_TIMEOUT) $(QEMU) -kernel $< $(QEMU_FLAGS_SMP) || \
		if [ $$? -eq 124 ]; then echo "QEMU timeout (expected)"; fi
	@echo ""

# =============================================================================
# Help
# =============================================================================

help:
	@echo "tcelm NUC Toolchain"
	@echo ""
	@echo "Targets:"
	@echo "  all              - Build runtime library and CRT"
	@echo "  install          - Install to lib directory"
	@echo "  test-runtime     - Build and run tcelm runtime tests in QEMU"
	@echo "  test-runtime-smp - Run runtime tests with 4 CPU cores"
	@echo "  test-smp         - Build and run SMP-specific tests"
	@echo "  test-multicore   - Run multi-core workload test"
	@echo "  test-ipi         - Run IPI (Inter-Processor Interrupt) demo"
	@echo "  clean            - Remove build artifacts"
	@echo "  scripts          - Make wrapper scripts executable"
	@echo "  help             - Show this help"
	@echo ""
	@echo "Usage:"
	@echo "  ./bin/tcelm-cc -c source.c -o output.o"
	@echo "  ./bin/tcelm-ld lib/crt0.o output.o lib/libtcelm.a -o program.elf"
