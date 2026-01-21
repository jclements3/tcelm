# TCC-NUC-BSP Makefile
#
# Build the TCC Board Support Package for Intel NUC running RTEMS
#
# Targets:
#   make          - Build everything
#   make crt      - Build C runtime objects
#   make install  - Install to TCC lib directory
#   make clean    - Clean build artifacts
#

# Local TCC path (use project's bundled TCC)
PROJECT_ROOT ?= $(shell cd .. && pwd)
TCC_DIR = $(PROJECT_ROOT)/tcc

# TCC installation
TCC_PREFIX ?= /usr/local
TCC_LIBDIR = $(TCC_PREFIX)/lib/tcc

# RTEMS installation (for headers/libs we link against)
RTEMS_PREFIX ?= $(HOME)/projects/hwil/rtems/7
RTEMS_TARGET = i386-rtems7
RTEMS_BSP = pc686

# Tools - use system assembler for CRT, TCC for C code
AS = as
AR = ar
# Use local TCC to compile the syscalls (self-hosting test)
TCC = $(TCC_DIR)/tcc
ASFLAGS = --32
TCC_CFLAGS = -m32 -Wall -Iinclude -DSTANDALONE

# Output directory
BUILD_DIR = build

# BSP name
BSP_NAME = tcc-nuc-bsp

.PHONY: all
all: crt lib

# Create build directory
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Build C runtime startup
.PHONY: crt
crt: $(BUILD_DIR)/crt0.o $(BUILD_DIR)/crti.o $(BUILD_DIR)/crtn.o

$(BUILD_DIR)/crt0.o: crt/crt0.S | $(BUILD_DIR)
	$(AS) $(ASFLAGS) -o $@ $<

# crti.o - init section prologue
$(BUILD_DIR)/crti.o: crt/crti.S | $(BUILD_DIR)
	$(AS) $(ASFLAGS) -o $@ $<

# crtn.o - init section epilogue
$(BUILD_DIR)/crtn.o: crt/crtn.S | $(BUILD_DIR)
	$(AS) $(ASFLAGS) -o $@ $<

# Build support library (syscalls)
.PHONY: lib
lib: $(BUILD_DIR)/libsyscalls.a

$(BUILD_DIR)/syscalls.o: lib/syscalls.c | $(BUILD_DIR)
	$(TCC) $(TCC_CFLAGS) -c $< -o $@

$(BUILD_DIR)/libsyscalls.a: $(BUILD_DIR)/syscalls.o
	$(AR) rcs $@ $^

# Install to TCC lib directory
.PHONY: install
install: crt lib
	mkdir -p $(TCC_LIBDIR)/$(BSP_NAME)
	mkdir -p $(TCC_LIBDIR)/$(BSP_NAME)/include
	cp $(BUILD_DIR)/crt0.o $(TCC_LIBDIR)/$(BSP_NAME)/
	cp $(BUILD_DIR)/crti.o $(TCC_LIBDIR)/$(BSP_NAME)/
	cp $(BUILD_DIR)/crtn.o $(TCC_LIBDIR)/$(BSP_NAME)/
	cp $(BUILD_DIR)/libsyscalls.a $(TCC_LIBDIR)/$(BSP_NAME)/
	cp linkcmds.nuc $(TCC_LIBDIR)/$(BSP_NAME)/
	cp config.h $(TCC_LIBDIR)/$(BSP_NAME)/
	cp include/tcc_nuc.h $(TCC_LIBDIR)/$(BSP_NAME)/include/
	@echo "Installed to $(TCC_LIBDIR)/$(BSP_NAME)"

# Create a tcc wrapper script
.PHONY: wrapper
wrapper: $(BUILD_DIR)/tcc-nuc
	@echo "Created $(BUILD_DIR)/tcc-nuc"

$(BUILD_DIR)/tcc-nuc: | $(BUILD_DIR)
	@echo '#!/bin/sh' > $@
	@echo '# TCC cross-compiler for Intel NUC RTEMS (32-bit)' >> $@
	@echo 'TCC_BSP_DIR="$$(dirname "$$0")/../lib/tcc/tcc-nuc-bsp"' >> $@
	@echo 'exec tcc \' >> $@
	@echo '    -nostdlib \' >> $@
	@echo '    -nostdinc \' >> $@
	@echo '    -I$(RTEMS_PREFIX)/$(RTEMS_TARGET)/include \' >> $@
	@echo '    -L$(RTEMS_PREFIX)/$(RTEMS_TARGET)/$(RTEMS_BSP)/lib \' >> $@
	@echo '    -L"$$TCC_BSP_DIR" \' >> $@
	@echo '    "$$TCC_BSP_DIR/crt0.o" \' >> $@
	@echo '    "$$@" \' >> $@
	@echo '    -T"$$TCC_BSP_DIR/linkcmds.nuc" \' >> $@
	@echo '    -lrtemsbsp -lrtemscpu -lc -lm' >> $@
	chmod +x $@

# Clean
.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)

# Help
.PHONY: help
help:
	@echo "TCC-NUC-BSP Build System"
	@echo ""
	@echo "Targets:"
	@echo "  make          - Build C runtime objects"
	@echo "  make crt      - Build C runtime objects"
	@echo "  make install  - Install to TCC lib directory"
	@echo "  make wrapper  - Create tcc-nuc wrapper script"
	@echo "  make clean    - Clean build artifacts"
	@echo ""
	@echo "Environment:"
	@echo "  TCC_PREFIX    - TCC installation (default: /usr/local)"
	@echo "  RTEMS_PREFIX  - RTEMS installation (default: ~/projects/hwil/rtems/7)"
