#
# Makefile for tcelm tests
#
# Targets:
#   make test          - Run all tests
#   make test-unit     - Run unit tests only
#   make test-subsystem - Run subsystem tests only
#   make test-system   - Run system tests only
#   make clean         - Clean test artifacts
#

.PHONY: all test test-unit test-subsystem test-system clean

PROJECT_ROOT := $(shell cd .. && pwd)
RUNTIME_DIR := $(PROJECT_ROOT)/runtime

# Compiler flags
CC := gcc
CFLAGS := -Wall -Wextra -g -I$(RUNTIME_DIR)
LDFLAGS := -lm

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

# Build unit test executables
$(UNIT_DIR)/test_types: $(UNIT_DIR)/test_types.c $(PROP_DIR)/proptest.h $(RUNTIME_OBJS)
	$(CC) $(CFLAGS) $< $(RUNTIME_OBJS) -o $@ $(LDFLAGS)

$(UNIT_DIR)/test_list: $(UNIT_DIR)/test_list.c $(PROP_DIR)/proptest.h $(RUNTIME_OBJS)
	$(CC) $(CFLAGS) $< $(RUNTIME_OBJS) -o $@ $(LDFLAGS)

$(UNIT_DIR)/test_string: $(UNIT_DIR)/test_string.c $(PROP_DIR)/proptest.h $(RUNTIME_OBJS)
	$(CC) $(CFLAGS) $< $(RUNTIME_OBJS) -o $@ $(LDFLAGS)

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

# Build unit tests without running
build-unit: $(UNIT_TESTS)

# Help
help:
	@echo "tcelm Test Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  test            - Run all tests (unit, subsystem, system)"
	@echo "  test-unit       - Run property-based unit tests for C runtime"
	@echo "  test-subsystem  - Run code generator subsystem tests"
	@echo "  test-system     - Run full system integration tests"
	@echo "  test-seed SEED=N - Run tests with specific random seed"
	@echo "  build-unit      - Build unit test executables"
	@echo "  clean           - Remove test artifacts"
	@echo "  help            - Show this help"
