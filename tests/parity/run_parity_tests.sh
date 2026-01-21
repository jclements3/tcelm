#!/bin/bash
#
# run_parity_tests.sh - Host vs Board output parity tests
#
# Verifies that tcelm generates correct code for both targets.
# Tests compilation and verifies the generated result matches expected.
#
# Usage: ./run_parity_tests.sh [fixture_name]
#        ./run_parity_tests.sh --qemu [fixture_name]  # Also run in QEMU
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
FIXTURES_DIR="$SCRIPT_DIR/fixtures"
TMP_DIR="/tmp/tcelm_parity_tests"
TCELM="$PROJECT_ROOT/bin/tcelm"
TCELM_COMPILE="$PROJECT_ROOT/bin/tcelm-compile.js"

# Options
RUN_QEMU=false
if [ "$1" = "--qemu" ]; then
    RUN_QEMU=true
    shift
fi

# QEMU settings
QEMU="qemu-system-i386"
QEMU_TIMEOUT=3

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Counters
PASSED=0
FAILED=0
SKIPPED=0

mkdir -p "$TMP_DIR"

log_pass() {
    echo -e "  ${GREEN}✓${NC} $1"
    PASSED=$((PASSED + 1))
}

log_fail() {
    echo -e "  ${RED}✗${NC} $1"
    echo -e "    ${RED}$2${NC}"
    FAILED=$((FAILED + 1))
}

log_skip() {
    echo -e "  ${YELLOW}○${NC} $1 (skipped: $2)"
    SKIPPED=$((SKIPPED + 1))
}

# Extract expected result from generated C code
# Compiles a small test program to evaluate the elm_main() result
extract_c_result() {
    local c_file="$1"
    local test_prog="$TMP_DIR/test_extract.c"
    local test_bin="$TMP_DIR/test_extract"

    # Check if it's a string or int return
    if grep -q 'const char \*elm_main' "$c_file" 2>/dev/null; then
        # String result - extract directly
        local str_result
        str_result=$(grep -A1 'elm_main(void)' "$c_file" 2>/dev/null | grep -oP 'return\s*"\K[^"]*' | head -1)
        echo "$str_result"
        return
    fi

    # Int result - compile and run to evaluate
    # Extract user-defined functions and elm_main
    local ctor_defines=""
    local user_funcs=""
    local elm_main_func=""

    # Extract custom type definitions if present (includes structs and constructor functions)
    if grep -q '/\* Custom type definitions \*/' "$c_file" 2>/dev/null; then
        ctor_defines=$(sed -n '/^\/\* Custom type definitions \*\//,/^\/\* \(User-defined\|Lifted\|Elm main\)/p' "$c_file" 2>/dev/null | head -n -1)
    fi

    # Extract user-defined functions if present
    if grep -q '/\* User-defined functions \*/' "$c_file" 2>/dev/null; then
        user_funcs=$(sed -n '/^\/\* User-defined functions \*\//,/^\/\* Elm main value \*\//p' "$c_file" 2>/dev/null | head -n -1)
    fi

    # Extract elm_main function
    elm_main_func=$(sed -n '/^static int elm_main/,/^}/p' "$c_file" 2>/dev/null)

    if [ -z "$elm_main_func" ]; then
        echo ""
        return
    fi

    # Create test program
    cat > "$test_prog" << EOF
#include <stdio.h>
typedef struct { int tag; int data; } elm_union_t;
/* Built-in Order type tags */
#define TAG_LT 0
#define TAG_EQ 1
#define TAG_GT 2
/* String length function */
static int elm_strlen(const char *s) {
    int len = 0;
    while (*s++) len++;
    return len;
}
/* Integer power function */
static int elm_pow(int base, int exp) {
    int result = 1;
    while (exp > 0) {
        if (exp & 1) result *= base;
        exp >>= 1;
        base *= base;
    }
    return result;
}
$ctor_defines
$user_funcs
$elm_main_func
int main(void) {
    printf("%d", elm_main());
    return 0;
}
EOF

    # Compile with system compiler (not TCC, for host execution)
    if gcc -o "$test_bin" "$test_prog" 2>/dev/null; then
        "$test_bin" 2>/dev/null
    else
        # Fallback: try to extract simple literal
        grep -A1 'elm_main(void)' "$c_file" 2>/dev/null | grep -oP 'return\s*\K[^;]+' | head -1
    fi
}

# Run a single parity test
run_test() {
    local name="$1"
    local elm_file="$FIXTURES_DIR/${name}.elm"
    local expected_file="$FIXTURES_DIR/${name}.expected"

    if [ ! -f "$elm_file" ]; then
        log_skip "$name" "Elm file not found"
        return
    fi

    if [ ! -f "$expected_file" ]; then
        log_skip "$name" "No .expected file"
        return
    fi

    local c_file="$TMP_DIR/${name}.c"
    local board_elf="$TMP_DIR/${name}_board.elf"

    # Generate C code
    if ! node "$TCELM_COMPILE" "$elm_file" --target i386-rtems-nuc > "$c_file" 2>/dev/null; then
        log_fail "$name" "Failed to generate C code"
        return
    fi

    # Extract result from generated C
    local c_result
    c_result=$(extract_c_result "$c_file")

    # Get expected result
    local expected
    expected=$(cat "$expected_file" | tr -d '\n\r')

    # Compare generated result with expected
    if [ "$c_result" = "$expected" ]; then
        # Also verify it compiles
        if "$TCELM" -b "$elm_file" -o "$board_elf" 2>/dev/null; then
            log_pass "$name: '$c_result' (compiles)"
        else
            log_fail "$name" "Result correct but compilation failed"
        fi
    else
        log_fail "$name" "Expected '$expected', generated '$c_result'"
    fi

    # Optional: Run in QEMU
    if $RUN_QEMU && [ -f "$board_elf" ]; then
        if command -v "$QEMU" &>/dev/null; then
            local board_log="$TMP_DIR/${name}_board.log"
            timeout "$QEMU_TIMEOUT" "$QEMU" \
                -kernel "$board_elf" \
                -display none \
                -nodefaults \
                -chardev stdio,id=ser0 \
                -serial chardev:ser0 \
                2>/dev/null > "$board_log" || true

            if [ -s "$board_log" ]; then
                local qemu_result
                qemu_result=$(grep -oP '(?<=Result: ).*' "$board_log" | tr -d '\r\n')
                if [ "$qemu_result" = "$expected" ]; then
                    echo -e "    ${GREEN}QEMU: '$qemu_result'${NC}"
                else
                    echo -e "    ${YELLOW}QEMU: '$qemu_result' (expected '$expected')${NC}"
                fi
            fi
        fi
    fi
}

echo ""
echo "=== tcelm Parity Tests ==="
echo ""
echo "Verifying generated code produces correct results"
echo ""

# Run tests
if [ -n "$1" ]; then
    # Single test
    run_test "$1"
else
    # All tests
    for elm_file in "$FIXTURES_DIR"/*.elm; do
        if [ -f "$elm_file" ]; then
            name=$(basename "$elm_file" .elm)
            run_test "$name"
        fi
    done
fi

echo ""
echo "=== Summary ==="
echo ""
echo -e "Passed:  ${GREEN}$PASSED${NC}"
echo -e "Failed:  ${RED}$FAILED${NC}"
echo -e "Skipped: ${YELLOW}$SKIPPED${NC}"
echo ""

# Cleanup
rm -rf "$TMP_DIR"

[ $FAILED -eq 0 ]
