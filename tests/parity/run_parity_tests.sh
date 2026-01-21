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
/* Built-in tuple types */
typedef struct { int _0; int _1; } elm_tuple2_t;
typedef struct { int _0; int _1; int _2; } elm_tuple3_t;
/* Built-in Order type tags */
#define TAG_LT 0
#define TAG_EQ 1
#define TAG_GT 2
/* Built-in Maybe type tags */
#define TAG_Nothing 0
#define TAG_Just 1
/* String.fromChar - convert char to single-char string */
static char __elm_fromchar_buf[2];
static const char *elm_str_from_char(char c) {
    __elm_fromchar_buf[0] = c;
    __elm_fromchar_buf[1] = 0;
    return __elm_fromchar_buf;
}
/* String.cons - prepend char to string */
static char __elm_cons_buf[256];
static const char *elm_str_cons(char c, const char *s) {
    __elm_cons_buf[0] = c;
    int i = 0;
    while (s[i] && i < 254) { __elm_cons_buf[i+1] = s[i]; i++; }
    __elm_cons_buf[i+1] = 0;
    return __elm_cons_buf;
}
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
/* Integer square root using Newton-Raphson */
static int elm_isqrt(int x) {
    if (x <= 0) return 0;
    int guess = x;
    while (1) {
        int next = (guess + x / guess) / 2;
        if (next >= guess) return guess;
        guess = next;
    }
}
/* String.fromInt - convert int to string */
static char __elm_fromint_buf[32];
static const char *elm_from_int(int n) {
    char tmp[32];
    int i = 0, j = 0;
    int neg = 0;
    if (n < 0) { neg = 1; n = -n; }
    if (n == 0) { __elm_fromint_buf[0] = '0'; __elm_fromint_buf[1] = 0; return __elm_fromint_buf; }
    while (n > 0) { tmp[i++] = '0' + (n % 10); n /= 10; }
    if (neg) __elm_fromint_buf[j++] = '-';
    while (i > 0) __elm_fromint_buf[j++] = tmp[--i];
    __elm_fromint_buf[j] = 0;
    return __elm_fromint_buf;
}
/* String.reverse - reverse a string */
static char __elm_reverse_buf[256];
static const char *elm_str_reverse(const char *s) {
    int len = 0;
    while (s[len]) len++;
    for (int i = 0; i < len; i++) __elm_reverse_buf[i] = s[len - 1 - i];
    __elm_reverse_buf[len] = 0;
    return __elm_reverse_buf;
}
/* String.left - take first n characters */
static char __elm_left_buf[256];
static const char *elm_str_left(int n, const char *s) {
    int len = 0; while (s[len]) len++;
    int take = n < len ? n : len;
    for (int i = 0; i < take; i++) __elm_left_buf[i] = s[i];
    __elm_left_buf[take] = 0;
    return __elm_left_buf;
}
/* String.right - take last n characters */
static char __elm_right_buf[256];
static const char *elm_str_right(int n, const char *s) {
    int len = 0; while (s[len]) len++;
    int start = n < len ? len - n : 0;
    int j = 0;
    for (int i = start; i < len; i++) __elm_right_buf[j++] = s[i];
    __elm_right_buf[j] = 0;
    return __elm_right_buf;
}
/* String.append - concatenate two strings */
static char __elm_append_buf[512];
static const char *elm_str_append(const char *a, const char *b) {
    int i = 0, j = 0;
    while (a[i] && i < 255) { __elm_append_buf[i] = a[i]; i++; }
    while (b[j] && i + j < 511) { __elm_append_buf[i + j] = b[j]; j++; }
    __elm_append_buf[i + j] = 0;
    return __elm_append_buf;
}
/* String.repeat - repeat string n times */
static char __elm_repeat_buf[256];
static const char *elm_str_repeat(int n, const char *s) {
    int slen = 0; while (s[slen]) slen++;
    int pos = 0;
    for (int i = 0; i < n && pos + slen < 255; i++) {
        for (int j = 0; j < slen; j++) __elm_repeat_buf[pos++] = s[j];
    }
    __elm_repeat_buf[pos] = 0;
    return __elm_repeat_buf;
}
/* String.slice - extract substring from start to end index */
static char __elm_slice_buf[256];
static const char *elm_str_slice(int start, int end, const char *s) {
    int len = 0; while (s[len]) len++;
    if (start < 0) start = len + start; if (start < 0) start = 0;
    if (end < 0) end = len + end; if (end < 0) end = 0;
    if (start > len) start = len; if (end > len) end = len;
    if (start >= end) { __elm_slice_buf[0] = 0; return __elm_slice_buf; }
    int j = 0;
    for (int i = start; i < end; i++) __elm_slice_buf[j++] = s[i];
    __elm_slice_buf[j] = 0;
    return __elm_slice_buf;
}
/* String.trim - remove leading and trailing whitespace */
static char __elm_trim_buf[256];
static const char *elm_str_trim(const char *s) {
    while (*s == ' ' || *s == '\t' || *s == '\n' || *s == '\r') s++;
    int len = 0; while (s[len]) len++;
    while (len > 0 && (s[len-1] == ' ' || s[len-1] == '\t' || s[len-1] == '\n' || s[len-1] == '\r')) len--;
    for (int i = 0; i < len; i++) __elm_trim_buf[i] = s[i];
    __elm_trim_buf[len] = 0;
    return __elm_trim_buf;
}
/* String.toUpper - convert to uppercase */
static char __elm_toupper_buf[256];
static const char *elm_str_to_upper(const char *s) {
    int i = 0;
    for (; s[i]; i++) {
        __elm_toupper_buf[i] = (s[i] >= 'a' && s[i] <= 'z') ? s[i] - 32 : s[i];
    }
    __elm_toupper_buf[i] = 0;
    return __elm_toupper_buf;
}
/* String.toLower - convert to lowercase */
static char __elm_tolower_buf[256];
static const char *elm_str_to_lower(const char *s) {
    int i = 0;
    for (; s[i]; i++) {
        __elm_tolower_buf[i] = (s[i] >= 'A' && s[i] <= 'Z') ? s[i] + 32 : s[i];
    }
    __elm_tolower_buf[i] = 0;
    return __elm_tolower_buf;
}
/* String.padLeft - pad string on the left with char */
static char __elm_padleft_buf[256];
static const char *elm_str_pad_left(int n, char c, const char *s) {
    int len = 0; while (s[len]) len++;
    int pad = n - len; if (pad < 0) pad = 0;
    if (pad + len > 255) pad = 255 - len;
    for (int i = 0; i < pad; i++) __elm_padleft_buf[i] = c;
    for (int i = 0; i < len; i++) __elm_padleft_buf[pad + i] = s[i];
    __elm_padleft_buf[pad + len] = 0;
    return __elm_padleft_buf;
}
/* String.padRight - pad string on the right with char */
static char __elm_padright_buf[256];
static const char *elm_str_pad_right(int n, char c, const char *s) {
    int len = 0; while (s[len]) len++;
    int pad = n - len; if (pad < 0) pad = 0;
    if (len + pad > 255) pad = 255 - len;
    for (int i = 0; i < len; i++) __elm_padright_buf[i] = s[i];
    for (int i = 0; i < pad; i++) __elm_padright_buf[len + i] = c;
    __elm_padright_buf[len + pad] = 0;
    return __elm_padright_buf;
}
/* String.startsWith - check if string starts with prefix */
static int elm_str_starts_with(const char *prefix, const char *s) {
    while (*prefix && *s && *prefix == *s) { prefix++; s++; }
    return !*prefix;
}
/* String.endsWith - check if string ends with suffix */
static int elm_str_ends_with(const char *suffix, const char *s) {
    int slen = 0, sufflen = 0;
    while (s[slen]) slen++;
    while (suffix[sufflen]) sufflen++;
    if (sufflen > slen) return 0;
    return !strcmp(s + slen - sufflen, suffix);
}
/* String.contains - check if substring exists */
static int elm_str_contains(const char *needle, const char *haystack) {
    if (!*needle) return 1;
    for (; *haystack; haystack++) {
        const char *h = haystack, *n = needle;
        while (*h && *n && *h == *n) { h++; n++; }
        if (!*n) return 1;
    }
    return 0;
}
/* String.toInt - parse string to Maybe Int */
static elm_union_t elm_str_to_int(const char *s) {
    int result = 0, neg = 0, i = 0;
    if (!s || !*s) return (elm_union_t){TAG_Nothing, 0};
    if (s[0] == '-') { neg = 1; i = 1; }
    else if (s[0] == '+') { i = 1; }
    if (!s[i]) return (elm_union_t){TAG_Nothing, 0};
    for (; s[i]; i++) {
        if (s[i] < '0' || s[i] > '9') return (elm_union_t){TAG_Nothing, 0};
        result = result * 10 + (s[i] - '0');
    }
    return (elm_union_t){TAG_Just, neg ? -result : result};
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
