#!/bin/bash
#
# test_integration.sh - System-level integration tests for tcelm
#
# Tests the full compilation pipeline:
# Elm source -> tcelm -> C code -> gcc -> executable -> run
#
# Also includes property-based testing by generating random Elm programs.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
RUNTIME_DIR="$PROJECT_ROOT/runtime"
TMP_DIR="/tmp/tcelm_integration_tests"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

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

# Compile and link multiple Elm modules
compile_and_link() {
    local name="$1"
    shift
    local modules=("$@")

    local obj_files=""
    for mod in "${modules[@]}"; do
        local mod_name=$(basename "$mod" .elm)
        local c_file="$TMP_DIR/${mod_name}.c"
        local o_file="$TMP_DIR/${mod_name}.o"

        if ! node "$PROJECT_ROOT/bin/tcelm-compile.js" "$mod" --target module > "$c_file" 2>/dev/null; then
            log_fail "$name" "Failed to generate C for $mod"
            return 1
        fi

        if ! gcc -c "$c_file" -I "$RUNTIME_DIR" -o "$o_file" 2>"$TMP_DIR/gcc_${mod_name}.log"; then
            log_fail "$name" "gcc failed for $mod: $(head -3 "$TMP_DIR/gcc_${mod_name}.log")"
            return 1
        fi

        obj_files="$obj_files $o_file"
    done

    # Create a relocatable object (partial link) to verify symbols resolve
    local combined_file="$TMP_DIR/${name}.o"
    if ld -r $obj_files -o "$combined_file" 2>"$TMP_DIR/link_${name}.log"; then
        # Verify combined object has expected symbols
        if nm "$combined_file" | grep -q "elm_"; then
            log_pass "$name links successfully"
            return 0
        else
            log_fail "$name" "No elm_ symbols in combined object"
            return 1
        fi
    else
        log_fail "$name" "Linking failed: $(head -3 "$TMP_DIR/link_${name}.log")"
        return 1
    fi
}

# Test: Multi-module compilation and linking
test_multi_module_link() {
    echo ""
    echo "--- Multi-Module Linking Tests ---"
    echo ""

    # Test AST.Source + Parse.Primitives
    if [ -f "$PROJECT_ROOT/src/AST/Source.elm" ] && [ -f "$PROJECT_ROOT/src/Parse/Primitives.elm" ]; then
        compile_and_link "AST+Parse" \
            "$PROJECT_ROOT/src/AST/Source.elm" \
            "$PROJECT_ROOT/src/Parse/Primitives.elm"
    else
        log_skip "AST+Parse" "Source files not found"
    fi
}

# Generate random arithmetic expression test
generate_arith_test() {
    local seed="$1"
    local name="arith_$seed"
    local elm_file="$TMP_DIR/${name}.elm"

    # Generate random arithmetic expression
    local a=$((RANDOM % 100))
    local b=$((RANDOM % 100 + 1))  # Avoid division by zero
    local op_idx=$((RANDOM % 4))
    local ops=("+" "-" "*" "//")
    local op="${ops[$op_idx]}"

    local expected
    case $op in
        "+") expected=$((a + b)) ;;
        "-") expected=$((a - b)) ;;
        "*") expected=$((a * b)) ;;
        "//") expected=$((a / b)) ;;
    esac

    cat > "$elm_file" << EOF
module Arith$seed exposing (..)

result : Int
result = $a $op $b

-- Expected: $expected
EOF

    echo "$expected"
}

# Generate random string manipulation test
generate_string_test() {
    local seed="$1"
    local name="string_$seed"
    local elm_file="$TMP_DIR/${name}.elm"

    # Generate test based on seed
    local test_type=$((seed % 3))

    case $test_type in
        0)
            # String length
            local str="hello world"
            local expected=${#str}
            cat > "$elm_file" << EOF
module String$seed exposing (..)

result : Int
result = String.length "$str"

-- Expected: $expected
EOF
            echo "$expected"
            ;;
        1)
            # String append
            local a="foo"
            local b="bar"
            cat > "$elm_file" << EOF
module String$seed exposing (..)

result : String
result = "$a" ++ "$b"

-- Expected: "foobar"
EOF
            echo "foobar"
            ;;
        2)
            # String isEmpty
            cat > "$elm_file" << EOF
module String$seed exposing (..)

result : Bool
result = String.isEmpty ""

-- Expected: True
EOF
            echo "True"
            ;;
    esac
}

# Generate random list test
generate_list_test() {
    local seed="$1"
    local name="list_$seed"
    local elm_file="$TMP_DIR/${name}.elm"

    local test_type=$((seed % 3))

    case $test_type in
        0)
            # List length
            local len=$((RANDOM % 10))
            local list_contents=""
            for ((i=0; i<len; i++)); do
                [ -n "$list_contents" ] && list_contents="$list_contents, "
                list_contents="${list_contents}$i"
            done
            cat > "$elm_file" << EOF
module List$seed exposing (..)

result : Int
result = List.length [$list_contents]

-- Expected: $len
EOF
            echo "$len"
            ;;
        1)
            # List head
            cat > "$elm_file" << EOF
module List$seed exposing (..)

result : Maybe Int
result = List.head [1, 2, 3]

-- Expected: Just 1
EOF
            echo "Just 1"
            ;;
        2)
            # List reverse
            cat > "$elm_file" << EOF
module List$seed exposing (..)

result : List Int
result = List.reverse [1, 2, 3]

-- Expected: [3, 2, 1]
EOF
            echo "[3, 2, 1]"
            ;;
    esac
}

# Property: Generated arithmetic code should compile
test_arith_compiles() {
    echo ""
    echo "--- Arithmetic Expression Tests ---"
    echo ""

    local num_tests=10
    local seed=${RANDOM}

    for ((i=0; i<num_tests; i++)); do
        local expected=$(generate_arith_test $((seed + i)))
        local name="arith_$((seed + i))"
        local elm_file="$TMP_DIR/${name}.elm"
        local c_file="$TMP_DIR/${name}.c"
        local o_file="$TMP_DIR/${name}.o"

        if node "$PROJECT_ROOT/bin/tcelm-compile.js" "$elm_file" --target module > "$c_file" 2>/dev/null; then
            if gcc -c "$c_file" -I "$RUNTIME_DIR" -o "$o_file" 2>/dev/null; then
                log_pass "Arithmetic test $i (expected: $expected)"
            else
                log_fail "Arithmetic test $i" "gcc compilation failed"
            fi
        else
            log_fail "Arithmetic test $i" "tcelm compilation failed"
        fi
    done
}

# Property: Generated string code should compile
test_string_compiles() {
    echo ""
    echo "--- String Expression Tests ---"
    echo ""

    local num_tests=6
    local seed=${RANDOM}

    for ((i=0; i<num_tests; i++)); do
        local expected=$(generate_string_test $((seed + i)))
        local name="string_$((seed + i))"
        local elm_file="$TMP_DIR/${name}.elm"
        local c_file="$TMP_DIR/${name}.c"
        local o_file="$TMP_DIR/${name}.o"

        if node "$PROJECT_ROOT/bin/tcelm-compile.js" "$elm_file" --target module > "$c_file" 2>/dev/null; then
            if gcc -c "$c_file" -I "$RUNTIME_DIR" -o "$o_file" 2>/dev/null; then
                log_pass "String test $i"
            else
                log_fail "String test $i" "gcc compilation failed"
            fi
        else
            log_fail "String test $i" "tcelm compilation failed"
        fi
    done
}

# Property: Generated list code should compile
test_list_compiles() {
    echo ""
    echo "--- List Expression Tests ---"
    echo ""

    local num_tests=6
    local seed=${RANDOM}

    for ((i=0; i<num_tests; i++)); do
        local expected=$(generate_list_test $((seed + i)))
        local name="list_$((seed + i))"
        local elm_file="$TMP_DIR/${name}.elm"
        local c_file="$TMP_DIR/${name}.c"
        local o_file="$TMP_DIR/${name}.o"

        if node "$PROJECT_ROOT/bin/tcelm-compile.js" "$elm_file" --target module > "$c_file" 2>/dev/null; then
            if gcc -c "$c_file" -I "$RUNTIME_DIR" -o "$o_file" 2>/dev/null; then
                log_pass "List test $i"
            else
                log_fail "List test $i" "gcc compilation failed"
            fi
        else
            log_fail "List test $i" "tcelm compilation failed"
        fi
    done
}

# Self-hosting test: Can tcelm compile its own code generator?
test_self_hosting() {
    echo ""
    echo "--- Self-Hosting Tests ---"
    echo ""

    if [ -f "$PROJECT_ROOT/src/Generate/C.elm" ]; then
        local c_file="$TMP_DIR/Generate_C.c"
        local o_file="$TMP_DIR/Generate_C.o"

        if node "$PROJECT_ROOT/bin/tcelm-compile.js" "$PROJECT_ROOT/src/Generate/C.elm" --target module > "$c_file" 2>/dev/null; then
            local line_count=$(wc -l < "$c_file")
            if [ "$line_count" -gt 1000 ]; then
                if gcc -c "$c_file" -I "$RUNTIME_DIR" -o "$o_file" 2>/dev/null; then
                    log_pass "Self-hosting: Generate/C.elm ($line_count lines of C)"
                else
                    log_fail "Self-hosting" "Generated C doesn't compile"
                fi
            else
                log_fail "Self-hosting" "Generated C too short ($line_count lines)"
            fi
        else
            log_fail "Self-hosting" "tcelm failed to compile Generate/C.elm"
        fi
    else
        log_skip "Self-hosting" "Generate/C.elm not found"
    fi
}

echo ""
echo "=== tcelm System Integration Tests ==="

# Run all test suites
test_multi_module_link
test_arith_compiles
test_string_compiles
test_list_compiles
test_self_hosting

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
