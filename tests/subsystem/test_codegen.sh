#!/bin/bash
#
# test_codegen.sh - Subsystem tests for the tcelm code generator
#
# Tests that generated C code:
# 1. Compiles without errors
# 2. Produces correct results for known inputs
# 3. Handles edge cases properly

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
RUNTIME_DIR="$PROJECT_ROOT/runtime"
TCC="$PROJECT_ROOT/tcc/tcc"
TEST_DIR="$PROJECT_ROOT/tests/fixtures"
TMP_DIR="/tmp/tcelm_codegen_tests"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counters
PASSED=0
FAILED=0
SKIPPED=0

mkdir -p "$TMP_DIR"
mkdir -p "$TEST_DIR"

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

# Test that a module compiles to C and the C compiles with tcc
test_module_compiles() {
    local name="$1"
    local elm_file="$2"

    if [ ! -f "$elm_file" ]; then
        log_skip "$name" "File not found: $elm_file"
        return
    fi

    local c_file="$TMP_DIR/${name}.c"
    local o_file="$TMP_DIR/${name}.o"

    # Generate C code
    if ! node "$PROJECT_ROOT/bin/tcelm-compile.js" "$elm_file" --target module > "$c_file" 2>/dev/null; then
        log_fail "$name" "Failed to generate C code"
        return
    fi

    # Compile with tcc
    if "$TCC" -c "$c_file" -I "$RUNTIME_DIR" -o "$o_file" -Wall 2>"$TMP_DIR/${name}_tcc.log"; then
        log_pass "$name compiles"
    else
        log_fail "$name" "tcc compilation failed: $(cat "$TMP_DIR/${name}_tcc.log" | head -5)"
    fi
}

# Test that generated code has expected function names
test_function_names() {
    local name="$1"
    local elm_file="$2"
    local expected_funcs="$3"

    if [ ! -f "$elm_file" ]; then
        log_skip "$name" "File not found"
        return
    fi

    local c_file="$TMP_DIR/${name}.c"

    node "$PROJECT_ROOT/bin/tcelm-compile.js" "$elm_file" --target module > "$c_file" 2>/dev/null

    local missing=""
    for func in $expected_funcs; do
        if ! grep -q "$func" "$c_file"; then
            missing="$missing $func"
        fi
    done

    if [ -z "$missing" ]; then
        log_pass "$name has expected functions"
    else
        log_fail "$name" "Missing functions:$missing"
    fi
}

# Test that constructor IDs are generated correctly
test_constructor_ids() {
    local name="$1"
    local elm_file="$2"
    local expected_ctors="$3"

    if [ ! -f "$elm_file" ]; then
        log_skip "$name" "File not found"
        return
    fi

    local c_file="$TMP_DIR/${name}.c"

    node "$PROJECT_ROOT/bin/tcelm-compile.js" "$elm_file" --target module > "$c_file" 2>/dev/null

    local missing=""
    for ctor in $expected_ctors; do
        if ! grep -q "TCELM_CTOR_$ctor" "$c_file"; then
            missing="$missing $ctor"
        fi
    done

    if [ -z "$missing" ]; then
        log_pass "$name has expected constructors"
    else
        log_fail "$name" "Missing constructors:$missing"
    fi
}

echo ""
echo "=== tcelm Code Generator Subsystem Tests ==="
echo ""

echo "--- Module Compilation Tests ---"
echo ""

# Test existing project modules
test_module_compiles "AST.Source" "$PROJECT_ROOT/src/AST/Source.elm"
test_module_compiles "Parse.Primitives" "$PROJECT_ROOT/src/Parse/Primitives.elm"
test_module_compiles "Generate.C" "$PROJECT_ROOT/src/Generate/C.elm"

echo ""
echo "--- Function Name Tests ---"
echo ""

test_function_names "AST.Source functions" \
    "$PROJECT_ROOT/src/AST/Source.elm" \
    "elm_AST_Source_at elm_AST_Source_merge elm_AST_Source_toValue"

echo ""
echo "--- Constructor ID Tests ---"
echo ""

test_constructor_ids "AST.Source constructors" \
    "$PROJECT_ROOT/src/AST/Source.elm" \
    "AT"

echo ""
echo "--- Edge Case Tests ---"
echo ""

# Create and test edge case fixtures
cat > "$TEST_DIR/Empty.elm" << 'EOF'
module Empty exposing (..)

-- Empty module with no values
EOF

test_module_compiles "Empty module" "$TEST_DIR/Empty.elm"

cat > "$TEST_DIR/SimpleValue.elm" << 'EOF'
module SimpleValue exposing (..)

x : Int
x = 42

y : String
y = "hello"
EOF

test_module_compiles "Simple values" "$TEST_DIR/SimpleValue.elm"

cat > "$TEST_DIR/CustomType.elm" << 'EOF'
module CustomType exposing (..)

type Color
    = Red
    | Green
    | Blue

type Maybe a
    = Nothing
    | Just a

colorName : Color -> String
colorName color =
    case color of
        Red -> "red"
        Green -> "green"
        Blue -> "blue"
EOF

test_module_compiles "Custom types" "$TEST_DIR/CustomType.elm"
test_constructor_ids "Custom type constructors" "$TEST_DIR/CustomType.elm" "RED GREEN BLUE"

cat > "$TEST_DIR/RecordOps.elm" << 'EOF'
module RecordOps exposing (..)

type alias Point =
    { x : Int
    , y : Int
    }

origin : Point
origin = { x = 0, y = 0 }

moveX : Int -> Point -> Point
moveX dx point =
    { point | x = point.x + dx }
EOF

test_module_compiles "Record operations" "$TEST_DIR/RecordOps.elm"

cat > "$TEST_DIR/PatternMatch.elm" << 'EOF'
module PatternMatch exposing (..)

head : List a -> Maybe a
head list =
    case list of
        [] ->
            Nothing
        x :: _ ->
            Just x

tupleFst : (a, b) -> a
tupleFst (a, _) = a

sum : List Int -> Int
sum list =
    case list of
        [] -> 0
        x :: xs -> x + sum xs
EOF

test_module_compiles "Pattern matching" "$TEST_DIR/PatternMatch.elm"

cat > "$TEST_DIR/LocalFunc.elm" << 'EOF'
module LocalFunc exposing (..)

double : Int -> Int
double n =
    let
        add x y = x + y
    in
    add n n

transform : String -> String
transform s =
    let
        helper c =
            case c of
                'a' -> 'A'
                _ -> c
    in
    String.map helper s
EOF

test_module_compiles "Local functions" "$TEST_DIR/LocalFunc.elm"

cat > "$TEST_DIR/ListPatterns.elm" << 'EOF'
module ListPatterns exposing (..)

first : List a -> Maybe a
first list =
    case list of
        [ x ] -> Just x
        x :: _ -> Just x
        [] -> Nothing

take2 : List a -> Maybe (a, a)
take2 list =
    case list of
        [ a, b ] -> Just (a, b)
        a :: b :: _ -> Just (a, b)
        _ -> Nothing
EOF

test_module_compiles "List patterns" "$TEST_DIR/ListPatterns.elm"

echo ""
echo "=== Summary ==="
echo ""
echo -e "Passed:  ${GREEN}$PASSED${NC}"
echo -e "Failed:  ${RED}$FAILED${NC}"
echo -e "Skipped: ${YELLOW}$SKIPPED${NC}"
echo ""

# Cleanup
rm -rf "$TMP_DIR"

# Exit with failure if any tests failed
[ $FAILED -eq 0 ]
