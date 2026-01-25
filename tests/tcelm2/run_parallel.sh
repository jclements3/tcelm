#!/bin/bash
# Parallel test suite for tcelm2
# Uses 7 cores for parallel execution

TCELM2="./bin/tcelm2"
CORES=7
TMPDIR="/tmp/tcelm2_test_$$"

mkdir -p "$TMPDIR"

run_single_test() {
    local file="$1"
    local name=$(basename "$file" .elm)
    local expected=$(head -1 "$file" | sed 's/-- expect: //')
    local outfile="$TMPDIR/${name}.c"

    result=$($TCELM2 "$file" -o "$outfile" 2>/dev/null && tcc -run "$outfile" 2>/dev/null)

    if [ "$result" = "$expected" ]; then
        echo "PASS:$name"
    else
        echo "FAIL:$name:$expected:$result"
    fi
}

export -f run_single_test
export TCELM2
export TMPDIR

echo "=== tcelm2 Parallel Test Suite (using $CORES cores) ==="
echo ""

# Find all .elm test files and run in parallel
results=$(find tests/tcelm2 -name "*.elm" -type f | \
    parallel -j$CORES run_single_test {} 2>/dev/null)

if [ -z "$results" ]; then
    # Fallback if parallel not available
    echo "GNU parallel not found, using xargs..."
    results=$(find tests/tcelm2 -name "*.elm" -type f | \
        xargs -P$CORES -I{} bash -c 'run_single_test "$@"' _ {})
fi

PASS=0
FAIL=0
FAILED_TESTS=""

while IFS= read -r line; do
    status=$(echo "$line" | cut -d: -f1)
    name=$(echo "$line" | cut -d: -f2)

    if [ "$status" = "PASS" ]; then
        echo "✓ $name"
        ((PASS++))
    elif [ "$status" = "FAIL" ]; then
        expected=$(echo "$line" | cut -d: -f3)
        got=$(echo "$line" | cut -d: -f4)
        echo "✗ $name (expected: $expected, got: $got)"
        ((FAIL++))
        FAILED_TESTS="$FAILED_TESTS $name"
    fi
done <<< "$results"

# Cleanup
rm -rf "$TMPDIR"

echo ""
echo "=== Results: $PASS passed, $FAIL failed ==="

if [ $FAIL -gt 0 ]; then
    echo "Failed tests:$FAILED_TESTS"
fi

exit $FAIL
