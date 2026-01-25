#!/bin/bash
# Comprehensive test suite for tcelm2

TCELM2="./bin/tcelm2"
PASS=0
FAIL=0

run_test() {
    name="$1"
    expected="$2"
    file="$3"
    
    result=$($TCELM2 "$file" -o /tmp/test_out.c 2>/dev/null && tcc -run /tmp/test_out.c 2>/dev/null)
    
    if [ "$result" = "$expected" ]; then
        echo "✓ $name"
        ((PASS++))
    else
        echo "✗ $name (expected: $expected, got: $result)"
        ((FAIL++))
    fi
}

echo "=== tcelm2 Test Suite ==="
echo ""

# Run all tests
for test in tests/tcelm2/*.elm; do
    if [ -f "$test" ]; then
        expected=$(head -1 "$test" | sed 's/-- expect: //')
        name=$(basename "$test" .elm)
        run_test "$name" "$expected" "$test"
    fi
done

echo ""
echo "=== Results: $PASS passed, $FAIL failed ==="
exit $FAIL
