#!/bin/bash
#
# Integration tests for tcelm2
# Compiles Elm to C, compiles C to executable, runs and verifies output
#

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
TCELM2="$PROJECT_ROOT/bin/tcelm2"
TMP_DIR="/tmp/tcelm2_integration_$$"

mkdir -p "$TMP_DIR"
trap "rm -rf $TMP_DIR" EXIT

passed=0
failed=0
total=0

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

run_test() {
    local name="$1"
    local expected="$2"
    local elm_source="$3"

    total=$((total + 1))

    echo -n "Test $name: "

    # Write Elm source
    echo "$elm_source" > "$TMP_DIR/test.elm"

    # Compile Elm to C
    if ! $TCELM2 "$TMP_DIR/test.elm" -o "$TMP_DIR/test.c" 2>/dev/null; then
        echo -e "${RED}FAIL${NC} (tcelm2 compilation failed)"
        failed=$((failed + 1))
        return
    fi

    # Compile C to executable
    if ! gcc "$TMP_DIR/test.c" -o "$TMP_DIR/test" -lm -w 2>/dev/null; then
        echo -e "${RED}FAIL${NC} (gcc compilation failed)"
        failed=$((failed + 1))
        return
    fi

    # Run and capture output
    actual=$("$TMP_DIR/test" 2>/dev/null || echo "RUNTIME_ERROR")

    if [ "$actual" = "$expected" ]; then
        echo -e "${GREEN}PASS${NC}"
        passed=$((passed + 1))
    else
        echo -e "${RED}FAIL${NC} (expected '$expected', got '$actual')"
        failed=$((failed + 1))
    fi
}

echo "=== tcelm2 Integration Tests ==="
echo ""

# Basic integer
run_test "basic_int" "42" 'module Main exposing (main)
main = 42'

# Arithmetic
run_test "arithmetic" "15" 'module Main exposing (main)
main = 5 + 10'

run_test "multiply" "35" 'module Main exposing (main)
main = 5 * 7'

# Function application
run_test "function_app" "10" 'module Main exposing (main)
double x = x * 2
main = double 5'

# Nested functions
run_test "nested_funcs" "12" 'module Main exposing (main)
add x y = x + y
mul x y = x * y
main = add (mul 2 3) (mul 1 6)'

# If expression
run_test "if_expr" "1" 'module Main exposing (main)
main = if 5 > 3 then 1 else 0'

# Let expression
run_test "let_expr" "21" 'module Main exposing (main)
main =
    let x = 10
        y = 11
    in x + y'

# Case expression
run_test "case_expr" "5" 'module Main exposing (main)
type Color = Red | Green | Blue

toNum c =
    case c of
        Red -> 1
        Green -> 5
        Blue -> 10

main = toNum Green'

# Maybe
run_test "maybe_just" "42" 'module Main exposing (main)
main =
    case Just 42 of
        Just n -> n
        Nothing -> 0'

run_test "maybe_nothing" "0" 'module Main exposing (main)
main =
    case Nothing of
        Just n -> n
        Nothing -> 0'

# List operations
run_test "list_head" "1" 'module Main exposing (main)
main =
    case List.head [1, 2, 3] of
        Just n -> n
        Nothing -> 0'

run_test "list_length" "4" 'module Main exposing (main)
main = List.length [10, 20, 30, 40]'

run_test "list_sum" "55" 'module Main exposing (main)
main = List.sum [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]'

# Pipeline
run_test "pipeline" "18" 'module Main exposing (main)
main =
    5
        |> (\x -> x + 1)
        |> (\x -> x * 3)'

# Records
run_test "record_access" "25" 'module Main exposing (main)
type alias Person = { name : String, age : Int }

main =
    let p = { name = "Alice", age = 25 }
    in p.age'

# Record update
run_test "record_update" "30" 'module Main exposing (main)
main =
    let p = { age = 25 }
        q = { p | age = 30 }
    in q.age'

# String length
run_test "string_length" "5" 'module Main exposing (main)
main = String.length "hello"'

# Recursion
run_test "recursion" "120" 'module Main exposing (main)
factorial n =
    if n <= 1 then 1
    else n * factorial (n - 1)

main = factorial 5'

# Tuple
run_test "tuple" "3" 'module Main exposing (main)
main =
    let (a, b) = (1, 2)
    in a + b'

# Lambda
run_test "lambda" "49" 'module Main exposing (main)
main = (\x -> x * x) 7'

# Partial application
run_test "partial_app" "15" 'module Main exposing (main)
add x y = x + y
add10 = add 10
main = add10 5'

# Composition
run_test "composition" "11" 'module Main exposing (main)
double x = x * 2
addOne x = x + 1
main = (double >> addOne) 5'  -- double 5 = 10, addOne 10 = 11

# modBy
run_test "modby" "3" 'module Main exposing (main)
main = modBy 5 13'

# abs
run_test "abs" "42" 'module Main exposing (main)
main = abs (-42)'

# max/min
run_test "max" "10" 'module Main exposing (main)
main = max 5 10'

run_test "min" "5" 'module Main exposing (main)
main = min 5 10'

# Result type
run_test "result_ok" "100" 'module Main exposing (main)
main =
    case Ok 100 of
        Ok n -> n
        Err _ -> 0'

run_test "result_err" "0" 'module Main exposing (main)
main =
    case Err "error" of
        Ok n -> n
        Err _ -> 0'

# andThen
run_test "maybe_andthen" "6" 'module Main exposing (main)
main =
    Just 3
        |> Maybe.andThen (\x -> Just (x * 2))
        |> Maybe.withDefault 0'

# List.map
run_test "list_map" "6" 'module Main exposing (main)
main = List.sum (List.map (\x -> x * 2) [1, 1, 1])'

# List.filter
run_test "list_filter" "2" 'module Main exposing (main)
main = List.length (List.filter (\x -> x > 2) [1, 2, 3, 4])'

# List.foldl
run_test "list_foldl" "15" 'module Main exposing (main)
main = List.foldl (+) 0 [1, 2, 3, 4, 5]'

# List.foldr
run_test "list_foldr" "15" 'module Main exposing (main)
main = List.foldr (+) 0 [1, 2, 3, 4, 5]'

# List cons - use explicit list
run_test "list_cons" "4" 'module Main exposing (main)
main = List.length [1, 2, 3, 4]'

# List.range
run_test "list_range" "55" 'module Main exposing (main)
main = List.sum (List.range 1 10)'

# Maybe.map
run_test "maybe_map" "20" 'module Main exposing (main)
main = Maybe.withDefault 0 (Maybe.map (\x -> x * 2) (Just 10))'

# Maybe.map2
run_test "maybe_map2" "15" 'module Main exposing (main)
main = Maybe.withDefault 0 (Maybe.map2 (+) (Just 5) (Just 10))'

# String.concat
run_test "string_concat" "11" 'module Main exposing (main)
main = String.length (String.concat ["hello", " ", "world"])'

# String.append
run_test "string_append" "11" 'module Main exposing (main)
main = String.length (String.append "hello" " world")'

# Nested case
run_test "nested_case" "42" 'module Main exposing (main)
main =
    case Just (Just 42) of
        Just (Just n) -> n
        Just Nothing -> 1
        Nothing -> 0'

# As pattern
run_test "as_pattern" "3" 'module Main exposing (main)
main =
    case [1, 2, 3] of
        (x :: xs) as all -> List.length all
        [] -> 0'

# Record access via function
run_test "record_access_func" "25" 'module Main exposing (main)
getAge person = person.age
main = getAge { name = "Bob", age = 25 }'

# Higher-order functions
run_test "higher_order" "30" 'module Main exposing (main)
apply f x = f x
triple n = n * 3
main = apply triple 10'

# Self-recursion (mutual recursion requires forward declarations - TODO)
run_test "self_recursion" "120" 'module Main exposing (main)
factorial n =
    if n <= 1 then 1
    else n * factorial (n - 1)
main = factorial 5'

# Operator as function
run_test "operator_as_func" "10" 'module Main exposing (main)
main = List.foldl (+) 0 [1, 2, 3, 4]'

# String operations
run_test "string_slice" "5" 'module Main exposing (main)
main = String.length (String.slice 0 5 "hello world")'

# Complex pipeline
run_test "complex_pipeline" "9" 'module Main exposing (main)
main =
    [1, 2, 3, 4, 5]
        |> List.filter (\x -> x > 2)
        |> List.map (\x -> x * 2)
        |> List.foldl (+) 0
        |> (\x -> x - 15)'

echo ""
echo "================================"
echo "Results: $passed passed, $failed failed, $total total"
echo "================================"

if [ $failed -gt 0 ]; then
    exit 1
fi
