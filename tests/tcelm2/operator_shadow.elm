-- expect: 55
module Test exposing (main)

-- Test that user variable names don't conflict with operators
main =
    let
        a = 10
        b = 5
        c = 3

        -- Arithmetic
        mySum = a + b + c        -- 18
        myProd = a * b           -- 50
        myDiff = myProd - mySum  -- 32

        -- Local vars with operator-like names (gt, lt, eq, and, or)
        gt = if a > b then 1 else 0   -- 1
        lt = if b < a then 1 else 0   -- 1
        eq = if c == 3 then 1 else 0  -- 1

        -- These should use the comparison operators, not the local vars
        andRes = if (a > 5) && (b < 10) then 1 else 0  -- 1
        orRes = if (a < 5) || (b > 2) then 1 else 0    -- 1
    in
    myDiff + gt + lt + eq + andRes + orRes + mySum - (mySum - 18)
    -- 32 + 1 + 1 + 1 + 1 + 1 + 18 - 0 = 55
