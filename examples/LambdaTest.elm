module LambdaTest exposing (main)

-- Test simple lambda
addOne : Int -> Int
addOne x =
    (\n -> n + 1) x


-- Test pipeline operator
pipeline : Int -> Int
pipeline x =
    x |> (\n -> n * 2)


-- Main function
main : Int
main =
    addOne 5 + pipeline 3
