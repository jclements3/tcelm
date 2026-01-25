-- expect: 22
module Test exposing (main)

dimap : (a -> b) -> (c -> d) -> (b -> c) -> (a -> d)
dimap f g h =
    g << h << f

processString : String -> Int
processString s = String.length s

transformed : String -> Int
transformed = dimap (\s -> String.append "hello " s) (\n -> n * 2) processString

result : Int
result = transformed "world"

main = result
