module Parse.Keyword exposing
    ( if_, then_, else_
    , case_, of_
    , let_, in_
    , type_, alias_
    , port_
    , module_, exposing_, import_, as_
    , where_
    , effect_, command_, subscription_
    , infix_, left_, right_, non_
    )

{-| Keyword parsers for Elm reserved words.
-}

import Parse.Primitives exposing (Parser, keyword)


if_ : x -> Parser x ()
if_ expecting =
    keyword "if" expecting


then_ : x -> Parser x ()
then_ expecting =
    keyword "then" expecting


else_ : x -> Parser x ()
else_ expecting =
    keyword "else" expecting


case_ : x -> Parser x ()
case_ expecting =
    keyword "case" expecting


of_ : x -> Parser x ()
of_ expecting =
    keyword "of" expecting


let_ : x -> Parser x ()
let_ expecting =
    keyword "let" expecting


in_ : x -> Parser x ()
in_ expecting =
    keyword "in" expecting


type_ : x -> Parser x ()
type_ expecting =
    keyword "type" expecting


alias_ : x -> Parser x ()
alias_ expecting =
    keyword "alias" expecting


port_ : x -> Parser x ()
port_ expecting =
    keyword "port" expecting


module_ : x -> Parser x ()
module_ expecting =
    keyword "module" expecting


exposing_ : x -> Parser x ()
exposing_ expecting =
    keyword "exposing" expecting


import_ : x -> Parser x ()
import_ expecting =
    keyword "import" expecting


as_ : x -> Parser x ()
as_ expecting =
    keyword "as" expecting


where_ : x -> Parser x ()
where_ expecting =
    keyword "where" expecting


effect_ : x -> Parser x ()
effect_ expecting =
    keyword "effect" expecting


command_ : x -> Parser x ()
command_ expecting =
    keyword "command" expecting


subscription_ : x -> Parser x ()
subscription_ expecting =
    keyword "subscription" expecting


infix_ : x -> Parser x ()
infix_ expecting =
    keyword "infix" expecting


left_ : x -> Parser x ()
left_ expecting =
    keyword "left" expecting


right_ : x -> Parser x ()
right_ expecting =
    keyword "right" expecting


non_ : x -> Parser x ()
non_ expecting =
    keyword "non" expecting
