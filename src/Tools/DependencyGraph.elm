module Tools.DependencyGraph exposing
    ( Graph, Node, Edge
    , buildGraph, fromModules
    , toDot, toMermaid, toAscii
    , topologicalSort, detectCycles, findPath
    , directDependencies, transitiveDependencies, dependents
    )

{-| Module Dependency Graph - Visualization and analysis of module dependencies.

@docs Graph, Node, Edge
@docs buildGraph, fromModules
@docs toDot, toMermaid, toAscii
@docs topologicalSort, detectCycles, findPath
@docs directDependencies, transitiveDependencies, dependents

-}

import AST.Source as Src exposing (Located(..))
import Dict exposing (Dict)
import Set exposing (Set)


{-| A dependency graph.
-}
type alias Graph =
    { nodes : List Node
    , edges : List Edge
    }


{-| A node in the dependency graph (a module).
-}
type alias Node =
    { name : String
    , path : Maybe String
    }


{-| An edge in the dependency graph (an import relationship).
-}
type alias Edge =
    { from : String
    , to : String
    , exposing_ : ExposingInfo
    }


{-| What is exposed in an import.
-}
type ExposingInfo
    = ExposingAll
    | ExposingSome (List String)
    | ExposingNone



-- BUILDING GRAPHS


{-| Build a graph from a list of parsed modules.
-}
fromModules : List ( String, Src.Module ) -> Graph
fromModules modules =
    let
        nodes =
            List.map
                (\( path, mod ) ->
                    { name = getModuleName mod
                    , path = Just path
                    }
                )
                modules

        edges =
            List.concatMap
                (\( _, mod ) ->
                    let
                        moduleName =
                            getModuleName mod
                    in
                    List.map (importToEdge moduleName) mod.imports
                )
                modules
    in
    { nodes = nodes, edges = edges }


getModuleName : Src.Module -> String
getModuleName mod =
    case mod.name of
        Just (At _ name) ->
            name

        Nothing ->
            "Main"


importToEdge : String -> Src.Import -> Edge
importToEdge fromModule imp =
    let
        (At _ toModule) =
            imp.name

        exposingInfo =
            case imp.exposing_ of
                Src.Open ->
                    ExposingAll

                Src.Explicit [] ->
                    ExposingNone

                Src.Explicit exposed ->
                    ExposingSome (List.map exposedName exposed)
    in
    { from = fromModule
    , to = toModule
    , exposing_ = exposingInfo
    }


exposedName : Src.Exposed -> String
exposedName exp =
    case exp of
        Src.Lower (At _ name) ->
            name

        Src.Upper (At _ name) _ ->
            name

        Src.Operator _ name ->
            "(" ++ name ++ ")"


{-| Build a graph from import statements.
-}
buildGraph : List { module_ : String, imports : List String } -> Graph
buildGraph modules =
    let
        nodes =
            List.map (\m -> { name = m.module_, path = Nothing }) modules

        edges =
            List.concatMap
                (\m ->
                    List.map
                        (\imp ->
                            { from = m.module_
                            , to = imp
                            , exposing_ = ExposingNone
                            }
                        )
                        m.imports
                )
                modules
    in
    { nodes = nodes, edges = edges }



-- VISUALIZATION


{-| Export graph to DOT format (for Graphviz).
-}
toDot : Graph -> String
toDot graph =
    let
        nodeLines =
            List.map (\n -> "  \"" ++ n.name ++ "\";") graph.nodes

        edgeLines =
            List.map (\e -> "  \"" ++ e.from ++ "\" -> \"" ++ e.to ++ "\";") graph.edges
    in
    String.join "\n"
        [ "digraph Dependencies {"
        , "  rankdir=TB;"
        , "  node [shape=box];"
        , String.join "\n" nodeLines
        , String.join "\n" edgeLines
        , "}"
        ]


{-| Export graph to Mermaid format.
-}
toMermaid : Graph -> String
toMermaid graph =
    let
        edgeLines =
            List.map (\e -> "  " ++ sanitizeMermaid e.from ++ " --> " ++ sanitizeMermaid e.to) graph.edges
    in
    String.join "\n"
        [ "graph TD"
        , String.join "\n" edgeLines
        ]


sanitizeMermaid : String -> String
sanitizeMermaid name =
    String.replace "." "_" name


{-| Export graph to ASCII art.
-}
toAscii : Graph -> String
toAscii graph =
    let
        -- Group by source module
        grouped =
            List.foldl
                (\edge acc ->
                    Dict.update edge.from
                        (\maybe ->
                            case maybe of
                                Just deps ->
                                    Just (edge.to :: deps)

                                Nothing ->
                                    Just [ edge.to ]
                        )
                        acc
                )
                Dict.empty
                graph.edges

        nodeLines =
            List.map
                (\node ->
                    let
                        deps =
                            Dict.get node.name grouped
                                |> Maybe.withDefault []

                        depStr =
                            if List.isEmpty deps then
                                ""

                            else
                                " -> " ++ String.join ", " deps
                    in
                    node.name ++ depStr
                )
                graph.nodes
    in
    String.join "\n" nodeLines



-- ANALYSIS


{-| Get direct dependencies of a module.
-}
directDependencies : String -> Graph -> List String
directDependencies moduleName graph =
    graph.edges
        |> List.filter (\e -> e.from == moduleName)
        |> List.map .to


{-| Get transitive dependencies of a module.
-}
transitiveDependencies : String -> Graph -> Set String
transitiveDependencies moduleName graph =
    transitiveDepsHelper [ moduleName ] Set.empty graph


transitiveDepsHelper : List String -> Set String -> Graph -> Set String
transitiveDepsHelper queue visited graph =
    case queue of
        [] ->
            visited

        current :: rest ->
            if Set.member current visited then
                transitiveDepsHelper rest visited graph

            else
                let
                    newVisited =
                        Set.insert current visited

                    deps =
                        directDependencies current graph

                    newQueue =
                        rest ++ List.filter (\d -> not (Set.member d newVisited)) deps
                in
                transitiveDepsHelper newQueue newVisited graph


{-| Get modules that depend on the given module.
-}
dependents : String -> Graph -> List String
dependents moduleName graph =
    graph.edges
        |> List.filter (\e -> e.to == moduleName)
        |> List.map .from


{-| Perform topological sort of modules.
-}
topologicalSort : Graph -> Result (List String) (List String)
topologicalSort graph =
    let
        nodeNames =
            List.map .name graph.nodes

        inDegree =
            List.foldl
                (\name acc -> Dict.insert name 0 acc)
                Dict.empty
                nodeNames
                |> (\d ->
                        List.foldl
                            (\edge acc ->
                                Dict.update edge.to
                                    (Maybe.map ((+) 1))
                                    acc
                            )
                            d
                            graph.edges
                   )

        startNodes =
            Dict.toList inDegree
                |> List.filter (\( _, deg ) -> deg == 0)
                |> List.map Tuple.first
    in
    topoSortHelper startNodes inDegree graph []


topoSortHelper : List String -> Dict String Int -> Graph -> List String -> Result (List String) (List String)
topoSortHelper queue inDegree graph result =
    case queue of
        [] ->
            if List.length result == List.length graph.nodes then
                Ok (List.reverse result)

            else
                -- Cycle detected
                let
                    remaining =
                        Dict.keys inDegree
                            |> List.filter (\n -> not (List.member n result))
                in
                Err remaining

        current :: rest ->
            let
                newResult =
                    current :: result

                deps =
                    directDependencies current graph

                ( newQueue, newInDegree ) =
                    List.foldl
                        (\dep ( q, d ) ->
                            let
                                newDeg =
                                    Dict.get dep d
                                        |> Maybe.withDefault 0
                                        |> (\x -> x - 1)

                                updatedD =
                                    Dict.insert dep newDeg d

                                updatedQ =
                                    if newDeg == 0 then
                                        q ++ [ dep ]

                                    else
                                        q
                            in
                            ( updatedQ, updatedD )
                        )
                        ( rest, inDegree )
                        deps
            in
            topoSortHelper newQueue newInDegree graph newResult


{-| Detect cycles in the dependency graph.
-}
detectCycles : Graph -> List (List String)
detectCycles graph =
    case topologicalSort graph of
        Ok _ ->
            []

        Err cycleNodes ->
            -- Find actual cycles using DFS
            findCyclesIn cycleNodes graph


findCyclesIn : List String -> Graph -> List (List String)
findCyclesIn nodes graph =
    let
        findCycleFrom : String -> List String -> Set String -> Maybe (List String)
        findCycleFrom node path visited =
            if List.member node path then
                -- Found a cycle
                let
                    cycleStart =
                        List.head (List.filter ((==) node) path)
                in
                case cycleStart of
                    Just _ ->
                        Just (node :: takeUntil node path)

                    Nothing ->
                        Nothing

            else if Set.member node visited then
                Nothing

            else
                let
                    deps =
                        directDependencies node graph

                    newPath =
                        node :: path

                    newVisited =
                        Set.insert node visited
                in
                List.filterMap (\d -> findCycleFrom d newPath newVisited) deps
                    |> List.head
    in
    List.filterMap (\n -> findCycleFrom n [] Set.empty) nodes


takeUntil : String -> List String -> List String
takeUntil target list =
    case list of
        [] ->
            []

        x :: rest ->
            if x == target then
                [ x ]

            else
                x :: takeUntil target rest


{-| Find a path between two modules.
-}
findPath : String -> String -> Graph -> Maybe (List String)
findPath from to graph =
    findPathHelper [ [ from ] ] to Set.empty graph


findPathHelper : List (List String) -> String -> Set String -> Graph -> Maybe (List String)
findPathHelper queue target visited graph =
    case queue of
        [] ->
            Nothing

        path :: rest ->
            case path of
                [] ->
                    findPathHelper rest target visited graph

                current :: _ ->
                    if current == target then
                        Just (List.reverse path)

                    else if Set.member current visited then
                        findPathHelper rest target visited graph

                    else
                        let
                            newVisited =
                                Set.insert current visited

                            deps =
                                directDependencies current graph

                            newPaths =
                                List.map (\d -> d :: path) deps
                        in
                        findPathHelper (rest ++ newPaths) target newVisited graph
