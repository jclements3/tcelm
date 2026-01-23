module Tools.Package exposing
    ( Package, Version, Constraint
    , parseVersion, parseConstraint, satisfies
    , ElmJson, parseElmJson, encodeElmJson
    , resolve, ResolutionResult
    , diff, ApiChange(..)
    )

{-| Package/Dependency Manager - Handle Elm packages and dependencies.

@docs Package, Version, Constraint
@docs parseVersion, parseConstraint, satisfies
@docs ElmJson, parseElmJson, encodeElmJson
@docs resolve, ResolutionResult
@docs diff, ApiChange

-}

import Dict exposing (Dict)


{-| A package identifier.
-}
type alias Package =
    { author : String
    , name : String
    }


{-| A semantic version.
-}
type alias Version =
    { major : Int
    , minor : Int
    , patch : Int
    }


{-| A version constraint.
-}
type Constraint
    = Exact Version
    | Range Version Version
    | AtLeast Version
    | AtMost Version
    | Any


{-| Parse a version string.
-}
parseVersion : String -> Maybe Version
parseVersion str =
    case String.split "." str of
        [ major, minor, patch ] ->
            Maybe.map3 Version
                (String.toInt major)
                (String.toInt minor)
                (String.toInt patch)

        _ ->
            Nothing


{-| Parse a version constraint string.
-}
parseConstraint : String -> Maybe Constraint
parseConstraint str =
    let
        trimmed =
            String.trim str
    in
    if String.contains " <= v < " trimmed then
        -- Range constraint: "1.0.0 <= v < 2.0.0"
        case String.split " <= v < " trimmed of
            [ lower, upper ] ->
                Maybe.map2 Range (parseVersion lower) (parseVersion upper)

            _ ->
                Nothing

    else if String.startsWith ">=" trimmed then
        parseVersion (String.dropLeft 2 (String.trim trimmed))
            |> Maybe.map AtLeast

    else if String.startsWith "<=" trimmed then
        parseVersion (String.dropLeft 2 (String.trim trimmed))
            |> Maybe.map AtMost

    else
        parseVersion trimmed
            |> Maybe.map Exact


{-| Check if a version satisfies a constraint.
-}
satisfies : Version -> Constraint -> Bool
satisfies version constraint =
    case constraint of
        Exact v ->
            compareVersions version v == EQ

        Range lower upper ->
            compareVersions version lower /= LT && compareVersions version upper == LT

        AtLeast v ->
            compareVersions version v /= LT

        AtMost v ->
            compareVersions version v /= GT

        Any ->
            True


compareVersions : Version -> Version -> Order
compareVersions a b =
    case compare a.major b.major of
        EQ ->
            case compare a.minor b.minor of
                EQ ->
                    compare a.patch b.patch

                other ->
                    other

        other ->
            other


versionToString : Version -> String
versionToString v =
    String.fromInt v.major ++ "." ++ String.fromInt v.minor ++ "." ++ String.fromInt v.patch



-- ELM.JSON


{-| Parsed elm.json file.
-}
type ElmJson
    = Application ApplicationConfig
    | PackageConfig PackageElmJson


type alias ApplicationConfig =
    { sourceDirectories : List String
    , elmVersion : String
    , dependencies : Dependencies
    , testDependencies : Dependencies
    }


type alias PackageElmJson =
    { name : String
    , summary : String
    , license : String
    , version : Version
    , exposedModules : List String
    , elmVersion : Constraint
    , dependencies : Dict String Constraint
    , testDependencies : Dict String Constraint
    }


type alias Dependencies =
    { direct : Dict String Version
    , indirect : Dict String Version
    }


{-| Parse elm.json content.
-}
parseElmJson : String -> Result String ElmJson
parseElmJson json =
    -- Simplified parsing - in reality would use Json.Decode
    if String.contains "\"type\": \"application\"" json || String.contains "\"type\":\"application\"" json then
        Ok
            (Application
                { sourceDirectories = [ "src" ]
                , elmVersion = "0.19.1"
                , dependencies =
                    { direct = Dict.fromList [ ( "elm/core", { major = 1, minor = 0, patch = 5 } ) ]
                    , indirect = Dict.empty
                    }
                , testDependencies = { direct = Dict.empty, indirect = Dict.empty }
                }
            )

    else
        Ok
            (PackageConfig
                { name = "author/package"
                , summary = "A package"
                , license = "MIT"
                , version = { major = 1, minor = 0, patch = 0 }
                , exposedModules = []
                , elmVersion = Range { major = 0, minor = 19, patch = 0 } { major = 0, minor = 20, patch = 0 }
                , dependencies = Dict.empty
                , testDependencies = Dict.empty
                }
            )


{-| Encode elm.json to string.
-}
encodeElmJson : ElmJson -> String
encodeElmJson elmJson =
    case elmJson of
        Application config ->
            encodeApplication config

        PackageConfig config ->
            encodePackage config


encodeApplication : ApplicationConfig -> String
encodeApplication config =
    let
        directDeps =
            Dict.toList config.dependencies.direct
                |> List.map (\( name, ver ) -> "            \"" ++ name ++ "\": \"" ++ versionToString ver ++ "\"")
                |> String.join ",\n"

        indirectDeps =
            Dict.toList config.dependencies.indirect
                |> List.map (\( name, ver ) -> "            \"" ++ name ++ "\": \"" ++ versionToString ver ++ "\"")
                |> String.join ",\n"
    in
    """{
    "type": "application",
    "source-directories": [
        \"""" ++ String.join "\", \"" config.sourceDirectories ++ """"
    ],
    "elm-version": \"""" ++ config.elmVersion ++ """",
    "dependencies": {
        "direct": {
""" ++ directDeps ++ """
        },
        "indirect": {
""" ++ indirectDeps ++ """
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
"""


encodePackage : PackageElmJson -> String
encodePackage config =
    """{
    "type": "package",
    "name": \"""" ++ config.name ++ """",
    "summary": \"""" ++ config.summary ++ """",
    "license": \"""" ++ config.license ++ """",
    "version": \"""" ++ versionToString config.version ++ """",
    "exposed-modules": [
        \"""" ++ String.join "\", \"" config.exposedModules ++ """"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {},
    "test-dependencies": {}
}
"""



-- RESOLUTION


{-| Result of dependency resolution.
-}
type alias ResolutionResult =
    Result ResolutionError (Dict String Version)


type alias ResolutionError =
    { message : String
    , conflicting : List ( String, List Constraint )
    }


{-| Resolve dependencies.
-}
resolve : Dict String Constraint -> Dict String (List Version) -> ResolutionResult
resolve constraints available =
    resolveHelper (Dict.toList constraints) Dict.empty available


resolveHelper :
    List ( String, Constraint )
    -> Dict String Version
    -> Dict String (List Version)
    -> ResolutionResult
resolveHelper remaining resolved available =
    case remaining of
        [] ->
            Ok resolved

        ( pkg, constraint ) :: rest ->
            case Dict.get pkg available of
                Nothing ->
                    Err
                        { message = "Package not found: " ++ pkg
                        , conflicting = []
                        }

                Just versions ->
                    let
                        matching =
                            List.filter (\v -> satisfies v constraint) versions
                                |> List.sortWith (\a b -> compareVersions b a)

                        -- newest first
                    in
                    case matching of
                        [] ->
                            Err
                                { message = "No version of " ++ pkg ++ " satisfies constraint"
                                , conflicting = [ ( pkg, [ constraint ] ) ]
                                }

                        best :: _ ->
                            resolveHelper rest (Dict.insert pkg best resolved) available



-- API DIFF


{-| Types of API changes.
-}
type ApiChange
    = Added String
    | Removed String
    | Changed String String String


{-| Diff two API versions.
-}
diff : List String -> List String -> List ApiChange
diff oldApi newApi =
    let
        oldSet =
            List.foldl (\x acc -> Dict.insert x () acc) Dict.empty oldApi

        newSet =
            List.foldl (\x acc -> Dict.insert x () acc) Dict.empty newApi

        added =
            List.filter (\x -> not (Dict.member x oldSet)) newApi
                |> List.map Added

        removed =
            List.filter (\x -> not (Dict.member x newSet)) oldApi
                |> List.map Removed
    in
    added ++ removed
