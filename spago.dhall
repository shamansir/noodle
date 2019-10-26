{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "purescript-rpd"
, dependencies =
    [ "aff"
    , "arrays"
    , "behaviors"
    , "canvas"
    , "console"
    , "control"
    , "coroutines"
    , "datetime"
    , "debug"
    , "effect"
    , "event"
    , "foldable-traversable"
    , "node-fs"
    , "parsing"
    , "prelude"
    , "profunctor-lenses"
    , "psci-support"
    , "refs"
    , "sequences"
    , "smolder"
    , "spec"
    , "spork"
    , "st"
    , "strings"
    , "string-parsers"
    , "test-unit"
    , "transformers"
    , "tuples"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
