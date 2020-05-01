{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "purescript-noodle"
, dependencies =
  [ "aff"
  , "affjax"
  , "arrays"
  , "behaviors"
  , "canvas"
  , "colors"
  , "console"
  , "control"
  , "coroutines"
  , "datetime"
  , "debug"
  , "effect"
  , "event"
  , "foldable-traversable"
  , "node-fs"
  , "now"
  , "numbers"
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
  , "string-parsers"
  , "strings"
  , "test-unit"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
