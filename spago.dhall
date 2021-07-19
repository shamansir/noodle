{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "noodle"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "datetime"
  , "debug"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-subscriptions"
  , "halogen-svg-elems"
  , "identity"
  , "integers"
  , "invariant"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "refs"
  , "signal"
  , "sized-vectors"
  , "tuples"
  , "typelevel"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
