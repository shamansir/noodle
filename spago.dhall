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
  , "colors"
  , "console"
  , "const"
  , "control"
  , "css"
  , "datetime"
  , "debug"
  , "dom-indexed"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
  , "halogen-css"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "halogen-svg-elems"
  , "identity"
  , "integers"
  , "invariant"
  , "lists"
  , "math"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "numbers"
  , "ordered-collections"
  , "ordered-set"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "read"
  , "refs"
  , "signal"
  , "sized-vectors"
  , "strings"
  , "tuples"
  , "typelevel"
  , "typelevel-prelude"
  , "unfoldable"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
