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
  , "codec-argonaut"
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
  , "exists"
  , "foldable-traversable"
  , "foreign"
  , "foreign-generic"
  , "free"
  , "functions"
  , "halogen"
  , "halogen-css"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "halogen-svg-elems"
  , "heterogeneous"
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
  , "purescript-wire"
  , "read"
  , "record"
  , "refs"
  , "signal"
  , "sized-vectors"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "typelevel-prelude"
  , "unfoldable"
  , "unsafe-coerce"
  , "variant"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
