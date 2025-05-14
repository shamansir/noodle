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
  , "blessed"
  , "colors"
  , "console"
  , "const"
  , "control"
  , "css"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "free"
  , "functions"
  , "halogen"
  , "halogen-css"
  , "halogen-subscriptions"
  , "halogen-svg-elems"
  , "halogen-vdom"
  , "identity"
  , "integers"
  , "language-cst-parser"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "node-http"
  , "node-path"
  , "nonempty"
  , "numbers"
  , "optparse"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "random"
  , "read"
  , "record"
  , "refs"
  , "signal"
  , "strings"
  , "strings-extra"
  , "tailrec"
  , "text-formatting"
  , "tidy-codegen"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel-lists"
  , "typelevel-peano"
  , "typelevel-prelude"
  , "unicode"
  , "unsafe-coerce"
  , "web-events"
  , "web-file"
  , "web-html"
  , "web-socket"
  , "web-uievents"
  , "yoga-json"
  , "yoga-tree"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
