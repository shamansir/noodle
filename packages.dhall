{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
  with halogen-vdom.dependencies = [ "extra-dependency" ] # halogen-vdom.dependencies
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230605/packages.dhall sha256:f11cab6a50a55dfc53c4a86e5c611c502aa0fc7f280134c4ffd6dbc62c27faf8

in  upstream
  with halogen-svg-elems.repo
       = "https://github.com/shamansir/purescript-halogen-svg-elems.git"
  with halogen-svg-elems.version = "2bc876ede676dba9544f03ab3caa618f35621e2c"
  with purescript-wire =
    { dependencies = [ "aff", "filterable", "refs", "unsafe-reference" ]
    , repo = "https://github.com/robertdp/purescript-wire.git"
    , version = "v0.4.2"
    }
  with default-values =
    { dependencies =
      [ "prelude"
      , "either"
      , "maybe"
      , "tuples"
      , "lists"
      , "ordered-collections"
      , "foreign-object"
      ]
    , repo = "https://github.com/imsaravana369/purescript-default.git"
    , version = "v1.0.1"
    }
  with language-cst-parser =
    (upstream.language-cst-parser with version = "v0.13.0")
  with tidy =
    { dependencies =
      [ "arrays"
      , "dodo-printer"
      , "foldable-traversable"
      , "lists"
      , "maybe"
      , "ordered-collections"
      , "partial"
      , "prelude"
      , "language-cst-parser"
      , "strings"
      , "tuples"
      ]
    , repo = "https://github.com/natefaubion/purescript-tidy.git"
    , version = "v0.10.0"
    }
  with tidy-codegen =
    { dependencies =
      [ "aff"
      , "ansi"
      , "arrays"
      , "avar"
      , "bifunctors"
      , "console"
      , "control"
      , "dodo-printer"
      , "effect"
      , "either"
      , "enums"
      , "exceptions"
      , "filterable"
      , "foldable-traversable"
      , "free"
      , "identity"
      , "integers"
      , "language-cst-parser"
      , "lazy"
      , "lists"
      , "maybe"
      , "newtype"
      , "node-buffer"
      , "node-child-process"
      , "node-fs-aff"
      , "node-path"
      , "node-process"
      , "node-streams"
      , "ordered-collections"
      , "parallel"
      , "partial"
      , "posix-types"
      , "prelude"
      , "record"
      , "safe-coerce"
      , "st"
      , "strings"
      , "tidy"
      , "transformers"
      , "tuples"
      , "type-equality"
      , "unicode"
      ]
    , repo = "https://github.com/natefaubion/purescript-tidy-codegen.git"
    , version = "v4.0.0"
    }
