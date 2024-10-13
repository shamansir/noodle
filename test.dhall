let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/Main.purs", "test/Spec/**/*.purs", "test/Example/**/*.purs" ],
  dependencies =
    conf.dependencies #
    [ "spec"
    , "test-unit"
    , "either"
    , "exceptions"
    , "foldable-traversable"
    , "functions"
    , "transformers"
    , "newtype"
    , "refs"
    , "expect-inferred"
    , "partial"
    , "these"
    ]
}