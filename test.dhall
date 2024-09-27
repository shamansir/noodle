let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs", "example/Hydra/Hydra/**/*.purs" ],
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
    ]
}