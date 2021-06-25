let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs" ],
  dependencies =
    conf.dependencies #
    [ "spec"
    , "test-unit"
    , "either"
    , "exceptions"
    , "foldable-traversable"
    , "functions"
    , "newtype"
    , "refs"
    ]
}