let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/hydra-examples/examples/*.purs" ],
  dependencies =
    conf.dependencies
}