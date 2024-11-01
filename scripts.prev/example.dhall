let conf = ../spago.dhall

in conf // {
  sources = conf.sources # [ "example/**/*.purs" ],
  dependencies =
    conf.dependencies
}