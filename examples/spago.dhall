let config = ../spago.dhall

in config // {
  sources = [ "../src/**/*.purs", "../test/**/*.purs", "./examples/**/*.purs" ],
  dependencies = config.dependencies # [ "elmish-html" ]
}
