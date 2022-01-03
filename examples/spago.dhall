
let config = ../spago.dhall

in config // {
  sources = [ "../src/**/*.purs", "../test/**/*.purs", "./src/**/*.purs" ],
  dependencies = config.dependencies # [ "elmish-html", "foldable-traversable", "foreign", "unsafe-coerce", "web-html", "web-storage" ]
}
