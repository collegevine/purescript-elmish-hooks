
let config = ../spago.dhall

in config // {
  sources = [ "../src/**/*.purs", "./src/**/*.purs" ],
  dependencies = config.dependencies # [ "elmish-html", "foreign", "maybe", "unsafe-coerce", "web-html", "web-storage" ]
}
