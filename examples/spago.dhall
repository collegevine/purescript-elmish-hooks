
let config = ../spago.dhall

in config // {
  sources = [ "../src/**/*.purs", "../test/**/*.purs", "./src/**/*.purs" ],
  dependencies = config.dependencies # [ "elmish-html", "foreign", "maybe", "web-html", "web-storage" ]
}
