
let config = ../spago.dhall
in let packages = ../packages.dhall with elmish-html.version = "change-ref-type-2" -- TODO: remove when 0.4.0 is released

in config // {
  packages = packages,
  sources = [ "../src/**/*.purs", "./src/**/*.purs" ],
  dependencies = config.dependencies # [ "effect", "elmish-html", "foldable-traversable", "foreign", "maybe", "unsafe-coerce", "web-html", "web-storage" ]
}
