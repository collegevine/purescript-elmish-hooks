
let config = ../spago.dhall
in let packages = ../packages.dhall

in config // {
  packages = packages,
  sources = [ "../src/**/*.purs", "./src/**/*.purs" ],
  dependencies = config.dependencies # [ "effect", "elmish-html", "foldable-traversable", "foreign", "maybe", "unsafe-coerce", "web-dom", "web-html", "web-storage" ]
}
