let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs" ],
  dependencies = conf.dependencies # [ "effect", "elmish-enzyme", "elmish-html", "aff-promise", "spec" ]
}
