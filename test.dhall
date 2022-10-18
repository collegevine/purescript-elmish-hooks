let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs" ],
  dependencies = conf.dependencies #
    [ "arrays"
    , "control"
    , "effect"
    , "elmish-html"
    , "foldable-traversable"
    , "foreign"
    , "nullable"
    , "spec"
    ]
}
