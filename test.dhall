let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs" ],
  dependencies = conf.dependencies #
    [ "aff"
    , "arrays"
    , "avar"
    , "control"
    , "datetime"
    , "effect"
    , "elmish-html"
    , "elmish-testing-library"
    , "foldable-traversable"
    , "foreign"
    , "nullable"
    , "spec"
    , "tailrec"
    , "tuples"
    ]
}
