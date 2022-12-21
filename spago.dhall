{ name = "elmish-hooks"
, dependencies =
  [ "aff"
  , "debug"
  , "effect"
  , "elmish"
  , "maybe"
  , "prelude"
  , "tuples"
  , "undefined-is-not-a-problem"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/collegevine/purescript-elmish-hooks.git"
}
