{ name = "elmish-hooks"
, dependencies =
  [ "aff"
  , "console"
  , "debug"
  , "effect"
  , "elmish"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/collegevine/purescript-elmish-hooks.git"
}
