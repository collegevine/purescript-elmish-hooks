{ name = "elmish-hooks"
, dependencies =
  [ "aff"
  , "debug"
  , "elmish"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "tuples"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/collegevine/purescript-elmish-hooks.git"
}
