{ name = "elmish-hooks"
, dependencies =
  [ "aff"
  , "debug"
  , "elmish"
  , "maybe"
  , "prelude"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/collegevine/purescript-elmish-hooks.git"
}
