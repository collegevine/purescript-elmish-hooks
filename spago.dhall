{ name = "elmish-hooks"
, dependencies =
  [ "debug"
  , "effect"
  , "elmish"
  , "maybe"
  , "prelude"
  , "undefined-is-not-a-problem"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/collegevine/purescript-elmish-hooks.git"
}
