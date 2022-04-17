let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.14.5-20220102/src/packages.dhall sha256:17ca27f650e91813019dd8c21595b3057d6f4986118d22205bdc7d6ed1ca28e8

in  upstream
  with elmish-html.version = "opt"
  with elmish.version = "opt"
  with elmish.dependencies =
      [ "aff"
      , "argonaut-core"
      , "arrays"
      , "bifunctors"
      , "console"
      , "debug"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "foreign"
      , "foreign-object"
      , "functions"
      , "integers"
      , "js-date"
      , "maybe"
      , "nullable"
      , "partial"
      , "prelude"
      , "refs"
      , "strings"
      , "typelevel-prelude"
      , "undefined-is-not-a-problem"
      , "unsafe-coerce"
      , "web-dom"
      , "web-html"
      ]

  with elmish-enzyme.version = "v0.0.3"
