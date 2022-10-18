let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.15.4-20220901/src/packages.dhall
        sha256:f1531b29c21ac437ffe5666c1b6cc76f0a9c29d3c9d107ff047aa2567744994f

in  upstream
  with elmish-testing-library =
    { dependencies = [ "prelude", "aff-promise" ]
    , repo =
        "https://github.com/collegevine/purescript-elmish-testing-library.git"
    , version = "v0.3.1"
    }
