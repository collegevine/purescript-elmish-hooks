let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.15.4-20221015/src/packages.dhall
        sha256:4949f9f5c3626ad6a83ea6b8615999043361f50905f736bc4b7795cba6251927

in  upstream
  with elmish.version = "v0.9.0"
  with elmish-html.version = "v0.8.0"
