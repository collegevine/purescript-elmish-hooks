let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.15.15-20240711/src/packages.dhall
        sha256:81881d9e15484551b4293ab0a2639355f38d0cab1dfa49a077b5f1af374c292a

in  upstream
  with elmish.version = "v0.13.0"
