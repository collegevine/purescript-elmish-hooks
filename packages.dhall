let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.15.13-20231201/src/packages.dhall
        sha256:706a855400108a03b35bd37afe7f50802deed882c555171d266338d4694ddbe8

in  upstream
  with elmish.version = "v0.11.1"
  with elmish-html.version = "v0.8.2"
