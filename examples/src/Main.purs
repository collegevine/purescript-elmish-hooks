module Main
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Elmish (Dispatch, ReactElement)
import Elmish.Boot (defaultMain)
import Elmish.HTML.Styled as H
import Examples.UseEffect as UseEffect
import Examples.UseEffectPrime as UseEffectPrime
import Examples.UseLocalStorage as UseLocalStorage
import Examples.UseMouseMove as UseMouseMove
import Examples.UseRef as UseRef
import Examples.UseState as UseState

main :: Effect Unit
main = defaultMain
  { def:
      { init: pure unit
      , update: \_ msg -> absurd msg
      , view
      }
  , elementId: "app"
  }

view :: Unit -> Dispatch Void -> ReactElement
view _ _ =
  H.div "container mt-4"
  [ H.h1 ""
    [ H.code "" "elmish-hooks"
    , H.text " examples"
    ]
  , H.hr "my-4"
  , UseState.view
  , H.hr "my-4"
  , UseEffect.view
  , H.hr "my-4"
  , UseEffectPrime.view
  , H.hr "my-4"
  , UseRef.view
  , H.hr "my-4"
  , H.h2 "" "Custom hooks"
  , UseLocalStorage.view
  , UseMouseMove.view
  ]
