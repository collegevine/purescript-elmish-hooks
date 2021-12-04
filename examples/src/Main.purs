module Main
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Elmish (Dispatch, ReactElement)
import Elmish.Boot (defaultMain)
import Elmish.HTML.Styled as H
import Examples.Custom as Custom
import Examples.Multiple as Multiple
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
  , Multiple.view
  , H.hr "my-4"
  , Custom.view
  ]
