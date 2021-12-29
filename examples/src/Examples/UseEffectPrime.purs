module Examples.UseEffectPrime
  ( view
  ) where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Elmish.Hooks (useState, withHooks)
import Elmish.Hooks.UseEffect (useEffect')
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

view :: ReactElement
view = withHooks do
  count /\ setCount <- useState 0

  useEffect' count \c -> liftEffect do
    doc <- Window.document =<< window
    HTMLDocument.setTitle ("You clicked " <> show c <> " times") doc

  pure $
    H.div "row mt-3"
    [ H.div "col-12 col-md-6 col-lg-4"
      [ H.h2 ""
        [ H.code "" "useEffect'"
        , H.text " hook"
        ]
      , H.p ""
        [ H.text "You clicked "
        , H.text $ show count
        , H.text " times. Clicking will also update "
        , H.code "" "document.title"
        , H.text "."
        ]
      , H.button_ "btn btn-primary" { onClick: setCount $ count + 1 } "Click to update title"
      ]
    ]
