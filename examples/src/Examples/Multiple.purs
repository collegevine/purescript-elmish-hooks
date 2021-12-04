module Examples.Multiple
  ( view
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Elmish (ReactElement, (<?|))
import Elmish.Foreign (readForeign)
import Elmish.HTML.Styled as H
import Elmish.Hooks (HookName(..), useEffect, useState, withHooks)
import Foreign (Foreign)

view :: ReactElement
view = withHooks do
  { state: name, setState: setName } <- useState (HookName "Name") "World"

  useEffect (HookName "SetName") do
    delay $ Milliseconds 2000.0
    liftEffect $ setName "useEffect"

  pure $
    H.div ""
    [ H.h2 "" "Combine multiple hooks"
    , H.div "form-group"
      [ H.label_ "form-label" { htmlFor: "name" } "What’s your name?"
      , H.input_ "form-control" { value: name, onChange: setName <?| eventTargetValue, id: "name" }
      ]
    , H.blockquote "blockquote mt-4"
      [ H.text "Hello, "
      , H.strong "bg-warning" name
      , H.text "!"
      ]
    ]

eventTargetValue :: Foreign -> Maybe String
eventTargetValue = toEvent >>> map _.target.value
  where
    toEvent :: Foreign -> Maybe { target :: { value :: String } }
    toEvent = readForeign
