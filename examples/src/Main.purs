module Main
  ( main
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Elmish (Dispatch, ReactElement, Transition, (<?|))
import Elmish.Boot (defaultMain)
import Elmish.Foreign (class CanReceiveFromJavaScript, readForeign)
import Elmish.HTML.Styled as H
import Elmish.Hooks (Name(..), useEffect, useState, withHooks)
import Foreign (Foreign)
import Foreign.Object as F

main :: Effect Unit
main = defaultMain { def: { init, view, update }, elementId: "app" }

-- Nothing happens in our UI so far, so there are no messages
data Message

-- The UI is just static text, so there is no initial state
type State = Unit

-- Since there is no state, there is nothing to initialize
init :: Transition Message State
init = pure unit

-- Since there are no messages, the `update` function is also trivial
update :: State -> Message -> Transition Message State
update _ _ = pure unit

view :: State -> Dispatch Message -> ReactElement
view _ _ = withHooks do
  { state: name, setState: setName } <- useState (Name "Name") ""

  useEffect (Name "SetName") do
    delay $ Milliseconds 2000.0
    liftEffect $ when (String.null name) $ setName "World"

  pure $
    H.div "container"
    [ H.div "form-group"
      [ H.label_ "form-label" { htmlFor: "name" } "What’s your name?"
      , H.input_ "form-control" { value: name, onChange: setName <?| eventTargetValue, id: "name" }
      ]
    , H.div "display-6 mt-4"
      [ H.text "Hello, "
      , H.strong "bg-warning" name
      , H.text "!"
      ]
    ]

--
-- Helpers
--

eventTarget :: forall a. CanReceiveFromJavaScript a => Foreign -> Maybe a
eventTarget = attribute "target"

attribute :: forall a. CanReceiveFromJavaScript a => String -> Foreign -> Maybe a
attribute name = readForeign >=> F.lookup name >=> readForeign

-- Get the value of the target passed to a JavaScript event
eventTargetValue :: Foreign -> Maybe String
eventTargetValue = eventTarget >=> attribute "value"
