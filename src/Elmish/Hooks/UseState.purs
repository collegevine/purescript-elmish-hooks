module Elmish.Hooks.UseState
  ( useState
  ) where

import Prelude

import Data.Tuple (curry)
import Data.Tuple.Nested (type (/\))
import Elmish (Dispatch)
import Elmish.Hooks.Type (Hook, genComponentName, mkHook)

-- | The `useState` hook takes an initial state and returns a `Hook`
-- | encapsulating the current state and a `setState` function. E.g.:
-- |
-- | ```purs
-- | view :: ReactElement
-- | view = withHooks do
-- |   visible /\ setVisible <- useState false
-- |   pure $
-- |     H.fragment
-- |     [ H.button_ "" { onClick: setVisible $ not visible } "Toggle visibility"
-- |     , if visible
-- |         then H.div "" "Content"
-- |         else H.empty
-- |     ]
-- | ```
useState :: forall state. state -> Hook (state /\ Dispatch state)
useState initialState =
  mkHook name \render ->
    { init: pure initialState
    , update: \_ newState -> pure newState
    , view: curry render
    }
  where
    name = genComponentName { skipFrames: 2 }
