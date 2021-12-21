module Elmish.Hooks.UseState
  ( useState
  )
  where

import Prelude

import Data.Tuple (curry)
import Data.Tuple.Nested (type (/\))
import Elmish (Dispatch)
import Elmish.Component (wrapWithLocalState)
import Elmish.Hooks.Type (Hook, HookName, mkHook)

-- | The `useState` hook takes an initial state and returns a `Hook`
-- | encapsulating the current state and a `setState` function. E.g.:
-- |
-- | ```purs
-- | view :: ReactElement
-- | view = withHooks do
-- |   visible /\ setVisible <- useState (HookName "ContentVisible") false
-- |   pure $
-- |     H.fragment
-- |     [ H.button_ "" { onClick: setVisible $ not visible } "Toggle visibility"
-- |     , if visible
-- |         then H.div "" "Content"
-- |         else H.empty
-- |     ]
-- | ```
useState :: forall state. HookName -> state -> Hook (state /\ Dispatch state)
useState name initialState =
  mkHook name \n render -> useState' n { initialState, render }
  where
    useState' n = wrapWithLocalState n \{ initialState: init, render } ->
      { init: pure init
      , update: \_ newState -> pure newState
      , view: curry render
      }

