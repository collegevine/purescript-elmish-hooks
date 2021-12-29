module Elmish.Hooks.UseState
  ( traced
  , useState
  ) where

import Prelude

import Data.Tuple (curry)
import Data.Tuple.Nested (type (/\))
import Debug (class DebugWarning)
import Elmish (ComponentDef, Dispatch, withTrace)
import Elmish.Component (ComponentName)
import Elmish.Hooks.Type (Hook, mkHook, uniqueNameFromCurrentCallStack, uniqueNameFromCurrentCallStackTraced)

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
useState state = useState' name identity state
  where
    name = uniqueNameFromCurrentCallStack { skipFrames: 3 }

-- | A version of `useState` that logs messages, state changes, render times,
-- | and info from the name-generating function. Intended to be used with
-- | qualified imports: `UseState.traced`.
traced :: forall state. DebugWarning => state -> Hook (state /\ Dispatch state)
traced state = useState' name withTrace state
  where
    name = uniqueNameFromCurrentCallStackTraced { skipFrames: 3 }

useState' :: forall state.
  ComponentName
  -> (ComponentDef state state -> ComponentDef state state)
  -> state
  -> Hook (state /\ Dispatch state)
useState' name = \f initialState ->
  mkHook name \render -> f
    { init: pure initialState
    , update: \_ newState -> pure newState
    , view: curry render
    }
