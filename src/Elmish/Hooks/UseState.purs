module Elmish.Hooks.UseState
  ( UseState
  , traced
  , useState
  ) where

import Prelude

import Data.Tuple (curry)
import Data.Tuple.Nested (type (/\))
import Debug (class DebugWarning)
import Elmish (ComponentDef, Dispatch, withTrace)
import Elmish.Component (ComponentName(..))
import Elmish.Hooks.Type (Hook, HookType, mkHook)

foreign import data UseState :: Type -> HookType

-- | The `useState` hook takes an initial state and returns a `Hook`
-- | encapsulating the current state and a `setState` function. E.g.:
-- |
-- | ```purs
-- | view :: ReactElement
-- | view = Hooks.component Hooks.do
-- |   visible /\ setVisible <- useState false
-- |   Hooks.pure $
-- |     H.fragment
-- |     [ H.button_ "" { onClick: setVisible <| not visible } "Toggle visibility"
-- |     , if visible
-- |         then H.div "" "Content"
-- |         else H.empty
-- |     ]
-- | ```
useState :: ∀ @state. state -> Hook (UseState state) (state /\ Dispatch state)
useState state = useState' (ComponentName "UseState") identity state

-- | A version of `useState` that logs messages, state changes and render times.
-- | Intended to be used with qualified imports: `UseState.traced`.
traced :: ∀ @state. DebugWarning => state -> Hook (UseState state) (state /\ Dispatch state)
traced state = useState' (ComponentName "UseState_Traced") withTrace state

useState' :: ∀ @state.
  ComponentName
  -> (ComponentDef state state -> ComponentDef state state)
  -> state
  -> Hook (UseState state) (state /\ Dispatch state)
useState' name = \f initialState ->
  mkHook name \render -> f
    { init: pure initialState
    , update: \_ newState -> pure newState
    , view: curry render
    }
