module Elmish.Hooks.UseState
  ( useState
  )
  where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Elmish (Dispatch, ReactElement)
import Elmish.Component (ComponentName, wrapWithLocalState)
import Elmish.Hooks.Type (Hook, HookName, mkHook)

type Args state =
  { initialState :: state
  , render :: state /\ Dispatch state -> ReactElement
  }

useState' :: forall state. ComponentName -> Args state -> ReactElement
useState' name = wrapWithLocalState name \{ initialState, render } ->
  { init: pure initialState
  , update: \_ newState -> pure newState
  , view: \state setState -> render (state /\ setState)
  }

-- | The `useState` hook takes an initial state and its callback has access to
-- | the current state and a setter for the state.
useState :: forall state. HookName -> state -> Hook (state /\ Dispatch state)
useState name initialState =
  mkHook name \n render -> useState' n { initialState, render }
