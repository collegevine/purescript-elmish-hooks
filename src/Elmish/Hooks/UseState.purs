module Elmish.Hooks.UseState
  ( useState
  )
  where

import Prelude

import Elmish (Dispatch, ReactElement)
import Elmish.Component (ComponentName, wrapWithLocalState)
import Elmish.Hooks.Type (Hook, HookName, mkHook)

type RenderArgs state =
  { state :: state
  , setState :: Dispatch state
  }

type Args state =
  { initialState :: state
  , render :: RenderArgs state -> ReactElement
  }

useState' :: forall state. ComponentName -> Args state -> ReactElement
useState' name = wrapWithLocalState name \{ initialState, render } ->
  { init: pure initialState
  , update: \_ newState -> pure newState
  , view: \state setState -> render { state, setState }
  }

-- | The `useState` hook takes an initial state and its callback has access to
-- | the current state and a setter for the state.
useState :: forall state. HookName -> state -> Hook (RenderArgs state)
useState name initialState =
  mkHook name \n render -> useState' n { initialState, render }
