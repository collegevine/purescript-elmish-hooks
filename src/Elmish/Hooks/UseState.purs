module Elmish.Hooks.UseState
  ( RenderArgs
  , useState
  )
  where

import Prelude

import Elmish (Dispatch, ReactElement)
import Elmish.Component (ComponentName, wrapWithLocalState)

type RenderArgs state =
  { state :: state
  , setState :: Dispatch state
  }

type Args state =
  { initialState :: state
  , render :: RenderArgs state -> ReactElement
  }

data Message state = SetState state

useState :: forall state. ComponentName -> Args state -> ReactElement
useState name = wrapWithLocalState name \{ initialState, render } ->
  { init: pure initialState
  , update: \_ (SetState state) -> pure state
  , view: \state dispatch ->
      render
        { state
        , setState: dispatch <<< SetState
        }
  }
