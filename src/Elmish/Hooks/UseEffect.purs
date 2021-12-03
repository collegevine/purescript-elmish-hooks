module Elmish.Hooks.UseEffect
  ( useEffect
  ) where

import Prelude

import Effect.Aff (Aff)
import Elmish (ReactElement, fork)
import Elmish.Component (ComponentName, wrapWithLocalState)

useEffect :: ComponentName -> { init :: Aff Unit, render :: Unit -> ReactElement } -> ReactElement
useEffect name = wrapWithLocalState name \{ init, render } ->
  { init: fork init
  , update: \_ _ -> pure unit
  , view: \_ _ -> render unit
  }
