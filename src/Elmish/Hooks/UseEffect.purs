module Elmish.Hooks.UseEffect
  ( useEffect
  ) where

import Prelude

import Effect.Aff (Aff)
import Elmish (ReactElement, forkVoid)
import Elmish.Component (ComponentName, wrapWithLocalState)
import Elmish.Hooks.Type (Hook, HookName, mkHook)

useEffect' :: ComponentName -> { init :: Aff Unit, render :: Unit -> ReactElement } -> ReactElement
useEffect' name = wrapWithLocalState name \{ init, render } ->
  { init: forkVoid init
  , update: \_ msg -> absurd msg
  , view: \_ _ -> render unit
  }

-- | The `useEffect` hook takes an effect (`Aff`) to run and runs it in the
-- | `init` of the resulting component.
useEffect :: HookName -> Aff Unit -> Hook Unit
useEffect name init =
  mkHook name \n render -> useEffect' n { init, render }
