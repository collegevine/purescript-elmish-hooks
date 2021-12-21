module Elmish.Hooks.UseEffect
  ( useEffect
  ) where

import Prelude

import Effect.Aff (Aff)
import Elmish (forkVoid)
import Elmish.Hooks.Type (Hook, HookName, mkHook)

-- | The `useEffect` hook takes an effect (`Aff`) to run and runs it in the
-- | `init` of the resulting component. E.g.:
-- |
-- | ```purs
-- | todos :: ReactElement
-- | todos = withHooks do
-- |   todos /\ setTodos <- useState (HookName "TodoState") []
-- |
-- |   useEffect (HookName "FetchTodos") do
-- |     todos <- API.fetchTodos
-- |     liftEffect $ setTodos todos
-- |
-- |   pure $ H.fragment $ todoView <$> todos
-- | ```
useEffect :: HookName -> Aff Unit -> Hook Unit
useEffect name init =
  mkHook name \render ->
    { init: forkVoid init
    , update: \_ msg -> absurd msg
    , view: \_ _ -> render unit
    }
