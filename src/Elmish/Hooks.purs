-- | A React hook-like library for Elmish.
-- |
-- | ```purs
-- | todos :: ReactElement
-- | todos = withHooks do
-- |   todos /\ setTodos <- useState []
-- |
-- |   useEffect do
-- |     todos <- API.fetchTodos
-- |     liftEffect $ setTodos todos
-- |
-- |   pure $ H.fragment $ todoView <$> todos
-- | ```
module Elmish.Hooks
  ( withHooks
  , module Type
  , module UseEffect
  , module UseState
  ) where

import Prelude

import Control.Monad.Cont (runCont)
import Elmish (ReactElement)
import Elmish.Hooks.Type (Hook, mkHook, genComponentName) as Type
import Elmish.Hooks.UseEffect (useEffect) as UseEffect
import Elmish.Hooks.UseState (useState) as UseState

-- | Unwraps a `Hook ReactElement`
withHooks :: Type.Hook ReactElement -> ReactElement
withHooks hooks = runCont hooks identity
