-- | A React hook-like library for Elmish. Uses a continuation monad to
-- | encapsulate state or effects.
-- |
-- | ```purs
-- | import Elmish.Hooks (withHooks, useEffect, useState)
-- | import Elmish.Hooks as Hooks
-- |
-- | todos :: ReactElement
-- | todos = withHooks Hooks.do
-- |   todos /\ setTodos <- useState []
-- |
-- |   useEffect do
-- |     todos <- API.fetchTodos
-- |     liftEffect $ setTodos todos
-- |
-- |   Hooks.pure $ H.fragment $ todoView <$> todos
-- | ```
module Elmish.Hooks
  ( module Type
  , module UseEffect
  , module UseState
  ) where

import Elmish.Hooks.Type (Hook, HookType, Pure, type (<>), bind, discard, mkHook, pure, uniqueNameFromCurrentCallStack, withHooks, (==>), (=/>)) as Type
import Elmish.Hooks.UseEffect (UseEffect, useEffect) as UseEffect
import Elmish.Hooks.UseState (UseState, useState) as UseState
