-- | A React hook-like library for Elmish. Uses a continuation monad to
-- | encapsulate state or effects. Similarly to React, hooks should be used at
-- | the top level of a `withHooks do …` block, not inside conditionals.
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
  ( module Type
  , module UseEffect
  , module UseState
  ) where

import Elmish.Hooks.Type (Hook, mkHook, uniqueNameFromCurrentCallStack, withHooks) as Type
import Elmish.Hooks.UseEffect (useEffect) as UseEffect
import Elmish.Hooks.UseState (useState) as UseState
