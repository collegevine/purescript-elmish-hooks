-- | A React hook-like library for Elmish. Uses a continuation monad to
-- | encapsulate state or effects.
-- |
-- | ```purs
-- | import Elmish.Hooks (useEffect, useState)
-- | import Elmish.Hooks as Hooks
-- |
-- | todos :: ReactElement
-- | todos = Hooks.component Hooks.do
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
  , module UseRef
  , module UseState
  , module UseSubscription
  ) where

import Elmish.Hooks.Type (Hook, HookType, type (<>), bind, component, discard, mkHook, pure, (==>), (=/>)) as Type
import Elmish.Hooks.UseEffect (UseEffect, useEffect, useEffect') as UseEffect
import Elmish.Hooks.UseRef (UseRef, useRef) as UseRef
import Elmish.Hooks.UseState (UseState, useState) as UseState
import Elmish.Hooks.UseSubscription (UseSubscription, useSubscription) as UseSubscription
