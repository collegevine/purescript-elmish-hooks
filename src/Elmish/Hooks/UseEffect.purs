module Elmish.Hooks.UseEffect
  ( traced
  , useEffect
  ) where

import Prelude

import Debug (class DebugWarning)
import Effect.Aff (Aff)
import Elmish (ComponentDef, forkVoid, withTrace)
import Elmish.Component (ComponentName)
import Elmish.Hooks.Type (Hook, mkHook, uniqueNameFromCurrentCallStack, uniqueNameFromCurrentCallStackTraced)

-- | The `useEffect` hook takes an effect (`Aff`) to run and runs it in the
-- | `init` of the resulting component. E.g.:
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
useEffect :: Aff Unit -> Hook Unit
useEffect = useEffect' identity uniqueNameFromCurrentCallStack

-- | A version of `useEffect` that logs messages, state changes, render times,
-- | and info from the name-generating function. Intended to be used with
-- | qualified imports: `UseEffect.traced`.
traced :: DebugWarning => Aff Unit -> Hook Unit
traced = useEffect' withTrace uniqueNameFromCurrentCallStackTraced

useEffect' ::
  (ComponentDef Void Unit -> ComponentDef Void Unit)
  -> ({ skipFrames :: Int } -> ComponentName)
  -> Aff Unit
  -> Hook Unit
useEffect' f genName init =
  mkHook name \render -> f
    { init: forkVoid init
    , update: \_ msg -> absurd msg
    , view: \_ _ -> render unit
    }
  where
    name = genName { skipFrames: 2 }
