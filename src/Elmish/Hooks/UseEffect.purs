module Elmish.Hooks.UseEffect
  ( traced
  , traced'
  , useEffect
  , useEffect'
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
useEffect = useEffect_ identity uniqueNameFromCurrentCallStack unit

-- | This is like `useEffect`, but allows passing a value which, when it
-- | changes, will trigger the effect to run again. E.g.:
-- |
-- | ```purs
-- | view :: ReactElement
-- | view = withHooks do
-- |   count /\ setCount <- useState 0
-- |
-- |   useEffect' count $ liftEffect $
-- |     HTMLDocument.setTitle ("You clicked " <> show count <> " times") =<< document =<< window
-- |
-- |   pure H.button_ "" { onClick: setCount $ count + 1 } "Click me"
-- | ```
useEffect' :: forall a. a -> Aff Unit -> Hook Unit
useEffect' = useEffect_ identity uniqueNameFromCurrentCallStack

-- | A version of `useEffect` that logs messages, state changes, render times,
-- | and info from the name-generating function. Intended to be used with
-- | qualified imports: `UseEffect.traced`.
traced :: DebugWarning => Aff Unit -> Hook Unit
traced = useEffect_ withTrace uniqueNameFromCurrentCallStackTraced unit

-- | A version of `useEffect'` that logs messages, state changes, render times,
-- | and info from the name-generating function. Intended to be used with
-- | qualified imports: `UseEffect.traced'`.
traced' :: forall a. DebugWarning => a -> Aff Unit -> Hook Unit
traced' = useEffect_ withTrace uniqueNameFromCurrentCallStackTraced

useEffect_ :: forall a.
  (ComponentDef Void Unit -> ComponentDef Void Unit)
  -> ({ skipFrames :: Int } -> ComponentName)
  -> a
  -> Aff Unit
  -> Hook Unit
useEffect_ f genName cache init =
  mkHook name \render -> f
    { init: forkVoid init
    , update: \_ msg -> absurd msg
    , view: \_ _ -> render unit
    }
  where
    generatedName = genName { skipFrames: 2 }
    name = generateComponentName { name: generatedName, value: cache }

foreign import generateComponentName :: forall a. { name :: ComponentName, value :: a } -> ComponentName
