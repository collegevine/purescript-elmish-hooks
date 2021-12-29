module Elmish.Hooks.UseEffect
  ( traced
  , traced'
  , useEffect
  , useEffect'
  ) where

import Prelude

import Debug (class DebugWarning)
import Effect.Aff (Aff)
import Elmish (fork)
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
useEffect aff =
  useEffect_ name unit $ const aff
  where
    name = uniqueNameFromCurrentCallStack { skipFrames: 3 }

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
useEffect' :: forall a. Eq a => a -> (a -> Aff Unit) -> Hook Unit
useEffect' deps = \aff ->
  useEffect_ name deps aff
  where
    name = uniqueNameFromCurrentCallStack { skipFrames: 3 }

-- | A version of `useEffect` that logs info from the name-generating function.
-- | Intended to be used with qualified imports: `UseEffect.traced`.
traced :: DebugWarning => Aff Unit -> Hook Unit
traced aff =
  useEffect_ name unit $ const aff
  where
    name = uniqueNameFromCurrentCallStackTraced { skipFrames: 3 }

-- | A version of `useEffect'` that logs info from the name-generating function.
-- | Intended to be used with qualified imports: `UseEffect.traced'`.
traced' :: forall a. DebugWarning => Eq a => a -> (a -> Aff Unit) -> Hook Unit
traced' deps = \aff ->
  useEffect_ name deps aff
  where
    name = uniqueNameFromCurrentCallStackTraced { skipFrames: 3 }

useEffect_ :: forall a. Eq a => ComponentName -> a -> (a -> Aff Unit) -> Hook Unit
useEffect_ name = \deps init ->
  let _ = set name deps
  in
  mkHook name \render ->
    { init: do
        fork do
          pure $ set name deps
          init deps
        pure deps
    , update: \deps' _ -> do
        let newDeps = get name
        if deps' == newDeps then
          fork $ pure unit
        else
          fork do
            pure $ set name newDeps
            init newDeps
        pure newDeps
    , view: \_ _ -> render unit
    }

foreign import get :: forall a. ComponentName -> a

foreign import set :: forall a. ComponentName -> a -> Unit
