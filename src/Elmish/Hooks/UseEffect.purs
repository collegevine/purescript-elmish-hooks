module Elmish.Hooks.UseEffect
  ( traced
  , traced'
  , useEffect
  , useEffect'
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Debug (class DebugWarning)
import Effect.Aff (Aff)
import Elmish (EffectFn1, ComponentDef, createElement, forkVoid, withTrace, (<?|))
import Elmish.Component (ComponentName)
import Elmish.Hooks.Type (Hook, mkHook, uniqueNameFromCurrentCallStack, uniqueNameFromCurrentCallStackTraced)
import Elmish.React.Import (EmptyProps, ImportedReactComponent, ImportedReactComponentConstructorWithContent)
import Elmish.Ref (Ref, deref, ref)

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
  useEffect_ name identity unit $ const aff
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
-- |   useEffect' count \c -> liftEffect $
-- |     HTMLDocument.setTitle ("You clicked " <> show c <> " times") =<< document =<< window
-- |
-- |   pure H.button_ "" { onClick: setCount $ count + 1 } "Click me"
-- | ```
useEffect' :: forall a. Eq a => a -> (a -> Aff Unit) -> Hook Unit
useEffect' deps = \runEffect ->
  useEffect_ name identity deps runEffect
  where
    name = uniqueNameFromCurrentCallStack { skipFrames: 3 }

-- | A version of `useEffect` that logs info from the name-generating function.
-- | Intended to be used with qualified imports: `UseEffect.traced`.
traced :: DebugWarning => Aff Unit -> Hook Unit
traced runEffect =
  useEffect_ name withTrace unit $ const runEffect
  where
    name = uniqueNameFromCurrentCallStackTraced { skipFrames: 3 }

-- | A version of `useEffect'` that logs info from the name-generating function.
-- | Intended to be used with qualified imports: `UseEffect.traced'`.
traced' :: forall a. DebugWarning => Eq a => a -> (a -> Aff Unit) -> Hook Unit
traced' deps = \runEffect ->
  useEffect_ name withTrace deps runEffect
  where
    name = uniqueNameFromCurrentCallStackTraced { skipFrames: 3 }

useEffect_ :: forall a.
  Eq a
  => ComponentName
  -> (ComponentDef a a -> ComponentDef a a)
  -> a
  -> (a -> Aff Unit)
  -> Hook Unit
useEffect_ name f deps runEffect =
  mkHook name \render -> f
    { init: do
        forkVoid $ runEffect deps
        pure deps
    , update: \_ newDeps -> do
        forkVoid $ runEffect newDeps
        pure newDeps
    , view: \_ dispatch ->
        useEffectLifeCycles
          { componentDidUpdate: dispatch <?| \prevDeps ->
              if deref prevDeps /= deps then
                Just deps
              else
                Nothing
          , deps: ref deps
          } $
          render unit
    }

type Props deps r =
  ( componentDidUpdate :: EffectFn1 (Ref "deps" deps) Unit
  , deps :: Ref "deps" deps
  | r
  )

useEffectLifeCycles :: forall deps. ImportedReactComponentConstructorWithContent (Props deps) EmptyProps
useEffectLifeCycles = createElement useEffectLifeCycles_

foreign import useEffectLifeCycles_ :: ImportedReactComponent
