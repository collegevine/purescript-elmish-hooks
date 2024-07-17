module Elmish.Hooks.UseEffect
  ( UseEffect
  , traced
  , traced'
  , useEffect
  , useEffect'
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Undefined.NoProblem (Req)
import Debug (class DebugWarning)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1)
import Elmish (ComponentDef, createElement, forkVoid, withTrace, (<?|))
import Elmish.Component (ComponentName(..))
import Elmish.Hooks.Type (Hook, HookType, mkHook)
import Elmish.Opaque (Opaque, unwrap, wrap) as Opaque
import Elmish.React.Import (ImportedReactComponent, ImportedReactComponentConstructorWithContent)

foreign import data UseEffect :: Type -> HookType

-- | The `useEffect` hook takes an effect (`Aff`) to run and runs it in the
-- | `init` of the resulting component. E.g.:
-- |
-- | ```purs
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
useEffect :: Aff Unit -> Hook (UseEffect Unit) Unit
useEffect runEffect = useEffect_ (ComponentName "UseEffect") identity unit $ const runEffect

-- | This is like `useEffect`, but allows passing a value which, when it
-- | changes, will trigger the effect to run again. E.g.:
-- |
-- | ```purs
-- | view :: ReactElement
-- | view = Hooks.component Hooks.do
-- |   count /\ setCount <- useState 0
-- |
-- |   useEffect' count \c -> liftEffect $
-- |     HTMLDocument.setTitle ("You clicked " <> show c <> " times") =<< document =<< window
-- |
-- |   Hooks.pure H.button_ "" { onClick: setCount $ count + 1 } "Click me"
-- | ```
useEffect' :: ∀ @a. Eq a => a -> (a -> Aff Unit) -> Hook (UseEffect a) Unit
useEffect' deps runEffect = useEffect_ (ComponentName "UseEffectPrime") identity deps runEffect

-- | A version of `useEffect` that logs messages, state changes and render
-- | times. Intended to be used with qualified imports: `UseEffect.traced`.
traced :: DebugWarning => Aff Unit -> Hook (UseEffect Unit) Unit
traced runEffect = useEffect_ (ComponentName "UseEffect_Traced") withTrace unit $ const runEffect

-- | A version of `useEffect'` that logs messages, state changes and render
-- | times. Intended to be used with qualified imports: `UseEffect.traced'`.
traced' :: ∀ @a. DebugWarning => Eq a => a -> (a -> Aff Unit) -> Hook (UseEffect a) Unit
traced' deps runEffect = useEffect_ (ComponentName "UseEffect_TracedPrime") withTrace deps runEffect

useEffect_ :: ∀ @a.
  Eq a
  => ComponentName
  -> (ComponentDef a a -> ComponentDef a a)
  -> a
  -> (a -> Aff Unit)
  -> Hook (UseEffect a) Unit
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
              if Opaque.unwrap @"deps" prevDeps /= deps then
                Just deps
              else
                Nothing
          , deps: Opaque.wrap @"deps" deps
          } $
          render unit
    }

type Props deps =
  ( componentDidUpdate :: Req (EffectFn1 (Opaque.Opaque "deps" deps) Unit)
  , deps :: Req (Opaque.Opaque "deps" deps)
  )

useEffectLifeCycles :: forall deps. ImportedReactComponentConstructorWithContent (Props deps)
useEffectLifeCycles = createElement useEffectLifeCycles_

foreign import useEffectLifeCycles_ :: ImportedReactComponent
