-- | Models React Hooks as a continuation monad. For example:
-- |
-- | ```purescript
-- | collapsibleComponent :: ReactElement
-- | collapsibleComponent = withHooks do
-- |   { state: expanded, setState: setExpanded } <- useState (HookName "Collapsible") false
-- |
-- |   useEffect (HookName "Expand") do
-- |     delay $ Milliseconds 1000.0
-- |     setExpanded true
-- |
-- |   pure $
-- |     H.div ""
-- |     [ H.div "" $ "I’m " <> if expanded then "expanded" else "collapsed"
-- |     , H.button_ "" { onClick: setExpanded $ not expanded } "Toggle me!"
-- |     ]
-- | ```
module Elmish.Hooks
  ( Hook
  , HookName(..)
  , useEffect
  , useState
  , withHooks
  )
  where

import Prelude

import Effect.Aff (Aff)
import Elmish (ReactElement)
import Elmish.Component (ComponentName(..))
import Elmish.Hooks.UseEffect as UseEffect
import Elmish.Hooks.UseState as UseState

newtype HookName = HookName String

newtype Hook a = Hook ((a -> ReactElement) -> ReactElement)

instance Functor Hook where
  map :: forall a b. (a -> b) -> Hook a -> Hook b
  map fn (Hook hookA) = Hook \renderB ->
    hookA \a -> renderB $ fn a

instance Apply Hook where
  apply :: forall a b. Hook (a -> b) -> Hook a -> Hook b
  apply (Hook hookFn) (Hook hookA) = Hook \renderB ->
    hookA \a -> hookFn \fn -> renderB $ fn a

instance Applicative Hook where
  pure :: forall a. a -> Hook a
  pure a = Hook (_ $ a)

instance Bind Hook where
  bind :: forall a b. Hook a -> (a -> Hook b) -> Hook b
  bind (Hook hookA) cb = Hook \renderB ->
    hookA \a -> case cb a of
      Hook hookB -> hookB renderB

instance Monad Hook

withHooks :: Hook ReactElement -> ReactElement
withHooks (Hook hookElem) = hookElem identity

useState :: forall state. HookName -> state -> Hook (UseState.RenderArgs state)
useState (HookName name) initialState = Hook \render ->
  UseState.useState (ComponentName name) { initialState, render }

useEffect :: HookName -> Aff Unit -> Hook Unit
useEffect (HookName name) init = Hook \render ->
  UseEffect.useEffect (ComponentName name) { init, render }
