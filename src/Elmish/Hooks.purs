-- | A React hook-like library for Elmish.
-- |
-- | ```purescript
-- | todos :: ReactElement
-- | todos = withHooks do
-- |   { state: todos, setState: setTodos } <- useState (HookName "TodoState") []
-- |
-- |   useEffect (HookName "FetchTodos") do
-- |     todos <- API.fetchTodos
-- |     liftEffect $ setTodos todos
-- |
-- |   pure $ H.fragment $ todoView <$> todos
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

-- | A unique name for a given hook.
newtype HookName = HookName String

-- | The type of a hook, e.g. the result of calling `useState`. It turns out
-- | that hooks can be modeled as a continuation (isomorphic to `Cont
-- | ReactElement`), where the callback function returns a new component
-- | (usually created with `wrapWithLocalState`) given the encapsulated value.
-- | E.g., in the case of `useState`, you can think of it as accepting a
-- | callback function, which gets passed the current state and a setter for the
-- | current state:
-- |
-- | ```purescript
-- | useState (HookName "Foo") "" \{ state: foo, setState: setFoo } -> …
-- | ```
-- |
-- | Modeling it as a continuation allows us to make it a monad and write in
-- | do-notation, which looks a lot like the React hooks syntax:
-- |
-- | ```purescript
-- | withHooks do
-- |   { state: foo, setState: setFoo } <- useState (HookName "Foo") ""
-- |   pure …
-- | ```
newtype Hook a = Hook ((a -> ReactElement) -> ReactElement)

instance Functor Hook where
  map fn (Hook hookA) = Hook \renderB ->
    hookA \a -> renderB $ fn a

instance Apply Hook where
  apply (Hook hookFn) (Hook hookA) = Hook \renderB ->
    hookA \a -> hookFn \fn -> renderB $ fn a

instance Applicative Hook where
  pure a = Hook (_ $ a)

instance Bind Hook where
  bind (Hook hookA) cb = Hook \renderB ->
    hookA \a -> case cb a of
      Hook hookB -> hookB renderB

instance Monad Hook

-- | Unwraps a `Hook ReactElement` by passing `identity` as the callback.
withHooks :: Hook ReactElement -> ReactElement
withHooks (Hook hookElem) = hookElem identity

-- The `useState` hook takes an initial state and its callback has access to the
-- current state and a setter for the state.
useState :: forall state. HookName -> state -> Hook (UseState.RenderArgs state)
useState (HookName name) initialState = Hook \render ->
  UseState.useState (ComponentName name) { initialState, render }

-- The `useEffect` hook takes an effect (`Aff`) to run and runs it in the `init`
-- of the resulting component.
useEffect :: HookName -> Aff Unit -> Hook Unit
useEffect (HookName name) init = Hook \render ->
  UseEffect.useEffect (ComponentName name) { init, render }
