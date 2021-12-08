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
  , mkHook
  , useEffect
  , useState
  , withHooks
  )
  where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Cont (Cont, cont, runCont)
import Control.Monad.Writer (WriterT(..), runWriterT, tell)
import Data.Array (filter, fold, group, intercalate, null, sort)
import Data.Array.NonEmpty as NE
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Elmish (ReactElement)
import Elmish.Component (ComponentName(..))
import Elmish.Hooks.UseEffect as UseEffect
import Elmish.Hooks.UseState as UseState

-- | A unique name for a given hook.
newtype HookName = HookName String

-- | The type of a hook, e.g. the result of calling `useState`. It turns out
-- | that hooks can be modeled as a continuation, where the callback function
-- | returns a new component (usually created with `wrapWithLocalState`) given
-- | the encapsulated value. E.g., in the case of `useState`, you can think of
-- | it as accepting a callback function, which gets passed the current state
-- | and a setter for the current state:
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
-- |
-- | Wrapping it in a `WriterT (Array String)` allows us to keep track of all of
-- | the hook names so `withHooks` can log an error if there are any duplicate
-- | names. If two names are duplicated and can appear in the same spot in the
-- | DOM, React might confuse one for the other, causing unexpected
-- | side-effects.
type Hook = WriterT (Array String) (Cont ReactElement)

-- | Unwraps a `Hook ReactElement` and logs an error to the console if two hooks
-- | with the same name have been used.
withHooks :: Hook ReactElement -> ReactElement
withHooks hooks = runCont (runWriterT hooks) toElem
  where
    toElem (Tuple elem names) =
      let
        _ = case error names of
          Just err -> unsafePerformEffect $ Console.error err
          Nothing -> unit
      in elem

    error names = do
      let duplicates = names # sort # group # filter ((_ > 1) <<< NE.length) <#> NE.head
      guard (not null duplicates)
      pure $ fold
        [ "Error in Elmish Hook: Hooks must have unique names. The following "
        , "hook names appear more than once in a `withHooks` block: '"
        , intercalate "', '" duplicates
        , "'"
        ]

-- | The `useState` hook takes an initial state and its callback has access to
-- | the current state and a setter for the state.
useState :: forall state. HookName -> state -> Hook (UseState.RenderArgs state)
useState name initialState =
  mkHook name \n render -> UseState.useState n { initialState, render }

-- | The `useEffect` hook takes an effect (`Aff`) to run and runs it in the
-- | `init` of the resulting component.
useEffect :: HookName -> Aff Unit -> Hook Unit
useEffect name init =
  mkHook name \n render -> UseEffect.useEffect n { init, render }

-- | Given a name and a function to create a component (from a name and a
-- | callback of `a`), `mkHook` creates a `Hook a` and keep track of the name.
mkHook :: forall a. HookName -> (ComponentName -> (a -> ReactElement) -> ReactElement) -> Hook a
mkHook (HookName name) mkComponent = do
  tell [name]
  WriterT $ cont \render -> mkComponent (ComponentName name) \args -> render $ Tuple args []
