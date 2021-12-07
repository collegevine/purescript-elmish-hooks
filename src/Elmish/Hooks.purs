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
  , hook
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

-- type Hook = ContT ReactElement (Writer (Array String))
type Hook = WriterT (Array String) (Cont ReactElement)

-- | Unwraps a `Hook ReactElement` by passing `identity` as the callback.
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
      guard (nodeEnv == "development")
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
  hook name \n render -> UseState.useState n { initialState, render }

-- | The `useEffect` hook takes an effect (`Aff`) to run and runs it in the
-- | `init` of the resulting component.
useEffect :: HookName -> Aff Unit -> Hook Unit
useEffect name init =
  hook name \n render -> UseEffect.useEffect n { init, render }

hook :: forall a. HookName -> (ComponentName -> (a -> ReactElement) -> ReactElement) -> Hook a
hook (HookName name) mkHook = do
  tell [name]
  WriterT $ cont \render -> mkHook (ComponentName name) \args -> render $ Tuple args []

foreign import nodeEnv :: String
