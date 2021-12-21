module Elmish.Hooks.Type
  ( Hook
  , HookName(..)
  , mkHook
  )
  where

import Prelude

import Control.Monad.Cont (Cont, cont)
import Control.Monad.Writer (WriterT(..), tell)
import Data.Tuple (Tuple(..))
import Elmish (ReactElement, ComponentDef)
import Elmish.Component (ComponentName(..), wrapWithLocalState)

-- | A unique name for a given hook.
newtype HookName = HookName String

-- | The type of a hook, e.g. the result of calling `useState`. It turns out
-- | that hooks can be modeled as a continuation, where the callback function
-- | returns a new component (created with `wrapWithLocalState`) given the
-- | encapsulated value. E.g., in the case of `useState`, you can think of it as
-- | accepting a callback function, which gets passed the current state and a
-- | setter for the current state:
-- |
-- | ```purescript
-- | useState (HookName "Foo") "" \(foo /\ setFoo) -> …
-- | ```
-- |
-- | Modeling it as a continuation allows us to make it a monad and write in
-- | do-notation, which looks a lot like the React hooks syntax:
-- |
-- | ```purescript
-- | withHooks do
-- |   foo /\ setFoo <- useState (HookName "Foo") ""
-- |   pure …
-- | ```
-- |
-- | Wrapping it in a `WriterT (Array String)` allows us to keep track of all of
-- | the hook names so `withHooks` can log an error if there are any duplicate
-- | names. If two names are duplicated and can appear in the same spot in the
-- | DOM, React might confuse one for the other, causing unexpected
-- | side-effects.
type Hook = WriterT (Array String) (Cont ReactElement)

-- | Given a name and a function to create a `ComponentDef` (from a render
-- | function `a -> ReactElement`), `mkHook` creates a `Hook a` and keeps track
-- | of the name so that it can track duplicates. E.g. `useEffect` uses `mkHook`
-- | like so:
-- |
-- | ```purs
-- | useEffect :: HookName -> Aff Unit -> Hook Unit
-- | useEffect name init =
-- |   mkHook name \render ->
-- |     { init: forkVoid init
-- |     , update: \_ msg -> absurd msg
-- |     , view: \_ _ -> render unit
-- |     }
-- | ```
mkHook :: forall msg state a. HookName -> ((a -> ReactElement) -> ComponentDef msg state) -> Hook a
mkHook (HookName name) mkDef = do
  tell [name]
  WriterT $ cont \render ->
    wrapWithLocalState (ComponentName name) mkDef \args ->
      render $ Tuple args []
