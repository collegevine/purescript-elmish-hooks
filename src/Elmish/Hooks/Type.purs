module Elmish.Hooks.Type
  ( Hook
  , genComponentName
  , genComponentNameWithTrace
  , mkHook
  ) where

import Prelude

import Control.Monad.Cont (Cont, cont)
import Debug (class DebugWarning)
import Elmish (ReactElement, ComponentDef)
import Elmish.Component (ComponentName(..), wrapWithLocalState)

-- | The type of a hook, e.g. the result of calling `useState`. It turns out
-- | that hooks can be modeled as a continuation, where the callback function
-- | returns a new component (created with `wrapWithLocalState`) given the
-- | encapsulated value. E.g., in the case of `useState`, you can think of it as
-- | accepting a callback function, which gets passed the current state and a
-- | setter for the current state:
-- |
-- | ```purs
-- | useState "" \(foo /\ setFoo) -> …
-- | ```
-- |
-- | Modeling it as a continuation allows us to make it a monad and write in
-- | do-notation, which looks a lot like the React hooks syntax:
-- |
-- | ```purs
-- | withHooks do
-- |   foo /\ setFoo <- useState ""
-- |   pure …
-- | ```
type Hook = Cont ReactElement

-- | Given a `ComponentName` and a function to create a `ComponentDef` (from a
-- | render function `a -> ReactElement`), `mkHook` creates a `Hook a`. The name
-- | can be anything, but it’s recommended to use `genComponentName` to create a
-- | unique name based on where the hook is called from in the stack trace.
-- | `genComponentName` accepts a `skipFrames :: Int` argument to indicate how
-- | many frames back it should look for the call site of the hook.
-- |
-- | It’s also recommended to create the name in a where clause and make your
-- | hook a function accepting one argument. Even if your hook takes more than
-- | one argument, you can put the rest o the arguments in a lambda in the
-- | function body:
-- |
-- | ```purs
-- | myHook x = \y z -> …
-- |   where
-- |     name = …
-- |
-- | This ensures that the number of frames to skip is predictably 2. If
-- | defining the function differently, a different number of frames can be
-- | passed. `genComponentNameWithTrace` can be used to help find the correct
-- | number.
-- |
-- | As an example of how to use `mkHook`, `useEffect` uses it like so:
-- |
-- | ```purs
-- | useEffect :: Aff Unit -> Hook Unit
-- | useEffect init =
-- |   mkHook name \render ->
-- |     { init: forkVoid init
-- |     , update: \_ msg -> absurd msg
-- |     , view: \_ _ -> render unit
-- |     }
-- |   where
-- |     name = ComponentName $ genStableUUID { skipFrames: 2 }
-- | ```
mkHook :: forall msg state a. ComponentName -> ((a -> ReactElement) -> ComponentDef msg state) -> Hook a
mkHook name mkDef =
  cont \render -> wrapWithLocalState name mkDef render

-- | Generates a `ComponentName` to be passed to `mkHook`.
genComponentName :: { skipFrames :: Int } -> ComponentName
genComponentName = ComponentName <<< genStableUUID_

-- | Generates a `ComponentName` to be passed to `mkHook`, like
-- | `genComponentName`, but logs the stack trace and specific line to the
-- | console.
genComponentNameWithTrace :: DebugWarning => { skipFrames :: Int } -> ComponentName
genComponentNameWithTrace = ComponentName <<< genStableUUIDWithTrace_

foreign import genStableUUID_ :: { skipFrames :: Int } -> String

foreign import genStableUUIDWithTrace_ :: { skipFrames :: Int } -> String
