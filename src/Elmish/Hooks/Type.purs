module Elmish.Hooks.Type
  ( Hook
  , mkHook
  , uniqueNameFromCurrentCallStack
  , uniqueNameFromCurrentCallStackTraced
  , withHooks
  ) where

import Prelude

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
newtype Hook a = Hook ((a -> ReactElement) -> ReactElement)

instance Functor Hook where
  map f (Hook hook) = Hook \render -> hook (render <<< f)

instance Apply Hook where
  apply (Hook hookF) (Hook hook) = Hook \render ->
    hookF \f -> hook (render <<< f)

instance Applicative Hook where
  pure a = Hook \render -> render a

instance Bind Hook where
  bind (Hook hookA) k = Hook \render ->
    hookA \a -> case k a of Hook hookB -> hookB \b -> render b

-- | Given a `ComponentName` and a function to create a `ComponentDef` (from a
-- | render function `a -> ReactElement`), `mkHook` creates a `Hook a`. The name
-- | can be anything, but it’s recommended to use
-- | `uniqueNameFromCurrentCallStack` to create a unique name based on where the
-- | hook is called from in the stack trace. `uniqueNameFromCurrentCallStack`
-- | accepts a `skipFrames :: Int` argument to indicate how many frames back it
-- | should look for the call site of the hook.
-- |
-- | It’s also recommended to create the name in a where clause and make your
-- | hook a function accepting one argument. Even if your hook takes more than
-- | one argument, you can put the rest o the arguments in a lambda in the
-- | function body:
-- |
-- | ```purs
-- | myHook x = \y z -> mkHook …
-- |   where
-- |     name = …
-- |
-- | This ensures that the number of frames to skip is predictably 2. If
-- | defining the function differently, a different number of frames can be
-- | passed. `uniqueNameFromCurrentCallStackTraced` can be used to help find the
-- | correct number.
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
  Hook \render -> wrapWithLocalState name mkDef render

-- | Unwraps a `Hook ReactElement`
withHooks :: Hook ReactElement -> ReactElement
withHooks (Hook hook) = hook identity

-- | Generates a `ComponentName` to be passed to `mkHook`.
uniqueNameFromCurrentCallStack :: { skipFrames :: Int } -> ComponentName
uniqueNameFromCurrentCallStack = ComponentName <<< uniqueNameFromCurrentCallStack_

-- | Generates a `ComponentName` to be passed to `mkHook`, like
-- | `uniqueNameFromCurrentCallStack`, but logs the stack trace and specific
-- | line to the console.
uniqueNameFromCurrentCallStackTraced :: DebugWarning => { skipFrames :: Int } -> ComponentName
uniqueNameFromCurrentCallStackTraced = ComponentName <<< uniqueNameFromCurrentCallStackTraced_

foreign import uniqueNameFromCurrentCallStack_ :: { skipFrames :: Int } -> String

foreign import uniqueNameFromCurrentCallStackTraced_ :: { skipFrames :: Int } -> String
