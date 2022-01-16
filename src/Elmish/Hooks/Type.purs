module Elmish.Hooks.Type
  ( AppendHookType
  , Hook
  , HookType
  , Pure
  , type (<>)
  , bind
  , discard
  , mkHook
  , pure
  , uniqueNameFromCurrentCallStack
  , uniqueNameFromCurrentCallStackTraced
  , withHook
  , withHookCurried
  , withHooks
  , (=/>)
  , (==>)
  )
  where

import Prelude hiding (bind, discard, pure)

import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\))
import Debug (class DebugWarning)
import Elmish (ReactElement, ComponentDef)
import Elmish.Component (ComponentName(..), wrapWithLocalState)
import Prelude as Prelude

foreign import data HookType :: Type

foreign import data Pure :: HookType

foreign import data AppendHookType :: HookType -> HookType -> HookType

infixr 1 type AppendHookType as <>

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
newtype Hook (t :: HookType) a = Hook ((a -> ReactElement) -> ReactElement)

instance Functor (Hook t) where
  map f (Hook hook) = Hook \render -> hook (render <<< f)

bind :: forall t t' a b. Hook t a -> (a -> Hook t' b) -> Hook (t <> t') b
bind (Hook hookA) k = Hook \render ->
  hookA \a -> case k a of Hook hookB -> hookB \b -> render b

discard :: forall t t' a b. Discard a => Hook t a -> (a -> Hook t' b) -> Hook (t <> t') b
discard = bind

pure :: forall a. a -> Hook Pure a
pure a = Hook \render -> render a

-- | Given a `ComponentName` and a function to create a `ComponentDef` (from a
-- | render function `a -> ReactElement`), `mkHook` creates a `Hook a`. The name
-- | can be anything, but it’s recommended to use
-- | `uniqueNameFromCurrentCallStack` to create a unique name based on where the
-- | hook is called from in the stack trace. `uniqueNameFromCurrentCallStack`
-- | accepts a `skipFrames :: Int` argument to indicate how many frames back it
-- | should look for the call site of the hook, as well as a `prefix` argument
-- | that will be the prefix of the resulting `ComponentName`.
-- |
-- | It’s also recommended to create the name in a where clause and make your
-- | hook a function accepting one argument. Even if your hook takes more than
-- | one argument, you can put the rest o the arguments in a lambda in the
-- | function body:
-- |
-- | ```purs
-- | myHook x = \y z -> mkHook …
-- |   where
-- |     name = uniqueNameFromCurrentCallStack { skipFrames: 3, prefix: "MyHook" }
-- | ```
-- |
-- | This ensures that the number of frames to skip is predictably 3. If
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
-- |     name = ComponentName $ genStableUUID { skipFrames: 3, prefix: "UseEffect" }
-- | ```
mkHook :: forall msg state t a. ComponentName -> ((a -> ReactElement) -> ComponentDef msg state) -> Hook t a
mkHook name mkDef =
  Hook \render -> wrapWithLocalState name mkDef render

-- | Unwraps a `Hook ReactElement`, which is usually created by using one or
-- | more hooks and then using `pure` to encapsulate a `ReactElement`. E.g.:
-- |
-- | ```purs
-- | view :: ReactElement
-- | view = withHooks do
-- |   name /\ setName <- useState ""
-- |   pure $ H.input_ "" { value: name, onChange: setName <?| eventTargetValue }
-- | ```
withHooks :: forall t. Hook t ReactElement -> ReactElement
withHooks hook = withHooks' name hook
  where
    name = uniqueNameFromCurrentCallStack { skipFrames: 3, prefix: "WithHooks" }

withHooks' :: forall t. ComponentName -> Hook t ReactElement -> ReactElement
withHooks' name (Hook hook) =
  unit # wrapWithLocalState name \_ ->
    { init: Prelude.pure unit
    , update: const absurd
    , view: const $ const $ hook identity
    }

-- | When there is only one hook, it might make more sense to invoke it with
-- | continuation-passing style. This helper makes that easier, accepting a
-- | `render` callback.
-- |
-- | Intended to be used via the `==>` operator:
-- |
-- | ```purs
-- | view :: ReactElement
-- | view = useState "" ==> \(name /\ setName) ->
-- |   H.input_ "" { value: name, onChange: setName <?| eventTargetValue }
-- | ```
withHook :: forall t a. Hook t a -> (a -> ReactElement) -> ReactElement
withHook hook = \render -> withHooks' name $ render <$> hook
  where
    name = uniqueNameFromCurrentCallStack { skipFrames: 3, prefix: "WithHook" }

infixl 1 withHook as ==>

-- | Given a `Hook (a /\ b)`, this allows invoking it with a curried `render`
-- | callback.
-- |
-- | Intended to be used via the `=/>` operator:
-- |
-- | ```purs
-- | view :: ReactElement
-- | view = useState "" =/> \name setName ->
-- |   H.input_ "" { value: name, onChange: setName <?| eventTargetValue }
-- | ```
withHookCurried :: forall t a b. Hook t (a /\ b) -> (a -> b -> ReactElement) -> ReactElement
withHookCurried hook = \render -> withHooks' name $ (uncurry render) <$> hook
  where
    name = uniqueNameFromCurrentCallStack { skipFrames: 3, prefix: "WithHookCurried" }

infixl 1 withHookCurried as =/>

-- | Generates a `ComponentName` to be passed to `mkHook`.
uniqueNameFromCurrentCallStack :: { skipFrames :: Int, prefix :: String } -> ComponentName
uniqueNameFromCurrentCallStack = ComponentName <<< uniqueNameFromCurrentCallStack_

-- | Generates a `ComponentName` to be passed to `mkHook`, like
-- | `uniqueNameFromCurrentCallStack`, but logs the stack trace and specific
-- | line to the console.
uniqueNameFromCurrentCallStackTraced :: DebugWarning => { skipFrames :: Int, prefix :: String } -> ComponentName
uniqueNameFromCurrentCallStackTraced = ComponentName <<< uniqueNameFromCurrentCallStackTraced_

foreign import uniqueNameFromCurrentCallStack_ :: { skipFrames :: Int, prefix :: String } -> String

foreign import uniqueNameFromCurrentCallStackTraced_ :: { skipFrames :: Int, prefix :: String } -> String
