module Elmish.Hooks.Type
  ( class ComposedHookTypes
  , AppendHookType
  , Hook, Hook'
  , HookType, HookType'
  , Pure
  , type (<>)
  , bind
  , component
  , discard
  , mkHook
  , pure
  , withHook
  , withHookCurried
  , (=/>)
  , (==>)
  ) where

import Prelude hiding (bind, discard, pure)

import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\))
import Elmish (ReactElement, ComponentDef)
import Elmish.Component (ComponentName(..), wrapWithLocalState)
import Prelude as Prelude

-- | Represents the type of a hook and is used to ensure hooks are safe. For
-- | example, the following will not compile because we track `HookType`:
-- |
-- | ```purs
-- | Hooks.component Hooks.do
-- |   if someCondition then Hooks.do
-- |     x <- useState ""
-- |     _ <- useState 0
-- |     Hooks.pure x
-- |   else Hooks.do
-- |     _ <- useState 0
-- |     useState ""
-- | ```
-- |
-- | because the first block has a `HookType` of `UseState String <> UseState
-- | Int` and the second is `UseState Int <> UseState String`. The same hooks
-- | need to be used in the same order for the hook types to match.
foreign import data HookType' :: Type

type HookType = HookType' -> HookType'

-- | The `HookType` of `pure` — the identity of the `HookType` monoid.
foreign import data Pure :: HookType'

-- | A type which allows appending `HookType`s via type application.
type AppendHookType (h :: HookType) t = h t

infixr 6 type AppendHookType as <>

-- | This class represents the type level function for composing `HookType'`s,
-- | with instances for appending the identity and arbitrary `HookType'`s.
class ComposedHookTypes (left :: HookType') (right :: HookType') (result :: HookType') | left right -> result

-- Base case 1: Pure <> t = t
instance ComposedHookTypes Pure t t
-- Base case 2: t <> Pure = t
else instance ComposedHookTypes t Pure t
-- Recursive case: Recursively “unnests” the right-most operand of the left
-- argument, accumulating the result into `right'`. E.g.:
--
-- (a <> (b <> c)) <> (d <> (e <> f))
-- = a <> ((b <> c) <> (d <> (e <> f)))
-- = a <> (b <> (c <> (d <> (e <> f)))) <- grouped correctly
else instance ComposedHookTypes l2 right right' => ComposedHookTypes (l1 <> l2) right (l1 <> right')

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
-- | Hooks.component do
-- |   foo /\ setFoo <- useState ""
-- |   pure …
-- | ```
-- |
-- | Finally, to make sure we don’t use two hooks with different state-types in
-- | a conditional (which could cause unexpected issues for React) we have a
-- | `HookType'` parameter, which accumulates when we call `bind`. For this we
-- | need to use qualified do notation:
-- |
-- | ```purs
-- | Hooks.component Hooks.do
-- |   foo /\ setFoo <- useState ""
-- |   Hooks.pure …
-- | ```
data Hook' (t :: HookType') a = Hook ((a -> ReactElement) -> ReactElement)
type role Hook' nominal representational

-- | A Convenient wrapper which applies `Pure` to the given hook type.
type Hook (t :: HookType) a = Hook' (t Pure) a

instance Functor (Hook' t) where
  map f (Hook hook) = Hook \render -> hook (render <<< f)

bind :: forall ta tb tr a b.
  ComposedHookTypes ta tb tr
  => Hook' ta a
  -> (a -> Hook' tb b)
  -> Hook' tr b
bind (Hook hookA) k = Hook \render ->
  hookA \a -> case k a of Hook hookB -> hookB render

discard :: forall ta tb tr a b.
  Discard a
  => ComposedHookTypes ta tb tr
  => Hook' ta a
  -> (a -> Hook' tb b)
  -> Hook' tr b
discard = bind

pure :: forall a. a -> Hook' Pure a
pure a = Hook \render -> render a

-- | Given a `ComponentName` and a function to create a `ComponentDef` (from a
-- | render function `a -> ReactElement`), `mkHook` creates a `Hook a`. When
-- | creating a hook with `mkHook`, you’ll need to create a `HookType` by
-- | `foreign import`ing it.
-- |
-- | As an example of how to use `mkHook`, `useEffect` uses it like so:
-- |
-- | ```purs
-- | foreign import data UseEffect :: Type -> HookType
-- |
-- | useEffect :: Aff Unit -> Hook (UseEffect Unit) Unit
-- | useEffect init =
-- |   mkHook (ComponentName "UseEffect") \render ->
-- |     { init: forkVoid init
-- |     , update: \_ msg -> absurd msg
-- |     , view: \_ _ -> render unit
-- |     }
-- | ```
mkHook :: forall msg state t a. ComponentName -> ((a -> ReactElement) -> ComponentDef msg state) -> Hook' t a
mkHook name mkDef =
  Hook \render -> wrapWithLocalState name mkDef render

-- | Unwraps a `Hook ReactElement`, which is usually created by using one or
-- | more hooks and then using `pure` to encapsulate a `ReactElement`. E.g.:
-- |
-- | ```purs
-- | view :: ReactElement
-- | view = Hooks.component Hooks.do
-- |   name /\ setName <- useState ""
-- |   Hooks.pure $ H.input_ "" { value: name, onChange: setName <?| eventTargetValue }
-- | ```
component :: forall t. Hook' t ReactElement -> ReactElement
component hook = component' name hook
  where
    name = uniqueNameFromCurrentCallStack { skipFrames: 3, prefix: "HooksComponent" }

component' :: forall t. ComponentName -> Hook' t ReactElement -> ReactElement
component' name (Hook hook) =
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
withHook :: forall t a. Hook' t a -> (a -> ReactElement) -> ReactElement
withHook hook = \render -> component' name $ render <$> hook
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
withHookCurried :: forall t a b. Hook' t (a /\ b) -> (a -> b -> ReactElement) -> ReactElement
withHookCurried hook = \render -> component' name $ (uncurry render) <$> hook
  where
    name = uniqueNameFromCurrentCallStack { skipFrames: 3, prefix: "WithHookCurried" }

infixl 1 withHookCurried as =/>

-- | Generates a `ComponentName` to be passed to `mkHook`.
uniqueNameFromCurrentCallStack :: { skipFrames :: Int, prefix :: String } -> ComponentName
uniqueNameFromCurrentCallStack = ComponentName <<< uniqueNameFromCurrentCallStack_

foreign import uniqueNameFromCurrentCallStack_ :: { skipFrames :: Int, prefix :: String } -> String
