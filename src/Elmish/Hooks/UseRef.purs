module Elmish.Hooks.UseRef
  ( Ref
  , readRef
  , traced
  , useRef
  , writeRef
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Nullable as Nullable
import Debug (class DebugWarning)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn1, runEffectFn2)
import Elmish (ComponentDef, EffectFn1, withTrace)
import Elmish.Component (ComponentName)
import Elmish.Hooks.Type (Hook, mkHook, uniqueNameFromCurrentCallStack, uniqueNameFromCurrentCallStackTraced)

-- | The `useRef` hook takes a value and returns a `Hook` encapsulating a
-- | mutable `Ref`, initialized to that value. E.g.:
-- |
-- | ```pursview :: ReactElement
-- | view = withHooks do
-- |   inputEl <- useRef Nothing
-- |   let onButtonClick = traverse_ focus =<< readRef inputEl
-- |   pure $
-- |     H.fragment
-- |     [ H.input_ "form-control" { ref: unsafeCoerce inputEl, defaultValue: "" }
-- |     , H.button_ "btn btn-primary" { onClick: onButtonClick } "Focus the input"
-- |     ]
-- | ```
useRef :: forall a. Maybe a -> Hook (Ref a)
useRef a = useRef' name identity a
  where
    name = uniqueNameFromCurrentCallStack { skipFrames: 3 }

-- | A version of `useRef` that logs messages, state changes, render times, and
-- | info from the name-generating function. Intended to be used with qualified
-- | imports: `UseRef.traced`.
traced :: forall a. DebugWarning => Maybe a -> Hook (Ref a)
traced a = useRef' name withTrace a
  where
    name = uniqueNameFromCurrentCallStackTraced { skipFrames: 3 }

useRef' :: forall a.
  ComponentName
  -> (ComponentDef (Ref a) (Ref a) -> ComponentDef (Ref a) (Ref a))
  -> Maybe a
  -> Hook (Ref a)
useRef' name f a =
  mkHook name \render -> f
    { init: pure $ newRef a
    , update: \_ ref -> pure ref
    , view: \ref _ -> render ref
    }

foreign import data Ref :: Type -> Type

-- | Reads the current value of a mutable `Ref`.
readRef :: forall a. Ref a -> Effect (Maybe a)
readRef = map Nullable.toMaybe <<< runEffectFn1 readRef_

-- | Updates the value of a mutable `Ref`.
writeRef :: forall a. Ref a -> Maybe a -> Effect Unit
writeRef ref = runEffectFn2 writeRef_ ref <<< toNullable

newRef :: forall a. Maybe a -> Ref a
newRef = newRef_ <<< toNullable

foreign import readRef_ :: forall a. EffectFn1 (Ref a) (Nullable a)

foreign import writeRef_ :: forall a. EffectFn2 (Ref a) (Nullable a) Unit

foreign import newRef_ :: forall a. Nullable a -> Ref a
