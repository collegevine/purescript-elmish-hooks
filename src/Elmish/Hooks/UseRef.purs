module Elmish.Hooks.UseRef
  ( Ref
  , useRef
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Elmish (EffectFn1, (<?|))
import Elmish.Hooks (UseState, useState)
import Elmish.Hooks.Type (Hook)
import Elmish.Hooks.Type as Hooks
import Web.HTML (HTMLElement)

type UseRef = UseState (Maybe HTMLElement)

type Ref = Maybe HTMLElement /\ (EffectFn1 HTMLElement Unit)

-- | The `useRef` hook returns a `Hook` encapsulating an element paired with a
-- | setter for that element. This setter can be passed to a `ref` prop to get a
-- | reference to an element, but it has to be `unsafeCoerce`d because
-- | `elmish-html` expects `ref` to be a string.
-- |
-- | ```purs
-- | view :: ReactElement
-- | view = withHooks do
-- |   inputEl /\ setInputEl <- useRef Nothing
-- |   let onButtonClick = traverse_ focus inputEl
-- |   pure $
-- |     H.fragment
-- |     [ H.input_ "form-control" { ref: unsafeCoerce setInputEl, defaultValue: "" }
-- |     , H.button_ "btn btn-primary" { onClick: onButtonClick } "Focus the input"
-- |     ]
-- | ```
useRef :: Hook UseRef Ref
useRef = Hooks.do
  ref /\ setRef <- useState Nothing
  Hooks.pure $ ref /\ setRef <?| \r -> if (eqByReference r <$> ref) == Just true then Nothing else Just (Just r)

foreign import eqByReference :: forall a. a -> a -> Boolean
