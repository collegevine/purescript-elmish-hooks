module Elmish.Hooks.UseRef
  ( UseRef
  , useRef
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Elmish (EffectFn1, (<?|))
import Elmish.Hooks.Type (Hook)
import Elmish.Hooks.Type as Hooks
import Elmish.Hooks.UseState (UseState, useState)

type UseRef el = UseState (Maybe el)

-- | The `useRef` hook returns a `Hook` encapsulating an element paired with a
-- | setter for that element. This setter can be passed to a `ref` prop to get a
-- | reference to an element.
-- |
-- | ```purs
-- | view :: ReactElement
-- | view = withHooks do
-- |   inputEl /\ setInputEl <- useRef Nothing
-- |   let onButtonClick = traverse_ (focus <<< HTMLInputElement.toHTMLElement) inputEl
-- |   pure $
-- |     H.fragment
-- |     [ H.input_ "form-control" { ref: setInputEl, defaultValue: "" }
-- |     , H.button_ "btn btn-primary" { onClick: onButtonClick } "Focus the input"
-- |     ]
-- | ```
useRef :: forall el. Hook (UseRef el) (Maybe el /\ (EffectFn1 el Unit))
useRef = Hooks.do
  ref /\ setRef <- useState Nothing
  Hooks.pure $ ref /\ setRef <?| \r -> case eqByReference r <$> ref of
    Just true -> Nothing
    _ -> Just $ Just r

foreign import eqByReference :: forall a. a -> a -> Boolean
