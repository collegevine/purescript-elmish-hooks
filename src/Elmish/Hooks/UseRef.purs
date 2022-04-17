module Elmish.Hooks.UseRef
  ( UseRef
  , useRef
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Elmish.Hooks.Type (Hook)
import Elmish.Hooks.Type as Hooks
import Elmish.Hooks.UseState (UseState, useState)
import Elmish.React.ReactRef (ReactRef, callbackRef)
import Web.HTML (HTMLElement)

type UseRef = UseState (Maybe HTMLElement)

-- | The `useRef` hook returns a `Hook` encapsulating an element paired with a
-- | setter for that element. This setter can be passed to a `ref` prop to get a
-- | reference to an element.
-- |
-- | ```purs
-- | view :: ReactElement
-- | view = withHooks do
-- |   inputEl /\ inputRef <- useRef
-- |   let onButtonClick = traverse_ focus inputEl
-- |   pure $
-- |     H.fragment
-- |     [ H.input_ "form-control" { ref: inputRef, defaultValue: "" }
-- |     , H.button_ "btn btn-primary" { onClick: onButtonClick } "Focus the input"
-- |     ]
-- | ```
useRef :: Hook UseRef (Maybe HTMLElement /\ ReactRef)
useRef = Hooks.do
  ref /\ setRef <- useState Nothing
  Hooks.pure $ ref /\ callbackRef ref (setRef <<< Just)
