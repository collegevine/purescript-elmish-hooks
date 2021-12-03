-- | Models React Hooks as a continuation monad. For example:
-- |
-- | ```purescript
-- | collapsibleComponent :: ReactElement
-- | collapsibleComponent = withHooks do
-- |   { state: expanded, setState: setExpanded } <- useState (Name "Collapsible") false
-- |
-- |   useEffect (Name "Expand") do
-- |     delay $ Milliseconds 1000.0
-- |     setExpanded true
-- |
-- |   pure $
-- |     H.div ""
-- |     [ H.div "" $ "I’m " <> if expanded then "expanded" else "collapsed"
-- |     , H.button_ "" { onClick: setExpanded $ not expanded } "Toggle me!"
-- |     ]
-- | ```
module Elmish.Hooks
  ( Hook
  , Name(..)
  , useEffect
  , useState
  , withHooks
  )
  where

import Prelude

import Control.Monad.Cont (Cont, ContT(..), runCont)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Elmish (ReactElement)
import Elmish.Component (ComponentName(..))
import Elmish.Hooks.UseEffect as UseEffect
import Elmish.Hooks.UseState as UseState

type Hook = Cont ReactElement

newtype Name = Name String

withHooks :: Hook ReactElement -> ReactElement
withHooks comp = runCont comp identity

useState :: forall state. Name -> state -> Hook (UseState.RenderArgs state)
useState (Name name) initialState = ContT \render ->
  pure $ UseState.useState (ComponentName name) { initialState, render: unwrap <<< render }

useEffect :: Name -> Aff Unit -> Hook Unit
useEffect (Name name) init = ContT \render ->
  pure $ UseEffect.useEffect (ComponentName name) { init, render: unwrap <<< render }
