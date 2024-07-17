module Elmish.Hooks.UseSubscription
  ( UseSubscription
  , useSubscription
  ) where

import Prelude

import Effect.Aff (Aff)
import Elmish.Component (ComponentName(..), forkVoid)
import Elmish.Hooks.Type (Hook, HookType, mkHook)
import Elmish.Subscription (Subscription)
import Elmish.Subscription as Sub

foreign import data UseSubscription :: Type -> HookType

-- | Subscribes to the given subscription and calls the provided callback every
-- | time the subscription yields a value.
-- |
-- | ```purs
-- | listenToNetwork :: Subscription Aff NetworkSignal
-- | listenToNetwork = ...
-- |
-- | view :: ReactElement
-- | view = Hooks.component Hooks.do
-- |   lastSignal /\ setLastSignal <- Hooks.useState Nothing
-- |   useSubscription listenToNetwork \signal -> setLastSignal (Just signal)
-- |   Hooks.pure $
-- |     case lastSignal of
-- |       Nothing -> H.div "bg-secondary" "Waiting for signal..."
-- |       Just signal -> H.div "bg-success" $ "Received signal: " <> show signal
-- | ```
useSubscription :: ∀ a. Subscription Aff a -> (a -> Aff Unit) -> Hook (UseSubscription a) Unit
useSubscription subscription onValue =
  mkHook (ComponentName "UseSubscription") \render ->
    { init: do
        Sub.subscribe identity subscription
        pure unit
    , update: \_ value -> do
        forkVoid $ onValue value
        pure unit
    , view: \_ _ ->
        render unit
    }
