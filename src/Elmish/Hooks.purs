-- | A React hook-like library for Elmish.
-- |
-- | ```purescript
-- | todos :: ReactElement
-- | todos = withHooks do
-- |   todos /\ setTodos <- useState (HookName "TodoState") []
-- |
-- |   useEffect (HookName "FetchTodos") do
-- |     todos <- API.fetchTodos
-- |     liftEffect $ setTodos todos
-- |
-- |   pure $ H.fragment $ todoView <$> todos
-- | ```
module Elmish.Hooks
  ( withHooks
  , module Type
  , module UseEffect
  , module UseState
  )
  where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Cont (runCont)
import Control.Monad.Writer (runWriterT)
import Data.Array (filter, fold, group, intercalate, null, sort)
import Data.Array.NonEmpty as NE
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Elmish (ReactElement)
import Elmish.Hooks.Type ( Hook, HookName(..), mkHook) as Type
import Elmish.Hooks.UseEffect (useEffect) as UseEffect
import Elmish.Hooks.UseState (useState) as UseState

-- | Unwraps a `Hook ReactElement` and logs an error to the console if two hooks
-- | with the same name have been used.
withHooks :: Type.Hook ReactElement -> ReactElement
withHooks hooks = runCont (runWriterT hooks) toElem
  where
    toElem (Tuple elem names) =
      let
        _ = case error names of
          Just err -> unsafePerformEffect $ Console.error err
          Nothing -> unit
      in elem

    error names = do
      let duplicates = names # sort # group # filter ((_ > 1) <<< NE.length) <#> NE.head
      guard (not null duplicates)
      pure $ fold
        [ "Error in Elmish Hook: Hooks must have unique names. The following "
        , "hook names appear more than once in a `withHooks` block: '"
        , intercalate "', '" duplicates
        , "'"
        ]
