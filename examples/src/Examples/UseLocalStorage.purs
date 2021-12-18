module Examples.UseLocalStorage
  ( view
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Elmish (ReactElement, Dispatch, (<?|))
import Elmish.HTML.Styled as H
import Elmish.Hooks (Hook, HookName(..), useEffect, useState, withHooks)
import Utils (eventTargetValue)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

view :: ReactElement
view = withHooks do
  foo /\ setFoo <- useLocalStorage (HookName "Foo") "foo" ""
  pure $
    H.div "row"
    [ H.div "col-12 col-md-6 col-lg-4"
      [ H.h4 "" $
          H.code "" "useLocalStorage"
      , H.div "form-group"
        [ H.label_ "form-label" { htmlFor: "foo" }
          [ H.text "Typing here will update the state and save to "
          , H.code "" "localStorage"
          ]
        , H.input_ "form-control" { value: foo, onChange: setFoo <?| eventTargetValue, id: "foo" }
        ]
      ]
    ]

useLocalStorage :: HookName -> String -> String -> Hook (String /\ (Dispatch String))
useLocalStorage (HookName name) key defaultValue = do
  state /\ setState <- useState (HookName $ name <> ".State") defaultValue
  useEffect (HookName $ name <> ".Effect") $ liftEffect do
    v <- getItem key =<< localStorage =<< window
    case v of
      Just v' -> setState v'
      Nothing -> setItem key defaultValue =<< localStorage =<< window
  pure $ state /\ \v -> do
    setState v
    setItem key v =<< localStorage =<< window
