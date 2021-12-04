module Examples.Custom
  ( view
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Elmish (ReactElement, Dispatch, (<?|))
import Elmish.Foreign (readForeign)
import Elmish.HTML.Styled as H
import Elmish.Hooks (Hook, HookName(..), useEffect, useState, withHooks)
import Foreign (Foreign)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

view :: ReactElement
view = withHooks do
  { value: foo, setValue: setFoo } <- useLocalStorage (HookName "Foo") "foo" ""
  pure $
    H.div ""
    [ H.h2 "" "Custom hook"
    , H.div "form-group"
      [ H.label_ "form-label" { htmlFor: "foo" }
        [ H.text "Typing here will update the state and save to "
        , H.code "" "localStorage"
        ]
      , H.input_ "form-control" { value: foo, onChange: setFoo <?| eventTargetValue, id: "foo" }
      ]
    ]

useLocalStorage :: HookName -> String -> String -> Hook { value :: String, setValue :: Dispatch String }
useLocalStorage (HookName name) key defaultValue = do
  { state, setState } <- useState (HookName $ name <> ".State") defaultValue
  useEffect (HookName $ name <> ".Effect") $ liftEffect do
    v <- getItem key =<< localStorage =<< window
    case v of
      Just v' -> setState v'
      Nothing -> setItem key defaultValue =<< localStorage =<< window
  pure
    { value: state
    , setValue: \v -> do
        setState v
        setItem key v =<< localStorage =<< window
    }

eventTargetValue :: Foreign -> Maybe String
eventTargetValue = toEvent >>> map _.target.value
  where
    toEvent :: Foreign -> Maybe { target :: { value :: String } }
    toEvent = readForeign
