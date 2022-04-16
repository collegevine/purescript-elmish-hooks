module Examples.UseRef
  ( view
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Elmish (EffectFn1, ReactElement)
import Elmish.HTML.Styled as H
import Elmish.Hooks (Hook, UseRef, withHooks)
import Elmish.Hooks as Hooks
import Elmish.Hooks.UseRef (useRef)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.HTMLElement (focus)
import Web.HTML.HTMLInputElement (HTMLInputElement)
import Web.HTML.HTMLInputElement as HTMLInputElement

view :: ReactElement
view = withHooks Hooks.do
  inputEl /\ setInputEl <- useRef
  let onButtonClick = traverse_ (focus <<< HTMLInputElement.toHTMLElement) inputEl
  Hooks.pure $
    H.div "row mt-3"
    [ H.div "col-12 col-md-6 col-lg-4"
      [ H.h2 ""
        [ H.code "" "useRef"
        , H.text " hook"
        ]
      , H.div "row"
        [ H.div "col" $
            H.input_ "form-control" { ref: setInputEl, defaultValue: "" }
        , H.div "col-auto" $
            H.button_ "btn btn-primary" { onClick: onButtonClick } "Focus the input"
        ]
      ]
    ]
