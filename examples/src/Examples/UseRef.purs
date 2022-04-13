module Examples.UseRef
  ( view
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Tuple.Nested ((/\))
import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Elmish.Hooks (withHooks)
import Elmish.Hooks as Hooks
import Elmish.Hooks.UseRef (useRef)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.HTMLElement (focus)

view :: ReactElement
view = withHooks Hooks.do
  inputEl /\ setInputEl <- useRef
  let onButtonClick = traverse_ focus inputEl
  Hooks.pure $
    H.div "row mt-3"
    [ H.div "col-12 col-md-6 col-lg-4"
      [ H.h2 ""
        [ H.code "" "useRef"
        , H.text " hook"
        ]
      , H.div "row"
        [ H.div "col" $
            H.input_ "form-control" { ref: unsafeCoerce setInputEl, defaultValue: "" }
        , H.div "col-auto" $
            H.button_ "btn btn-primary" { onClick: onButtonClick } "Focus the input"
        ]
      ]
    ]
