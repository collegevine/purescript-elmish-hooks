module Examples.UseRef
  ( view
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Elmish.Hooks (withHooks)
import Elmish.Hooks.UseRef (readRef, useRef)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.HTMLElement (focus)

view :: ReactElement
view = withHooks do
  inputEl <- useRef Nothing
  let onButtonClick = traverse_ focus =<< readRef inputEl
  pure $
    H.div "row mt-3"
    [ H.div "col-12 col-md-6 col-lg-4"
      [ H.h2 ""
        [ H.code "" "useRef"
        , H.text " hook"
        ]
      , H.div "row"
        [ H.div "col" $
            H.input_ "form-control" { ref: unsafeCoerce inputEl, defaultValue: "" }
        , H.div "col-auto" $
            H.button_ "btn btn-primary" { onClick: onButtonClick } "Focus the input"
        ]
      ]
    ]
