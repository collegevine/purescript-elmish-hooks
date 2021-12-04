module Examples.UseState
  ( view
  ) where

import Prelude

import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Elmish.Hooks (HookName(..), useState, withHooks)

view :: ReactElement
view = withHooks do
  { state: visible, setState: setVisible } <- useState (HookName "ModalVisible") false
  pure $
    H.div ""
    [ H.h2 ""
      [ H.code "" "useState"
      , H.text " hook"
      ]
    , H.button_ "btn btn-primary" { onClick: setVisible $ not visible } "Show"
    , if visible then
        H.fragment
        [ H.div_ "modal fade show d-block" { style: H.css { pointerEvents: "none" } } $
            H.div "modal-dialog" $
              H.div "modal-content" $
                H.div "modal-body"
                [ H.div "row"
                  [ H.div "col" $
                      H.h3 "header-title" "Modal"
                  , H.div "col-auto" $
                      H.button_ "btn btn-icon btn-lg p-0 text-muted" { onClick: setVisible false } "×"
                  ]
                , H.div "py-4" "Content"
                ]
        , H.div_ "modal-backdrop fade show" { onClick: setVisible false } H.empty
        ]
      else
        H.empty
    ]
