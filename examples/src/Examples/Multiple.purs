module Examples.Multiple
  ( view
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Elmish.Hooks (HookName(..), useEffect, useState, withHooks)

view :: ReactElement
view = withHooks do
  { state: todos, setState: setTodos } <- useState (HookName "Todos") Nothing

  useEffect (HookName "FetchTodos") do
    delay $ Milliseconds 2000.0
    liftEffect $ setTodos $ Just ["Do thing", "Do another thing", "Some more stuff"]

  pure $
    H.div "row"
    [ H.div "col-12 col-md-6 col-lg-4"
      [ H.h2 ""
        [ H.code "" "useEffect"
        , H.text " hook"
        ]
      , H.h4 "mb-3" "Todos"
      , case todos of
          Nothing ->
            H.div ""
            [ H.div "progress" $
                H.div_ "progress-bar progress-bar-striped progress-bar-animated" { role: "progressbar", style: H.css { width: "100%" } } H.empty
            , H.div "mt-2" "Loading todos…"
            ]
          Just todos' ->
            H.div "ul" $
              H.li "" <$> todos'
      ]
    ]
