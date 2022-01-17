module Examples.UseEffect
  ( view
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Elmish.Hooks (useEffect, useState, withHooks)
import Elmish.Hooks as Hooks

view :: ReactElement
view = withHooks Hooks.do
  todos /\ setTodos <- useState Nothing

  useEffect do
    delay $ Milliseconds 2000.0
    liftEffect $ setTodos $ Just ["Do thing", "Do another thing", "Some more stuff"]

  _ <-
    if true then Hooks.do
      x <- useState 0
      Hooks.pure x
    else
      useState 0

  Hooks.pure $
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
