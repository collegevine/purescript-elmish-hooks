module Examples.UseMouseMove
  ( view
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Elmish (ReactElement, mkEffectFn1, (<|))
import Elmish.HTML.Styled as H
import Elmish.Hooks (Hook, HookName(..), mkHook, withHooks)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.HTMLElement (HTMLElement, getBoundingClientRect)

view :: ReactElement
view =
  H.div "mt-3 mb-2"
  [ H.h4 "" $
      H.code "" "useMousePosition"
  , H.p "text-muted"
    [ H.text "This example uses "
    , H.code "" "mkHook"
    , H.text " to make a custom hook."
    ]
  , H.div_ "w-100 py-6 rounded bg-light border position-relative"
      { style: H.css { height: 200, cursor: "none" } }$
        withHooks do
          pos <- useMousePosition (HookName "Examples.UseMouseMove.mousePosition") "position-absolute h-100 w-100"
          pure $ case pos of
            Just { x, y } ->
              H.img_ "position-absolute"
                { src: "data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0idXRmLTgiPz4KPCFET0NUWVBFIHN2ZyBQVUJMSUMgIi0vL1czQy8vRFREIFNWRyAxLjAvL0VOIiAiaHR0cDovL3d3dy53My5vcmcvVFIvMjAwMS9SRUMtU1ZHLTIwMDEwOTA0L0RURC9zdmcxMC5kdGQiPgo8c3ZnIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgeG1sbnM6eGxpbms9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkveGxpbmsiIGZpbGw9IndoaXRlIiB2ZXJzaW9uPSIxLjAiIHg9IjBweCIgeT0iMHB4IiB3aWR0aD0iMjAwcHgiIGhlaWdodD0iMTI4cHgiIHZpZXdCb3g9IjAgMCAxMDAgNjQiIGVuYWJsZS1iYWNrZ3JvdW5kPSJuZXcgMCAwIDEwMCA2NCIgeG1sOnNwYWNlPSJwcmVzZXJ2ZSIgc3R5bGU9IiI+Cjxwb2x5Z29uIHBvaW50cz0iNjUuNjE5LDUwLjI0MSA1OS4xNTYsNDQuMjA2IDMwLjc3NSw0NC4yMDYgMzcuMjM4LDUwLjI0MSAiIHN0eWxlPSImIzEwOyAgICBmaWxsOiAjNUVCNUNBOyYjMTA7ICAgIHN0cm9rZTogIzVFQjVDQTsmIzEwOyIvPgo8cG9seWdvbiBwb2ludHM9IjM3LjIzOCwyOS4wODUgMzAuNzc1LDM1LjEyIDU5LjE1NiwzNS4xMiA2NS42MTksMjkuMDg1ICIgc3R5bGU9IiYjMTA7ICAgIGZpbGw6ICNFRkFDMDA7JiMxMDsgICAgc3Ryb2tlOiAjRUZBQzAwOyYjMTA7Ii8+Cjxwb2x5Z29uIHBvaW50cz0iNjUuNjE3LDE5Ljk5NyA1OS4xNTYsMTMuOTYgMzAuNzc3LDEzLjk2IDM3LjIzOCwxOS45OTcgIiBzdHlsZT0iJiMxMDsgICAgZmlsbDogIzdFRDAzQTsmIzEwOyAgICBzdHJva2U6ICM3RUQwM0E7JiMxMDsiLz4KPHBhdGggZD0iTTI3Ljc4OSwyNS45N2wtNC4yNy00LjI3MUw3LjY4OSwzNy41M2MtMC41NjgsMC41NjctMC44ODIsMS4zMjgtMC44OCwyLjEzNGMwLDAuODA4LDAuMzEyLDEuNTY1LDAuODgsMi4xMzMgIGwxNS44MywxNS44M2w0LjI3LTQuMjY3TDE0LjA5NCwzOS42NjNMMjcuNzg5LDI1Ljk3eiIgc3R5bGU9IiYjMTA7ICAgIGZpbGw6ICM1OTYzNzY7JiMxMDsgICAgc3Ryb2tlOiAjNTk2Mzc2OyYjMTA7Ii8+CjxwYXRoIGQ9Ik04OC43MDUsMjIuNDA3TDcyLjg4MSw2LjU3NWwtNC4yNjgsNC4yNjlMODIuMzAxLDI0LjU0TDY4LjYxMywzOC4yMzVsNC4yNjgsNC4yNjlsMTUuODI0LTE1LjgyNyAgYzAuNTctMC41NzIsMC44ODUtMS4zMzEsMC44ODUtMi4xMzlDODkuNTg4LDIzLjczMSw4OS4yNzUsMjIuOTc2LDg4LjcwNSwyMi40MDciIHN0eWxlPSImIzEwOyAgICBmaWxsOiAjNTk2Mzc2OyYjMTA7ICAgIHN0cm9rZTogIzU5NjM3NjsmIzEwOyIvPgo8L3N2Zz4="
                , style: H.css { height: "1.25rem", top: y, left: x }
                }
            Nothing ->
              H.empty
  ]

useMousePosition :: HookName -> String -> Hook (Maybe { x :: Number, y :: Number })
useMousePosition name className =
  mkHook name \render ->
    { init: pure Nothing
    , update: \_ pos -> pure pos
    , view: \pos dispatch ->
        H.div_ className
          { onMouseMove: unsafeCoerce $ mkEffectFn1 \(event :: { clientX :: Number, clientY :: Number, currentTarget :: HTMLElement }) -> do
              { top, left, width, height } <- getBoundingClientRect event.currentTarget
              let
                x = event.clientX - left
                y = event.clientY - top
                mouseLeft = x < 0.0 || y < 0.0 || y > height || x > width
              dispatch if mouseLeft then Nothing else Just { x, y }
          , onMouseLeave: dispatch <| const Nothing
          } $
          render pos
    }
