# purescript-elmish-hooks

This library offers an analog of [React Hooks](https://reactjs.org/docs/hooks-intro.html) for use with [PureScript Elmish](https://github.com/collegevine/purescript-elmish).

```purs
view :: ReactElement
view = withHooks do
  visible /\ setVisible <- useState (HookName "ContentVisible") false
  pure $
    H.fragment
    [ H.button_ "" { onClick: setVisible $ not visible } "Toggle visibility"
    , if visible
        then H.div "" "Content"
        else H.empty
    ]
```
