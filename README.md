# purescript-elmish-hooks

This library offers an analog of [React Hooks](https://reactjs.org/docs/hooks-intro.html) for use with [PureScript Elmish](https://github.com/collegevine/purescript-elmish).

```purs
view :: ReactElement
view = withHooks do
  visible /\ setVisible <- useState false
  pure $
    H.fragment
    [ H.button_ "" { onClick: setVisible $ not visible } "Toggle visibility"
    , if visible
        then H.div "" "Content"
        else H.empty
    ]
```

### Examples

There are some examples in the [examples](https://github.com/collegevine/purescript-elmish-hooks/tree/main/examples) folder, which can be seen live [here](https://collegevine.github.io/purescript-elmish-hooks).
