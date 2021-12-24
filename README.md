# purescript-elmish-hooks

This library offers an analog of [React Hooks](https://reactjs.org/docs/hooks-intro.html) for use with [PureScript Elmish](https://github.com/collegevine/purescript-elmish).

### Hooks

Hooks allow introducing local state or effects without writing a new component. This library comes with two builtin hooks: `useState` and `useEffect`:

```purs
todos :: ReactElement
todos = withHooks do
  todos /\ setTodos <- useState []

  useEffect do
    todos <- API.fetchTodos
    liftEffect $ setTodos todos

  pure $
    H.fragment $ todoView <$> todos
```

### Custom Hooks

Custom hooks can also be created. One way is to build on other hooks using the `Hook` monad:

```purs
useLocalStorage :: String -> String -> Hook (String /\ Dispatch String)
useLocalStorage key defaultValue = do
  state /\ setState <- useState defaultValue

  useEffect $ liftEffect do
    window >>= localStorage >>= getItem key >>= case _ of
      Just v -> setState v
      Nothing -> setItem key defaultValue =<< localStorage =<< window

  pure $ state /\ \v -> do
    setState v
    setItem key v =<< localStorage =<< window
```

A more flexible approach, when that doesn’t work, is to use the `mkHook` function provided by this library:

```purs
useMousePosition :: String -> Hook (Maybe { x :: Number, y :: Number })
useMousePosition className =
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
  where
    name = genComponentName { skipFrames: 2 }
```

### Examples

There are some examples in the [examples](https://github.com/collegevine/purescript-elmish-hooks/tree/main/examples) folder, which can be seen live [here](https://collegevine.github.io/purescript-elmish-hooks).
