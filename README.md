# purescript-elmish-hooks

This library offers an analog of [React Hooks](https://reactjs.org/docs/hooks-intro.html) for use with [PureScript Elmish](https://github.com/collegevine/purescript-elmish).

### Getting Started

To use this library, install `elmish-hooks`, as well as the npm package [stacktrace-parser](https://github.com/errwischt/stacktrace-parser).

```
npx spago install elmish-hooks
npm install stacktrace-parser --save
```

### Hooks

Hooks allow introducing local state or effects without writing a new `ComponentDef`. This library comes with three builtin hooks: `useState`, `useEffect`, and `useRef`. Here’s what hooks look like in practice:

```purs
todos :: ReactElement
todos = Hooks.component Hooks.do
  todos /\ setTodos <- useState []

  useEffect do
    todos <- API.fetchTodos
    liftEffect $ setTodos todos

  Hooks.pure $
    H.fragment $ todoView <$> todos
```

### Custom Hooks

Custom hooks can also be created. One way is to build on other hooks using the `Hook` monad:

```purs
type UseLocalStorage t = UseState String <> UseEffect Unit <> t

useLocalStorage :: String -> String -> Hook UseLocalStorage (String /\ Dispatch String)
useLocalStorage key defaultValue = Hooks.do
  state /\ setState <- useState defaultValue

  useEffect $ liftEffect do
    window >>= localStorage >>= getItem key >>= case _ of
      Just v -> setState v
      Nothing -> setItem key defaultValue =<< localStorage =<< window

  Hooks.pure $ state /\ \v -> do
    setState v
    setItem key v =<< localStorage =<< window
```

A more flexible approach, when that doesn’t work, is to use the `mkHook` function provided by this library:

```purs
foreign import data :: UseMousePosition :: HookType

useMousePosition :: String -> Hook UseMousePosition (Maybe { x :: Number, y :: Number })
useMousePosition className =
  mkHook (ComponentName "UseMousePosition") \render ->
    { init: pure Nothing
    , update: \_ pos -> pure pos
    , view: \pos dispatch ->
        H.div_ className
          { onMouseMove: E.handleEffect \(E.MouseEvent event) -> do
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
```

### Continuation-Passing Style

If you’re only using a single hook, sometimes it might be more concise to use CPS via the `==>` or `=/>` operators.

```purs
myInput :: ReactElement
myInput = useState "" =/> \name setName ->
  H.input_ "" { value: name, onChange: setName <| E.inputText }
```

### Examples

There are some examples in the [examples](https://github.com/collegevine/purescript-elmish-hooks/tree/main/examples) folder, which can be seen live [here](https://collegevine.github.io/purescript-elmish-hooks).

### Documentation

Documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-elmish-hooks).
