module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array (head, singleton)
import Data.Foldable (indexl)
import Data.Maybe (Maybe, fromMaybe)
import Data.Nullable as N
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Elmish (ReactElement)
import Elmish.Foreign (readForeign)
import Elmish.HTML.Styled as H
import Elmish.Hooks (useEffect, useState, (=/>), (==>))
import Elmish.Hooks as Hooks
import Elmish.Hooks.UseEffect (useEffect')
import Foreign (unsafeFromForeign, unsafeToForeign)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Assertions.String (shouldContain)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (runSpec)
import Test.UseEffect as UseEffect
import Test.UseSubscription as UseSubscription

main :: Effect Unit
main = launchAff_ $ runSpec [specReporter] spec

spec :: Spec Unit
spec = do
  UseEffect.spec
  UseSubscription.spec

  describe "naming" do
    describe "component" do
      let
        component = Hooks.do
          _ <- useState ""
          Hooks.pure $ H.div "" "content"

        hooksComponent = Hooks.component component

        wrappedComponent = H.div ""
          [ H.div "" $ Hooks.component component
          , H.div "" $ Hooks.component component
          , H.div "" hooksComponent
          , H.div "" hooksComponent
          ]

        nthComponentName = nameOfNthChildsChild wrappedComponent

      it "has a unique name when used twice" do
        nthComponentName 0 `shouldContain` "HooksComponent"
        nthComponentName 1 `shouldContain` "HooksComponent"
        nthComponentName 0 `shouldNotEqual` nthComponentName 1

      it "has the same name when same reference used twice" do
        nthComponentName 2 `shouldContain` "HooksComponent"
        nthComponentName 2 `shouldEqual` nthComponentName 3

    describe "withHook" do
      let
        view _ = H.div "" "withHookComponent"
        hook = useState ""
        withHookComponent = hook ==> view

        wrappedWithHookComponent = H.div ""
          [ H.div "" $ hook ==> view
          , H.div "" $ hook ==> view
          , H.div "" withHookComponent
          , H.div "" withHookComponent
          ]

        nthComponentName = nameOfNthChildsChild wrappedWithHookComponent

      it "has a unique name when used twice" do
        nthComponentName 0 `shouldContain` "WithHook"
        nthComponentName 1 `shouldContain` "WithHook"
        nthComponentName 0 `shouldNotEqual` nthComponentName 1

      it "has the same name when same reference used twice" do
        nthComponentName 2 `shouldContain` "WithHook"
        nthComponentName 2 `shouldEqual` nthComponentName 3

    describe "withHookCurried" do
      let
        view _ _ = H.div "" "withHookComponent"
        hook = useState ""
        withHookComponent = hook =/> view

        wrappedWithHookComponent = H.div ""
          [ H.div "" $ hook =/> view
          , H.div "" $ hook =/> view
          , H.div "" withHookComponent
          , H.div "" withHookComponent
          ]

        nthComponentName = nameOfNthChildsChild wrappedWithHookComponent

      it "has a unique name when used twice" do
        nthComponentName 0 `shouldContain` "WithHookCurried"
        nthComponentName 1 `shouldContain` "WithHookCurried"
        nthComponentName 0 `shouldNotEqual` nthComponentName 1

      it "has the same name when same reference used twice" do
        nthComponentName 2 `shouldContain` "WithHookCurried"
        nthComponentName 2 `shouldEqual` nthComponentName 3

    describe "ComposedHookTypes" do
      it "is associative" do
        let
          hook1 = Hooks.do
            Hooks.do
              Hooks.do
                _ <- useState ""
                Hooks.do
                  useEffect $ pure unit
                  useEffect' 0 $ const $ pure unit
              _ <- useState false
              useEffect $ pure unit
            useEffect' "" $ const $ pure unit
            Hooks.do
              _ <- useState 0
              useEffect $ pure unit

          hook2 = Hooks.do
            _ <- useState ""
            useEffect $ pure unit
            useEffect' 0 $ const $ pure unit
            _ <- useState false
            useEffect $ pure unit
            useEffect' "" $ const $ pure unit
            _ <- useState 0
            useEffect $ pure unit

        assertSameType hook1 hook2

      it "has an identity" do
        let
          hook1 = Hooks.do
            x <- useState ""
            Hooks.pure x

          hook2 = useState ""

        assertSameType hook1 hook2

assertSameType :: forall a. a -> a -> Aff Unit
assertSameType _ _ = pure unit

componentName :: ReactElement -> Maybe String
componentName e =
  unsafeToForeign e
  # (readForeign :: _ -> _ { type :: { displayName :: _ } })
  <#> _.type.displayName

componentChildren :: ReactElement -> Array ReactElement
componentChildren = unsafeToForeign >>> \f -> manyChildren f <|> singleChild f # fromMaybe []
  where
    manyChildren f =
      (readForeign f :: _ { props :: { children :: _ } })
      <#> _.props.children
      >>= N.toMaybe
      <#> map (unsafeFromForeign :: _ -> ReactElement)

    singleChild f =
      (readForeign f :: _ { props :: { children :: _ } })
      <#> _.props.children
      >>= N.toMaybe
      <#> (unsafeFromForeign :: _ -> ReactElement)
      <#> singleton

-- Name of the child of the Nth child of the given div.
-- For example, for when the given div is this:
--
--      <div>
--        <a><b></b></a>
--        <i><a></a></i>
--      </div>
--
--     * nameOfNthChildsChild div 0 == "b"   - first child's child
--     * nameOfNthChildsChild div 1 == "a"   - second child's child
--     * nameOfNthChildsChild div 2 == ""    - there is no third child
--
nameOfNthChildsChild :: ReactElement -> Int -> String
nameOfNthChildsChild div n = fromMaybe "" do
  nthChild <- div # componentChildren # indexl n
  grandchild <- nthChild # componentChildren # head
  componentName grandchild
