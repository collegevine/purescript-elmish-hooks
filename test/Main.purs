module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Elmish.Enzyme (childAt, find, name, testElement, (>>))
import Elmish.Enzyme as Enzyme
import Elmish.Enzyme.Adapter as Adapter
import Elmish.HTML.Styled as H
import Elmish.Hooks (useEffect, useState, (=/>), (==>))
import Elmish.Hooks as Hooks
import Elmish.Hooks.UseEffect (useEffect')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Assertions.String (shouldContain)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (runSpec)

foreign import _configureJsDomViaFfi :: Type

main :: Effect Unit
main = launchAff_ do
  adapter <- Adapter.unofficialReact_17
  liftEffect $ Enzyme.configure adapter
  runSpec [specReporter] spec

spec :: Spec Unit
spec = do
  describe "naming" do
    let
      component = Hooks.do
        _ <- useState ""
        Hooks.pure $ H.div "" "content"

      hooksComponent = Hooks.component component

      wrappedComponent = H.div ""
        [ H.div "component-1-parent" $ Hooks.component component
        , H.div "component-2-parent" $ Hooks.component component
        , H.div "component-3-parent" hooksComponent
        , H.div "component-4-parent" hooksComponent
        ]

    describe "component" do
      it "has a unique name when used twice" $
        testElement wrappedComponent do
          component1Name <- find ".component-1-parent" >> childAt 0 >> name
          component2Name <- find ".component-2-parent" >> childAt 0 >> name
          component1Name `shouldContain` "HooksComponent"
          component2Name `shouldContain` "HooksComponent"
          component1Name `shouldNotEqual` component2Name

      it "has the same name when same reference used twice" $
        testElement wrappedComponent do
          component3Name <- find ".component-3-parent" >> childAt 0 >> name
          component4Name <- find ".component-4-parent" >> childAt 0 >> name
          component3Name `shouldContain` "HooksComponent"
          component3Name `shouldEqual` component4Name

    describe "withHook" do
      let
        view _ = H.div "" "withHookComponent"
        hook = useState ""
        withHookComponent = hook ==> view

        wrappedWithHookComponent = H.div ""
          [ H.div "with-hook-1-parent" $ hook ==> view
          , H.div "with-hook-2-parent" $ hook ==> view
          , H.div "with-hook-3-parent" withHookComponent
          , H.div "with-hook-4-parent" withHookComponent
          ]

      it "has a unique name when used twice" $
        testElement wrappedWithHookComponent do
          withHook1Name <- find ".with-hook-1-parent" >> childAt 0 >> name
          withHook2Name <- find ".with-hook-2-parent" >> childAt 0 >> name
          withHook1Name `shouldContain` "WithHook"
          withHook2Name `shouldContain` "WithHook"
          withHook1Name `shouldNotEqual` withHook2Name

      it "has the same name when same reference used twice" $
        testElement wrappedWithHookComponent do
          withHook3Name <- find ".with-hook-3-parent" >> childAt 0 >> name
          withHook4Name <- find ".with-hook-4-parent" >> childAt 0 >> name
          withHook3Name `shouldContain` "WithHook"
          withHook3Name `shouldEqual` withHook4Name

    describe "withHookCurried" do
      let
        view _ _ = H.div "" "withHookComponent"
        hook = useState ""
        withHookComponent = hook =/> view

        wrappedWithHookComponent = H.div ""
          [ H.div "with-hook-1-parent" $ hook =/> view
          , H.div "with-hook-2-parent" $ hook =/> view
          , H.div "with-hook-3-parent" withHookComponent
          , H.div "with-hook-4-parent" withHookComponent
          ]

      it "has a unique name when used twice" $
        testElement wrappedWithHookComponent do
          withHook1Name <- find ".with-hook-1-parent" >> childAt 0 >> name
          withHook2Name <- find ".with-hook-2-parent" >> childAt 0 >> name
          withHook1Name `shouldContain` "WithHookCurried"
          withHook2Name `shouldContain` "WithHookCurried"
          withHook1Name `shouldNotEqual` withHook2Name

      it "has the same name when same reference used twice" $
        testElement wrappedWithHookComponent do
          withHook3Name <- find ".with-hook-3-parent" >> childAt 0 >> name
          withHook4Name <- find ".with-hook-4-parent" >> childAt 0 >> name
          withHook3Name `shouldContain` "WithHookCurried"
          withHook3Name `shouldEqual` withHook4Name

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
