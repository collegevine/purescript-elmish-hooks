module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Elmish.Enzyme (childAt, find, name, testElement, (>>))
import Elmish.Enzyme as Enzyme
import Elmish.Enzyme.Adapter as Adapter
import Elmish.HTML.Styled as H
import Elmish.Hooks (useState, withHooks, (=/>), (==>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Assertions.String (shouldContain)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (runSpec)

foreign import _configureJsDomViaFfi :: Type

main :: Effect Unit
main = do
  Enzyme.configure Adapter.unofficialReact_17
  launchAff_ $ runSpec [specReporter] spec

spec :: Spec Unit
spec = do
  describe "naming" do
    let
      component = do
        _ <- useState ""
        pure $ H.div "" "content"

      withHooksComponent = withHooks component

      wrappedComponent = H.div ""
        [ H.div "with-hooks-1-parent" $ withHooks component
        , H.div "with-hooks-2-parent" $ withHooks component
        , H.div "with-hooks-3-parent" withHooksComponent
        , H.div "with-hooks-4-parent" withHooksComponent
        , H.div "with-hooks-5-parent" $ withHooks do
            _ <- useState ""
            pure $ H.div "" "content"
        , H.div "with-hooks-6-parent" $ withHooks do
            _ <- useState ""
            pure $ H.div "" "content"
        ]

    describe "withHooks" do
      it "has a unique name when used twice" $
        testElement wrappedComponent do
          withHooks1Name <- find ".with-hooks-1-parent" >> childAt 0 >> name
          withHooks2Name <- find ".with-hooks-2-parent" >> childAt 0 >> name
          withHooks1Name `shouldContain` "WithHooks"
          withHooks2Name `shouldContain` "WithHooks"
          withHooks1Name `shouldNotEqual` withHooks2Name

      it "has the same when same reference used twice" $
        testElement wrappedComponent do
          withHooks3Name <- find ".with-hooks-3-parent" >> childAt 0 >> name
          withHooks4Name <- find ".with-hooks-4-parent" >> childAt 0 >> name
          withHooks3Name `shouldContain` "WithHooks"
          withHooks3Name `shouldEqual` withHooks4Name

    describe "useState" do
      it "has a unique name when used twice" $
        testElement wrappedComponent do
          useState5Name <- find ".with-hooks-5-parent" >> childAt 0 >> childAt 0 >> name
          useState6Name <- find ".with-hooks-6-parent" >> childAt 0 >> childAt 0 >> name
          useState5Name `shouldContain` "UseState"
          useState6Name `shouldContain` "UseState"
          useState5Name `shouldNotEqual` useState6Name

      it "has the same when same reference used twice" $
        testElement wrappedComponent do
          useState1Name <- find ".with-hooks-1-parent" >> childAt 0 >> childAt 0 >> name
          useState2Name <- find ".with-hooks-2-parent" >> childAt 0 >> childAt 0 >> name
          useState1Name `shouldContain` "UseState"
          useState1Name `shouldEqual` useState2Name

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
          withHooks1Name <- find ".with-hook-1-parent" >> childAt 0 >> name
          withHooks2Name <- find ".with-hook-2-parent" >> childAt 0 >> name
          withHooks1Name `shouldContain` "WithHook"
          withHooks2Name `shouldContain` "WithHook"
          withHooks1Name `shouldNotEqual` withHooks2Name

      it "has the same when same reference used twice" $
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
          withHooks1Name <- find ".with-hook-1-parent" >> childAt 0 >> name
          withHooks2Name <- find ".with-hook-2-parent" >> childAt 0 >> name
          withHooks1Name `shouldContain` "WithHookCurried"
          withHooks2Name `shouldContain` "WithHookCurried"
          withHooks1Name `shouldNotEqual` withHooks2Name

      it "has the same when same reference used twice" $
        testElement wrappedWithHookComponent do
          withHook3Name <- find ".with-hook-3-parent" >> childAt 0 >> name
          withHook4Name <- find ".with-hook-4-parent" >> childAt 0 >> name
          withHook3Name `shouldContain` "WithHookCurried"
          withHook3Name `shouldEqual` withHook4Name
