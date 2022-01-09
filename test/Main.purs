module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Elmish.Enzyme (childAt, find, name, spy, testElement, trace, (>>))
import Elmish.Enzyme as Enzyme
import Elmish.Enzyme.Adapter as Adapter
import Elmish.HTML.Styled as H
import Elmish.Hooks (useState, withHooks)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldNotEqual)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (runSpec)

foreign import _configureJsDomViaFfi :: Type

main :: Effect Unit
main = do
  Enzyme.configure Adapter.unofficialReact_17
  launchAff_ $ runSpec [specReporter] spec

spec :: Spec Unit
spec = do
  describe "withHooks" $
    it "has a unique name when used twice" do
      let
        component = do
          _ <- useState ""
          pure H.empty

        wrappedComponent = H.div ""
          [ H.div "with-hooks-1-parent" $
              withHooks component
          , H.div "with-hooks-2-parent" $
              withHooks component
          ]

      testElement wrappedComponent do
        withHooks1Name <- spy >> find ".with-hooks-1-parent" >> childAt 0 >> spy >> name
        withHooks2Name <- find ".with-hooks-2-parent" >> childAt 0 >> spy >> name
        withHooks1Name `shouldNotEqual` withHooks2Name
