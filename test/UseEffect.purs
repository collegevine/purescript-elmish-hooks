module Test.UseEffect where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Elmish ((<|))
import Elmish.HTML.Styled as H
import Elmish.Hooks as Hk
import Elmish.Test (click, find, testElement, text, (>>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "useEffect" do
    it "calls the most current closure" do
      let component = Hk.component Hk.do
            effectRuns /\ setEffectRuns <- Hk.useState 0
            clicks /\ setClicks <- Hk.useState 0

            -- This here is the tricky part. The second argument of `useEffect'`
            -- is a closure that captures `effectRuns`. If the same closure was
            -- called every time, the value of `effectRuns` would always be
            -- zero, so it would always be calling `setEffectRuns 1`. But if the
            -- the close from the most recent run is called, the value would be
            -- up to date.
            --
            -- To test this we use the second effect (two lines below) that
            -- writes the values of `clicks` and `effectRuns` to the `output`
            -- mutable cell, and then we check that, after a few button clicks,
            -- the cell has the right values.
            Hk.useEffect' clicks \_ -> liftEffect $
              setEffectRuns $ effectRuns + 1

            Hk.pure $ H.fragment
              [ H.div "clicks" $ show clicks
              , H.div "effectRuns" $ show effectRuns
              , H.button_ "" { onClick: setClicks <| clicks + 1 } ""
              ]

          assertOutput expected = do
            liftAff $ delay $ Milliseconds 10.0
            find ".clicks" >> text >>= (_ `shouldEqual` show expected.clicks)
            find ".effectRuns" >> text >>= (_ `shouldEqual` show expected.effectRuns)

      testElement component do
        find "button" >> click
        assertOutput { clicks: 1, effectRuns: 1 }

        find "button" >> click
        assertOutput { clicks: 2, effectRuns: 2 }

        find "button" >> click
        liftAff $ delay $ Milliseconds 1.0
        find "button" >> click
        assertOutput { clicks: 4, effectRuns: 4 }
