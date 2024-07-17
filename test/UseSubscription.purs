module Test.UseSubscription where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array ((:))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (delay)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Elmish.HTML.Styled as H
import Elmish.Hooks as Hk
import Elmish.Subscription (Subscription(..))
import Elmish.Test (find, testElement, text, (>>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "useSubscription" do
    it "calls the most current closure" do
      source <- liftAff AVar.empty

      let subscription = Subscription \dispatch -> do
            void $ forever do
              value <- AVar.take source
              liftEffect $ dispatch value
            pure $ pure unit

      let component = Hk.component Hk.do
            receivedValues /\ setReceivedValues <- Hk.useState []

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
            Hk.useSubscription subscription \value -> liftEffect $
              setReceivedValues $ value : receivedValues

            Hk.pure $
              H.div "" $ show receivedValues

          assertReceivedValues expected = do
            liftAff $ delay $ Milliseconds 10.0
            find "div" >> text >>= (_ `shouldEqual` show expected)

      testElement component do
        liftAff $ AVar.put "one" source
        assertReceivedValues ["one"]

        liftAff $ AVar.put "two" source
        assertReceivedValues ["two", "one"]

        liftAff $ AVar.put "third" source
        liftAff $ delay $ Milliseconds 1.0
        liftAff $ AVar.put "cuatro" source
        assertReceivedValues ["cuatro", "third", "two", "one"]
