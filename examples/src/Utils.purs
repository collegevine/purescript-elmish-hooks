module Utils
  ( eventTargetValue
  ) where

import Prelude

import Data.Maybe (Maybe)
import Elmish.Foreign (readForeign)
import Foreign (Foreign)

eventTargetValue :: Foreign -> Maybe String
eventTargetValue = toEvent >>> map _.target.value
  where
    toEvent :: Foreign -> Maybe { target :: { value :: String } }
    toEvent = readForeign
