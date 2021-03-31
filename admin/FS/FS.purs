module FS where

import Prelude
import Effect (Effect)

foreign import replace :: forall a. a -> Effect Unit
