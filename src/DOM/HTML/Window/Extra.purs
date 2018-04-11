module DOM.HTML.Window.Extra (onPopStateImpl, queryParams) where

import Prelude
import Data.StrMap (StrMap)
import Data.Foreign (Foreign)
import DOM.HTML.Types (Window, Location)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2)


foreign import onPopStateImpl :: forall eff. EffFn2 eff (EffFn1 eff Foreign Unit) Window Unit

foreign import queryParams :: Location -> StrMap String
