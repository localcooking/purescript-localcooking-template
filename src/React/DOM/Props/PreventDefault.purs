module React.DOM.Props.PreventDefault where

import Prelude (bind, pure, unit, Unit)
import React (Event, EventHandlerContext)
import Unsafe.Coerce (unsafeCoerce)


preventDefault :: forall eff props state
                . Event -> EventHandlerContext eff props state Unit
preventDefault e = do
  (_ :: Unit) <- (unsafeCoerce e).stopPropagation
  (_ :: Unit) <- (unsafeCoerce e).nativeEvent.stopImmediatePropagation
  (_ :: Unit) <- (unsafeCoerce e).preventDefault
  pure unit
