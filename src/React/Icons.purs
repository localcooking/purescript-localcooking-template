module React.Icons where

import Prelude (unit)
import React (ReactElement, ReactClass, createElement)


foreign import facebookImpl :: forall props. ReactClass props

facebookIcon :: ReactElement
facebookIcon = createElement facebookImpl unit []


foreign import twitterImpl :: forall props. ReactClass props

twitterIcon :: ReactElement
twitterIcon = createElement twitterImpl unit []


foreign import googleImpl :: forall props. ReactClass props

googleIcon :: ReactElement
googleIcon = createElement googleImpl unit []


