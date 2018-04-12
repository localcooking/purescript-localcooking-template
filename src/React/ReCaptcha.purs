module React.ReCaptcha
  ( reCaptcha
  ) where

import Prelude (Unit)
import Google.ReCaptcha (ReCaptchaResponse)
import React (ReactClass, ReactElement, createElement)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1)


foreign import reCaptchaImpl :: forall props. ReactClass props

type ReCaptchaProps eff =
  { sitekey :: String
  , verifyCallback :: EffFn1 eff ReCaptchaResponse Unit
  , onloadCallback :: Eff eff Unit
  }


reCaptcha :: forall eff
           . ReCaptchaProps eff -> ReactElement
reCaptcha ps = createElement reCaptchaImpl ps []
