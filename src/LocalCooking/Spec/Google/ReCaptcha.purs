module LocalCooking.Spec.Google.ReCaptcha where

import Google.ReCaptcha (ReCaptchaResponse)
import LocalCooking.Types.Env (Env)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Text.Email.Validate (EmailAddress, emailAddress)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)

import Thermite as T
import React as R
import React.DOM as R
import React.ReCaptcha as RG

import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.TextField (textField)

import Unsafe.Coerce (unsafeCoerce)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue (READ)
import IxQueue (IxQueue)
import IxQueue as IxQueue


type State = Unit

initialState :: State
initialState = unit

data Action
  = ChangedReCaptcha (Maybe ReCaptchaResponse)

type Effects eff =
  ( ref :: REF
  | eff)

spec :: forall eff
      . { reCaptchaSignal :: IxSignal (Effects eff) (Maybe ReCaptchaResponse)
        , env :: Env
        }
     -> T.Spec (Effects eff) State Unit Action
spec {reCaptchaSignal,env} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedReCaptcha x -> liftEff (IxSignal.set x reCaptchaSignal)

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ RG.reCaptcha
        { sitekey: env.googleReCaptchaSiteKey
        , verifyCallback: mkEffFn1 (dispatch <<< ChangedReCaptcha <<< Just)
        , onloadCallback: pure unit
        }
      ]


reCaptcha :: forall eff
           . { reCaptchaSignal :: IxSignal (Effects eff) (Maybe ReCaptchaResponse)
             , env :: Env
             }
          -> R.ReactElement
reCaptcha params =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec (spec params) initialState
  in  R.createElement (R.createClass reactSpec) unit []
