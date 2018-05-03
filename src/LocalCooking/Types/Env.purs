module LocalCooking.Types.Env where

import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Auth.Error (PreliminaryAuthToken)
import Facebook.State (FacebookLoginUnsavedFormData)

import Data.Maybe (Maybe)


type Env =
  { development :: Boolean
  , facebookClientID :: String
  , googleReCaptchaSiteKey :: String
  , authToken :: PreliminaryAuthToken
  , formData :: Maybe FacebookLoginUnsavedFormData
  , salt :: HashedPassword
  }
