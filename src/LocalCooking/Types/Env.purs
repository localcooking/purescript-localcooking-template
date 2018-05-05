module LocalCooking.Types.Env where

import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.AccessToken.Email (EmailToken)
import LocalCooking.Client.Dependencies.AuthToken (PreliminaryAuthToken)
import Facebook.State (FacebookLoginUnsavedFormData)

import Data.Maybe (Maybe)


type Env =
  { development :: Boolean
  , facebookClientID :: String
  , googleReCaptchaSiteKey :: String
  , emailToken :: Maybe EmailToken
  , authToken :: PreliminaryAuthToken
  , formData :: Maybe FacebookLoginUnsavedFormData
  , salt :: HashedPassword
  }
