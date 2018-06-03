module LocalCooking.Types.Env where

import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Common.AccessToken.Email (EmailToken)
import LocalCooking.Dependencies.AuthToken (PreliminaryAuthToken)
import Facebook.State (FacebookLoginUnsavedFormData)
import Facebook.Types (FacebookClientId)
import Google.ReCaptcha (ReCaptchaSiteKey)

import Data.Maybe (Maybe)


type Env =
  { development            :: Boolean
  , facebookClientId       :: FacebookClientId
  , googleReCaptchaSiteKey :: ReCaptchaSiteKey
  , emailToken             :: Maybe EmailToken
  , authToken              :: PreliminaryAuthToken
  , formData               :: Maybe FacebookLoginUnsavedFormData
  , salt                   :: HashedPassword
  }
