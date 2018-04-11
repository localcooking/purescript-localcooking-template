module LocalCooking.Types.Env where

import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Auth.Error (PreliminaryAuthToken)


type Env =
  { development :: Boolean
  , facebookClientID :: String
  , googleReCaptchaSiteKey :: String
  , authToken :: PreliminaryAuthToken
  , salt :: HashedPassword
  }
