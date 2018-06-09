module LocalCooking.Types.ServerToClient where

import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Common.AccessToken.Email (EmailToken)
import LocalCooking.Dependencies.AuthToken (PreliminaryAuthToken)
import LocalCooking.Spec.Types.Env (Env)
import Facebook.State (FacebookLoginUnsavedFormData)
import Facebook.Types (FacebookClientId)
import Google.ReCaptcha (ReCaptchaSiteKey)

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either (..))
import Data.Generic (class Generic, gShow)
import Data.Argonaut (Json, class DecodeJson, decodeJson, (.?))
import Data.Typelevel.Undefined (undefined)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Console (error)


newtype ServerToClient = ServerToClient
  { development            :: Boolean
  , facebookClientId       :: FacebookClientId
  , googleReCaptchaSiteKey :: ReCaptchaSiteKey
  , emailToken             :: Maybe EmailToken
  , authToken              :: PreliminaryAuthToken
  , formData               :: Maybe FacebookLoginUnsavedFormData
  , salt                   :: HashedPassword
  }

derive instance genericServerToClient :: Generic ServerToClient

instance showServerToClient :: Show ServerToClient where
  show = gShow


instance decodeJsonServerToClient :: DecodeJson ServerToClient where
  decodeJson json = do
    o <- decodeJson json
    development <- o .? "development"
    facebookClientId <- o .? "facebookClientId"
    googleReCaptchaSiteKey <- o .? "googleReCaptchaSiteKey"
    emailToken <- o .? "emailToken"
    authToken <- o .? "authToken"
    formData <- o .? "formData"
    salt <- o .? "salt"
    pure $ ServerToClient
      { development
      , facebookClientId
      , googleReCaptchaSiteKey
      , emailToken
      , authToken
      , formData
      , salt
      }


foreign import serverToClientImpl :: Json

serverToClient :: ServerToClient
serverToClient = unsafePerformEff $ case decodeJson serverToClientImpl of
  Right x -> pure x
  Left e -> undefined <$ error e


env :: Env
env = case serverToClient of
  ServerToClient {development,facebookClientId,googleReCaptchaSiteKey,salt} ->
    { development
    , facebookClientId
    , googleReCaptchaSiteKey
    , salt
    }
